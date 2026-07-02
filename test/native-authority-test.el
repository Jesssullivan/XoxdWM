;;; native-authority-test.el --- Native WM authority guardrails -*- lexical-binding: t; -*-

;;; Commentary:
;; Static tests for migration gates that keep XoxdWM native-first and keep
;; Emacs/eGreg in the application/control-client layer.

;;; Code:

(require 'ert)

(defconst native-authority-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun native-authority-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative native-authority-test--root))
    (buffer-string)))

(defun native-authority-test--section (content heading)
  "Return CONTENT section that starts with HEADING."
  (when (string-match (concat "^" (regexp-quote heading) "\n") content)
    (let ((start (match-end 0)))
      (if (string-match "^%\\(files\\|package\\|description\\|post\\|preun\\|postun\\)\\b"
                        content start)
          (substring content start (match-beginning 0))
        (substring content start)))))

(ert-deftest native-authority/boot-without-emacs-smoke-script-exists ()
  "The native-authority boot smoke should be a repo-owned executable script."
  (let ((script (expand-file-name
                 "packaging/scripts/xoxdwm-headless-no-emacs-smoke"
                 native-authority-test--root)))
    (should (file-exists-p script))
    (should (file-executable-p script))))

(ert-deftest native-authority/boot-without-emacs-smoke-uses-headless-compositor ()
  "The boot smoke should start the compositor headless and avoid Emacs services."
  (let ((script (native-authority-test--read-file
                 "packaging/scripts/xoxdwm-headless-no-emacs-smoke")))
    (should (string-match-p "--backend headless" script))
    (should (string-match-p "--headless-exit-after" script))
    (should (string-match-p "emacs_required=no" script))
    (should (string-match-p "unset EMACS INSIDE_EMACS" script))
    (should-not (string-match-p "emacsclient" script))
    (should-not (string-match-p "exwm-vr-emacs\\.service" script))))

(ert-deftest native-authority/boot-without-emacs-smoke-exercises-ipc ()
  "The boot smoke should probe a minimal native WM control surface."
  (let ((script (native-authority-test--read-file
                 "packaging/scripts/xoxdwm-headless-no-emacs-smoke")))
    (should (string-match-p "XOXDWM_IPC_PROBE_SOCKET" script))
    (should (string-match-p "python3 -" script))
    (should (string-match-p (regexp-quote ":hello :id 1") script))
    (should (string-match-p (regexp-quote ":workspace-list :id 2") script))
    (should (string-match-p "ipc_probe=passed" script))
    (should (string-match-p "reason=ipc_probe_failed" script))
    (should (string-match-p "compositor_exited_before_ipc_probe" script))
    (should (string-match-p "compositor_exit_status=" script))))

(ert-deftest native-authority/boot-without-emacs-smoke-is-in-justfile ()
  "The Justfile should expose the native-authority smoke directly."
  (let ((justfile (native-authority-test--read-file "justfile")))
    (should (string-match-p "^boot-without-emacs-smoke" justfile))
    (should (string-match-p "xoxdwm-headless-no-emacs-smoke" justfile))))

(ert-deftest native-authority/runtime-proof-lane-is-repo-owned ()
  "The Linux runtime proof gate should be explicit and guarded."
  (let* ((script-path (expand-file-name
                       "packaging/scripts/xoxdwm-native-authority-proof"
                       native-authority-test--root))
         (script (native-authority-test--read-file
                  "packaging/scripts/xoxdwm-native-authority-proof"))
         (spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
         (justfile (native-authority-test--read-file "justfile"))
         (template (native-authority-test--read-file
                    "docs/native-authority-runtime-proof-template.md"))
         (guide (native-authority-test--read-file "docs/user-guide.md")))
    (should (file-exists-p script-path))
    (should (file-executable-p script-path))
    (dolist (needle '("native_authority_gate=runtime_proof"
                      "emacs_required=no"
                      "exwm-vr-emacs.service"
                      ":workspace-list"
                      ":workspace-switch"
                      ":layout-set"
                      ":app-launch"
                      "XOXDWM_PROOF_LAUNCH_TARGET"
                      "workspace_visibility_observed"
                      "layout_reflow_observed"))
      (should (string-match-p needle script)))
    (dolist (needle '("packaging/scripts/xoxdwm-native-authority-proof"
                      "%{_libexecdir}/%{project_name}/native-authority-proof"
                      "%{_bindir}/xoxdwm-native-authority-proof"))
      (should (string-match-p needle spec)))
    (should (string-match-p "^native-authority-proof" justfile))
    (should (string-match-p "xoxdwm-native-authority-proof" justfile))
    (should (string-match-p "^native-authority-proof-remote" justfile))
    (should (string-match-p "^honey-native-authority-proof" justfile))
    (dolist (needle '("This target does not reboot"
                      "does not stop or modify rke2 services"
                      "XOXDWM_PROOF_MUTATE"
                      "proof_source=installed"
                      "proof_source=repo"
                      "XOXDWM_PROOF_LAUNCH_TARGET"
                      "rke2-server="
                      "xoxdwm-native-authority-proof"))
      (should (string-match-p needle justfile)))
    (dolist (needle '("visual_observed"
                      "app_launch_observed"
                      "focus_change_observed"
                      "workspace_visibility_observed"
                      "layout_reflow_observed"
                      "just native-authority-proof-remote honey 0"
                      "just native-authority-proof-remote yoga 1 terminal"
                      "Do not stop or modify `rke2-server`"))
      (should (string-match-p needle template)))
    (should (string-match-p "just native-authority-proof" guide))
    (should (string-match-p "xoxdwm-native-authority-proof" guide))
    (should (string-match-p "just native-authority-proof-remote honey 0" guide))
    (should (string-match-p "just native-authority-proof-remote yoga 1 terminal" guide))
    (should (string-match-p
             "human-visible app launch, focus, workspace visibility, and layout reflow"
             guide))))

(ert-deftest native-authority/rpm-meta-does-not-hard-require-elisp ()
  "The meta RPM should not force the legacy Lisp policy layer by default."
  (let ((spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec")))
    (should-not (string-match-p "^Requires:[[:space:]]+%{name}-elisp" spec))
    (should (string-match-p "^Suggests:[[:space:]]+%{name}-elisp" spec))))

(ert-deftest native-authority/compositor-rpm-does-not-own-emacs-app-layer ()
  "The compositor RPM should not own Emacs service/bootstrap files."
  (let* ((spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
         (compositor (native-authority-test--section spec "%files compositor"))
         (elisp (native-authority-test--section spec "%files elisp")))
    (should compositor)
    (should elisp)
    (should-not (string-match-p "exwm-vr-emacs\\.service" compositor))
    (should-not (string-match-p "exwm-vr-session-init\\.el" compositor))
    (should (string-match-p "exwm-vr-emacs\\.service" elisp))
    (should (string-match-p "exwm-vr-session-init\\.el" elisp))))

(ert-deftest native-authority/default-target-does-not-start-emacs ()
  "The packaged target should start the compositor, not the Emacs app layer."
  (let ((target (native-authority-test--read-file
                 "packaging/systemd/exwm-vr.target")))
    (should (string-match-p "Wants=exwm-vr-compositor\\.service" target))
    (should-not (string-match-p "Wants=.*exwm-vr-emacs\\.service" target))))

(ert-deftest native-authority/session-wrapper-makes-emacs-opt-in ()
  "The display-manager wrapper should only start Emacs when explicitly requested."
  (let ((session (native-authority-test--read-file
                  "packaging/desktop/exwm-vr-session")))
    (should (string-match-p
             (regexp-quote "EXWM_VR_START_EMACS=\"${EXWM_VR_START_EMACS:-0}\"")
             session))
    (should (string-match-p "systemctl --user start exwm-vr-emacs\\.service" session))
    (should (string-match-p
             (regexp-quote "[ \"${EXWM_VR_START_EMACS}\" = \"1\" ]")
             session))))

(ert-deftest native-authority/package-runtime-truth-lint-script-passes ()
  "The package runtime boundary lint should pass without hardware."
  (let* ((script (expand-file-name "scripts/package-runtime-truth-lint"
                                   native-authority-test--root))
         (buffer (get-buffer-create "*package-runtime-truth-lint*"))
         (exit-code (call-process "python3" nil buffer nil script)))
    (when (not (zerop exit-code))
      (message "%s" (with-current-buffer buffer (buffer-string))))
    (should (= 0 exit-code))))

(ert-deftest native-authority/compositor-startup-loads-native-config ()
  "Compositor startup should load native config before backend launch."
  (let ((main (native-authority-test--read-file "compositor/src/main.rs"))
        (backend (native-authority-test--read-file "compositor/src/backend/mod.rs"))
        (headless (native-authority-test--read-file "compositor/src/backend/headless.rs"))
        (guide (native-authority-test--read-file "docs/user-guide.md")))
    (should (string-match-p "config: Option<PathBuf>" main))
    (should (string-match-p "CompositorConfig::load_from_file" main))
    (should (string-match-p "CompositorConfig::load_or_default" main))
    (should (string-match-p "native compositor config initialized" main))
    (should (string-match-p
             (regexp-quote ".or_else(|| compositor_config.ipc_socket_pathbuf())")
             main))
    (should (string-match-p "backend::run" main))
    (should (string-match-p "compositor_config: CompositorConfig" backend))
    (should (string-match-p "headless::run" backend))
    (should (string-match-p "EwwmState::new_with_config" headless))
    (should (string-match-p
             (regexp-quote "A missing")
             guide))
    (should (string-match-p "uses built-in defaults" guide))
    (should (string-match-p "--config /path/to/compositor\\.json" guide))
    (should (string-match-p
             "Configuration is split between native compositor startup settings"
             guide))))

(ert-deftest native-authority/native-config-owns-manage-policy-scaffold ()
  "Native config should expose first-pass app manage policy fields."
  (let ((config (native-authority-test--read-file "compositor/src/config.rs"))
        (state (native-authority-test--read-file "compositor/src/state.rs"))
        (xdg (native-authority-test--read-file "compositor/src/handlers/xdg_shell.rs"))
        (xwayland (native-authority-test--read-file "compositor/src/handlers/xwayland.rs"))
        (compositor (native-authority-test--read-file "compositor/src/handlers/compositor.rs")))
    (should (string-match-p "workspace_app_rules" config))
    (should (string-match-p "floating_app_ids" config))
    (should (string-match-p "workspace_for_app_candidates" config))
    (should (string-match-p "should_float_app_candidates" config))
    (should (string-match-p "apply_native_manage_policy" state))
    (should (string-match-p "self\\.apply_native_manage_policy(&mut data, false)" xdg))
    (should (string-match-p "self\\.apply_native_manage_policy(&mut data, is_transient)" xwayland))
    (should (string-match-p "surface-workspace-changed" compositor))
    (should (string-match-p "surface-float-changed" compositor))))

(ert-deftest native-authority/native-config-owns-workspace-initial-state ()
  "Workspace count and initial active workspace should come from native config."
  (let ((config (native-authority-test--read-file "compositor/src/config.rs"))
        (state (native-authority-test--read-file "compositor/src/state.rs"))
        (dispatch (native-authority-test--read-file "compositor/src/ipc/dispatch.rs"))
        (workspace (native-authority-test--read-file "lisp/vr/ewwm-workspace.el")))
    (dolist (needle '("pub workspace_count: usize"
                      "pub active_workspace: usize"
                      "pub fn normalized_workspace_count"
                      "pub fn normalized_active_workspace"))
      (should (string-match-p needle config)))
    (should (string-match-p
             (regexp-quote "let workspace_count = config.normalized_workspace_count();")
             state))
    (should (string-match-p
             (regexp-quote "let active_workspace = config.normalized_active_workspace();")
             state))
    (should (string-match-p
             (regexp-quote "workspace_count,")
             state))
    (should (string-match-p
             (regexp-quote "active_workspace,")
             state))
    (should (string-match-p
             (regexp-quote "workspace >= state.workspace_count")
             dispatch))
    (should (string-match-p
             (regexp-quote "state.active_workspace = workspace;")
             dispatch))
    (should (string-match-p
             (regexp-quote "\"workspace-changed\"")
             dispatch))
    (should (string-match-p
             (regexp-quote "for i in 0..state.workspace_count")
             dispatch))
    (should (string-match-p
             (regexp-quote ":workspace-switch")
             workspace))
    (should (string-match-p "ewwm-ipc-send" workspace))))

(ert-deftest native-authority/native-key-actions-cover-core-wm-controls ()
  "Native config/input should cover core key actions before IPC key grabs."
  (let ((config (native-authority-test--read-file "compositor/src/config.rs"))
        (input (native-authority-test--read-file "compositor/src/input.rs"))
        (state (native-authority-test--read-file "compositor/src/state.rs"))
        (dispatch (native-authority-test--read-file "compositor/src/ipc/dispatch.rs"))
        (elisp-input (native-authority-test--read-file "lisp/vr/ewwm-input.el"))
        (launch (native-authority-test--read-file "lisp/vr/ewwm-launch.el"))
        (guide (native-authority-test--read-file "docs/user-guide.md")))
    (dolist (needle '("pub key_action_bindings: BTreeMap<String, String>"
                      "pub app_launch_commands: BTreeMap<String, String>"
                      "pub autostart_enabled: bool"
                      "pub autostart_targets: Vec<String>"
                      "pub session_lock_command: String"
                      "pub session_idle_enabled: bool"
                      "pub session_idle_command: String"
                      "pub fn native_action_for_key"
                      "pub fn app_launch_command"
                      "pub fn configured_autostart_targets"
                      "pub fn session_lock_command"
                      "pub fn session_idle_command"
                      "default_key_action_bindings"
                      "default_app_launch_commands"
                      "\"key_action_bindings\""
                      "\"app_launch_commands\""
                      "\"autostart_enabled\""
                      "\"autostart_targets\""
                      "\"session_lock_command\""
                      "\"session_idle_enabled\""
                      "\"session_idle_command\""))
      (should (string-match-p needle config)))
    (dolist (needle '("workspace:0"
                      "focus:next"
                      "focus:previous"
                      "layout:cycle"
                      "launch:terminal"
                      "compositor:reload"
                      "compositor:exit"))
      (should (string-match-p needle config))
      (should (string-match-p needle guide)))
    (dolist (needle '("workspace:"
                      "launch:"
                      "focus:next"
                      "focus:previous"
                      "layout:cycle"
                      "compositor:reload"
                      "compositor:exit"))
      (should (string-match-p needle input)))
    (should (string-match-p
             (regexp-quote "handle_native_key_action(state, &key_desc, time)")
             input))
    (should (string-match-p
             (regexp-quote "state.grabbed_keys.contains(&key_desc)")
             input))
    (should (< (string-match
                (regexp-quote "handle_native_key_action(state, &key_desc, time)")
                input)
               (string-match
                (regexp-quote "state.grabbed_keys.contains(&key_desc)")
                input)))
    (should (string-match-p "Command::new(\"sh\")" state))
    (should (string-match-p "pub fn launch_configured_app" state))
    (should (string-match-p "pub native_autostart_launched: HashSet<String>" state))
    (should (string-match-p "pub fn run_native_autostart" state))
    (should (string-match-p "pub fn run_startup_autostart" state))
    (should (string-match-p "pub fn launch_session_lock" state))
    (should (string-match-p "pub native_idle_process: Option<Child>" state))
    (should (string-match-p "pub fn native_idle_status" state))
    (should (string-match-p "pub fn start_native_idle" state))
    (should (string-match-p "pub fn stop_native_idle" state))
    (should (string-match-p "pub fn run_startup_idle" state))
    (should (string-match-p "config\\.autostart_enabled" state))
    (should (string-match-p "session_lock_command" state))
    (should (string-match-p "session_idle_command" state))
    (should (string-match-p "state\\.launch_configured_app(name)" input))
    (should (string-match-p "Some(\"app-launch-list\") => handle_app_launch_list" dispatch))
    (should (string-match-p "Some(\"app-launch\") => handle_app_launch" dispatch))
    (should (string-match-p "fn handle_app_launch_list" dispatch))
    (should (string-match-p "fn handle_app_launch" dispatch))
    (should (string-match-p "launch_configured_app" dispatch))
    (should (string-match-p "ewwm-input--ipc-connected-p" elisp-input))
    (should (string-match-p "ewwm-input--send-native" elisp-input))
    (should (string-match-p "(:type :workspace-switch :workspace ,ws)" elisp-input))
    (should (string-match-p "(:type :workspace-move-surface" elisp-input))
    (should (string-match-p ":surface-id ,ewwm-surface-id" elisp-input))
    (should (string-match-p "(:type :layout-cycle)" elisp-input))
    (should (string-match-p "(:type :config-reload)" elisp-input))
    (should (string-match-p "funcall 'ewwm-workspace-switch ws" elisp-input))
    (should (string-match-p "funcall 'ewwm-workspace-move-surface ewwm-surface-id ws" elisp-input))
    (should (string-match-p "funcall 'ewwm-layout-cycle" elisp-input))
    (should (string-match-p "funcall 'ewwm-reset" elisp-input))
    (should (string-match-p "ewwm-launch-native-target" launch))
    (should (string-match-p "ewwm-launch-native-target-list" launch))
    (should (string-match-p "(:type :app-launch :name ,target)" launch))
    (should (string-match-p "(:type :app-launch-list)" launch))
    (should (string-match-p ":app-launch-list" guide))
    (should (string-match-p ":app-launch" guide))
    (should (string-match-p "Some(\"autostart-list\") => handle_autostart_list" dispatch))
    (should (string-match-p "Some(\"autostart-run\") => handle_autostart_run" dispatch))
    (should (string-match-p "fn handle_autostart_list" dispatch))
    (should (string-match-p "fn handle_autostart_run" dispatch))
    (should (string-match-p "autostart-ran" dispatch))
    (should (string-match-p ":autostart-list" guide))
    (should (string-match-p ":autostart-run" guide))
    (should (string-match-p "Native Autostart Policy" guide))
    (should (string-match-p "Some(\"session-status\") => handle_session_status" dispatch))
    (should (string-match-p "Some(\"session-lock\") => handle_session_lock" dispatch))
    (should (string-match-p "Some(\"session-logout\") => handle_compositor_exit" dispatch))
    (should (string-match-p "fn handle_session_status" dispatch))
    (should (string-match-p "fn handle_session_lock" dispatch))
    (should (string-match-p ":session-status" guide))
    (should (string-match-p ":session-lock" guide))
    (should (string-match-p ":session-logout" guide))
    (should (string-match-p "Native Session Lock" guide))
    (should (string-match-p "Some(\"session-idle-status\") => handle_session_idle_status" dispatch))
    (should (string-match-p "Some(\"session-idle-start\") => handle_session_idle_start" dispatch))
    (should (string-match-p "Some(\"session-idle-stop\") => handle_session_idle_stop" dispatch))
    (should (string-match-p "fn handle_session_idle_status" dispatch))
    (should (string-match-p "fn handle_session_idle_start" dispatch))
    (should (string-match-p "fn handle_session_idle_stop" dispatch))
    (should (string-match-p ":session-idle-status" guide))
    (should (string-match-p ":session-idle-start" guide))
    (should (string-match-p ":session-idle-stop" guide))
    (should (string-match-p "Native Idle Supervision" guide))
    (should (string-match-p "Some(\"config-reload\") => handle_config_reload" dispatch))
    (should (string-match-p "Some(\"reload-config\") => handle_config_reload" dispatch))
    (should (string-match-p "fn handle_config_reload" dispatch))
    (should (string-match-p "config-reloaded" dispatch))
    (should (string-match-p "load_default_path_strict" state))
    (should (string-match-p "error_response(msg_id, &reason)" dispatch))
    (should (string-match-p ":source" dispatch))
    (should (string-match-p ":config-reload" guide))
    (should (string-match-p "native-key-action" input))
    (should (string-match-p "pub fn focus_adjacent_surface" state))
    (should (string-match-p "pub fn reload_native_config" state))
    (should (string-match-p
             "Emacs/eGreg can still[[:space:]\n]+register IPC key grabs"
             guide))))

(ert-deftest native-authority/native-idle-hooks-are-real-backend-policy ()
  "Startup idle supervision should run from native real-session backends."
  (let ((winit (native-authority-test--read-file "compositor/src/backend/winit.rs"))
        (drm (native-authority-test--read-file "compositor/src/backend/drm.rs"))
        (headless (native-authority-test--read-file "compositor/src/backend/headless.rs")))
    (should (string-match-p "state\\.run_startup_idle()" winit))
    (should (string-match-p "state\\.run_startup_idle()" drm))
    (should-not (string-match-p "state\\.run_startup_idle()" headless))))

(ert-deftest native-authority/native-autostart-hooks-are-real-backend-policy ()
  "Startup autostart should run from native real-session backends, not Lisp."
  (let ((winit (native-authority-test--read-file "compositor/src/backend/winit.rs"))
        (drm (native-authority-test--read-file "compositor/src/backend/drm.rs"))
        (autostart (native-authority-test--read-file "lisp/vr/ewwm-autostart.el")))
    (should (string-match-p "state\\.run_startup_autostart()" winit))
    (should (string-match-p "state\\.run_startup_autostart()" drm))
    (should (string-match-p "ewwm-autostart-desktop-name" autostart))
    (should (string-match-p "EXWM-VR" autostart))))

(ert-deftest native-authority/emacs-session-layer-uses-native-session-ipc ()
  "Emacs/eGreg session helpers should request native session IPC when connected."
  (let ((session (native-authority-test--read-file "lisp/vr/ewwm-session.el"))
        (guide (native-authority-test--read-file "docs/user-guide.md")))
    (should (string-match-p "(:type :session-lock)" session))
    (should (string-match-p "(:type :session-logout)" session))
    (should (string-match-p "(:type :session-idle-start)" session))
    (should (string-match-p "(:type :session-idle-stop)" session))
    (should (string-match-p "(:type :session-idle-status)" session))
    (should (string-match-p "(:type :dpms-set :state ,state)" session))
    (should (string-match-p "(:type :dpms-get)" session))
    (should (string-match-p "ewwm-session-shutdown-command" session))
    (should (string-match-p
             "Shutdown, reboot, suspend, and[[:space:]\n]+hibernate remain app-layer/operator policy"
             guide))))

(ert-deftest native-authority/emacs-surface-layer-mirrors-native-events ()
  "Emacs/eGreg surface helpers should request native state and mirror events."
  (let ((floating (native-authority-test--read-file "lisp/vr/ewwm-floating.el"))
        (manage (native-authority-test--read-file "lisp/vr/ewwm-manage.el"))
        (workspace (native-authority-test--read-file "lisp/vr/ewwm-workspace.el"))
        (ipc (native-authority-test--read-file "lisp/vr/ewwm-ipc.el"))
        (main (native-authority-test--read-file "lisp/vr/ewwm.el")))
    (should (string-match-p "(:type :surface-float :surface-id ,sid :enable ,enable)" floating))
    (should (string-match-p ":type :surface-fullscreen" manage))
    (should (string-match-p ":enable ,enable" manage))
    (should (string-match-p "ewwm-manage--on-float-changed" manage))
    (should (string-match-p "ewwm-workspace--on-surface-workspace-changed" workspace))
    (should (string-match-p ":surface-float-changed" ipc))
    (should (string-match-p ":surface-workspace-changed" ipc))
    (should (string-match-p "ewwm-ipc--on-surface-float-changed" main))
    (should (string-match-p "ewwm-ipc--on-surface-workspace-changed" main))))

(ert-deftest native-authority/ipc-workspace-helpers-do-not-override-workspace-layer ()
  "Raw IPC workspace helpers should not replace the workspace app-layer module."
  (let ((ipc (native-authority-test--read-file "lisp/vr/ewwm-ipc.el"))
        (workspace (native-authority-test--read-file "lisp/vr/ewwm-workspace.el")))
    (should (string-match-p "defun ewwm-ipc-workspace-switch" ipc))
    (should (string-match-p "defun ewwm-ipc-workspace-list" ipc))
    (should (string-match-p "unless (fboundp 'ewwm-workspace-switch)" ipc))
    (should (string-match-p "unless (fboundp 'ewwm-workspace-list)" ipc))
    (should (string-match-p "defun ewwm-workspace-switch" workspace))
    (should (string-match-p "defun ewwm-workspace-list" workspace))))

(ert-deftest native-authority/native-layout-reflow-is-compositor-owned ()
  "Native layout reflow should be in Rust and triggered by WM mutations."
  (let ((state (native-authority-test--read-file "compositor/src/state.rs"))
        (input (native-authority-test--read-file "compositor/src/input.rs"))
        (dispatch (native-authority-test--read-file "compositor/src/ipc/dispatch.rs"))
        (elisp-layout (native-authority-test--read-file "lisp/vr/ewwm-layout.el"))
        (elisp-ipc (native-authority-test--read-file "lisp/vr/ewwm-ipc.el"))
        (elisp-main (native-authority-test--read-file "lisp/vr/ewwm.el"))
        (xdg (native-authority-test--read-file "compositor/src/handlers/xdg_shell.rs"))
        (xwayland (native-authority-test--read-file "compositor/src/handlers/xwayland.rs"))
        (guide (native-authority-test--read-file "docs/user-guide.md")))
    (dolist (needle '("pub fn apply_native_layout"
                      "active_tiled_windows"
                      "native_layout_geometries"
                      "tiling_layout"
                      "grid_layout"
                      "send_pending_configure"
                      "surface.configure(Some(geometry))"))
      (should (string-match-p needle state)))
    (dolist (needle '("self.apply_native_layout()" "surface.send_configure()"))
      (should (string-match-p needle xdg)))
    (should (string-match-p "self.apply_native_layout()" xwayland))
    (dolist (needle '("state.apply_native_layout()"
                      "handle_workspace_switch"
                      "handle_workspace_move_surface"
                      "handle_surface_float"
                      "set_current_layout"))
      (should (string-match-p needle dispatch)))
    (dolist (needle '("state.apply_native_layout()"
                      "workspace:"
                      "layout:cycle"))
      (should (string-match-p needle input)))
    (should (string-match-p "(:type :layout-set :layout ,layout)" elisp-layout))
    (should (string-match-p "'(:type :layout-cycle)" elisp-layout))
    (should (string-match-p "ewwm-layout--on-layout-changed" elisp-layout))
    (should (string-match-p ":layout-changed" elisp-ipc))
    (should (string-match-p "ewwm-ipc--on-layout-changed" elisp-main))
    (should (string-match-p "Native Layout Policy" guide))
    (should (string-match-p
             "The compositor reflows non-floating surfaces"
             guide))))

(ert-deftest native-authority/native-workspace-visibility-is-compositor-owned ()
  "Workspace switches should update compositor visibility without Emacs."
  (let ((state (native-authority-test--read-file "compositor/src/state.rs"))
        (dispatch (native-authority-test--read-file "compositor/src/ipc/dispatch.rs"))
        (guide (native-authority-test--read-file "docs/user-guide.md")))
    (dolist (needle '("pub geometry: Option<Rectangle<i32, Logical>>"
                      "pub fn apply_workspace_visibility"
                      "surface_is_on_active_workspace"
                      "self.space.unmap_elem(&window)"
                      "self.apply_workspace_visibility()"
                      "data.geometry = Some(geometry)"))
      (should (string-match-p needle state)))
    (dolist (needle '("handle_workspace_switch"
                      "handle_workspace_move_surface"
                      "state.apply_native_layout()"))
      (should (string-match-p needle dispatch)))
    (should (string-match-p "Native Workspace Visibility" guide))
    (should (string-match-p
             "Inactive workspace surfaces are[[:space:]\n]+removed from the compositor space"
             guide))))

(ert-deftest native-authority/active-identity-does-not-make-emacs-the-wm ()
  "Active runtime/package surfaces should not describe Emacs as WM authority."
  (dolist (relative '("README.md"
                      "compositor/Cargo.toml"
                      "compositor/src/main.rs"
                      "packaging/rpm/exwm-vr.spec"
                      "packaging/systemd/exwm-vr-emacs.service"
                      "packaging/systemd/exwm-vr.target"
                      "docs/user-guide.md"
                      "docs/status.md"
                      "docs/support-matrix.md"
                      "docs/ipc-protocol.md"))
    (let ((content (native-authority-test--read-file relative)))
      (should-not (string-match-p "WM brain" content))
      (should-not (string-match-p "window manager brain" content))
      (should-not (string-match-p "transhuman Emacs window manager" content))
      (should-not (string-match-p "Emacs window[- ]management layer" content))
      (should-not (string-match-p "Emacs Window Manager" content)))))

(provide 'native-authority-test)
;;; native-authority-test.el ends here
