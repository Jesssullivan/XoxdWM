;;; native-authority-test.el --- Native XoxdWM authority guardrails -*- lexical-binding: t; -*-

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

(defun native-authority-test--toml-array (key contents)
  "Return TOML array text for KEY from CONTENTS."
  (let ((start (string-match (concat "^" (regexp-quote key) " = \\[") contents)))
    (should start)
    (let ((end (string-match "^\\]" contents start)))
      (should end)
      (substring contents start (match-end 0)))))

(ert-deftest native-authority/config-owns-workspace-layout-and-launch-policy ()
  "Native config should own core WM policy inputs."
  (let ((config (native-authority-test--read-file "compositor/src/config.rs")))
    (dolist (needle '("workspace_count"
                      "active_workspace"
                      "layout_mode"
                      "key_actions"
                      "app_launch_commands"
                      "autostart_enabled"
                      "session_lock_command"
                      "session_idle_command"))
      (should (string-match-p needle config)))))

(ert-deftest native-authority/state-has-native-layout-and-visibility-reflow ()
  "Rust state should apply workspace visibility and layout reflow."
  (let ((state (native-authority-test--read-file "compositor/src/state.rs")))
    (dolist (needle '("fn layout_rects"
                      "reflow_native_layout"
                      "apply_workspace_visibility"
                      "set_surface_geometry"
                      "current_layout"
                      "visible: bool"))
      (should (string-match-p needle state)))
    (should-not (string-match-p "layout set (Emacs-driven)" state))))

(ert-deftest native-authority/ipc-exposes-native-policy-surface ()
  "IPC should expose native policy commands instead of Lisp-only authority."
  (let ((dispatch (native-authority-test--read-file "compositor/src/ipc/dispatch.rs")))
    (dolist (cmd '("\"app-launch\""
                   "\"app-launch-list\""
                   "\"config-reload\""
                   "\"reload-config\""
                   "\"autostart-list\""
                   "\"autostart-run\""
                   "\"session-status\""
                   "\"session-lock\""
                   "\"session-logout\""
                   "\"session-idle-status\""
                   "\"session-idle-start\""
                   "\"session-idle-stop\""))
      (should (string-match-p cmd dispatch)))
    (should (string-match-p "state\\.set_native_layout" dispatch))
    (should (string-match-p "state\\.reflow_native_layout" dispatch))))

(ert-deftest native-authority/native-key-actions-run-before-emacs-grabs ()
  "Keyboard input should check native key actions before IPC key grabs."
  (let ((input (native-authority-test--read-file "compositor/src/input.rs")))
    (should (string-match-p "handle_native_key_action" input))
    (should (< (string-match "handle_native_key_action" input)
               (string-match "grabbed_keys\\.contains" input)))))

(ert-deftest native-authority/lisp-helpers-are-connected-ipc-clients ()
  "Bundled Lisp helpers should request native IPC when connected."
  (let ((input (native-authority-test--read-file "lisp/vr/ewwm-input.el"))
        (layout (native-authority-test--read-file "lisp/vr/ewwm-layout.el"))
        (session (native-authority-test--read-file "lisp/vr/ewwm-session.el"))
        (launch (native-authority-test--read-file "lisp/vr/ewwm-launch.el")))
    (dolist (needle '(":workspace-switch"
                      ":workspace-move-surface"
                      ":layout-cycle"
                      ":config-reload"))
      (should (string-match-p needle input)))
    (should (string-match-p ":layout-set" layout))
    (should (string-match-p ":session-lock" session))
    (should (string-match-p ":session-logout" session))
    (should (string-match-p ":session-idle-start" session))
    (should (string-match-p ":dpms-set" session))
    (should (string-match-p "ewwm-launch-native-target" launch))))

(ert-deftest native-authority/raw-ipc-workspace-helpers-are-explicit ()
  "Raw IPC workspace helpers should not override app-layer modules by load order."
  (let ((ipc (native-authority-test--read-file "lisp/vr/ewwm-ipc.el")))
    (should (string-match-p "ewwm-ipc-workspace-switch" ipc))
    (should (string-match-p "ewwm-ipc-workspace-list" ipc))
    (should (string-match-p "(unless (fboundp 'ewwm-workspace-switch)" ipc))
    (should (string-match-p ":surface-float-changed" ipc))
    (should (string-match-p ":surface-workspace-changed" ipc))
    (should (string-match-p ":layout-changed" ipc))))

(ert-deftest native-authority/proof-lanes-exist-and-avoid-emacs-authority ()
  "Proof helpers should exercise native authority without relying on Emacs."
  (let ((proof (native-authority-test--read-file
                "packaging/scripts/xoxdwm-native-authority-proof"))
        (boot (native-authority-test--read-file
               "packaging/scripts/boot-without-emacs-smoke"))
        (justfile (native-authority-test--read-file "justfile")))
    (should (string-match-p "native_authority" proof))
    (should (string-match-p ":workspace-list" proof))
    (should (string-match-p ":layout-cycle" proof))
    (should (string-match-p ":app-launch" proof))
    (should (string-match-p "boot_without_emacs" boot))
    (should (string-match-p "--backend headless" boot))
    (should (string-match-p "^native-authority-proof" justfile))
    (should (string-match-p "^remote-native-authority-proof" justfile))
    (should (string-match-p "no reboot" justfile))))

(ert-deftest native-authority/package-default-does-not-load-lisp-core ()
  "Default RPM init should not add lisp/core to load-path."
  (let* ((spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
         (start (string-match ";;; exwm-vr-init\\.el" spec))
         (end (and start (string-match "ELISP_EOF" spec start)))
         (snippet (and start end (substring spec start end))))
    (should snippet)
    (should-not (string-match-p "exwm-vr/core" snippet))
    (should (string-match-p "compatibility/archive surface" snippet))
    (should-not (string-match-p "^Requires:[[:space:]]+%{name}-elisp" spec))
    (should (string-match-p "^Suggests:[[:space:]]+%{name}-elisp" spec))))

(ert-deftest native-authority/default-session-target-does-not-pull-emacs ()
  "The primary XoxdWM target should start native authority without Emacs."
  (let ((xoxdwm-target
         (native-authority-test--read-file "packaging/systemd/xoxdwm.target"))
        (legacy-target
         (native-authority-test--read-file "packaging/systemd/exwm-vr.target"))
        (emacs-service
         (native-authority-test--read-file
          "packaging/systemd/exwm-vr-emacs.service"))
        (session
         (native-authority-test--read-file "packaging/desktop/exwm-vr-session"))
        (spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
        (workflow (native-authority-test--read-file
                   ".github/workflows/packaging.yml"))
        (nixos (native-authority-test--read-file "nix/modules/exwm-vr.nix"))
        (home (native-authority-test--read-file
               "nix/home-manager/exwm-vr.nix")))
    (should (string-match-p "Description=XoxdWM Native Session Target"
                            xoxdwm-target))
    (should (string-match-p "Wants=exwm-vr-compositor\\.service"
                            xoxdwm-target))
    (should-not (string-match-p "exwm-vr-emacs\\.service" xoxdwm-target))
    (should-not (string-match-p "Alias=xoxdwm\\.target" legacy-target))
    (should (string-match-p "Wants=exwm-vr-compositor\\.service exwm-vr-emacs\\.service"
                            legacy-target))
    (should (string-match-p "WantedBy=exwm-vr\\.target" emacs-service))
    (should-not (string-match-p "WantedBy=.*xoxdwm\\.target" emacs-service))
    (should-not (string-match-p "PartOf=.*xoxdwm\\.target" emacs-service))
    (should (string-match-p "session_target=xoxdwm\\.target" session))
    (should-not (string-match-p "/usr/bin/emacs --fg-daemon" session))
    (should (string-match-p "Source25:[[:space:]]+xoxdwm\\.target" spec))
    (should-not (string-match-p "ln -s exwm-vr\\.target[[:space:]\n]+%{buildroot}%{_userunitdir}/xoxdwm\\.target"
                                spec))
    (should (string-match-p "cp packaging/systemd/xoxdwm\\.target"
                            workflow))
    (should (string-match-p "systemd\\.user\\.targets\\.\"xoxdwm\"" nixos))
    (should (string-match-p "requires = \\[ \"exwm-vr-compositor\\.service\" \\]"
                            nixos))
    (should (string-match-p "systemd\\.user\\.targets\\.\"xoxdwm\"" home))
    (should (string-match-p "Wants = \\[ \"exwm-vr-compositor\\.service\" \\]"
                            home))))

(ert-deftest native-authority/xwayland-is-optional-compatibility-feature ()
  "XWayland should stay feature-gated rather than define the default runtime."
  (let* ((cargo (native-authority-test--read-file "compositor/Cargo.toml"))
         (full-backend (native-authority-test--toml-array "full-backend" cargo))
         (drm (native-authority-test--read-file "compositor/src/backend/drm.rs"))
         (winit (native-authority-test--read-file "compositor/src/backend/winit.rs"))
         (spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
         (justfile (native-authority-test--read-file "justfile"))
         (flake (native-authority-test--read-file "flake.nix"))
         (workflow (native-authority-test--read-file ".github/workflows/multi-arch.yml"))
         (inventory (native-authority-test--read-file
                     "docs/research/legacy-exwm-x-retirement-inventory-2026-05-12.md"))
         (wlroots (native-authority-test--read-file "nix/packages/wlroots-beyond.nix")))
    (should (string-match-p "default = \\[\\]" cargo))
    (should (string-match-p "^xwayland = \\[\"smithay/xwayland\"\\]" cargo))
    (should-not (string-match-p "\"xwayland\"" full-backend))
    (should (string-match-p "#\\[cfg(feature = \"xwayland\")\\]" drm))
    (should (string-match-p "#\\[cfg(not(feature = \"xwayland\"))\\]" drm))
    (should (string-match-p "#\\[cfg(feature = \"xwayland\")\\]" winit))
    (should (string-match-p "#\\[cfg(not(feature = \"xwayland\"))\\]" winit))
    (should (string-match-p "%bcond xwayland_compat 0" spec))
    (should (string-match-p "%global compositor_features full-backend" spec))
    (should (string-match-p "full-backend,xwayland" spec))
    (should (string-match-p "Requires:[[:space:]]+xorg-x11-server-Xwayland" spec))
    (should (string-match-p "build-compositor-xwayland" justfile))
    (should (string-match-p "test-compositor-xwayland" justfile))
    (should (string-match-p "packages\\.compositor-xwayland" flake))
    (should (string-match-p "features = \\[ \"full-backend\" \"xwayland\" \\]" flake))
    (should (string-match-p "cargo check --no-default-features --features full-backend,xwayland" workflow))
    (should (string-match-p "Default XoxdWM runtime authority is native Rust" inventory))
    (should (string-match-p "`full-backend,xwayland`" inventory))
    (should (string-match-p "Native XoxdWM keeps XWayland behind its Cargo feature gate" wlroots))))

(ert-deftest native-authority/package-surfaces-prefer-xoxdwm-identity ()
  "Package and session surfaces should expose XoxdWM as primary identity."
  (let ((spec (native-authority-test--read-file "packaging/rpm/exwm-vr.spec"))
        (desktop (native-authority-test--read-file "packaging/desktop/exwm-vr.desktop"))
        (session (native-authority-test--read-file "packaging/desktop/exwm-vr-session"))
        (nixos (native-authority-test--read-file "nix/modules/exwm-vr.nix"))
        (home (native-authority-test--read-file "nix/home-manager/exwm-vr.nix")))
    (dolist (needle '("xoxdwm.desktop"
                      "xoxdwm-session"
                      "xoxdwm-compositor.service"
                      "xoxdwm-emacs.service"
                      "xoxdwm.target"))
      (should (string-match-p needle spec)))
    (should (string-match-p "Name=XoxdWM" desktop))
    (should (string-match-p "DesktopNames=XoxdWM;EXWM-VR;" desktop))
    (should (string-match-p "XDG_CURRENT_DESKTOP=XoxdWM" session))
    (should (string-match-p "session_target=xoxdwm.target" session))
    (should (string-match-p "xoxdwm-compositor\\.service" nixos))
    (should (string-match-p "xoxdwm.target" nixos))
    (should (string-match-p "xoxdwm-compositor\\.service" home))
    (should (string-match-p "xoxdwm.target" home))))

(provide 'native-authority-test)
;;; native-authority-test.el ends here
