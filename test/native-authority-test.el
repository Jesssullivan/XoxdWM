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
    (should (string-match-p "compatibility/archive surface" snippet))))

(provide 'native-authority-test)
;;; native-authority-test.el ends here
