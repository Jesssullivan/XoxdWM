;;; week4-integration-test.el --- Week 4 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-ipc)

(defvar week4-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

;; ── IPC module structure ───────────────────────────────────

(ert-deftest week4-integration/ipc-rust-module ()
  "IPC Rust module has all required files."
  (dolist (file '("compositor/src/ipc/mod.rs"
                  "compositor/src/ipc/server.rs"
                  "compositor/src/ipc/dispatch.rs"))
    (should (file-exists-p (expand-file-name file week4-test--root)))))

(ert-deftest week4-integration/ipc-protocol-spec ()
  "IPC protocol specification exists and is substantial."
  (let ((path (expand-file-name "docs/ipc-protocol.md" week4-test--root)))
    (should (file-exists-p path))
    (with-temp-buffer
      (insert-file-contents path)
      ;; At least 200 lines
      (should (> (count-lines (point-min) (point-max)) 200)))))

(ert-deftest week4-integration/ipc-dispatch-handles-all-types ()
  "dispatch.rs handles all protocol message types."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/src/ipc/dispatch.rs" week4-test--root))
    (let ((content (buffer-string)))
      (dolist (handler '("handle_hello"
                         "handle_ping"
                         "handle_surface_list"
                         "handle_surface_focus"
                         "handle_surface_close"
                         "handle_surface_move"
                         "handle_surface_resize"
                         "handle_workspace_switch"
                         "handle_workspace_list"
                         "handle_key_grab"
                         "handle_key_ungrab"
                         "handle_vr_status"))
        (should (string-match-p handler content))))))

(ert-deftest week4-integration/ipc-server-calloop ()
  "IPC server uses calloop for event loop integration."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/src/ipc/server.rs" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "calloop" content))
      (should (string-match-p "UnixListener" content))
      (should (string-match-p "Generic::new" content)))))

;; ── Emacs IPC client ───────────────────────────────────────

(ert-deftest week4-integration/ewwm-ipc-features ()
  "ewwm-ipc.el provides required features."
  (should (featurep 'ewwm-ipc))
  (should (fboundp 'ewwm-ipc-connect))
  (should (fboundp 'ewwm-ipc-disconnect))
  (should (fboundp 'ewwm-ipc-send))
  (should (fboundp 'ewwm-ipc-send-sync))
  (should (fboundp 'ewwm-ipc-connected-p))
  (should (fboundp 'ewwm-ipc-status))
  (should (fboundp 'ewwm-ipc-trace-mode))
  (should (fboundp 'ewwm-ipc-benchmark))
  (should (fboundp 'ewwm-ipc-ping)))

(ert-deftest week4-integration/ewwm-convenience-wrappers ()
  "Convenience wrapper functions exist."
  (should (fboundp 'ewwm-surface-list))
  (should (fboundp 'ewwm-surface-focus))
  (should (fboundp 'ewwm-surface-close))
  (should (fboundp 'ewwm-surface-move))
  (should (fboundp 'ewwm-surface-resize))
  (should (fboundp 'ewwm-workspace-switch))
  (should (fboundp 'ewwm-workspace-list))
  (should (fboundp 'ewwm-key-grab))
  (should (fboundp 'ewwm-key-ungrab)))

(ert-deftest week4-integration/ewwm-ipc-hooks ()
  "IPC connection hooks are defined."
  (should (boundp 'ewwm-ipc-connected-hook))
  (should (boundp 'ewwm-ipc-disconnected-hook)))

(ert-deftest week4-integration/ewwm-ipc-customization ()
  "IPC customization variables exist."
  (should (boundp 'ewwm-ipc-socket-path))
  (should (boundp 'ewwm-ipc-reconnect-max-delay))
  (should (boundp 'ewwm-ipc-sync-timeout)))

;; ── Event handlers ─────────────────────────────────────────

(ert-deftest week4-integration/event-handlers-defined ()
  "All IPC event handlers are defined."
  (dolist (handler '(ewwm-ipc--on-surface-created
                     ewwm-ipc--on-surface-destroyed
                     ewwm-ipc--on-surface-title-changed
                     ewwm-ipc--on-surface-focused
                     ewwm-ipc--on-surface-geometry-changed
                     ewwm-ipc--on-workspace-changed
                     ewwm-ipc--on-key-pressed))
    (should (fboundp handler))))

;; ── State fields ───────────────────────────────────────────

(ert-deftest week4-integration/state-has-ipc-fields ()
  "Compositor state.rs has IPC-related fields."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/src/state.rs" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "ipc_server" content))
      (should (string-match-p "grabbed_keys" content))
      (should (string-match-p "active_workspace" content)))))

;; ── Debug tools ────────────────────────────────────────────

(ert-deftest week4-integration/python-debug-client ()
  "Python IPC debug client exists and is executable."
  (let ((path (expand-file-name "tools/ewwm-ipc-client.py" week4-test--root)))
    (should (file-exists-p path))
    (should (file-executable-p path))))

(ert-deftest week4-integration/python-client-has-features ()
  "Python client supports interactive, listen, and benchmark modes."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "tools/ewwm-ipc-client.py" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "--interactive" content))
      (should (string-match-p "--listen" content))
      (should (string-match-p "--benchmark" content)))))

;; ── Cargo.toml IPC deps ───────────────────────────────────

(ert-deftest week4-integration/cargo-deps-complete ()
  "Cargo.toml has all IPC-related dependencies."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/Cargo.toml" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "lexpr" content))
      (should (string-match-p "libc" content)))))

;; ── Event emission in handlers ─────────────────────────────

(ert-deftest week4-integration/xdg-shell-emits-events ()
  "xdg_shell handler emits IPC events."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/src/handlers/xdg_shell.rs" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "broadcast_event" content))
      (should (string-match-p "surface-created" content)))))

(ert-deftest week4-integration/input-handles-key-grabs ()
  "Input handler checks grabbed keys."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/src/input.rs" week4-test--root))
    (let ((content (buffer-string)))
      (should (string-match-p "grabbed_keys" content))
      (should (string-match-p "key-pressed" content))
      (should (string-match-p "FilterResult::Intercept" content)))))

;;; week4-integration-test.el ends here
