;;; compositor-test.el --- Tests for compositor crate  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defvar compositor-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Project root.")

;; ── source file existence tests ─────────────────────────

(ert-deftest compositor-test/cargo-toml-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/Cargo.toml" compositor-test--root))))

(ert-deftest compositor-test/main-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/main.rs" compositor-test--root))))

(ert-deftest compositor-test/state-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/state.rs" compositor-test--root))))

(ert-deftest compositor-test/handlers-exist ()
  (dolist (handler '("mod.rs" "compositor.rs" "xdg_shell.rs" "seat.rs" "shm.rs"))
    (should (file-exists-p
             (expand-file-name (concat "compositor/src/handlers/" handler)
                               compositor-test--root)))))

(ert-deftest compositor-test/backends-exist ()
  (dolist (backend '("mod.rs" "winit.rs" "drm.rs" "headless.rs"))
    (should (file-exists-p
             (expand-file-name (concat "compositor/src/backend/" backend)
                               compositor-test--root)))))

(ert-deftest compositor-test/input-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/input.rs" compositor-test--root))))

(ert-deftest compositor-test/render-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/render.rs" compositor-test--root))))

(ert-deftest compositor-test/rust-toolchain-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/rust-toolchain.toml" compositor-test--root))))

;; ── Cargo.toml content verification ─────────────────────

(ert-deftest compositor-test/cargo-deps-smithay ()
  "Cargo.toml depends on smithay."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/Cargo.toml" compositor-test--root))
    (should (string-match-p "smithay" (buffer-string)))))

(ert-deftest compositor-test/cargo-deps-calloop ()
  "Cargo.toml depends on calloop."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/Cargo.toml" compositor-test--root))
    (should (string-match-p "calloop" (buffer-string)))))

(ert-deftest compositor-test/cargo-has-renderer-gl ()
  "Cargo.toml uses renderer_gl feature (Smithay 0.7 name)."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/Cargo.toml" compositor-test--root))
    (should (string-match-p "renderer_gl" (buffer-string)))))

(ert-deftest compositor-test/cargo-deps-lexpr ()
  "Cargo.toml depends on lexpr for s-expression parsing."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "compositor/Cargo.toml" compositor-test--root))
    (should (string-match-p "lexpr" (buffer-string)))))

;; ── IPC module source files ──────────────────────────────

(ert-deftest compositor-test/ipc-mod-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/ipc/mod.rs" compositor-test--root))))

(ert-deftest compositor-test/ipc-server-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/ipc/server.rs" compositor-test--root))))

(ert-deftest compositor-test/ipc-dispatch-rs-exists ()
  (should (file-exists-p
           (expand-file-name "compositor/src/ipc/dispatch.rs" compositor-test--root))))

;; ── IPC protocol spec ──────────────────────────────────────

(ert-deftest compositor-test/ipc-protocol-doc-exists ()
  "IPC protocol specification exists."
  (should (file-exists-p
           (expand-file-name "docs/ipc-protocol.md" compositor-test--root))))

(ert-deftest compositor-test/ipc-protocol-has-message-types ()
  "IPC protocol docs contain >= 20 message types."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "docs/ipc-protocol.md" compositor-test--root))
    ;; Count `:type` occurrences in message type headers
    (let ((count 0))
      (goto-char (point-min))
      (while (re-search-forward "^####.*`:.*`" nil t)
        (setq count (1+ count)))
      (should (>= count 20)))))

;; ── Debug tools ────────────────────────────────────────────

(ert-deftest compositor-test/python-ipc-client-exists ()
  "Python debug client exists."
  (should (file-exists-p
           (expand-file-name "tools/ewwm-ipc-client.py" compositor-test--root))))

;;; compositor-test.el ends here
