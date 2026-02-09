;;; week10-integration-test.el --- Week 10 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-input)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week10/vr-interaction-module-exists ()
  "vr_interaction.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/vr_interaction.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week10/mod-rs-exports-vr-interaction ()
  "vr/mod.rs declares vr_interaction module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod vr_interaction;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week10/dispatch-has-pointer-state ()
  "dispatch.rs handles vr-pointer-state."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-pointer-state\"" nil t)))))

(ert-deftest week10/dispatch-has-vr-click ()
  "dispatch.rs handles vr-click."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-click\"" nil t)))))

(ert-deftest week10/dispatch-has-vr-grab ()
  "dispatch.rs handles vr-grab."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-grab\"" nil t)))))

(ert-deftest week10/dispatch-has-vr-grab-release ()
  "dispatch.rs handles vr-grab-release."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-grab-release\"" nil t)))))

(ert-deftest week10/dispatch-has-adjust-depth ()
  "dispatch.rs handles vr-adjust-depth."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-adjust-depth\"" nil t)))))

(ert-deftest week10/dispatch-has-set-follow ()
  "dispatch.rs handles vr-set-follow."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-set-follow\"" nil t)))))

(ert-deftest week10/dispatch-has-set-gaze-offset ()
  "dispatch.rs handles vr-set-gaze-offset."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-set-gaze-offset\"" nil t)))))

(ert-deftest week10/dispatch-has-calibrate-confirm ()
  "dispatch.rs handles vr-calibrate-confirm."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-calibrate-confirm\"" nil t)))))

;; ── Emacs integration ───────────────────────────────────────

(ert-deftest week10/ewwm-requires-vr-input ()
  "ewwm.el requires ewwm-vr-input."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(require 'ewwm-vr-input)" nil t)))))

(ert-deftest week10/ewwm-init-calls-vr-input-init ()
  "ewwm.el init calls ewwm-vr-input-init."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-input-init)" nil t)))))

(ert-deftest week10/ewwm-exit-calls-vr-input-teardown ()
  "ewwm.el exit calls ewwm-vr-input-teardown."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-input-teardown)" nil t)))))

;; ── Rust VrState integration ────────────────────────────────

(ert-deftest week10/openxr-state-has-interaction ()
  "openxr_state.rs has interaction field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ostate (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents ostate)
      (should (search-forward "pub interaction: VrInteraction" nil t)))))

(ert-deftest week10/stub-has-interaction ()
  "stub.rs has interaction field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (stub (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents stub)
      (should (search-forward "pub interaction: VrInteraction" nil t)))))

;; ── Rust vr_interaction structure ─────────────────────────────

(ert-deftest week10/vr-interaction-has-ray ()
  "vr_interaction.rs defines Ray."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct Ray" nil t)))))

(ert-deftest week10/vr-interaction-has-gaze-config ()
  "vr_interaction.rs defines GazeRayConfig."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct GazeRayConfig" nil t)))))

(ert-deftest week10/vr-interaction-has-grab-state ()
  "vr_interaction.rs defines GrabState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct GrabState" nil t)))))

(ert-deftest week10/vr-interaction-has-follow-mode ()
  "vr_interaction.rs defines FollowMode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum FollowMode" nil t)))))

(ert-deftest week10/vr-interaction-has-calibration ()
  "vr_interaction.rs defines CalibrationState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct CalibrationState" nil t)))))

(ert-deftest week10/vr-interaction-has-vr-interaction ()
  "vr_interaction.rs defines VrInteraction."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct VrInteraction" nil t)))))

(ert-deftest week10/vr-interaction-has-rust-tests ()
  "vr_interaction.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week10/follow-modes-consistent ()
  "Elisp follow modes match Rust FollowMode."
  (let ((elisp-modes '(none lazy sticky locked)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/vr_interaction.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (m elisp-modes)
          (goto-char (point-min))
          (should (search-forward (capitalize (symbol-name m)) nil t)))))))

(ert-deftest week10/ipc-commands-match-elisp ()
  "All 8 IPC command strings in dispatch match Elisp send calls."
  (let ((commands '("vr-pointer-state" "vr-click" "vr-grab" "vr-grab-release"
                    "vr-adjust-depth" "vr-set-follow" "vr-set-gaze-offset"
                    "vr-calibrate-confirm")))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
      (with-temp-buffer
        (insert-file-contents dispatch)
        (dolist (cmd commands)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" cmd) nil t)))))))

;;; week10-integration-test.el ends here
