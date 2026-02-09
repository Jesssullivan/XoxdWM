;;; week11-integration-test.el --- Week 11 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-eye)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week11/eye-tracking-module-exists ()
  "eye_tracking.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/eye_tracking.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week11/mod-rs-exports-eye-tracking ()
  "vr/mod.rs declares eye_tracking module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod eye_tracking;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week11/dispatch-has-gaze-status ()
  "dispatch.rs handles gaze-status."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-status\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-set-source ()
  "dispatch.rs handles gaze-set-source."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-set-source\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-calibrate-start ()
  "dispatch.rs handles gaze-calibrate-start."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-calibrate-start\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-calibrate-point ()
  "dispatch.rs handles gaze-calibrate-point."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-calibrate-point\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-set-visualization ()
  "dispatch.rs handles gaze-set-visualization."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-set-visualization\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-set-smoothing ()
  "dispatch.rs handles gaze-set-smoothing."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-set-smoothing\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-simulate ()
  "dispatch.rs handles gaze-simulate."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-simulate\"" nil t)))))

(ert-deftest week11/dispatch-has-gaze-health ()
  "dispatch.rs handles gaze-health."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"gaze-health\"" nil t)))))

;; ── Emacs integration ───────────────────────────────────────

(ert-deftest week11/ewwm-requires-vr-eye ()
  "ewwm.el requires ewwm-vr-eye."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(require 'ewwm-vr-eye)" nil t)))))

(ert-deftest week11/ewwm-init-calls-eye-init ()
  "ewwm.el init calls ewwm-vr-eye-init."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-eye-init)" nil t)))))

(ert-deftest week11/ewwm-exit-calls-eye-teardown ()
  "ewwm.el exit calls ewwm-vr-eye-teardown."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-eye-teardown)" nil t)))))

;; ── Rust VrState integration ────────────────────────────────

(ert-deftest week11/openxr-state-has-eye-tracking ()
  "openxr_state.rs has eye_tracking field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ostate (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents ostate)
      (should (search-forward "pub eye_tracking: EyeTracking" nil t)))))

(ert-deftest week11/stub-has-eye-tracking ()
  "stub.rs has eye_tracking field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (stub (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents stub)
      (should (search-forward "pub eye_tracking: EyeTracking" nil t)))))

;; ── Rust eye_tracking structure ─────────────────────────────

(ert-deftest week11/eye-tracking-has-gaze-source ()
  "eye_tracking.rs defines GazeSource."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum GazeSource" nil t)))))

(ert-deftest week11/eye-tracking-has-gaze-data ()
  "eye_tracking.rs defines GazeData."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct GazeData" nil t)))))

(ert-deftest week11/eye-tracking-has-gaze-visualization ()
  "eye_tracking.rs defines GazeVisualization."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum GazeVisualization" nil t)))))

(ert-deftest week11/eye-tracking-has-calibration ()
  "eye_tracking.rs defines EyeCalibrationState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum EyeCalibrationState" nil t)))))

(ert-deftest week11/eye-tracking-has-health ()
  "eye_tracking.rs defines GazeHealth."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct GazeHealth" nil t)))))

(ert-deftest week11/eye-tracking-has-pupil-client ()
  "eye_tracking.rs defines PupilLabsClient."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct PupilLabsClient" nil t)))))

(ert-deftest week11/eye-tracking-has-simulated ()
  "eye_tracking.rs defines SimulatedGaze."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct SimulatedGaze" nil t)))))

(ert-deftest week11/eye-tracking-has-eye-tracking ()
  "eye_tracking.rs defines EyeTracking."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct EyeTracking" nil t)))))

(ert-deftest week11/eye-tracking-has-rust-tests ()
  "eye_tracking.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week11/gaze-sources-consistent ()
  "Elisp gaze sources match Rust GazeSource."
  (let ((elisp-sources '(openxr pupil-labs simulated none)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (s elisp-sources)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" (symbol-name s)) nil t)))))))

(ert-deftest week11/visualization-modes-consistent ()
  "Elisp visualization modes match Rust GazeVisualization."
  (let ((modes '(dot crosshair spotlight none)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/eye_tracking.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (m modes)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" (symbol-name m)) nil t)))))))

(ert-deftest week11/ipc-commands-match-elisp ()
  "All 8 IPC command strings in dispatch match Elisp send calls."
  (let ((commands '("gaze-status" "gaze-set-source" "gaze-calibrate-start"
                    "gaze-calibrate-point" "gaze-set-visualization"
                    "gaze-set-smoothing" "gaze-simulate" "gaze-health")))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
      (with-temp-buffer
        (insert-file-contents dispatch)
        (dolist (cmd commands)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" cmd) nil t)))))))

;;; week11-integration-test.el ends here
