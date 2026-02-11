;;; week19-integration-test.el --- Week 19 BCI integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-core)
(require 'ewwm-bci-attention)
(require 'ewwm-bci-ssvep)
(require 'ewwm-bci-p300)
(require 'ewwm-bci-mi)
(require 'ewwm-bci-nfb)
(require 'ewwm-bci-multimodal)

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-ipc--event-handlers)
(defvar ewwm-bci-multimodal-enabled)
(defvar ewwm-bci-multimodal--active)
(defvar ewwm-bci-multimodal-adaptive-dwell)
(defvar ewwm-bci-multimodal-dwell-focused-ms)
(defvar ewwm-bci-multimodal-dwell-relaxed-ms)
(defvar ewwm-bci-multimodal-dwell-default-ms)
(defvar ewwm-bci-multimodal-focused-threshold)
(defvar ewwm-bci-multimodal-relaxed-threshold)
(defvar ewwm-bci-multimodal--dwell-override)
(defvar ewwm-bci-multimodal--last-attention-score)
(defvar ewwm-bci-multimodal--pending-confirm)
(defvar ewwm-bci-multimodal--confirm-timer)
(defvar ewwm-bci-multimodal-fusion-hook)
(defvar ewwm-bci-attention-enabled)
(defvar ewwm-bci-attention--score)
(defvar ewwm-bci-attention--state)
(defvar ewwm-bci-attention--band-powers)
(defvar ewwm-bci-attention--dnd-active)
(defvar ewwm-bci-attention--state-start-time)
(defvar ewwm-bci-attention--last-update-time)
(defvar ewwm-bci-attention--history)
(defvar ewwm-bci-attention-change-hook)
(defvar ewwm-bci-attention-dnd-threshold)
(defvar ewwm-bci-attention-threshold)
(defvar ewwm-bci-attention-drowsy-threshold)
(defvar ewwm-bci-attention-update-interval)
(defvar ewwm-bci-attention-auto-save)
(defvar ewwm-bci-p300-enabled)
(defvar ewwm-bci-p300--active)
(defvar ewwm-bci-p300--callback)
(defvar ewwm-bci-p300--targets)
(defvar ewwm-bci-p300--timeout-timer)
(defvar ewwm-bci-p300--trial-count)
(defvar ewwm-bci-p300--last-result)
(defvar ewwm-bci-p300-detect-hook)

;; ── All modules load without error ──────────────────────────

(ert-deftest week19/all-modules-loaded ()
  "All 7 BCI modules provide their features."
  (dolist (feat '(ewwm-bci-core
                  ewwm-bci-attention
                  ewwm-bci-ssvep
                  ewwm-bci-p300
                  ewwm-bci-mi
                  ewwm-bci-nfb
                  ewwm-bci-multimodal))
    (should (featurep feat))))

(ert-deftest week19/module-count ()
  "Exactly 7 ewwm-bci-* modules loaded."
  (let ((count 0))
    (dolist (feat '(ewwm-bci-core
                    ewwm-bci-attention
                    ewwm-bci-ssvep
                    ewwm-bci-p300
                    ewwm-bci-mi
                    ewwm-bci-nfb
                    ewwm-bci-multimodal))
      (when (featurep feat)
        (cl-incf count)))
    (should (= count 7))))

;; ── Init functions callable ─────────────────────────────────

(ert-deftest week19/all-init-functions-callable ()
  "All module init functions are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (dolist (fn '(ewwm-bci-init
                  ewwm-bci-attention-init
                  ewwm-bci-ssvep-init
                  ewwm-bci-p300-init
                  ewwm-bci-mi-init
                  ewwm-bci-nfb-init
                  ewwm-bci-multimodal-init))
      (should (fboundp fn))
      (funcall fn))))

;; ── Teardown functions callable ─────────────────────────────

(ert-deftest week19/all-teardown-functions-callable ()
  "All module teardown functions are callable."
  (dolist (fn '(ewwm-bci-teardown
                ewwm-bci-attention-teardown
                ewwm-bci-ssvep-teardown
                ewwm-bci-p300-teardown
                ewwm-bci-mi-teardown
                ewwm-bci-nfb-teardown
                ewwm-bci-multimodal-teardown))
    (should (fboundp fn))))

;; ── Defcustom groups exist ──────────────────────────────────

(ert-deftest week19/defcustom-groups-exist ()
  "All expected defcustom groups exist."
  (dolist (grp '(ewwm-bci
                 ewwm-bci-attention
                 ewwm-bci-ssvep
                 ewwm-bci-p300
                 ewwm-bci-mi
                 ewwm-bci-nfb
                 ewwm-bci-multimodal))
    (should (get grp 'custom-group))))

;; ── Cross-module: attention -> multimodal dwell ─────────────

(ert-deftest week19/attention-affects-multimodal-dwell ()
  "Attention state change feeds into multimodal dwell."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 150)
        (ewwm-bci-multimodal-dwell-relaxed-ms 400)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      ;; Simulate attention score feeding into multimodal
      (ewwm-bci-multimodal--handle-attention '(:score 0.85))
      (should (= ewwm-bci-multimodal--dwell-override 150))
      ;; Score drops -> relaxed dwell
      (ewwm-bci-multimodal--handle-attention '(:score 0.3))
      (should (= ewwm-bci-multimodal--dwell-override 400)))))

;; ── Cross-module: P300 callback with multimodal ─────────────

(ert-deftest week19/p300-callback-mechanism ()
  "P300 callback mechanism works for multimodal integration."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.6)
        (ewwm-bci-p300-detect-hook nil)
        (cb-result nil))
    (let ((ewwm-bci-p300--callback
           (lambda (target confidence)
             (setq cb-result (list target confidence)))))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "confirm"
         :confidence 0.9
         :latency-ms 350
         :status detected))
      (should (equal (car cb-result) "confirm"))
      (should (= (cadr cb-result) 0.9)))))

;; ── IPC event handlers are functions ────────────────────────

(ert-deftest week19/ipc-handlers-are-functions ()
  "All IPC event handlers are valid functions."
  (dolist (fn '(ewwm-bci--on-bci-connected
                ewwm-bci--on-bci-disconnected
                ewwm-bci--on-bci-quality
                ewwm-bci--on-bci-error
                ewwm-bci--on-bci-frame
                ewwm-bci-attention--on-bci-attention
                ewwm-bci-ssvep--on-bci-ssvep
                ewwm-bci-p300--on-bci-p300
                ewwm-bci-mi--on-bci-mi
                ewwm-bci-mi--on-bci-mi-calibration
                ewwm-bci-nfb--on-bci-nfb-frame
                ewwm-bci-multimodal--on-bci-multimodal))
    (should (fboundp fn))))

;; ── Interactive commands are functions ───────────────────────

(ert-deftest week19/interactive-commands-exist ()
  "All interactive commands are defined."
  (dolist (cmd '(ewwm-bci-start
                 ewwm-bci-stop
                 ewwm-bci-restart
                 ewwm-bci-status
                 ewwm-bci-signal-quality
                 ewwm-bci-hardware-check
                 ewwm-bci-attention-status
                 ewwm-bci-attention-calibrate
                 ewwm-bci-attention-toggle
                 ewwm-bci-ssvep-status
                 ewwm-bci-ssvep-configure
                 ewwm-bci-p300-start
                 ewwm-bci-p300-stop
                 ewwm-bci-p300-status
                 ewwm-bci-mi-calibrate
                 ewwm-bci-mi-status
                 ewwm-bci-mi-toggle
                 ewwm-bci-nfb-status
                 ewwm-bci-nfb-export-session
                 ewwm-bci-multimodal-status
                 ewwm-bci-multimodal-toggle))
    (should (fboundp cmd))))

;; ── Event registration creates expected entries ──────────────

(ert-deftest week19/event-registration-all-handlers ()
  "All modules register their expected event handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci--register-events)
    (ewwm-bci-attention--register-events)
    (ewwm-bci-ssvep--register-events)
    (ewwm-bci-p300--register-events)
    (ewwm-bci-mi--register-events)
    (ewwm-bci-nfb--register-events)
    (ewwm-bci-multimodal--register-events)
    ;; Check all expected event keys
    (dolist (key '(:bci-connected
                   :bci-disconnected
                   :bci-quality
                   :bci-error
                   :bci-frame
                   :bci-attention
                   :bci-ssvep
                   :bci-p300
                   :bci-mi
                   :bci-mi-calibration
                   :bci-nfb-frame
                   :bci-multimodal))
      (should (assq key ewwm-ipc--event-handlers)))))

;; ── Hook variables exist and are nil by default ─────────────

(ert-deftest week19/hooks-exist-nil-default ()
  "All hook variables exist and default to nil."
  (dolist (hook '(ewwm-bci-connected-hook
                  ewwm-bci-disconnected-hook
                  ewwm-bci-quality-change-hook
                  ewwm-bci-error-hook
                  ewwm-bci-attention-change-hook
                  ewwm-bci-ssvep-select-hook
                  ewwm-bci-p300-detect-hook
                  ewwm-bci-mi-classify-hook
                  ewwm-bci-nfb-start-hook
                  ewwm-bci-nfb-stop-hook
                  ewwm-bci-multimodal-fusion-hook))
    (should (boundp hook))
    (should-not (default-value hook))))

;; ── Status commands produce messages ────────────────────────

(ert-deftest week19/status-commands-callable ()
  "All status commands are interactive."
  (dolist (cmd '(ewwm-bci-status
                 ewwm-bci-attention-status
                 ewwm-bci-ssvep-status
                 ewwm-bci-p300-status
                 ewwm-bci-mi-status
                 ewwm-bci-nfb-status
                 ewwm-bci-multimodal-status))
    (should (commandp cmd))))

;; ── Defcustom types are correct ─────────────────────────────

(ert-deftest week19/defcustom-types-correct ()
  "Key defcustoms have the expected types."
  ;; Boolean types
  (dolist (var '(ewwm-bci-mi-enabled
                 ewwm-bci-mi-feedback
                 ewwm-bci-multimodal-enabled
                 ewwm-bci-multimodal-two-factor
                 ewwm-bci-multimodal-three-factor-security
                 ewwm-bci-p300-enabled
                 ewwm-bci-ssvep-enabled
                 ewwm-bci-attention-enabled
                 ewwm-bci-artifact-rejection
                 ewwm-bci-auto-reconnect
                 ewwm-bci-nfb-auto-export))
    (should (eq (get var 'custom-type) 'boolean)))
  ;; Number types
  (dolist (var '(ewwm-bci-mi-min-confidence
                 ewwm-bci-multimodal-focused-threshold
                 ewwm-bci-multimodal-relaxed-threshold
                 ewwm-bci-p300-min-confidence
                 ewwm-bci-ssvep-min-confidence
                 ewwm-bci-attention-threshold
                 ewwm-bci-nfb-log-interval))
    (should (eq (get var 'custom-type) 'number)))
  ;; Integer types
  (dolist (var '(ewwm-bci-mi-cooldown-ms
                 ewwm-bci-multimodal-dwell-focused-ms
                 ewwm-bci-multimodal-dwell-relaxed-ms
                 ewwm-bci-multimodal-dwell-default-ms
                 ewwm-bci-p300-repetitions
                 ewwm-bci-ssvep-cooldown-ms
                 ewwm-bci-nfb-display-channels
                 ewwm-bci-nfb-bar-width
                 ewwm-bci-board-id
                 ewwm-bci-sample-rate))
    (should (eq (get var 'custom-type) 'integer))))

;; ── Elisp module files exist ────────────────────────────────

(ert-deftest week19/elisp-module-files-exist ()
  "All 7 BCI Elisp module files exist."
  (let ((root (locate-dominating-file default-directory ".git")))
    (dolist (file '("lisp/vr/ewwm-bci-core.el"
                    "lisp/vr/ewwm-bci-attention.el"
                    "lisp/vr/ewwm-bci-ssvep.el"
                    "lisp/vr/ewwm-bci-p300.el"
                    "lisp/vr/ewwm-bci-mi.el"
                    "lisp/vr/ewwm-bci-nfb.el"
                    "lisp/vr/ewwm-bci-multimodal.el"))
      (should (file-exists-p
               (expand-file-name file root))))))

;;; week19-integration-test.el ends here
