;;; ewwm-secrets-gaze-away-test.el --- Tests for ewwm-secrets-gaze-away  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-secrets-gaze-away)

;; Forward-declare so `let' creates dynamic binding
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-secrets-gaze-away-enable) t)))

(ert-deftest ewwm-secrets-gaze-away/pause-ms-defcustom ()
  "Default pause-ms is 500."
  (should (= (default-value 'ewwm-secrets-gaze-away-pause-ms) 500)))

(ert-deftest ewwm-secrets-gaze-away/resume-ms-defcustom ()
  "Default resume-ms is 300."
  (should (= (default-value 'ewwm-secrets-gaze-away-resume-ms) 300)))

(ert-deftest ewwm-secrets-gaze-away/abort-ms-defcustom ()
  "Default abort-ms is 5000."
  (should (= (default-value 'ewwm-secrets-gaze-away-abort-ms) 5000)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/monitoring-exists ()
  "Monitoring variable exists."
  (should (boundp 'ewwm-secrets-gaze-away--monitoring)))

(ert-deftest ewwm-secrets-gaze-away/target-surface-exists ()
  "Target surface variable exists."
  (should (boundp 'ewwm-secrets-gaze-away--target-surface)))

(ert-deftest ewwm-secrets-gaze-away/away-since-exists ()
  "Away-since variable exists."
  (should (boundp 'ewwm-secrets-gaze-away--away-since)))

(ert-deftest ewwm-secrets-gaze-away/paused-exists ()
  "Paused variable exists."
  (should (boundp 'ewwm-secrets-gaze-away--paused)))

(ert-deftest ewwm-secrets-gaze-away/abort-timer-exists ()
  "Abort timer variable exists."
  (should (boundp 'ewwm-secrets-gaze-away--abort-timer)))

;; ── Hooks ─────────────────────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/pause-hook-exists ()
  "Pause hook exists."
  (should (boundp 'ewwm-secrets-gaze-away-pause-hook)))

(ert-deftest ewwm-secrets-gaze-away/resume-hook-exists ()
  "Resume hook exists."
  (should (boundp 'ewwm-secrets-gaze-away-resume-hook)))

(ert-deftest ewwm-secrets-gaze-away/abort-hook-exists ()
  "Abort hook exists."
  (should (boundp 'ewwm-secrets-gaze-away-abort-hook)))

;; ── Functions ─────────────────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/start-fboundp ()
  "ewwm-secrets-gaze-away-start is defined."
  (should (fboundp 'ewwm-secrets-gaze-away-start)))

(ert-deftest ewwm-secrets-gaze-away/stop-fboundp ()
  "ewwm-secrets-gaze-away-stop is defined."
  (should (fboundp 'ewwm-secrets-gaze-away-stop)))

;; ── start errors when disabled ──────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/start-errors-when-disabled ()
  "start errors when enable is nil."
  (let ((ewwm-secrets-gaze-away-enable nil))
    (should-error (ewwm-secrets-gaze-away-start))))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/on-autotype-paused-sets-state ()
  "on-autotype-paused sets paused to t and runs pause hook."
  (let ((ewwm-secrets-gaze-away--paused nil)
        (ewwm-secrets-gaze-away--monitoring t)
        (hook-ran nil))
    (let ((ewwm-secrets-gaze-away-pause-hook
           (list (lambda () (setq hook-ran t)))))
      (ewwm-secrets-gaze-away--on-autotype-paused '())
      (should (eq ewwm-secrets-gaze-away--paused t))
      (should hook-ran))))

(ert-deftest ewwm-secrets-gaze-away/on-autotype-resumed-clears-state ()
  "on-autotype-resumed clears paused and away-since, runs resume hook."
  (let ((ewwm-secrets-gaze-away--paused t)
        (ewwm-secrets-gaze-away--monitoring t)
        (ewwm-secrets-gaze-away--away-since (current-time))
        (ewwm-secrets-gaze-away--abort-timer nil)
        (hook-ran nil))
    (let ((ewwm-secrets-gaze-away-resume-hook
           (list (lambda () (setq hook-ran t)))))
      (ewwm-secrets-gaze-away--on-autotype-resumed '())
      (should-not ewwm-secrets-gaze-away--paused)
      (should-not ewwm-secrets-gaze-away--away-since)
      (should hook-ran))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/register-events ()
  "ewwm-secrets-gaze-away--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-secrets-gaze-away--register-events)
    (should (assq :gaze-update ewwm-ipc--event-handlers))
    (should (assq :autotype-paused ewwm-ipc--event-handlers))
    (should (assq :autotype-resumed ewwm-ipc--event-handlers))
    (should (assq :gaze-target-changed ewwm-ipc--event-handlers))))

(ert-deftest ewwm-secrets-gaze-away/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-secrets-gaze-away--register-events)
    (ewwm-secrets-gaze-away--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :gaze-update))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Teardown (via stop) ─────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/stop-clears-state ()
  "ewwm-secrets-gaze-away-stop clears all state."
  (let ((ewwm-secrets-gaze-away--monitoring t)
        (ewwm-secrets-gaze-away--target-surface 42)
        (ewwm-secrets-gaze-away--away-since (current-time))
        (ewwm-secrets-gaze-away--paused t)
        (ewwm-secrets-gaze-away--abort-timer nil))
    (ewwm-secrets-gaze-away-stop)
    (should-not ewwm-secrets-gaze-away--monitoring)
    (should-not ewwm-secrets-gaze-away--target-surface)
    (should-not ewwm-secrets-gaze-away--away-since)
    (should-not ewwm-secrets-gaze-away--paused)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-gaze-away/provides-feature ()
  "ewwm-secrets-gaze-away provides its feature."
  (should (featurep 'ewwm-secrets-gaze-away)))

;;; ewwm-secrets-gaze-away-test.el ends here
