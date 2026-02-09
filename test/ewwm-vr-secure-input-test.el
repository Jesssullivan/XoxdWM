;;; ewwm-vr-secure-input-test.el --- Tests for ewwm-vr-secure-input  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-vr-secure-input)

;; Forward-declare so `let' creates dynamic binding
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-vr-secure-input-enable) t)))

(ert-deftest ewwm-vr-secure-input/timeout-defcustom ()
  "Default timeout is 30."
  (should (= (default-value 'ewwm-vr-secure-input-timeout) 30)))

(ert-deftest ewwm-vr-secure-input/border-color-defcustom ()
  "Default border color is red."
  (should (string= (default-value 'ewwm-vr-secure-input-border-color) "red")))

(ert-deftest ewwm-vr-secure-input/pause-gaze-defcustom ()
  "Default pause gaze is t."
  (should (eq (default-value 'ewwm-vr-secure-input-pause-gaze) t)))

(ert-deftest ewwm-vr-secure-input/pause-wink-defcustom ()
  "Default pause wink is t."
  (should (eq (default-value 'ewwm-vr-secure-input-pause-wink) t)))

(ert-deftest ewwm-vr-secure-input/pause-eeg-defcustom ()
  "Default pause eeg is t."
  (should (eq (default-value 'ewwm-vr-secure-input-pause-eeg) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/active-exists ()
  "Active variable exists."
  (should (boundp 'ewwm-vr-secure-input--active)))

(ert-deftest ewwm-vr-secure-input/reason-exists ()
  "Reason variable exists."
  (should (boundp 'ewwm-vr-secure-input--reason)))

(ert-deftest ewwm-vr-secure-input/surface-id-exists ()
  "Surface-id variable exists."
  (should (boundp 'ewwm-vr-secure-input--surface-id)))

(ert-deftest ewwm-vr-secure-input/timer-exists ()
  "Timer variable exists."
  (should (boundp 'ewwm-vr-secure-input--timer)))

;; ── Hooks ─────────────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/enter-hook-exists ()
  "Enter hook exists."
  (should (boundp 'ewwm-vr-secure-input-enter-hook)))

(ert-deftest ewwm-vr-secure-input/exit-hook-exists ()
  "Exit hook exists."
  (should (boundp 'ewwm-vr-secure-input-exit-hook)))

;; ── Interactive commands ──────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/toggle-interactive ()
  "ewwm-vr-secure-input-toggle is interactive."
  (should (commandp 'ewwm-vr-secure-input-toggle)))

(ert-deftest ewwm-vr-secure-input/status-interactive ()
  "ewwm-vr-secure-input-status is interactive."
  (should (commandp 'ewwm-vr-secure-input-status)))

;; ── Functions ─────────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/enter-fboundp ()
  "ewwm-vr-secure-input-enter is defined."
  (should (fboundp 'ewwm-vr-secure-input-enter)))

(ert-deftest ewwm-vr-secure-input/exit-fboundp ()
  "ewwm-vr-secure-input-exit is defined."
  (should (fboundp 'ewwm-vr-secure-input-exit)))

(ert-deftest ewwm-vr-secure-input/active-p-fboundp ()
  "ewwm-vr-secure-input-active-p is defined."
  (should (fboundp 'ewwm-vr-secure-input-active-p)))

;; ── active-p ──────────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/active-p-returns-active ()
  "active-p returns ewwm-vr-secure-input--active."
  (let ((ewwm-vr-secure-input--active t))
    (should (ewwm-vr-secure-input-active-p)))
  (let ((ewwm-vr-secure-input--active nil))
    (should-not (ewwm-vr-secure-input-active-p))))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/on-entered-sets-state ()
  "on-entered sets active, reason, surface-id."
  (let ((ewwm-vr-secure-input--active nil)
        (ewwm-vr-secure-input--reason nil)
        (ewwm-vr-secure-input--surface-id nil)
        (ewwm-vr-secure-input-enter-hook nil))
    (ewwm-vr-secure-input--on-entered '(:reason "test" :surface-id 42))
    (should (eq ewwm-vr-secure-input--active t))
    (should (string= ewwm-vr-secure-input--reason "test"))
    (should (= ewwm-vr-secure-input--surface-id 42))))

(ert-deftest ewwm-vr-secure-input/on-exited-clears-state ()
  "on-exited clears active, reason, surface-id."
  (let ((ewwm-vr-secure-input--active t)
        (ewwm-vr-secure-input--reason "test")
        (ewwm-vr-secure-input--surface-id 42)
        (ewwm-vr-secure-input-exit-hook nil))
    (ewwm-vr-secure-input--on-exited '())
    (should-not ewwm-vr-secure-input--active)
    (should-not ewwm-vr-secure-input--reason)
    (should-not ewwm-vr-secure-input--surface-id)))

(ert-deftest ewwm-vr-secure-input/on-timeout-clears-and-runs-hook ()
  "on-timeout clears active and runs exit hook."
  (let ((ewwm-vr-secure-input--active t)
        (ewwm-vr-secure-input--reason "test")
        (ewwm-vr-secure-input--surface-id 42)
        (hook-ran nil))
    (let ((ewwm-vr-secure-input-exit-hook
           (list (lambda () (setq hook-ran t)))))
      (ewwm-vr-secure-input--on-timeout '())
      (should-not ewwm-vr-secure-input--active)
      (should hook-ran))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/register-events ()
  "ewwm-vr-secure-input--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-secure-input--register-events)
    (should (assq :secure-input-entered ewwm-ipc--event-handlers))
    (should (assq :secure-input-exited ewwm-ipc--event-handlers))
    (should (assq :secure-input-auto-exit-timeout ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-secure-input/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-secure-input--register-events)
    (ewwm-vr-secure-input--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :secure-input-entered))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Teardown ──────────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/teardown-clears-state ()
  "Teardown clears all state."
  (let ((ewwm-vr-secure-input--active t)
        (ewwm-vr-secure-input--reason "test")
        (ewwm-vr-secure-input--surface-id 42)
        (ewwm-vr-secure-input--timer nil))
    (ewwm-vr-secure-input-exit)
    (should-not ewwm-vr-secure-input--active)
    (should-not ewwm-vr-secure-input--reason)
    (should-not ewwm-vr-secure-input--surface-id)
    (should-not ewwm-vr-secure-input--timer)))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/mode-line-active ()
  "Mode-line shows [SECURE] when active."
  (let ((ewwm-vr-secure-input--active t))
    (should (string= (ewwm-vr-secure-input-mode-line-string) " [SECURE]"))))

(ert-deftest ewwm-vr-secure-input/mode-line-inactive ()
  "Mode-line nil when not active."
  (let ((ewwm-vr-secure-input--active nil))
    (should-not (ewwm-vr-secure-input-mode-line-string))))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-secure-input/provides-feature ()
  "ewwm-vr-secure-input provides its feature."
  (should (featurep 'ewwm-vr-secure-input)))

;;; ewwm-vr-secure-input-test.el ends here
