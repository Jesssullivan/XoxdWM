;;; ewwm-secrets-compositor-test.el --- Tests for ewwm-secrets-compositor.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets-compositor)

;; Forward-declare so `let' creates dynamic binding
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/enable-defcustom ()
  "ewwm-secrets-compositor-enable defaults to t."
  (should (eq (default-value 'ewwm-secrets-compositor-enable) t)))

(ert-deftest ewwm-secrets-compositor/autotype-delay-defcustom ()
  "ewwm-vr-autotype-delay-ms defaults to 10."
  (should (= (default-value 'ewwm-vr-autotype-delay-ms) 10)))

(ert-deftest ewwm-secrets-compositor/verify-surface-defcustom ()
  "ewwm-secrets-compositor-verify-surface defaults to t."
  (should (eq (default-value 'ewwm-secrets-compositor-verify-surface) t)))

(ert-deftest ewwm-secrets-compositor/timeout-defcustom ()
  "ewwm-secrets-compositor-timeout-ms defaults to 2000."
  (should (= (default-value 'ewwm-secrets-compositor-timeout-ms) 2000)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/typing-p-variable-exists ()
  "ewwm-secrets-compositor--typing-p variable exists."
  (should (boundp 'ewwm-secrets-compositor--typing-p)))

(ert-deftest ewwm-secrets-compositor/last-surface-id-variable-exists ()
  "ewwm-secrets-compositor--last-surface-id variable exists."
  (should (boundp 'ewwm-secrets-compositor--last-surface-id)))

(ert-deftest ewwm-secrets-compositor/last-result-variable-exists ()
  "ewwm-secrets-compositor--last-result variable exists."
  (should (boundp 'ewwm-secrets-compositor--last-result)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/complete-hook-exists ()
  "ewwm-secrets-compositor-complete-hook exists."
  (should (boundp 'ewwm-secrets-compositor-complete-hook)))

(ert-deftest ewwm-secrets-compositor/error-hook-exists ()
  "ewwm-secrets-compositor-error-hook exists."
  (should (boundp 'ewwm-secrets-compositor-error-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/status-command-interactive ()
  "ewwm-secrets-compositor-status is interactive."
  (should (commandp 'ewwm-secrets-compositor-status)))

(ert-deftest ewwm-secrets-compositor/abort-command-interactive ()
  "ewwm-secrets-compositor-abort is interactive."
  (should (commandp 'ewwm-secrets-compositor-abort)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/available-p-exists ()
  "ewwm-secrets-compositor-available-p is defined."
  (should (fboundp 'ewwm-secrets-compositor-available-p)))

(ert-deftest ewwm-secrets-compositor/type-string-exists ()
  "ewwm-secrets-compositor--type-string is defined."
  (should (fboundp 'ewwm-secrets-compositor--type-string)))

(ert-deftest ewwm-secrets-compositor/type-credentials-exists ()
  "ewwm-secrets-compositor--type-credentials is defined."
  (should (fboundp 'ewwm-secrets-compositor--type-credentials)))

(ert-deftest ewwm-secrets-compositor/abort-fn-exists ()
  "ewwm-secrets-compositor--abort is defined."
  (should (fboundp 'ewwm-secrets-compositor--abort)))

;; ── Event handlers ──────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/on-autotype-complete-exists ()
  "ewwm-secrets-compositor--on-autotype-complete is defined."
  (should (fboundp 'ewwm-secrets-compositor--on-autotype-complete)))

(ert-deftest ewwm-secrets-compositor/on-autotype-paused-exists ()
  "ewwm-secrets-compositor--on-autotype-paused is defined."
  (should (fboundp 'ewwm-secrets-compositor--on-autotype-paused)))

(ert-deftest ewwm-secrets-compositor/on-autotype-resumed-exists ()
  "ewwm-secrets-compositor--on-autotype-resumed is defined."
  (should (fboundp 'ewwm-secrets-compositor--on-autotype-resumed)))

(ert-deftest ewwm-secrets-compositor/on-autotype-error-exists ()
  "ewwm-secrets-compositor--on-autotype-error is defined."
  (should (fboundp 'ewwm-secrets-compositor--on-autotype-error)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/register-events-adds-handlers ()
  "ewwm-secrets-compositor--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-secrets-compositor--register-events)
    (should (assq :autotype-complete ewwm-ipc--event-handlers))
    (should (assq :autotype-paused ewwm-ipc--event-handlers))
    (should (assq :autotype-resumed ewwm-ipc--event-handlers))
    (should (assq :autotype-error ewwm-ipc--event-handlers))))

(ert-deftest ewwm-secrets-compositor/register-events-idempotent ()
  "Calling register twice doesn't duplicate handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-secrets-compositor--register-events)
    (ewwm-secrets-compositor--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :autotype-complete))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Init ────────────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/init-clears-state ()
  "ewwm-secrets-compositor-init clears state variables."
  (let ((ewwm-secrets-compositor--typing-p t)
        (ewwm-secrets-compositor--last-surface-id 42)
        (ewwm-secrets-compositor--last-result '(:status :ok))
        (ewwm-ipc--event-handlers nil))
    (ewwm-secrets-compositor-init)
    (should-not ewwm-secrets-compositor--typing-p)
    (should-not ewwm-secrets-compositor--last-surface-id)
    (should-not ewwm-secrets-compositor--last-result)))

;; ── Teardown ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/teardown-clears-state ()
  "ewwm-secrets-compositor-teardown clears state variables."
  (let ((ewwm-secrets-compositor--typing-p nil)
        (ewwm-secrets-compositor--last-surface-id 42)
        (ewwm-secrets-compositor--last-result '(:status :ok)))
    (ewwm-secrets-compositor-teardown)
    (should-not ewwm-secrets-compositor--typing-p)
    (should-not ewwm-secrets-compositor--last-surface-id)
    (should-not ewwm-secrets-compositor--last-result)))

;; ── Event handler behavior ──────────────────────────────────

(ert-deftest ewwm-secrets-compositor/on-autotype-complete-sets-state ()
  "on-autotype-complete clears typing-p, sets last-result, runs hook."
  (let ((ewwm-secrets-compositor--typing-p t)
        (ewwm-secrets-compositor--last-result nil)
        (hook-ran nil))
    (let ((ewwm-secrets-compositor-complete-hook
           (list (lambda (_sid _chars) (setq hook-ran t)))))
      (ewwm-secrets-compositor--on-autotype-complete
       '(:surface-id 7 :chars-typed 12))
      (should-not ewwm-secrets-compositor--typing-p)
      (should (equal (plist-get ewwm-secrets-compositor--last-result :surface-id) 7))
      (should (equal (plist-get ewwm-secrets-compositor--last-result :chars-typed) 12))
      (should hook-ran))))

(ert-deftest ewwm-secrets-compositor/on-autotype-error-sets-state ()
  "on-autotype-error clears typing-p and runs error hook."
  (let ((ewwm-secrets-compositor--typing-p t)
        (hook-ran nil))
    (let ((ewwm-secrets-compositor-error-hook
           (list (lambda (_msg) (setq hook-ran t)))))
      (ewwm-secrets-compositor--on-autotype-error
       '(:message "surface lost focus"))
      (should-not ewwm-secrets-compositor--typing-p)
      (should hook-ran))))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-compositor/provides-feature ()
  "ewwm-secrets-compositor provides its feature."
  (should (featurep 'ewwm-secrets-compositor)))

;;; ewwm-secrets-compositor-test.el ends here
