;;; ewwm-secrets-passkey-test.el --- Tests for ewwm-secrets-passkey.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-secrets-passkey)

;; Forward-declare so `let' creates dynamic binding
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/enabled-defcustom ()
  "ewwm-secrets-passkeys-enabled defaults to t."
  (should (eq (default-value 'ewwm-secrets-passkeys-enabled) t)))

(ert-deftest ewwm-secrets-passkey/allow-registration-defcustom ()
  "ewwm-secrets-passkey-allow-registration defaults to t."
  (should (eq (default-value 'ewwm-secrets-passkey-allow-registration) t)))

(ert-deftest ewwm-secrets-passkey/confirm-registration-defcustom ()
  "ewwm-secrets-passkey-confirm-registration defaults to t."
  (should (eq (default-value 'ewwm-secrets-passkey-confirm-registration) t)))

(ert-deftest ewwm-secrets-passkey/timeout-defcustom ()
  "ewwm-secrets-passkey-timeout defaults to 60."
  (should (= (default-value 'ewwm-secrets-passkey-timeout) 60)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/pending-request-variable-exists ()
  "ewwm-secrets-passkey--pending-request variable exists."
  (should (boundp 'ewwm-secrets-passkey--pending-request)))

(ert-deftest ewwm-secrets-passkey/registered-origins-variable-exists ()
  "ewwm-secrets-passkey--registered-origins variable exists."
  (should (boundp 'ewwm-secrets-passkey--registered-origins)))

(ert-deftest ewwm-secrets-passkey/active-variable-exists ()
  "ewwm-secrets-passkey--active variable exists."
  (should (boundp 'ewwm-secrets-passkey--active)))

(ert-deftest ewwm-secrets-passkey/events-registered-variable-exists ()
  "ewwm-secrets-passkey--events-registered variable exists."
  (should (boundp 'ewwm-secrets-passkey--events-registered)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/register-hook-exists ()
  "ewwm-secrets-passkey-register-hook exists."
  (should (boundp 'ewwm-secrets-passkey-register-hook)))

(ert-deftest ewwm-secrets-passkey/authenticate-hook-exists ()
  "ewwm-secrets-passkey-authenticate-hook exists."
  (should (boundp 'ewwm-secrets-passkey-authenticate-hook)))

(ert-deftest ewwm-secrets-passkey/error-hook-exists ()
  "ewwm-secrets-passkey-error-hook exists."
  (should (boundp 'ewwm-secrets-passkey-error-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/status-interactive ()
  "ewwm-secrets-passkey-status is interactive."
  (should (commandp 'ewwm-secrets-passkey-status)))

(ert-deftest ewwm-secrets-passkey/list-interactive ()
  "ewwm-secrets-passkey-list is interactive."
  (should (commandp 'ewwm-secrets-passkey-list)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/register-events ()
  "ewwm-secrets-passkey--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil)
        (ewwm-secrets-passkey--events-registered nil))
    (ewwm-secrets-passkey--register-events)
    (should (assq :passkey-request ewwm-ipc--event-handlers))
    (should (assq :passkey-response ewwm-ipc--event-handlers))))

(ert-deftest ewwm-secrets-passkey/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil)
        (ewwm-secrets-passkey--events-registered nil))
    (ewwm-secrets-passkey--register-events)
    (ewwm-secrets-passkey--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :passkey-request))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Teardown ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/teardown-clears-state ()
  "ewwm-secrets-passkey-teardown clears all internal state."
  (let ((ewwm-secrets-passkey--pending-request '(:action "get"))
        (ewwm-secrets-passkey--registered-origins '("https://example.com"))
        (ewwm-secrets-passkey--active t)
        (ewwm-secrets-passkey--events-registered t))
    (ewwm-secrets-passkey-teardown)
    (should-not ewwm-secrets-passkey--pending-request)
    (should-not ewwm-secrets-passkey--registered-origins)
    (should-not ewwm-secrets-passkey--active)
    (should-not ewwm-secrets-passkey--events-registered)))

(ert-deftest ewwm-secrets-passkey/teardown-clears-events ()
  "ewwm-secrets-passkey-teardown removes event handlers."
  (let ((ewwm-ipc--event-handlers nil)
        (ewwm-secrets-passkey--events-registered nil))
    (ewwm-secrets-passkey--register-events)
    (should (assq :passkey-request ewwm-ipc--event-handlers))
    (ewwm-secrets-passkey-teardown)
    (should-not (assq :passkey-request ewwm-ipc--event-handlers))
    (should-not (assq :passkey-response ewwm-ipc--event-handlers))))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-passkey/provides-feature ()
  "ewwm-secrets-passkey provides its feature."
  (should (featurep 'ewwm-secrets-passkey)))

;;; ewwm-secrets-passkey-test.el ends here
