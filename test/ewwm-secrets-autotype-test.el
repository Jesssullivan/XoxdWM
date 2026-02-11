;;; ewwm-secrets-autotype-test.el --- Tests for ewwm-secrets-autotype.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets-autotype)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/backend-defcustom ()
  "ewwm-secrets-autotype-backend defaults to auto."
  (should (eq (default-value 'ewwm-secrets-autotype-backend) 'auto)))

(ert-deftest ewwm-secrets-autotype/secure-input-defcustom ()
  "ewwm-secrets-autotype-secure-input defaults to t."
  (should (eq (default-value 'ewwm-secrets-autotype-secure-input) t)))

(ert-deftest ewwm-secrets-autotype/gaze-away-defcustom ()
  "ewwm-secrets-autotype-gaze-away defaults to t."
  (should (eq (default-value 'ewwm-secrets-autotype-gaze-away) t)))

(ert-deftest ewwm-secrets-autotype/clear-after-defcustom ()
  "ewwm-secrets-autotype-clear-after defaults to t."
  (should (eq (default-value 'ewwm-secrets-autotype-clear-after) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/active-variable-exists ()
  "ewwm-secrets-autotype--active variable exists."
  (should (boundp 'ewwm-secrets-autotype--active)))

(ert-deftest ewwm-secrets-autotype/backend-name-variable-exists ()
  "ewwm-secrets-autotype--backend-name variable exists."
  (should (boundp 'ewwm-secrets-autotype--backend-name)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/start-hook-exists ()
  "ewwm-secrets-autotype-start-hook exists."
  (should (boundp 'ewwm-secrets-autotype-start-hook)))

(ert-deftest ewwm-secrets-autotype/complete-hook-exists ()
  "ewwm-secrets-autotype-complete-hook exists."
  (should (boundp 'ewwm-secrets-autotype-complete-hook)))

(ert-deftest ewwm-secrets-autotype/error-hook-exists ()
  "ewwm-secrets-autotype-error-hook exists."
  (should (boundp 'ewwm-secrets-autotype-error-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/login-interactive ()
  "ewwm-secrets-autotype-login is interactive."
  (should (commandp 'ewwm-secrets-autotype-login)))

(ert-deftest ewwm-secrets-autotype/status-interactive ()
  "ewwm-secrets-autotype-status is interactive."
  (should (commandp 'ewwm-secrets-autotype-status)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/select-backend-exists ()
  "ewwm-secrets-autotype--select-backend is defined."
  (should (fboundp 'ewwm-secrets-autotype--select-backend)))

(ert-deftest ewwm-secrets-autotype/available-p-exists ()
  "ewwm-secrets-autotype-available-p is defined."
  (should (fboundp 'ewwm-secrets-autotype-available-p)))

(ert-deftest ewwm-secrets-autotype/string-exists ()
  "ewwm-secrets-autotype-string is defined."
  (should (fboundp 'ewwm-secrets-autotype-string)))

(ert-deftest ewwm-secrets-autotype/credentials-exists ()
  "ewwm-secrets-autotype-credentials is defined."
  (should (fboundp 'ewwm-secrets-autotype-credentials)))

(ert-deftest ewwm-secrets-autotype/fields-exists ()
  "ewwm-secrets-autotype-fields is defined."
  (should (fboundp 'ewwm-secrets-autotype-fields)))

(ert-deftest ewwm-secrets-autotype/abort-exists ()
  "ewwm-secrets-autotype-abort is defined."
  (should (fboundp 'ewwm-secrets-autotype-abort)))

(ert-deftest ewwm-secrets-autotype/cleanup-exists ()
  "ewwm-secrets-autotype--cleanup is defined."
  (should (fboundp 'ewwm-secrets-autotype--cleanup)))

;; ── Backend selection ───────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/select-backend-auto-none ()
  "Auto backend returns nil when no backends available."
  (let ((ewwm-secrets-autotype-backend 'auto))
    (should-not (ewwm-secrets-autotype--select-backend))))

(ert-deftest ewwm-secrets-autotype/select-backend-compositor-unavailable ()
  "Compositor backend returns nil when not available."
  (let ((ewwm-secrets-autotype-backend 'compositor))
    (should-not (ewwm-secrets-autotype--select-backend))))

(ert-deftest ewwm-secrets-autotype/select-backend-ydotool-unavailable ()
  "Ydotool backend returns nil when not available."
  (let ((ewwm-secrets-autotype-backend 'ydotool))
    (should-not (ewwm-secrets-autotype--select-backend))))

(ert-deftest ewwm-secrets-autotype/available-p-none ()
  "available-p returns nil when no backend available."
  (let ((ewwm-secrets-autotype-backend 'auto))
    (should-not (ewwm-secrets-autotype-available-p))))

;; ── Cleanup ─────────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/cleanup-clears-state ()
  "ewwm-secrets-autotype--cleanup sets active and backend-name to nil."
  (let ((ewwm-secrets-autotype--active t)
        (ewwm-secrets-autotype--backend-name 'compositor)
        (ewwm-secrets-autotype-gaze-away nil)
        (ewwm-secrets-autotype-secure-input nil))
    (ewwm-secrets-autotype--cleanup)
    (should-not ewwm-secrets-autotype--active)
    (should-not ewwm-secrets-autotype--backend-name)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/teardown-clears-state ()
  "ewwm-secrets-autotype-teardown sets active and backend-name to nil."
  (let ((ewwm-secrets-autotype--active nil)
        (ewwm-secrets-autotype--backend-name nil))
    (ewwm-secrets-autotype-teardown)
    (should-not ewwm-secrets-autotype--active)
    (should-not ewwm-secrets-autotype--backend-name)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-autotype/provides-feature ()
  "ewwm-secrets-autotype provides its feature."
  (should (featurep 'ewwm-secrets-autotype)))

;;; ewwm-secrets-autotype-test.el ends here
