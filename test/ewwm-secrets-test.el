;;; ewwm-secrets-test.el --- Tests for ewwm-secrets.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets/default-collection-defcustom ()
  "Default collection is KeePassXC."
  (should (equal (default-value 'ewwm-secrets-default-collection) "KeePassXC")))

(ert-deftest ewwm-secrets/enable-defcustom ()
  "Default enable is t."
  (should (eq (default-value 'ewwm-secrets-enable) t)))

(ert-deftest ewwm-secrets/auto-unlock-defcustom ()
  "Default auto-unlock is t."
  (should (eq (default-value 'ewwm-secrets-auto-unlock) t)))

(ert-deftest ewwm-secrets/timeout-defcustom ()
  "Default timeout is 30."
  (should (= (default-value 'ewwm-secrets-timeout) 30)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets/connected-variable-exists ()
  "Connected variable exists."
  (should (boundp 'ewwm-secrets--connected)))

(ert-deftest ewwm-secrets/last-collection-variable-exists ()
  "Last-collection variable exists."
  (should (boundp 'ewwm-secrets--last-collection)))

(ert-deftest ewwm-secrets/unlock-attempts-variable-exists ()
  "Unlock-attempts variable exists."
  (should (boundp 'ewwm-secrets--unlock-attempts)))

;; ── Constants ───────────────────────────────────────────────

(ert-deftest ewwm-secrets/max-unlock-attempts-constant ()
  "Max unlock attempts is 3."
  (should (= ewwm-secrets--max-unlock-attempts 3)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets/status-interactive ()
  "ewwm-secrets-status is interactive."
  (should (commandp 'ewwm-secrets-status)))

(ert-deftest ewwm-secrets/list-interactive ()
  "ewwm-secrets-list is interactive."
  (should (commandp 'ewwm-secrets-list)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-secrets/get-function-exists ()
  "ewwm-secrets-get is defined."
  (should (fboundp 'ewwm-secrets-get)))

(ert-deftest ewwm-secrets/search-function-exists ()
  "ewwm-secrets-search is defined."
  (should (fboundp 'ewwm-secrets-search)))

(ert-deftest ewwm-secrets/list-items-function-exists ()
  "ewwm-secrets-list-items is defined."
  (should (fboundp 'ewwm-secrets-list-items)))

(ert-deftest ewwm-secrets/get-attributes-function-exists ()
  "ewwm-secrets-get-attributes is defined."
  (should (fboundp 'ewwm-secrets-get-attributes)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets/unlock-hook-exists ()
  "Unlock hook exists."
  (should (boundp 'ewwm-secrets-unlock-hook)))

(ert-deftest ewwm-secrets/error-hook-exists ()
  "Error hook exists."
  (should (boundp 'ewwm-secrets-error-hook)))

;; ── Auth-source backend ─────────────────────────────────────

(ert-deftest ewwm-secrets/auth-source-backend-parse-ewwm-secrets ()
  "Backend parse returns backend for ewwm-secrets symbol."
  (should (ewwm-secrets-auth-source-backend-parse 'ewwm-secrets)))

(ert-deftest ewwm-secrets/auth-source-backend-parse-other ()
  "Backend parse returns nil for other symbols."
  (should-not (ewwm-secrets-auth-source-backend-parse 'netrc))
  (should-not (ewwm-secrets-auth-source-backend-parse 'plstore))
  (should-not (ewwm-secrets-auth-source-backend-parse nil)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-secrets/teardown-clears-state ()
  "ewwm-secrets-teardown clears internal state."
  (let ((ewwm-secrets--connected t)
        (ewwm-secrets--last-collection "TestCollection")
        (ewwm-secrets--unlock-attempts 2))
    (ewwm-secrets-teardown)
    (should-not ewwm-secrets--connected)
    (should-not ewwm-secrets--last-collection)
    (should (= ewwm-secrets--unlock-attempts 0))))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets/provides-feature ()
  "ewwm-secrets provides its feature."
  (should (featurep 'ewwm-secrets)))

;;; ewwm-secrets-test.el ends here
