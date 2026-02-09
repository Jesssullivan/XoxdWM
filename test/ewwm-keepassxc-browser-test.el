;;; ewwm-keepassxc-browser-test.el --- Tests for ewwm-keepassxc-browser.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-keepassxc-browser)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/socket-path-defcustom ()
  "Default proxy socket is nil (auto-detect)."
  (should-not (default-value 'ewwm-keepassxc-proxy-socket)))

(ert-deftest ewwm-keepassxc-browser/association-file-defcustom ()
  "Association file path contains keepassxc-association.json."
  (should (string-match-p "keepassxc-association\\.json"
                           (default-value 'ewwm-keepassxc-association-file))))

(ert-deftest ewwm-keepassxc-browser/association-file-in-config-dir ()
  "Association file path contains .config/exwm-vr."
  (should (string-match-p "\\.config/exwm-vr"
                           (default-value 'ewwm-keepassxc-association-file))))

(ert-deftest ewwm-keepassxc-browser/timeout-defcustom ()
  "Default timeout is 5."
  (should (= (default-value 'ewwm-keepassxc-timeout) 5)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/connected-p-variable-exists ()
  "Connected-p variable exists."
  (should (boundp 'ewwm-keepassxc--connected-p)))

(ert-deftest ewwm-keepassxc-browser/association-name-variable-exists ()
  "Association name variable exists."
  (should (boundp 'ewwm-keepassxc--association-name)))

(ert-deftest ewwm-keepassxc-browser/association-key-variable-exists ()
  "Association key variable exists."
  (should (boundp 'ewwm-keepassxc--association-key)))

(ert-deftest ewwm-keepassxc-browser/database-hash-variable-exists ()
  "Database hash variable exists."
  (should (boundp 'ewwm-keepassxc--database-hash)))

(ert-deftest ewwm-keepassxc-browser/server-public-key-variable-exists ()
  "Server public key variable exists."
  (should (boundp 'ewwm-keepassxc--server-public-key)))

(ert-deftest ewwm-keepassxc-browser/process-variable-exists ()
  "Process variable exists."
  (should (boundp 'ewwm-keepassxc--process)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/connect-hook-exists ()
  "Connect hook exists."
  (should (boundp 'ewwm-keepassxc-connect-hook)))

(ert-deftest ewwm-keepassxc-browser/error-hook-exists ()
  "Error hook exists."
  (should (boundp 'ewwm-keepassxc-error-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/status-interactive ()
  "ewwm-keepassxc-status is interactive."
  (should (commandp 'ewwm-keepassxc-status)))

(ert-deftest ewwm-keepassxc-browser/associate-interactive ()
  "ewwm-keepassxc-associate is interactive."
  (should (commandp 'ewwm-keepassxc-associate)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/get-logins-function-exists ()
  "ewwm-keepassxc-get-logins is defined."
  (should (fboundp 'ewwm-keepassxc-get-logins)))

(ert-deftest ewwm-keepassxc-browser/get-totp-function-exists ()
  "ewwm-keepassxc-get-totp is defined."
  (should (fboundp 'ewwm-keepassxc-get-totp)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/teardown-clears-state ()
  "ewwm-keepassxc-teardown clears connection and protocol state."
  (let ((ewwm-keepassxc--connected-p t)
        (ewwm-keepassxc--process nil)
        (ewwm-keepassxc--database-hash "abc123")
        (ewwm-keepassxc--server-public-key "serverkey")
        (ewwm-keepassxc--association-name "test-assoc")
        (ewwm-keepassxc--association-key "assockey")
        (ewwm-keepassxc--client-public-key "pubkey")
        (ewwm-keepassxc--client-secret-key nil)
        (ewwm-keepassxc--shared-key nil)
        (ewwm-keepassxc--response '((test . t))))
    (ewwm-keepassxc-teardown)
    (should-not ewwm-keepassxc--connected-p)
    (should-not ewwm-keepassxc--process)
    (should-not ewwm-keepassxc--database-hash)
    (should-not ewwm-keepassxc--server-public-key)
    (should-not ewwm-keepassxc--association-name)
    (should-not ewwm-keepassxc--association-key)
    (should-not ewwm-keepassxc--client-public-key)
    (should-not ewwm-keepassxc--response)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-keepassxc-browser/provides-feature ()
  "ewwm-keepassxc-browser provides its feature."
  (should (featurep 'ewwm-keepassxc-browser)))

;;; ewwm-keepassxc-browser-test.el ends here
