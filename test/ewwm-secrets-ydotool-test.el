;;; ewwm-secrets-ydotool-test.el --- Tests for ewwm-secrets-ydotool.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets-ydotool)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/enable-defcustom ()
  "ewwm-secrets-ydotool-enable defaults to t."
  (should (eq (default-value 'ewwm-secrets-ydotool-enable) t)))

(ert-deftest ewwm-secrets-ydotool/socket-defcustom ()
  "ewwm-secrets-ydotool-socket defaults to /tmp/.ydotool_socket."
  (should (equal (default-value 'ewwm-secrets-ydotool-socket) "/tmp/.ydotool_socket")))

(ert-deftest ewwm-secrets-ydotool/char-delay-defcustom ()
  "ewwm-secrets-ydotool-char-delay-ms defaults to 50."
  (should (= (default-value 'ewwm-secrets-ydotool-char-delay-ms) 50)))

(ert-deftest ewwm-secrets-ydotool/executable-defcustom ()
  "ewwm-secrets-ydotool-executable defaults to ydotool."
  (should (equal (default-value 'ewwm-secrets-ydotool-executable) "ydotool")))

(ert-deftest ewwm-secrets-ydotool/clear-after-defcustom ()
  "ewwm-secrets-ydotool-clear-after defaults to t."
  (should (eq (default-value 'ewwm-secrets-ydotool-clear-after) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/available-variable-exists ()
  "ewwm-secrets-ydotool--available variable exists."
  (should (boundp 'ewwm-secrets-ydotool--available)))

(ert-deftest ewwm-secrets-ydotool/typing-p-variable-exists ()
  "ewwm-secrets-ydotool--typing-p variable exists."
  (should (boundp 'ewwm-secrets-ydotool--typing-p)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/type-start-hook-exists ()
  "ewwm-secrets-ydotool-type-start-hook exists."
  (should (boundp 'ewwm-secrets-ydotool-type-start-hook)))

(ert-deftest ewwm-secrets-ydotool/type-complete-hook-exists ()
  "ewwm-secrets-ydotool-type-complete-hook exists."
  (should (boundp 'ewwm-secrets-ydotool-type-complete-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/test-command-interactive ()
  "ewwm-secrets-ydotool-test is interactive."
  (should (commandp 'ewwm-secrets-ydotool-test)))

(ert-deftest ewwm-secrets-ydotool/status-command-interactive ()
  "ewwm-secrets-ydotool-status is interactive."
  (should (commandp 'ewwm-secrets-ydotool-status)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/available-p-exists ()
  "ewwm-secrets-ydotool-available-p is defined."
  (should (fboundp 'ewwm-secrets-ydotool-available-p)))

(ert-deftest ewwm-secrets-ydotool/verify-socket-exists ()
  "ewwm-secrets-ydotool--verify-socket is defined."
  (should (fboundp 'ewwm-secrets-ydotool--verify-socket)))

(ert-deftest ewwm-secrets-ydotool/type-string-exists ()
  "ewwm-secrets-ydotool--type-string is defined."
  (should (fboundp 'ewwm-secrets-ydotool--type-string)))

(ert-deftest ewwm-secrets-ydotool/send-key-exists ()
  "ewwm-secrets-ydotool--send-key is defined."
  (should (fboundp 'ewwm-secrets-ydotool--send-key)))

(ert-deftest ewwm-secrets-ydotool/type-credentials-exists ()
  "ewwm-secrets-ydotool--type-credentials is defined."
  (should (fboundp 'ewwm-secrets-ydotool--type-credentials)))

(ert-deftest ewwm-secrets-ydotool/type-with-tab-fields-exists ()
  "ewwm-secrets-ydotool--type-with-tab-fields is defined."
  (should (fboundp 'ewwm-secrets-ydotool--type-with-tab-fields)))

(ert-deftest ewwm-secrets-ydotool/secure-type-exists ()
  "ewwm-secrets-ydotool--secure-type is defined."
  (should (fboundp 'ewwm-secrets-ydotool--secure-type)))

;; ── Teardown ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/teardown-clears-state ()
  "ewwm-secrets-ydotool-teardown clears state variables."
  (let ((ewwm-secrets-ydotool--available t)
        (ewwm-secrets-ydotool--typing-p t))
    (ewwm-secrets-ydotool-teardown)
    (should-not ewwm-secrets-ydotool--available)
    (should-not ewwm-secrets-ydotool--typing-p)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-ydotool/provides-feature ()
  "ewwm-secrets-ydotool provides its feature."
  (should (featurep 'ewwm-secrets-ydotool)))

;;; ewwm-secrets-ydotool-test.el ends here
