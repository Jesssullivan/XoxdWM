;;; ewwm-secrets-totp-test.el --- Tests for ewwm-secrets-totp.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-secrets-totp)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/enable-defcustom ()
  "ewwm-secrets-totp-enable defaults to t."
  (should (eq (default-value 'ewwm-secrets-totp-enable) t)))

(ert-deftest ewwm-secrets-totp/delay-ms-defcustom ()
  "ewwm-secrets-totp-delay-ms defaults to 2000."
  (should (= (default-value 'ewwm-secrets-totp-delay-ms) 2000)))

(ert-deftest ewwm-secrets-totp/refresh-interval-defcustom ()
  "ewwm-secrets-totp-refresh-interval defaults to 30."
  (should (= (default-value 'ewwm-secrets-totp-refresh-interval) 30)))

(ert-deftest ewwm-secrets-totp/auto-copy-defcustom ()
  "ewwm-secrets-totp-auto-copy defaults to nil."
  (should (null (default-value 'ewwm-secrets-totp-auto-copy))))

(ert-deftest ewwm-secrets-totp/mode-line-defcustom ()
  "ewwm-secrets-totp-mode-line defaults to t."
  (should (eq (default-value 'ewwm-secrets-totp-mode-line) t)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/current-code-variable-exists ()
  "ewwm-secrets-totp--current-code variable exists."
  (should (boundp 'ewwm-secrets-totp--current-code)))

(ert-deftest ewwm-secrets-totp/expiry-time-variable-exists ()
  "ewwm-secrets-totp--expiry-time variable exists."
  (should (boundp 'ewwm-secrets-totp--expiry-time)))

(ert-deftest ewwm-secrets-totp/countdown-timer-variable-exists ()
  "ewwm-secrets-totp--countdown-timer variable exists."
  (should (boundp 'ewwm-secrets-totp--countdown-timer)))

(ert-deftest ewwm-secrets-totp/entry-uuid-variable-exists ()
  "ewwm-secrets-totp--entry-uuid variable exists."
  (should (boundp 'ewwm-secrets-totp--entry-uuid)))

(ert-deftest ewwm-secrets-totp/active-variable-exists ()
  "ewwm-secrets-totp--active variable exists."
  (should (boundp 'ewwm-secrets-totp--active)))

;; ── Hooks ───────────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/retrieved-hook-exists ()
  "ewwm-secrets-totp-retrieved-hook exists."
  (should (boundp 'ewwm-secrets-totp-retrieved-hook)))

(ert-deftest ewwm-secrets-totp/typed-hook-exists ()
  "ewwm-secrets-totp-typed-hook exists."
  (should (boundp 'ewwm-secrets-totp-typed-hook)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-secrets-totp/totp-interactive ()
  "ewwm-secrets-totp is interactive."
  (should (commandp 'ewwm-secrets-totp)))

(ert-deftest ewwm-secrets-totp/copy-interactive ()
  "ewwm-secrets-totp-copy is interactive."
  (should (commandp 'ewwm-secrets-totp-copy)))

(ert-deftest ewwm-secrets-totp/autotype-with-totp-interactive ()
  "ewwm-secrets-autotype-with-totp is interactive."
  (should (commandp 'ewwm-secrets-autotype-with-totp)))

;; ── Functions ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/get-function-exists ()
  "ewwm-secrets-totp-get is defined."
  (should (fboundp 'ewwm-secrets-totp-get)))

(ert-deftest ewwm-secrets-totp/type-function-exists ()
  "ewwm-secrets-totp-type is defined."
  (should (fboundp 'ewwm-secrets-totp-type)))

(ert-deftest ewwm-secrets-totp/time-remaining-function-exists ()
  "ewwm-secrets-totp--time-remaining is defined."
  (should (fboundp 'ewwm-secrets-totp--time-remaining)))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/mode-line-inactive ()
  "Mode-line returns nil when not active."
  (let ((ewwm-secrets-totp--active nil))
    (should-not (ewwm-secrets-totp-mode-line-string))))

(ert-deftest ewwm-secrets-totp/mode-line-active ()
  "Mode-line returns TOTP countdown when active."
  (let ((ewwm-secrets-totp--active t)
        (ewwm-secrets-totp--expiry-time (+ (float-time) 15))
        (ewwm-secrets-totp-mode-line t))
    (should (string-match-p " \\[TOTP:[0-9]+s\\]"
                            (ewwm-secrets-totp-mode-line-string)))))

;; ── Teardown ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/teardown-clears-state ()
  "ewwm-secrets-totp-teardown clears all internal state."
  (let ((ewwm-secrets-totp--current-code nil)
        (ewwm-secrets-totp--expiry-time 999.0)
        (ewwm-secrets-totp--countdown-timer nil)
        (ewwm-secrets-totp--entry-uuid "test-uuid")
        (ewwm-secrets-totp--active t))
    (ewwm-secrets-totp-teardown)
    (should-not ewwm-secrets-totp--current-code)
    (should-not ewwm-secrets-totp--expiry-time)
    (should-not ewwm-secrets-totp--countdown-timer)
    (should-not ewwm-secrets-totp--entry-uuid)
    (should-not ewwm-secrets-totp--active)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-secrets-totp/provides-feature ()
  "ewwm-secrets-totp provides its feature."
  (should (featurep 'ewwm-secrets-totp)))

;;; ewwm-secrets-totp-test.el ends here
