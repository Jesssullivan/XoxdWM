;;; ewwm-environment-test.el --- Tests for ewwm-environment.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-environment)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-environment/compositor-path-defcustom ()
  "Default compositor path."
  (should (stringp (default-value 'ewwm-environment-compositor-path))))

(ert-deftest ewwm-environment/monado-json-defcustom ()
  "Default Monado JSON path."
  (should (stringp (default-value 'ewwm-environment-monado-json))))

(ert-deftest ewwm-environment/brainflow-board-ids-defcustom ()
  "BrainFlow board ID mapping exists."
  (should (listp (default-value 'ewwm-environment-brainflow-board-ids)))
  (should (assoc "cyton" ewwm-environment-brainflow-board-ids))
  (should (= (cdr (assoc "cyton" ewwm-environment-brainflow-board-ids)) 0))
  (should (= (cdr (assoc "synthetic" ewwm-environment-brainflow-board-ids)) -1)))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-environment/check-results-variable ()
  "Check results variable exists and is initially nil."
  (should (boundp 'ewwm-environment--check-results)))

;; ── Check Env Var ───────────────────────────────────────────

(ert-deftest ewwm-environment/check-env-var-present ()
  "Check env var returns ok for set variable."
  (let ((process-environment (cons "TEST_VAR=hello" process-environment)))
    (let ((result (ewwm-environment--check-env-var "TEST_VAR" t)))
      (should (equal (nth 0 result) "TEST_VAR"))
      (should (eq (nth 1 result) 'ok))
      (should (equal (nth 2 result) "hello")))))

(ert-deftest ewwm-environment/check-env-var-missing-required ()
  "Check env var returns error for missing required variable."
  (let ((process-environment '("PATH=/usr/bin")))
    (let ((result (ewwm-environment--check-env-var "NONEXISTENT_12345" t)))
      (should (equal (nth 0 result) "NONEXISTENT_12345"))
      (should (eq (nth 1 result) 'error)))))

(ert-deftest ewwm-environment/check-env-var-missing-optional ()
  "Check env var returns warn for missing optional variable."
  (let ((process-environment '("PATH=/usr/bin")))
    (let ((result (ewwm-environment--check-env-var "NONEXISTENT_12345")))
      (should (eq (nth 1 result) 'warn)))))

;; ── Check Path ──────────────────────────────────────────────

(ert-deftest ewwm-environment/check-path-exists ()
  "Check path returns ok for existing path."
  (let ((result (ewwm-environment--check-path "/" "Root dir" t)))
    (should (eq (nth 1 result) 'ok))))

(ert-deftest ewwm-environment/check-path-missing-required ()
  "Check path returns error for missing required path."
  (let ((result (ewwm-environment--check-path "/nonexistent/path/12345" "Test" t)))
    (should (eq (nth 1 result) 'error))))

(ert-deftest ewwm-environment/check-path-missing-optional ()
  "Check path returns warn for missing optional path."
  (let ((result (ewwm-environment--check-path "/nonexistent/path/12345" "Test")))
    (should (eq (nth 1 result) 'warn))))

;; ── Check Executable ────────────────────────────────────────

(ert-deftest ewwm-environment/check-executable-found ()
  "Check executable returns ok for existing executable."
  (let ((result (ewwm-environment--check-executable "emacs" t)))
    (should (eq (nth 1 result) 'ok))
    (should (stringp (nth 2 result)))))

(ert-deftest ewwm-environment/check-executable-missing ()
  "Check executable returns error for missing required executable."
  (let ((result (ewwm-environment--check-executable "nonexistent_binary_12345" t)))
    (should (eq (nth 1 result) 'error))))

;; ── Subsystem Checks ────────────────────────────────────────

(ert-deftest ewwm-environment/check-wayland-returns-list ()
  "Wayland check returns a list of results."
  (let ((results (ewwm-environment--check-wayland)))
    (should (listp results))
    (should (>= (length results) 2))))

(ert-deftest ewwm-environment/check-vr-returns-list ()
  "VR check returns a list of results."
  (let ((results (ewwm-environment--check-vr)))
    (should (listp results))
    (should (>= (length results) 1))))

(ert-deftest ewwm-environment/check-compositor-returns-list ()
  "Compositor check returns a list of results."
  (let ((results (ewwm-environment--check-compositor)))
    (should (listp results))
    (should (>= (length results) 1))))

(ert-deftest ewwm-environment/check-dbus-returns-list ()
  "D-Bus check returns a list of results."
  (let ((results (ewwm-environment--check-dbus)))
    (should (listp results))))

(ert-deftest ewwm-environment/check-bci-returns-list ()
  "BCI check returns a list of results."
  (let ((results (ewwm-environment--check-bci)))
    (should (listp results))))

(ert-deftest ewwm-environment/check-eye-tracking-returns-list ()
  "Eye tracking check returns a list of results."
  (let ((results (ewwm-environment--check-eye-tracking)))
    (should (listp results))))

(ert-deftest ewwm-environment/check-secrets-returns-list ()
  "Secrets check returns a list of results."
  (let ((results (ewwm-environment--check-secrets)))
    (should (listp results))))

;; ── Full Check ──────────────────────────────────────────────

(ert-deftest ewwm-environment/check-all-returns-alist ()
  "Full check returns alist with all categories."
  (let ((results (ewwm-environment-check-all)))
    (should (listp results))
    (should (assoc "Wayland Session" results))
    (should (assoc "Compositor" results))
    (should (assoc "VR / OpenXR" results))
    (should (assoc "D-Bus" results))
    (should (assoc "Eye Tracking" results))
    (should (assoc "BCI / BrainFlow" results))
    (should (assoc "Secrets" results))))

;; ── Format Status ───────────────────────────────────────────

(ert-deftest ewwm-environment/format-status-ok ()
  "Format ok status."
  (should (string-match-p "OK" (ewwm-environment--format-status 'ok))))

(ert-deftest ewwm-environment/format-status-warn ()
  "Format warn status."
  (should (string-match-p "WARN" (ewwm-environment--format-status 'warn))))

(ert-deftest ewwm-environment/format-status-error ()
  "Format error status."
  (should (string-match-p "FAIL" (ewwm-environment--format-status 'error))))

;; ── Format Results ──────────────────────────────────────────

(ert-deftest ewwm-environment/format-results-includes-summary ()
  "Formatted results include summary line."
  (let* ((results (ewwm-environment-check-all))
         (formatted (ewwm-environment--format-results results)))
    (should (stringp formatted))
    (should (string-match-p "Summary" formatted))))

(ert-deftest ewwm-environment/format-results-includes-categories ()
  "Formatted results include category headers."
  (let* ((results (ewwm-environment-check-all))
         (formatted (ewwm-environment--format-results results)))
    (should (string-match-p "Wayland Session" formatted))
    (should (string-match-p "Compositor" formatted))))

;; ── Interactive Commands ────────────────────────────────────

(ert-deftest ewwm-environment/check-command-is-interactive ()
  "ewwm-check-environment is interactive."
  (should (commandp 'ewwm-check-environment)))

(ert-deftest ewwm-environment/ok-p-is-function ()
  "ewwm-environment-ok-p is a function."
  (should (functionp 'ewwm-environment-ok-p)))

(ert-deftest ewwm-environment/ok-p-returns-boolean ()
  "ewwm-environment-ok-p returns a boolean-like value."
  (let ((result (ewwm-environment-ok-p)))
    (should (or (eq result t) (eq result nil)))))

;; ── Environment Simulation ──────────────────────────────────

(ert-deftest ewwm-environment/wayland-check-with-env ()
  "Wayland check succeeds when variables are set."
  (let ((process-environment
         (append '("WAYLAND_DISPLAY=wayland-1"
                   "XDG_RUNTIME_DIR=/run/user/1000")
                 process-environment)))
    (let ((results (ewwm-environment--check-wayland)))
      (let ((wl-result (cl-find "WAYLAND_DISPLAY" results
                                :key #'car :test #'equal)))
        (should wl-result)
        (should (eq (nth 1 wl-result) 'ok))))))

(ert-deftest ewwm-environment/check-all-categories-count ()
  "Full check returns exactly 7 categories."
  (let ((results (ewwm-environment-check-all)))
    (should (= (length results) 7))))

;;; ewwm-environment-test.el ends here
