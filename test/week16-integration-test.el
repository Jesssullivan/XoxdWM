;;; week16-integration-test.el --- Week 16 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-headless)

;; ── Module Loading ──────────────────────────────────────────

(ert-deftest week16-integration/headless-module-loads ()
  "ewwm-headless module loads without error."
  (should (featurep 'ewwm-headless)))

(ert-deftest week16-integration/headless-provides-feature ()
  "ewwm-headless provides its feature."
  (should (featurep 'ewwm-headless)))

;; ── Headless Detection ──────────────────────────────────────

(ert-deftest week16-integration/detect-headless-simulated ()
  "Detect headless under simulated environment with no displays."
  (let ((process-environment '("HOME=/tmp" "PATH=/usr/bin")))
    (should (ewwm-headless-detect))))

(ert-deftest week16-integration/detect-not-headless-simulated ()
  "Detect non-headless under simulated environment with DISPLAY."
  (let ((process-environment
         (append '("DISPLAY=:0" "WAYLAND_DISPLAY=wayland-1")
                 process-environment)))
    (should-not (ewwm-headless-detect))))

;; ── Cross-architecture IPC encoding ─────────────────────────

(ert-deftest week16-integration/sexp-roundtrip-integers ()
  "S-expression round-trip for integers preserves values."
  (dolist (n (list 0 1 -1 42 255 256 65535 65536
                   (- (expt 2 30)) (1- (expt 2 30))))
    (let ((encoded (prin1-to-string n)))
      (should (= (car (read-from-string encoded)) n)))))

(ert-deftest week16-integration/sexp-roundtrip-floats ()
  "S-expression round-trip for floats preserves values."
  (dolist (f (list 0.0 1.0 -1.0 3.14159 1.0e10 1.0e-10))
    (let ((encoded (prin1-to-string f)))
      (should (= (car (read-from-string encoded)) f)))))

(ert-deftest week16-integration/sexp-roundtrip-strings-unicode ()
  "S-expression round-trip for strings with unicode preserves content."
  (dolist (s (list "" "hello" "hello world"
                   "unicode: \u00e9\u00e8\u00ea"
                   "cjk: \u4e16\u754c"
                   "emoji: \U0001F600"
                   "escapes: \"quoted\" and \\backslash"))
    (let ((encoded (prin1-to-string s)))
      (should (equal (car (read-from-string encoded)) s)))))

(ert-deftest week16-integration/sexp-roundtrip-nested-lists ()
  "S-expression round-trip for nested lists preserves structure."
  (dolist (lst (list '(:type :headless-add-output :resolution "1920x1080")
                     '(:status :ok :outputs ((:id 1 :name "v-1") (:id 2 :name "v-2")))
                     '(:nested (:deep (:deeper 42)))
                     nil
                     '(1 2 3)))
    (let ((encoded (prin1-to-string lst)))
      (should (equal (car (read-from-string encoded)) lst)))))

;; ── Architecture detection ──────────────────────────────────

(ert-deftest week16-integration/system-type-valid ()
  "system-type is a recognized value."
  (should (memq system-type
                '(gnu gnu/linux gnu/kfreebsd darwin ms-dos
                  windows-nt cygwin))))

(ert-deftest week16-integration/system-configuration-string ()
  "system-configuration is a non-empty string."
  (should (stringp system-configuration))
  (should (> (length system-configuration) 0)))

;; ── Full headless init/teardown cycle ───────────────────────

;; Forward-declare so `let' creates dynamic binding
(defvar ewwm-vr-fatigue-enable)

(ert-deftest week16-integration/full-init-teardown-cycle ()
  "Full headless init/teardown cycle works without error."
  (let ((ewwm-headless-mode nil)
        (ewwm-headless--active-outputs nil)
        (ewwm-headless--status-timer nil)
        (ewwm-headless-virtual-outputs 3)
        (ewwm-headless-resolution "1280x720")
        (ewwm-headless-status-interval 0)
        (ewwm-headless-terminal-keys nil)
        (ewwm-headless-disable-features nil))
    ;; Init
    (ewwm-headless-init)
    (should ewwm-headless-mode)
    (should (= (length ewwm-headless--active-outputs) 3))
    ;; Verify output structure
    (dolist (output ewwm-headless--active-outputs)
      (should (plist-get output :id))
      (should (equal (plist-get output :resolution) "1280x720"))
      (should (stringp (plist-get output :name))))
    ;; Teardown
    (ewwm-headless-teardown)
    (should-not ewwm-headless-mode)
    (should-not ewwm-headless--active-outputs)
    (should-not ewwm-headless--status-timer)))

(ert-deftest week16-integration/init-disables-features ()
  "Init cycle disables specified features."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-headless-mode nil)
        (ewwm-headless--active-outputs nil)
        (ewwm-headless--status-timer nil)
        (ewwm-headless-virtual-outputs 1)
        (ewwm-headless-resolution "1920x1080")
        (ewwm-headless-status-interval 0)
        (ewwm-headless-terminal-keys nil)
        (ewwm-headless-disable-features '(fatigue)))
    (ewwm-headless-init)
    (should-not ewwm-vr-fatigue-enable)
    (ewwm-headless-teardown)))

(ert-deftest week16-integration/status-message-headless ()
  "Status reports headless when mode is active."
  (let ((ewwm-headless-mode t)
        (ewwm-headless--active-outputs '((:id 1)))
        (ewwm--surface-buffer-alist nil)
        (ewwm-headless-disable-features '(vr))
        (captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (ewwm-headless-status))
    (should (stringp captured))
    (should (string-match-p "mode=ON" captured))))

(ert-deftest week16-integration/status-message-not-headless ()
  "Status reports not in headless mode."
  (let ((ewwm-headless-mode nil)
        (captured nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq captured (apply #'format fmt args)))))
      (ewwm-headless-status))
    (should (stringp captured))
    (should (string-match-p "not in headless" captured))))

;;; week16-integration-test.el ends here
