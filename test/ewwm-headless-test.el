;;; ewwm-headless-test.el --- Tests for ewwm-headless.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-headless)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-headless/provides-feature ()
  "ewwm-headless provides its feature."
  (should (featurep 'ewwm-headless)))

(ert-deftest ewwm-headless/group-exists ()
  "ewwm-headless customization group exists."
  (should (get 'ewwm-headless 'custom-group)))

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-headless/mode-defcustom ()
  "Default headless mode is nil."
  (should (eq (default-value 'ewwm-headless-mode) nil)))

(ert-deftest ewwm-headless/terminal-keys-defcustom ()
  "Default terminal keys is t."
  (should (eq (default-value 'ewwm-headless-terminal-keys) t)))

(ert-deftest ewwm-headless/virtual-outputs-defcustom ()
  "Default virtual outputs is 1."
  (should (= (default-value 'ewwm-headless-virtual-outputs) 1)))

(ert-deftest ewwm-headless/resolution-defcustom ()
  "Default resolution is 1920x1080."
  (should (equal (default-value 'ewwm-headless-resolution) "1920x1080")))

(ert-deftest ewwm-headless/status-interval-defcustom ()
  "Default status interval is 60."
  (should (= (default-value 'ewwm-headless-status-interval) 60)))

(ert-deftest ewwm-headless/disable-features-defcustom ()
  "Default disable features includes expected symbols."
  (let ((feats (default-value 'ewwm-headless-disable-features)))
    (should (listp feats))
    (should (memq 'vr feats))
    (should (memq 'gaze feats))
    (should (memq 'wink feats))
    (should (memq 'fatigue feats))
    (should (memq 'eye-tracking feats))))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-headless/status-timer-exists ()
  "Status timer variable exists."
  (should (boundp 'ewwm-headless--status-timer)))

(ert-deftest ewwm-headless/active-outputs-exists ()
  "Active outputs variable exists."
  (should (boundp 'ewwm-headless--active-outputs)))

;; ── Function existence ──────────────────────────────────────

(ert-deftest ewwm-headless/init-exists ()
  "ewwm-headless-init is defined."
  (should (fboundp 'ewwm-headless-init)))

(ert-deftest ewwm-headless/teardown-exists ()
  "ewwm-headless-teardown is defined."
  (should (fboundp 'ewwm-headless-teardown)))

(ert-deftest ewwm-headless/disable-vr-features-exists ()
  "ewwm-headless--disable-vr-features is defined."
  (should (fboundp 'ewwm-headless--disable-vr-features)))

(ert-deftest ewwm-headless/setup-terminal-keys-exists ()
  "ewwm-headless--setup-terminal-keys is defined."
  (should (fboundp 'ewwm-headless--setup-terminal-keys)))

(ert-deftest ewwm-headless/status-exists ()
  "ewwm-headless-status is defined."
  (should (fboundp 'ewwm-headless-status)))

(ert-deftest ewwm-headless/add-output-exists ()
  "ewwm-headless-add-output is defined."
  (should (fboundp 'ewwm-headless-add-output)))

(ert-deftest ewwm-headless/remove-output-exists ()
  "ewwm-headless-remove-output is defined."
  (should (fboundp 'ewwm-headless-remove-output)))

(ert-deftest ewwm-headless/set-resolution-exists ()
  "ewwm-headless-set-resolution is defined."
  (should (fboundp 'ewwm-headless-set-resolution)))

(ert-deftest ewwm-headless/log-status-exists ()
  "ewwm-headless--log-status is defined."
  (should (fboundp 'ewwm-headless--log-status)))

(ert-deftest ewwm-headless/format-output-exists ()
  "ewwm-headless--format-output is defined."
  (should (fboundp 'ewwm-headless--format-output)))

(ert-deftest ewwm-headless/detect-exists ()
  "ewwm-headless-detect is defined."
  (should (fboundp 'ewwm-headless-detect)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-headless/status-interactive ()
  "ewwm-headless-status is interactive."
  (should (commandp 'ewwm-headless-status)))

(ert-deftest ewwm-headless/add-output-interactive ()
  "ewwm-headless-add-output is interactive."
  (should (commandp 'ewwm-headless-add-output)))

(ert-deftest ewwm-headless/remove-output-interactive ()
  "ewwm-headless-remove-output is interactive."
  (should (commandp 'ewwm-headless-remove-output)))

(ert-deftest ewwm-headless/set-resolution-interactive ()
  "ewwm-headless-set-resolution is interactive."
  (should (commandp 'ewwm-headless-set-resolution)))

;; ── Feature disable logic ───────────────────────────────────

;; Forward-declare so `let' creates dynamic bindings
(defvar ewwm-vr-fatigue-enable)
(defvar ewwm-vr-wink-enable)
(defvar ewwm-vr-gaze-enable)

(ert-deftest ewwm-headless/disable-features-sets-nil ()
  "ewwm-headless--disable-vr-features sets enable vars to nil."
  (let ((ewwm-vr-fatigue-enable t)
        (ewwm-vr-wink-enable t)
        (ewwm-vr-gaze-enable t)
        (ewwm-headless-disable-features '(fatigue wink gaze)))
    (ewwm-headless--disable-vr-features)
    (should-not ewwm-vr-fatigue-enable)
    (should-not ewwm-vr-wink-enable)
    (should-not ewwm-vr-gaze-enable)))

(ert-deftest ewwm-headless/disable-features-skips-unbound ()
  "ewwm-headless--disable-vr-features skips unbound variables."
  (let ((ewwm-headless-disable-features '(nonexistent-feature-xyz)))
    ;; Should not error
    (ewwm-headless--disable-vr-features)))

;; ── Auto-detect logic ───────────────────────────────────────

(ert-deftest ewwm-headless/detect-headless-no-display ()
  "Detect headless when DISPLAY and WAYLAND_DISPLAY are unset."
  (let ((process-environment '("HOME=/tmp" "PATH=/usr/bin")))
    (should (ewwm-headless-detect))))

(ert-deftest ewwm-headless/detect-not-headless-with-display ()
  "Detect non-headless when DISPLAY is set."
  (let ((process-environment
         (cons "DISPLAY=:0" process-environment)))
    (should-not (ewwm-headless-detect))))

(ert-deftest ewwm-headless/detect-not-headless-with-wayland ()
  "Detect non-headless when WAYLAND_DISPLAY is set."
  (let ((process-environment
         (cons "WAYLAND_DISPLAY=wayland-1" process-environment)))
    (should-not (ewwm-headless-detect))))

;; ── Terminal key setup ──────────────────────────────────────

(ert-deftest ewwm-headless/terminal-keys-binds-keys ()
  "Terminal key setup binds C-c w s to status."
  (let ((ewwm-headless-terminal-keys t)
        (ewwm-mode-map (make-sparse-keymap)))
    (ewwm-headless--setup-terminal-keys)
    (should (eq (lookup-key ewwm-mode-map (kbd "C-c w s"))
                'ewwm-headless-status))))

(ert-deftest ewwm-headless/terminal-keys-disabled ()
  "Terminal key setup does nothing when disabled."
  (let ((ewwm-headless-terminal-keys nil)
        (ewwm-mode-map (make-sparse-keymap)))
    (ewwm-headless--setup-terminal-keys)
    (should-not (commandp (lookup-key ewwm-mode-map (kbd "C-c w s"))))))

;; ── Status formatting ───────────────────────────────────────

(ert-deftest ewwm-headless/format-output-basic ()
  "Format output returns expected string."
  (let ((out '(:id 1 :resolution "1920x1080" :name "virtual-1")))
    (should (string-match-p "output-1" (ewwm-headless--format-output out)))
    (should (string-match-p "1920x1080" (ewwm-headless--format-output out)))
    (should (string-match-p "virtual-1" (ewwm-headless--format-output out)))))

(ert-deftest ewwm-headless/format-output-missing-fields ()
  "Format output handles missing fields gracefully."
  (let ((out '(:id 2)))
    (should (stringp (ewwm-headless--format-output out)))
    (should (string-match-p "output-2" (ewwm-headless--format-output out)))))

;; ── Mode-line indicator ─────────────────────────────────────

(ert-deftest ewwm-headless/mode-line-active ()
  "Mode-line shows [H] when headless mode is active."
  (let ((ewwm-headless-mode t))
    (should (equal (ewwm-headless-mode-line-string) " [H]"))))

(ert-deftest ewwm-headless/mode-line-inactive ()
  "Mode-line returns nil when headless mode is off."
  (let ((ewwm-headless-mode nil))
    (should-not (ewwm-headless-mode-line-string))))

;; ── Output management ───────────────────────────────────────

(ert-deftest ewwm-headless/add-output-updates-list ()
  "Adding an output updates the active outputs list."
  (let ((ewwm-headless--active-outputs nil)
        (ewwm-headless-resolution "1280x720"))
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (_msg) 1)))
      (ewwm-headless-add-output "1280x720")
      (should (= (length ewwm-headless--active-outputs) 1))
      (should (equal (plist-get (car ewwm-headless--active-outputs) :resolution)
                     "1280x720")))))

(ert-deftest ewwm-headless/remove-output-updates-list ()
  "Removing an output updates the active outputs list."
  (let ((ewwm-headless--active-outputs
         (list (list :id 1 :resolution "1920x1080" :name "virtual-1")
               (list :id 2 :resolution "1280x720" :name "virtual-2"))))
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (_msg) 1)))
      (ewwm-headless-remove-output 1)
      (should (= (length ewwm-headless--active-outputs) 1))
      (should (= (plist-get (car ewwm-headless--active-outputs) :id) 2)))))

(ert-deftest ewwm-headless/set-resolution-updates-output ()
  "Setting resolution updates the output in the active list."
  (let ((ewwm-headless--active-outputs
         (list (list :id 1 :resolution "1920x1080" :name "virtual-1"))))
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (_msg) 1)))
      (ewwm-headless-set-resolution "2560x1440" 1)
      (should (equal (plist-get (car ewwm-headless--active-outputs) :resolution)
                     "2560x1440")))))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-headless/init-sets-mode ()
  "ewwm-headless-init sets headless mode to t."
  (let ((ewwm-headless-mode nil)
        (ewwm-headless--active-outputs nil)
        (ewwm-headless--status-timer nil)
        (ewwm-headless-virtual-outputs 2)
        (ewwm-headless-resolution "1920x1080")
        (ewwm-headless-status-interval 0)
        (ewwm-headless-terminal-keys nil)
        (ewwm-headless-disable-features nil))
    (ewwm-headless-init)
    (should ewwm-headless-mode)
    (should (= (length ewwm-headless--active-outputs) 2))
    ;; Clean up
    (ewwm-headless-teardown)))

(ert-deftest ewwm-headless/teardown-clears-state ()
  "ewwm-headless-teardown clears all state."
  (let ((ewwm-headless-mode t)
        (ewwm-headless--active-outputs '((:id 1)))
        (ewwm-headless--status-timer nil))
    (ewwm-headless-teardown)
    (should-not ewwm-headless-mode)
    (should-not ewwm-headless--status-timer)
    (should-not ewwm-headless--active-outputs)))

(ert-deftest ewwm-headless/init-creates-timer ()
  "ewwm-headless-init creates status timer when interval > 0."
  (let ((ewwm-headless-mode nil)
        (ewwm-headless--active-outputs nil)
        (ewwm-headless--status-timer nil)
        (ewwm-headless-virtual-outputs 1)
        (ewwm-headless-resolution "1920x1080")
        (ewwm-headless-status-interval 30)
        (ewwm-headless-terminal-keys nil)
        (ewwm-headless-disable-features nil))
    (ewwm-headless-init)
    (should ewwm-headless--status-timer)
    ;; Clean up timer
    (ewwm-headless-teardown)))

(ert-deftest ewwm-headless/init-no-timer-when-zero ()
  "ewwm-headless-init skips timer when interval is 0."
  (let ((ewwm-headless-mode nil)
        (ewwm-headless--active-outputs nil)
        (ewwm-headless--status-timer nil)
        (ewwm-headless-virtual-outputs 1)
        (ewwm-headless-resolution "1920x1080")
        (ewwm-headless-status-interval 0)
        (ewwm-headless-terminal-keys nil)
        (ewwm-headless-disable-features nil))
    (ewwm-headless-init)
    (should-not ewwm-headless--status-timer)
    (ewwm-headless-teardown)))

;;; ewwm-headless-test.el ends here
