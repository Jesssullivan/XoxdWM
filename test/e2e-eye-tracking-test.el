;;; e2e-eye-tracking-test.el --- E2E eye tracking tests  -*- lexical-binding: t -*-

;; End-to-end tests for eye tracking mode: gaze focus, dwell pipeline,
;; saccade filtering, wink detection, gaze zones, fatigue monitoring,
;; gaze scrolling, link hints, secure input, and qutebrowser gaze.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ring)
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-vr-eye)
(require 'ewwm-vr-wink)
(require 'ewwm-vr-gaze-zone)
(require 'ewwm-vr-fatigue)
(require 'ewwm-vr-secure-input)
(require 'ewwm-qutebrowser-gaze)
(require 'ewwm-secrets-gaze-away)

;; Forward-declare dynamic variables
(defvar ewwm-ipc--event-handlers)
(defvar ewwm--surface-buffer-alist)
(defvar ewwm-vr-gaze-surface)
(defvar ewwm-vr-gaze-position)
(defvar ewwm-vr-gaze-confidence)
(defvar ewwm-vr-gaze-source-active)
(defvar ewwm-vr-gaze-tracking-p)
(defvar ewwm-vr-gaze-calibrated-p)
(defvar ewwm-vr-eye--tracking-active)
(defvar ewwm-vr-eye--gaze-position)
(defvar ewwm-vr-eye--dwell-surface)
(defvar ewwm-vr-eye--dwell-progress)
(defvar ewwm-vr-eye--cooldown-remaining)
(defvar ewwm-vr-eye--in-saccade)
(defvar ewwm-vr-eye--in-reading)
(defvar ewwm-vr-eye--focus-ring)
(defvar ewwm-last-focus-method)
(defvar ewwm-vr-eye-enable)
(defvar ewwm-vr-eye-focus-policy)
(defvar ewwm-vr-gaze-mode-line)
(defvar ewwm-vr-eye-show-dwell-progress)
(defvar ewwm-vr-gaze-target-change-hook)
(defvar ewwm-vr-gaze-dwell-hook)
(defvar ewwm-vr-gaze-focus-hook)
(defvar ewwm-vr-wink-enable)
(defvar ewwm-vr-gaze-zone-enable)
(defvar ewwm-vr-fatigue-enable)
(defvar ewwm-vr-fatigue--level)
(defvar ewwm-vr-fatigue--blink-rate)
(defvar ewwm-vr-secure-input--active)
(defvar ewwm-workspace-current-index)

;; ── Module loading ──────────────────────────────────────────

(ert-deftest e2e-eye/all-modules-loaded ()
  "All eye tracking modules provide their features."
  (dolist (feat '(ewwm-vr-eye
                  ewwm-vr-wink
                  ewwm-vr-gaze-zone
                  ewwm-vr-fatigue
                  ewwm-vr-secure-input
                  ewwm-qutebrowser-gaze
                  ewwm-secrets-gaze-away))
    (should (featurep feat))))

;; ── Gaze visualization ──────────────────────────────────────

(ert-deftest e2e-eye/gaze-visualization-functions ()
  "Gaze visualization functions and options exist."
  (should (fboundp 'ewwm-vr-set-gaze-visualization))
  (should (boundp 'ewwm-vr-gaze-visualization))
  (should (memq ewwm-vr-gaze-visualization
                '(dot crosshair spotlight none))))

;; ── Dwell focus pipeline ────────────────────────────────────

(ert-deftest e2e-eye/dwell-focus-pipeline ()
  "Gaze data -> dwell -> focus pipeline works."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-vr-gaze-surface nil)
        (ewwm-vr-gaze-position nil)
        (ewwm-vr-gaze-confidence 0.0)
        (ewwm-vr-gaze-source-active nil)
        (ewwm-vr-gaze-tracking-p nil)
        (ewwm-vr-eye--gaze-position nil)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0))
    ;; Simulate gaze data arriving
    (ewwm-vr-eye--on-gaze-data
     '(:surface-id 42 :x 100 :y 200 :confidence 0.95 :source openxr))
    (should (= ewwm-vr-gaze-surface 42))
    (should (equal ewwm-vr-gaze-position '(100 . 200)))
    (should (= ewwm-vr-gaze-confidence 0.95))
    (should (eq ewwm-vr-gaze-source-active 'openxr))
    (should ewwm-vr-gaze-tracking-p)))

(ert-deftest e2e-eye/dwell-progress-update ()
  "Dwell progress event updates progress state."
  (let ((ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-eye--dwell-surface nil))
    (ewwm-vr-eye--on-gaze-dwell-progress
     '(:surface-id 42 :elapsed-ms 100 :threshold-ms 200))
    (should (= ewwm-vr-eye--dwell-progress 0.5))
    (should (= ewwm-vr-eye--dwell-surface 42))))

;; ── Saccade filtering ───────────────────────────────────────

(ert-deftest e2e-eye/saccade-filtering-functions ()
  "Saccade state event handler works."
  (let ((ewwm-vr-eye--in-saccade nil))
    (ewwm-vr-eye--on-gaze-saccade-state '(:active t))
    (should ewwm-vr-eye--in-saccade)
    (ewwm-vr-eye--on-gaze-saccade-state '(:active nil))
    (should-not ewwm-vr-eye--in-saccade)))

(ert-deftest e2e-eye/saccade-threshold-configurable ()
  "Saccade threshold is configurable."
  (should (boundp 'ewwm-vr-eye-saccade-threshold))
  (should (integerp ewwm-vr-eye-saccade-threshold)))

;; ── Wink detection ──────────────────────────────────────────

(ert-deftest e2e-eye/wink-detection-functions ()
  "Wink detection functions exist."
  (should (fboundp 'ewwm-vr-wink-calibrate))
  (should (fboundp 'ewwm-vr-wink-status))
  (should (fboundp 'ewwm-vr-wink-set-actions))
  (should (fboundp 'ewwm-vr-wink-mode-line-string)))

(ert-deftest e2e-eye/wink-calibration-callable ()
  "Wink calibration function is an interactive command."
  (should (commandp 'ewwm-vr-wink-calibrate)))

(ert-deftest e2e-eye/wink-init-teardown ()
  "Wink init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-wink-init)
    (ewwm-vr-wink-teardown)))

;; ── Gaze zones ──────────────────────────────────────────────

(ert-deftest e2e-eye/gaze-zone-9-regions ()
  "Gaze zone system provides all 9 zones."
  (let ((layout (ewwm-vr-gaze-zone--get-layout)))
    (should layout)
    (should (assq 'top-left layout))
    (should (assq 'top-right layout))
    (should (assq 'bottom-left layout))
    (should (assq 'bottom-right layout))
    (should (assq 'top-edge layout))
    (should (assq 'bottom-edge layout))
    (should (assq 'left-edge layout))
    (should (assq 'right-edge layout))
    (should (assq 'center layout))))

(ert-deftest e2e-eye/gaze-zone-layout-presets ()
  "Gaze zone layout presets are available."
  (should (boundp 'ewwm-vr-gaze-zone-layout))
  (should (memq ewwm-vr-gaze-zone-layout
                '(default vim-like spacemacs custom))))

(ert-deftest e2e-eye/gaze-zone-init-teardown ()
  "Gaze zone init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-gaze-zone-init)
    (ewwm-vr-gaze-zone-teardown)))

;; ── Fatigue monitoring ──────────────────────────────────────

(ert-deftest e2e-eye/fatigue-levels ()
  "Fatigue monitoring level is a known symbol."
  (should (boundp 'ewwm-vr-fatigue--level))
  (should (memq ewwm-vr-fatigue--level
                '(normal mild significant critical))))

(ert-deftest e2e-eye/fatigue-functions ()
  "Fatigue monitoring functions exist."
  (should (fboundp 'ewwm-vr-fatigue-status))
  (should (fboundp 'ewwm-vr-fatigue-reset))
  (should (fboundp 'ewwm-vr-fatigue-mode-line-string)))

(ert-deftest e2e-eye/fatigue-init-teardown ()
  "Fatigue init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-fatigue-init)
    (ewwm-vr-fatigue-teardown)))

;; ── Gaze scrolling ──────────────────────────────────────────

(ert-deftest e2e-eye/gaze-scroll-rust-file ()
  "Gaze scroll Rust module exists."
  (let ((root (locate-dominating-file default-directory ".git")))
    (should (file-exists-p
             (expand-file-name
              "compositor/src/vr/gaze_scroll.rs" root)))))

(ert-deftest e2e-eye/gaze-scroll-defcustoms ()
  "Gaze scroll defcustoms are configured."
  (should (boundp 'ewwm-qutebrowser-gaze-scroll-speed))
  (should (boundp 'ewwm-qutebrowser-gaze-scroll-margin)))

;; ── Link hints ──────────────────────────────────────────────

(ert-deftest e2e-eye/link-hints-rust-file ()
  "Link hints Rust module exists."
  (let ((root (locate-dominating-file default-directory ".git")))
    (should (file-exists-p
             (expand-file-name
              "compositor/src/vr/link_hints.rs" root)))))

(ert-deftest e2e-eye/link-hints-defcustoms ()
  "Link hints follow defcustoms are configured."
  (should (boundp 'ewwm-qutebrowser-gaze-follow-dwell-ms))
  (should (boundp 'ewwm-qutebrowser-gaze-follow-enable)))

;; ── Secure input interaction ────────────────────────────────

(ert-deftest e2e-eye/secure-input-functions ()
  "Secure input functions exist."
  (should (fboundp 'ewwm-vr-secure-input-enter))
  (should (fboundp 'ewwm-vr-secure-input-exit))
  (should (fboundp 'ewwm-vr-secure-input-active-p))
  (should (fboundp 'ewwm-vr-secure-input-toggle))
  (should (fboundp 'ewwm-vr-secure-input-status)))

(ert-deftest e2e-eye/secure-input-pauses-gaze ()
  "Secure input config includes gaze pause."
  (should (boundp 'ewwm-vr-secure-input-pause-gaze))
  (should (eq ewwm-vr-secure-input-pause-gaze t))
  (should (boundp 'ewwm-vr-secure-input-pause-wink))
  (should (eq ewwm-vr-secure-input-pause-wink t)))

;; ── Qutebrowser gaze integration ────────────────────────────

(ert-deftest e2e-eye/qutebrowser-gaze-module ()
  "Qutebrowser gaze module provides feature."
  (should (featurep 'ewwm-qutebrowser-gaze))
  (should (get 'ewwm-qutebrowser-gaze 'custom-group)))

;; ── Gaze-away safety ────────────────────────────────────────

(ert-deftest e2e-eye/gaze-away-functions ()
  "Gaze-away safety detection functions exist."
  (should (fboundp 'ewwm-secrets-gaze-away-start))
  (should (fboundp 'ewwm-secrets-gaze-away-stop))
  (should (fboundp 'ewwm-secrets-gaze-away-init))
  (should (fboundp 'ewwm-secrets-gaze-away-teardown)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest e2e-eye/all-event-handlers-registered ()
  "All eye tracking event handlers are registered."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-eye--register-events)
    (dolist (key '(:gaze-data
                   :gaze-target-changed
                   :gaze-fixation
                   :gaze-saccade
                   :gaze-tracking-lost
                   :gaze-dwell
                   :gaze-dwell-progress
                   :gaze-focus-request
                   :gaze-cooldown
                   :gaze-saccade-state
                   :gaze-reading-mode))
      (should (assq key ewwm-ipc--event-handlers)))))

;; ── Mode-line integration ───────────────────────────────────

(ert-deftest e2e-eye/mode-line-during-saccade ()
  "Mode-line shows saccade indicator."
  (let ((ewwm-vr-gaze-mode-line t)
        (ewwm-vr-eye--in-saccade t)
        (ewwm-vr-eye--cooldown-remaining 0)
        (ewwm-vr-gaze-tracking-p t)
        (ewwm-vr-eye--in-reading nil)
        (ewwm-vr-eye-show-dwell-progress nil)
        (ewwm-vr-eye--dwell-surface nil)
        (ewwm-vr-eye--dwell-progress 0.0)
        (ewwm-vr-gaze-surface nil))
    (should (equal (ewwm-vr-eye-mode-line-string) " [Eye:>>>]"))))

;; ── Reading mode ────────────────────────────────────────────

(ert-deftest e2e-eye/reading-mode-handler ()
  "Reading mode event handler works."
  (let ((ewwm-vr-eye--in-reading nil))
    (ewwm-vr-eye--on-gaze-reading-mode '(:active t))
    (should ewwm-vr-eye--in-reading)
    (ewwm-vr-eye--on-gaze-reading-mode '(:active nil))
    (should-not ewwm-vr-eye--in-reading)))

;; ── Defcustom groups ────────────────────────────────────────

(ert-deftest e2e-eye/defcustom-groups-exist ()
  "All eye tracking defcustom groups exist."
  (dolist (grp '(ewwm-vr-eye ewwm-vr-wink
                 ewwm-vr-gaze-zone ewwm-vr-fatigue
                 ewwm-vr-secure-input
                 ewwm-qutebrowser-gaze))
    (should (get grp 'custom-group))))

;; ── Hooks default to nil ────────────────────────────────────

(ert-deftest e2e-eye/hooks-default-nil ()
  "All eye tracking hooks default to nil."
  (dolist (hook '(ewwm-vr-gaze-target-change-hook
                  ewwm-vr-gaze-fixation-hook
                  ewwm-vr-gaze-tracking-lost-hook
                  ewwm-vr-gaze-calibration-drift-hook
                  ewwm-vr-gaze-dwell-hook
                  ewwm-vr-gaze-focus-hook
                  ewwm-vr-secure-input-enter-hook
                  ewwm-vr-secure-input-exit-hook))
    (should (boundp hook))
    (should-not (default-value hook))))

;;; e2e-eye-tracking-test.el ends here
