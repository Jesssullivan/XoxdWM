;;; e2e-full-stack-test.el --- E2E full stack tests  -*- lexical-binding: t -*-

;; End-to-end full stack integration: ALL modules from ALL weeks loaded
;; concurrently.  Validates cross-system composability, resource cleanup,
;; and complete boot/teardown sequences.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ── Load ALL modules ────────────────────────────────────────
;; Core
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-workspace)
(require 'ewwm-layout)
(require 'ewwm-input)
(require 'ewwm-manage)
(require 'ewwm-floating)
(require 'ewwm-launch)
;; VR
(require 'ewwm-vr)
(require 'ewwm-vr-scene)
(require 'ewwm-vr-display)
(require 'ewwm-vr-input)
(require 'ewwm-vr-eye)
(require 'ewwm-vr-wink)
(require 'ewwm-vr-gaze-zone)
(require 'ewwm-vr-fatigue)
(require 'ewwm-vr-hand)
(require 'ewwm-vr-gesture)
(require 'ewwm-vr-keyboard)
(require 'ewwm-vr-secure-input)
;; Browser
(require 'ewwm-qutebrowser)
(require 'ewwm-qutebrowser-ipc)
(require 'ewwm-qutebrowser-tabs)
(require 'ewwm-qutebrowser-theme)
(require 'ewwm-qutebrowser-consult)
(require 'ewwm-qutebrowser-downloads)
(require 'ewwm-qutebrowser-reader)
(require 'ewwm-qutebrowser-adblock)
(require 'ewwm-qutebrowser-userscript)
(require 'ewwm-qutebrowser-gaze)
;; Secrets
(require 'ewwm-secrets)
(require 'ewwm-keepassxc-browser)
(require 'ewwm-secrets-ydotool)
(require 'ewwm-secrets-compositor)
(require 'ewwm-secrets-autotype)
(require 'ewwm-secrets-gaze-away)
(require 'ewwm-secrets-totp)
(require 'ewwm-secrets-passkey)
;; BCI
(require 'ewwm-bci-core)
(require 'ewwm-bci-attention)
(require 'ewwm-bci-ssvep)
(require 'ewwm-bci-p300)
(require 'ewwm-bci-mi)
(require 'ewwm-bci-nfb)
(require 'ewwm-bci-multimodal)
;; Misc
(require 'ewwm-environment)
(require 'ewwm-headless)

;; Forward-declare dynamic variables
(defvar ewwm-ipc--event-handlers)
(defvar ewwm--surface-buffer-alist)
(defvar ewwm-workspace-current-index)
(defvar ewwm-workspace-number)
(defvar ewwm-workspace--configs)
(defvar ewwm-workspace--names)
(defvar ewwm-vr-session-state)
(defvar ewwm-vr-hmd-name)
(defvar ewwm-vr-hmd-info)
(defvar ewwm-vr-headless)
(defvar ewwm-vr-frame-stats)
(defvar ewwm-vr-enabled)
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
(defvar ewwm-vr-eye-enable)
(defvar ewwm-vr-eye-focus-policy)
(defvar ewwm-vr-eye--analytics)
(defvar ewwm-last-focus-method)
(defvar ewwm-vr-display-mode)
(defvar ewwm-vr-display-hmd)
(defvar ewwm-vr-display-connector)
(defvar ewwm-vr-fatigue--level)
(defvar ewwm-vr-hand-enable)
(defvar ewwm-vr-hand--left-active)
(defvar ewwm-vr-hand--right-active)
(defvar ewwm-vr-hand--left-confidence)
(defvar ewwm-vr-hand--right-confidence)
(defvar ewwm-vr-gesture-enable)
(defvar ewwm-vr-gesture--bindings)
(defvar ewwm-vr-gesture--last-gesture)
(defvar ewwm-vr-gesture--last-time)
(defvar ewwm-vr-gesture-verbose)
(defvar ewwm-vr-keyboard--visible)
(defvar ewwm-vr-secure-input--active)
(defvar ewwm-vr-secure-input--reason)
(defvar ewwm-bci-multimodal-enabled)
(defvar ewwm-bci-multimodal--active)
(defvar ewwm-bci-multimodal-adaptive-dwell)
(defvar ewwm-bci-multimodal-dwell-focused-ms)
(defvar ewwm-bci-multimodal-dwell-relaxed-ms)
(defvar ewwm-bci-multimodal-dwell-default-ms)
(defvar ewwm-bci-multimodal-focused-threshold)
(defvar ewwm-bci-multimodal-relaxed-threshold)
(defvar ewwm-bci-multimodal--dwell-override)
(defvar ewwm-bci-multimodal--last-attention-score)
(defvar ewwm-bci-attention-enabled)
(defvar ewwm-bci-attention--score)
(defvar ewwm-bci-attention--state)
(defvar ewwm-bci-attention-change-hook)
(defvar ewwm-bci-attention-threshold)
(defvar ewwm-bci-attention-dnd-threshold)
(defvar ewwm-bci-attention-drowsy-threshold)
(defvar ewwm-bci-attention-update-interval)
(defvar ewwm-bci-attention-auto-save)
(defvar ewwm-bci-attention--band-powers)
(defvar ewwm-bci-attention--dnd-active)
(defvar ewwm-bci-attention--state-start-time)
(defvar ewwm-bci-attention--last-update-time)
(defvar ewwm-bci-attention--history)
(defvar ewwm-layout--current)
(defvar ewwm-input--global-keys)
(defvar ewwm-headless-mode)

;; ── ALL modules loaded concurrently ─────────────────────────

(ert-deftest e2e-full/all-modules-loaded ()
  "All ewwm-* modules from all weeks are loaded."
  (dolist (feat '(;; Core
                  ewwm-core ewwm-ipc ewwm-workspace
                  ewwm-layout ewwm-input ewwm-manage
                  ewwm-floating ewwm-launch
                  ;; VR
                  ewwm-vr ewwm-vr-scene ewwm-vr-display
                  ewwm-vr-input ewwm-vr-eye ewwm-vr-wink
                  ewwm-vr-gaze-zone ewwm-vr-fatigue
                  ewwm-vr-hand ewwm-vr-gesture
                  ewwm-vr-keyboard ewwm-vr-secure-input
                  ;; Browser
                  ewwm-qutebrowser ewwm-qutebrowser-ipc
                  ewwm-qutebrowser-tabs ewwm-qutebrowser-theme
                  ewwm-qutebrowser-consult
                  ewwm-qutebrowser-downloads
                  ewwm-qutebrowser-reader
                  ewwm-qutebrowser-adblock
                  ewwm-qutebrowser-userscript
                  ewwm-qutebrowser-gaze
                  ;; Secrets
                  ewwm-secrets ewwm-keepassxc-browser
                  ewwm-secrets-ydotool ewwm-secrets-compositor
                  ewwm-secrets-autotype ewwm-secrets-gaze-away
                  ewwm-secrets-totp ewwm-secrets-passkey
                  ;; BCI
                  ewwm-bci-core ewwm-bci-attention
                  ewwm-bci-ssvep ewwm-bci-p300
                  ewwm-bci-mi ewwm-bci-nfb
                  ewwm-bci-multimodal
                  ;; Misc
                  ewwm-environment ewwm-headless))
    (should (featurep feat))))

;; ── Module count verification ───────────────────────────────

(ert-deftest e2e-full/module-count ()
  "At least 45 ewwm-* features loaded."
  (let ((count 0))
    (dolist (feat features)
      (when (string-prefix-p "ewwm-" (symbol-name feat))
        (cl-incf count)))
    (should (>= count 45))))

;; ── Cross-system: VR + eye tracking ─────────────────────────

(ert-deftest e2e-full/vr-eye-composability ()
  "VR session + eye tracking coexist and produce state."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-headless nil)
        (ewwm-vr-session-state-hook nil)
        (ewwm-vr-gaze-surface nil)
        (ewwm-vr-gaze-tracking-p nil)
        (ewwm-vr-eye--gaze-position nil)
        (ewwm-vr-gaze-confidence 0.0)
        (ewwm-vr-gaze-source-active nil)
        (ewwm-vr-gaze-position nil))
    ;; VR session comes up
    (ewwm-vr--on-session-state '(:state :focused :headless nil))
    (should (eq ewwm-vr-session-state :focused))
    ;; Eye tracking starts delivering gaze data
    (ewwm-vr-eye--on-gaze-data
     '(:surface-id 10 :x 500 :y 300 :confidence 0.9
       :source openxr))
    (should (= ewwm-vr-gaze-surface 10))
    (should ewwm-vr-gaze-tracking-p)))

;; ── Cross-system: eye tracking + BCI ────────────────────────

(ert-deftest e2e-full/eye-bci-adaptive-dwell ()
  "Eye tracking + BCI attention modulates dwell threshold."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 100)
        (ewwm-bci-multimodal-dwell-relaxed-ms 500)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      ;; User highly focused -> shorter dwell
      (ewwm-bci-multimodal--handle-attention '(:score 0.9))
      (should (= ewwm-bci-multimodal--dwell-override 100))
      ;; User drowsy -> longer dwell
      (ewwm-bci-multimodal--handle-attention '(:score 0.2))
      (should (= ewwm-bci-multimodal--dwell-override 500)))))

;; ── Cross-system: hand tracking + gaze ──────────────────────

(ert-deftest e2e-full/hand-gaze-look-and-tap ()
  "Hand tracking + gaze: both systems produce state simultaneously."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active nil)
        (ewwm-vr-hand--left-confidence 0.0)
        (ewwm-vr-hand--right-confidence 0.0)
        (ewwm-vr-gaze-surface nil)
        (ewwm-vr-gaze-tracking-p nil)
        (ewwm-vr-gaze-confidence 0.0)
        (ewwm-vr-gaze-source-active nil)
        (ewwm-vr-gaze-position nil)
        (ewwm-vr-eye--gaze-position nil))
    ;; Hand tracking starts
    (ewwm-vr-hand--on-tracking-started '(:hand right))
    (should ewwm-vr-hand--right-active)
    (ewwm-vr-hand--on-confidence-update
     '(:hand right :confidence 0.95))
    (should (= ewwm-vr-hand--right-confidence 0.95))
    ;; Gaze delivers target simultaneously
    (ewwm-vr-eye--on-gaze-data
     '(:surface-id 7 :x 200 :y 150 :confidence 0.92
       :source openxr))
    (should (= ewwm-vr-gaze-surface 7))
    ;; Both systems active at same time
    (should ewwm-vr-hand--right-active)
    (should ewwm-vr-gaze-tracking-p)))

;; ── Cross-system: BCI attention + gaze dwell ────────────────

(ert-deftest e2e-full/bci-attention-gaze-dwell ()
  "BCI attention score + gaze dwell modulation chain works."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention-change-hook nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 2.0)
        (ewwm-bci-attention-auto-save nil))
    ;; Attention update comes in
    (ewwm-bci-attention--on-bci-attention
     '(:score 0.8 :state focused
       :band-powers (:alpha 0.3 :beta 0.7)))
    (should (= ewwm-bci-attention--score 0.8))
    ;; State may be deep-focus when >= dnd-threshold
    (should (memq ewwm-bci-attention--state
                  '(focused deep-focus)))))

;; ── Cross-system: secure input pauses ALL biometrics ────────

(ert-deftest e2e-full/secure-input-pauses-biometrics ()
  "Secure input config pauses gaze, wink, and EEG."
  (should (boundp 'ewwm-vr-secure-input-pause-gaze))
  (should (boundp 'ewwm-vr-secure-input-pause-wink))
  (should (boundp 'ewwm-vr-secure-input-pause-eeg))
  (should (eq ewwm-vr-secure-input-pause-gaze t))
  (should (eq ewwm-vr-secure-input-pause-wink t))
  (should (eq ewwm-vr-secure-input-pause-eeg t)))

;; ── Cross-system: qutebrowser + gaze + BCI ──────────────────

(ert-deftest e2e-full/qutebrowser-gaze-bci ()
  "Qutebrowser, gaze, and BCI modules coexist."
  (should (featurep 'ewwm-qutebrowser))
  (should (featurep 'ewwm-qutebrowser-gaze))
  (should (featurep 'ewwm-bci-multimodal))
  ;; All provide their features without conflict
  (should (fboundp 'ewwm-qutebrowser-open-url))
  (should (boundp 'ewwm-qutebrowser-gaze-follow-enable))
  (should (fboundp 'ewwm-bci-multimodal-status)))

;; ── IPC commands in dispatch.rs ─────────────────────────────

(ert-deftest e2e-full/dispatch-rs-all-commands ()
  "dispatch.rs contains all IPC command strings."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch-file
          (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (when (file-exists-p dispatch-file)
      (let ((contents (with-temp-buffer
                        (insert-file-contents dispatch-file)
                        (buffer-string))))
        ;; Core commands
        (dolist (cmd '("hello" "ping" "surface-list"
                       "surface-focus" "surface-close"
                       "workspace-switch" "workspace-list"
                       "layout-set" "layout-cycle"
                       "key-grab" "key-ungrab"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; VR commands
        (dolist (cmd '("vr-status" "vr-set-reference-space"
                       "vr-scene-status" "vr-scene-set-layout"
                       "vr-display-info" "vr-display-set-mode"
                       "vr-grab" "vr-grab-release"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; Eye tracking commands
        (dolist (cmd '("gaze-status" "gaze-focus-config"
                       "gaze-focus-status" "gaze-focus-set-policy"
                       "wink-status" "gaze-zone-status"
                       "fatigue-status" "fatigue-metrics"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; Secrets commands
        (dolist (cmd '("autotype" "autotype-status"
                       "secure-input-mode"
                       "secure-input-status"
                       "gaze-away-monitor"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; Headless commands
        (dolist (cmd '("headless-status"
                       "headless-set-resolution"
                       "headless-add-output"
                       "headless-remove-output"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; Gaze scroll / link hints
        (dolist (cmd '("gaze-scroll-status"
                       "gaze-scroll-config"
                       "link-hints-load"
                       "link-hints-confirm"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; Hand tracking / gesture / keyboard
        (dolist (cmd '("hand-tracking-status"
                       "gesture-status" "gesture-bind"
                       "keyboard-show" "keyboard-hide"
                       "keyboard-status"))
          (should (string-match-p (regexp-quote cmd) contents)))
        ;; BCI commands
        (dolist (cmd '("bci-status" "bci-start" "bci-stop"
                       "bci-signal-quality"
                       "bci-attention-status"
                       "bci-ssvep-status"
                       "bci-p300-status"
                       "bci-mi-status"
                       "bci-fatigue-eeg-status"))
          (should (string-match-p (regexp-quote cmd) contents)))))))

;; ── All defcustom groups ────────────────────────────────────

(ert-deftest e2e-full/all-defcustom-groups ()
  "All expected defcustom groups exist."
  (dolist (grp '(ewwm ewwm-workspace ewwm-layout
                 ewwm-input ewwm-floating ewwm-launch
                 ewwm-vr ewwm-vr-scene ewwm-vr-display
                 ewwm-vr-input ewwm-vr-eye ewwm-vr-wink
                 ewwm-vr-gaze-zone ewwm-vr-fatigue
                 ewwm-vr-hand ewwm-vr-gesture
                 ewwm-vr-keyboard ewwm-vr-secure-input
                 ewwm-qutebrowser-gaze
                 ewwm-bci ewwm-bci-attention
                 ewwm-bci-ssvep ewwm-bci-p300
                 ewwm-bci-mi ewwm-bci-nfb
                 ewwm-bci-multimodal
                 ewwm-environment ewwm-headless))
    (should (get grp 'custom-group))))

;; ── All hooks default to nil ────────────────────────────────

(ert-deftest e2e-full/all-hooks-nil ()
  "All hooks default to nil."
  (dolist (hook '(ewwm-workspace-switch-hook
                  ewwm-layout-change-hook
                  ewwm-manage-finish-hook
                  ewwm-manage-close-hook
                  ewwm-ipc-connected-hook
                  ewwm-ipc-disconnected-hook
                  ewwm-mode-hook
                  ewwm-vr-session-state-hook
                  ewwm-vr-gaze-target-change-hook
                  ewwm-vr-gaze-fixation-hook
                  ewwm-vr-gaze-tracking-lost-hook
                  ewwm-vr-gaze-dwell-hook
                  ewwm-vr-gaze-focus-hook
                  ewwm-vr-secure-input-enter-hook
                  ewwm-vr-secure-input-exit-hook
                  ewwm-vr-hand-tracking-started-hook
                  ewwm-vr-hand-tracking-lost-hook
                  ewwm-vr-gesture-hook
                  ewwm-vr-keyboard-show-hook
                  ewwm-vr-keyboard-hide-hook
                  ewwm-bci-connected-hook
                  ewwm-bci-disconnected-hook
                  ewwm-bci-attention-change-hook
                  ewwm-bci-ssvep-select-hook
                  ewwm-bci-p300-detect-hook
                  ewwm-bci-mi-classify-hook
                  ewwm-bci-multimodal-fusion-hook))
    (should (boundp hook))
    (should-not (default-value hook))))

;; ── All interactive commands exist ──────────────────────────

(ert-deftest e2e-full/all-interactive-commands ()
  "Key interactive commands are defined."
  (dolist (cmd '(;; Core
                 ewwm-workspace-switch ewwm-workspace-switch-next
                 ewwm-workspace-switch-prev
                 ewwm-layout-set ewwm-layout-cycle
                 ewwm-launch ewwm-launch-interactively
                 ewwm-floating-toggle ewwm-floating-move
                 ewwm-ipc-connect ewwm-ipc-disconnect
                 ewwm-ipc-status ewwm-ipc-ping
                 ;; VR
                 ewwm-vr-status ewwm-vr-set-reference-space
                 ewwm-vr-restart ewwm-vr-frame-timing
                 ewwm-vr-scene-status ewwm-vr-scene-set-layout
                 ewwm-vr-display-info ewwm-vr-display-set-mode
                 ;; Eye
                 ewwm-vr-calibrate-eyes ewwm-vr-gaze-health
                 ewwm-vr-gaze-status ewwm-vr-eye-focus-back
                 ewwm-vr-wink-calibrate ewwm-vr-wink-status
                 ewwm-vr-gaze-zone-set-layout
                 ewwm-vr-fatigue-status ewwm-vr-fatigue-reset
                 ;; Hand / gesture / keyboard
                 ewwm-vr-hand-status ewwm-vr-hand-toggle
                 ewwm-vr-gesture-status ewwm-vr-gesture-toggle
                 ewwm-vr-keyboard-show ewwm-vr-keyboard-hide
                 ewwm-vr-keyboard-toggle
                 ;; BCI
                 ewwm-bci-start ewwm-bci-stop ewwm-bci-status
                 ewwm-bci-hardware-check
                 ewwm-bci-attention-status
                 ewwm-bci-ssvep-status ewwm-bci-p300-status
                 ewwm-bci-mi-status ewwm-bci-nfb-status
                 ewwm-bci-multimodal-status
                 ;; Headless
                 ewwm-headless-status))
    (should (fboundp cmd))))

;; ── All init/teardown pairs callable ────────────────────────

(ert-deftest e2e-full/all-init-functions ()
  "All module init functions are callable."
  (dolist (fn '(ewwm-vr-init ewwm-vr-scene-init
                ewwm-vr-display-init ewwm-vr-eye-init
                ewwm-vr-wink-init ewwm-vr-gaze-zone-init
                ewwm-vr-fatigue-init ewwm-vr-hand-init
                ewwm-vr-gesture-init ewwm-vr-keyboard-init
                ewwm-vr-secure-input-init
                ewwm-secrets-gaze-away-init
                ewwm-bci-init ewwm-bci-attention-init
                ewwm-bci-ssvep-init ewwm-bci-p300-init
                ewwm-bci-mi-init ewwm-bci-nfb-init
                ewwm-bci-multimodal-init
                ewwm-headless-init))
    (should (fboundp fn))))

(ert-deftest e2e-full/all-teardown-functions ()
  "All module teardown functions are callable."
  (dolist (fn '(ewwm-vr-teardown ewwm-vr-scene-teardown
                ewwm-vr-display-teardown ewwm-vr-eye-teardown
                ewwm-vr-wink-teardown ewwm-vr-gaze-zone-teardown
                ewwm-vr-fatigue-teardown ewwm-vr-hand-teardown
                ewwm-vr-gesture-teardown ewwm-vr-keyboard-teardown
                ewwm-vr-secure-input-teardown
                ewwm-secrets-gaze-away-teardown
                ewwm-bci-teardown ewwm-bci-attention-teardown
                ewwm-bci-ssvep-teardown ewwm-bci-p300-teardown
                ewwm-bci-mi-teardown ewwm-bci-nfb-teardown
                ewwm-bci-multimodal-teardown
                ewwm-headless-teardown))
    (should (fboundp fn))))

;; ── All mode-line functions callable ────────────────────────

(ert-deftest e2e-full/all-mode-line-functions ()
  "All mode-line functions are callable."
  (dolist (fn '(ewwm-vr-mode-line-string
                ewwm-vr-eye-mode-line-string
                ewwm-vr-wink-mode-line-string
                ewwm-vr-gaze-zone-mode-line-string
                ewwm-vr-fatigue-mode-line-string
                ewwm-vr-hand-mode-line-string
                ewwm-vr-keyboard-mode-line-string
                ewwm-vr-secure-input-mode-line-string
                ewwm-bci-mode-line-string
                ewwm-bci-attention-mode-line-string
                ewwm-bci-ssvep-mode-line-string
                ewwm-headless-mode-line-string))
    (should (fboundp fn))))

;; ── Composability matrix key patterns ───────────────────────

(ert-deftest e2e-full/composability-vr-hand-gesture ()
  "Hand tracking + gesture recognition compose."
  (let ((ewwm-vr-hand-enable t)
        (ewwm-vr-hand--left-active nil)
        (ewwm-vr-hand--right-active nil)
        (ewwm-vr-hand--left-confidence 0.0)
        (ewwm-vr-hand--right-confidence 0.0)
        (ewwm-vr-gesture-enable t)
        (ewwm-vr-gesture--bindings nil)
        (ewwm-vr-gesture--last-gesture nil)
        (ewwm-vr-gesture--last-time nil)
        (ewwm-vr-gesture-verbose nil)
        (dispatched nil))
    (ewwm-vr-hand--on-tracking-started '(:hand right))
    (ewwm-vr-hand--on-confidence-update
     '(:hand right :confidence 0.9))
    (ewwm-vr-gesture-bind
     'right 'pinch (lambda () (setq dispatched t)))
    (ewwm-vr-gesture--on-started
     '(:hand right :gesture pinch))
    (should dispatched)))

(ert-deftest e2e-full/composability-keyboard-text ()
  "Virtual keyboard text input works."
  (let ((ewwm-vr-keyboard--visible nil)
        (ewwm-vr-keyboard-show-hook nil)
        (ewwm-vr-keyboard-hide-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-vr-keyboard-show)
      (should ewwm-vr-keyboard--visible)
      (with-temp-buffer
        (ewwm-vr-keyboard--on-text-input '(:text "test"))
        (should (equal (buffer-string) "test")))
      (ewwm-vr-keyboard-hide)
      (should-not ewwm-vr-keyboard--visible))))

;; ── Zero resource leaks on teardown ─────────────────────────

(ert-deftest e2e-full/zero-resource-leaks ()
  "All state resets on teardown."
  (let ((ewwm-ipc--event-handlers nil))
    ;; Init everything
    (ewwm-vr-init)
    (ewwm-vr-scene-init)
    (ewwm-vr-display-init)
    (ewwm-vr-eye-init)
    (ewwm-vr-wink-init)
    (ewwm-vr-gaze-zone-init)
    (ewwm-vr-fatigue-init)
    (ewwm-vr-hand-init)
    (ewwm-vr-gesture-init)
    (ewwm-vr-keyboard-init)
    (ewwm-bci-init)
    (ewwm-bci-attention-init)
    (ewwm-bci-ssvep-init)
    (ewwm-bci-p300-init)
    (ewwm-bci-mi-init)
    (ewwm-bci-nfb-init)
    (ewwm-bci-multimodal-init)
    ;; Teardown everything
    (ewwm-vr-teardown)
    (ewwm-vr-scene-teardown)
    (ewwm-vr-display-teardown)
    (ewwm-vr-eye-teardown)
    (ewwm-vr-wink-teardown)
    (ewwm-vr-gaze-zone-teardown)
    (ewwm-vr-fatigue-teardown)
    (ewwm-vr-hand-teardown)
    (ewwm-vr-gesture-teardown)
    (ewwm-vr-keyboard-teardown)
    (ewwm-bci-teardown)
    (ewwm-bci-attention-teardown)
    (ewwm-bci-ssvep-teardown)
    (ewwm-bci-p300-teardown)
    (ewwm-bci-mi-teardown)
    (ewwm-bci-nfb-teardown)
    (ewwm-bci-multimodal-teardown)
    ;; Verify VR state is clean
    (should-not ewwm-vr-session-state)
    (should-not ewwm-vr-hmd-name)
    (should-not ewwm-vr-frame-stats)
    ;; Verify display state is clean
    (should-not ewwm-vr-display-mode)
    (should-not ewwm-vr-display-hmd)
    ;; Verify eye state is clean
    (should-not ewwm-vr-gaze-surface)
    (should-not ewwm-vr-gaze-tracking-p)
    (should-not ewwm-vr-eye--tracking-active)))

;; ── Full boot sequence simulation ───────────────────────────

(ert-deftest e2e-full/boot-teardown-sequence ()
  "Full boot: init all -> teardown all without error."
  (let ((ewwm-ipc--event-handlers nil)
        (ewwm-headless-disable-features nil))
    ;; Boot sequence
    (ewwm-vr-init)
    (ewwm-vr-scene-init)
    (ewwm-vr-display-init)
    (ewwm-vr-eye-init)
    (ewwm-vr-wink-init)
    (ewwm-vr-gaze-zone-init)
    (ewwm-vr-fatigue-init)
    (ewwm-vr-hand-init)
    (ewwm-vr-gesture-init)
    (ewwm-vr-keyboard-init)
    (ewwm-vr-secure-input-init)
    (ewwm-secrets-gaze-away-init)
    (ewwm-bci-init)
    (ewwm-bci-attention-init)
    (ewwm-bci-ssvep-init)
    (ewwm-bci-p300-init)
    (ewwm-bci-mi-init)
    (ewwm-bci-nfb-init)
    (ewwm-bci-multimodal-init)
    (ewwm-headless-init)
    ;; Verify all event handlers are registered
    (should (> (length ewwm-ipc--event-handlers) 20))
    ;; Teardown sequence (reverse order)
    (ewwm-headless-teardown)
    (ewwm-bci-multimodal-teardown)
    (ewwm-bci-nfb-teardown)
    (ewwm-bci-mi-teardown)
    (ewwm-bci-p300-teardown)
    (ewwm-bci-ssvep-teardown)
    (ewwm-bci-attention-teardown)
    (ewwm-bci-teardown)
    (ewwm-secrets-gaze-away-teardown)
    (ewwm-vr-secure-input-teardown)
    (ewwm-vr-keyboard-teardown)
    (ewwm-vr-gesture-teardown)
    (ewwm-vr-hand-teardown)
    (ewwm-vr-fatigue-teardown)
    (ewwm-vr-gaze-zone-teardown)
    (ewwm-vr-wink-teardown)
    (ewwm-vr-eye-teardown)
    (ewwm-vr-display-teardown)
    (ewwm-vr-scene-teardown)
    (ewwm-vr-teardown)
    ;; Should complete without error
    (should t)))

;; ── Rust module files all exist ─────────────────────────────

(ert-deftest e2e-full/all-rust-vr-modules ()
  "All Rust VR module files exist."
  (let ((root (locate-dominating-file default-directory ".git")))
    (dolist (file '("compositor/src/vr/scene.rs"
                    "compositor/src/vr/texture.rs"
                    "compositor/src/vr/vr_renderer.rs"
                    "compositor/src/vr/drm_lease.rs"
                    "compositor/src/vr/frame_timing.rs"
                    "compositor/src/vr/eye_tracking.rs"
                    "compositor/src/vr/gaze_focus.rs"
                    "compositor/src/vr/blink_wink.rs"
                    "compositor/src/vr/gaze_zone.rs"
                    "compositor/src/vr/fatigue.rs"
                    "compositor/src/vr/vr_interaction.rs"
                    "compositor/src/vr/gaze_scroll.rs"
                    "compositor/src/vr/link_hints.rs"
                    "compositor/src/vr/hand_tracking.rs"
                    "compositor/src/vr/gesture.rs"
                    "compositor/src/vr/virtual_keyboard.rs"
                    "compositor/src/vr/attention.rs"
                    "compositor/src/vr/ssvep.rs"
                    "compositor/src/vr/p300.rs"
                    "compositor/src/vr/motor_imagery.rs"
                    "compositor/src/vr/fatigue_eeg.rs"
                    "compositor/src/vr/bci_state.rs"))
      (should (file-exists-p
               (expand-file-name file root))))))

;; ── Elisp module files all exist ────────────────────────────

(ert-deftest e2e-full/all-elisp-modules ()
  "All Elisp module files exist."
  (let ((root (locate-dominating-file default-directory ".git")))
    (dolist (file '("lisp/vr/ewwm-core.el"
                    "lisp/vr/ewwm-ipc.el"
                    "lisp/vr/ewwm-workspace.el"
                    "lisp/vr/ewwm-layout.el"
                    "lisp/vr/ewwm-input.el"
                    "lisp/vr/ewwm-manage.el"
                    "lisp/vr/ewwm-floating.el"
                    "lisp/vr/ewwm-launch.el"
                    "lisp/vr/ewwm-vr.el"
                    "lisp/vr/ewwm-vr-scene.el"
                    "lisp/vr/ewwm-vr-display.el"
                    "lisp/vr/ewwm-vr-input.el"
                    "lisp/vr/ewwm-vr-eye.el"
                    "lisp/vr/ewwm-vr-wink.el"
                    "lisp/vr/ewwm-vr-gaze-zone.el"
                    "lisp/vr/ewwm-vr-fatigue.el"
                    "lisp/vr/ewwm-vr-hand.el"
                    "lisp/vr/ewwm-vr-gesture.el"
                    "lisp/vr/ewwm-vr-keyboard.el"
                    "lisp/vr/ewwm-vr-secure-input.el"
                    "lisp/vr/ewwm-qutebrowser.el"
                    "lisp/vr/ewwm-qutebrowser-gaze.el"
                    "lisp/vr/ewwm-secrets.el"
                    "lisp/vr/ewwm-secrets-gaze-away.el"
                    "lisp/vr/ewwm-bci-core.el"
                    "lisp/vr/ewwm-bci-attention.el"
                    "lisp/vr/ewwm-bci-multimodal.el"
                    "lisp/vr/ewwm-environment.el"
                    "lisp/vr/ewwm-headless.el"))
      (should (file-exists-p
               (expand-file-name file root))))))

;; ── Total test count verification ───────────────────────────

(ert-deftest e2e-full/total-test-count ()
  "Total ERT tests across all test files >= 1365."
  (let ((root (locate-dominating-file default-directory ".git")))
    (when root
      (let ((test-dir (expand-file-name "test/" root))
            (test-count 0))
        (when (file-directory-p test-dir)
          (dolist (file (directory-files test-dir t "\\.el$"))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward
                      "^(ert-deftest " nil t)
                (cl-incf test-count)))))
        (should (>= test-count 1365))))))

;; ── Surface buffer alist management end-to-end ──────────────

(ert-deftest e2e-full/surface-alist-full-lifecycle ()
  "Surface buffer alist: create, query, destroy across modules."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-manage-rules nil)
        (ewwm-manage-finish-hook nil)
        (ewwm-manage-close-hook nil)
        (ewwm-workspace-current-index 0))
    ;; Create 3 surfaces
    (dolist (id '(1 2 3))
      (ewwm-manage--on-create
       `(:id ,id :app-id ,(format "app-%d" id)
         :title ,(format "Surface %d" id))))
    (should (= (ewwm--surface-count) 3))
    (should (equal (sort (ewwm--all-surfaces) #'<) '(1 2 3)))
    ;; Query workspace buffers
    (let ((bufs (ewwm--buffers-on-workspace 0)))
      (should (= (length bufs) 3)))
    ;; Destroy one
    (ewwm-manage--on-destroy '(:id 2))
    (should (= (ewwm--surface-count) 2))
    ;; Cleanup
    (ewwm-manage--on-destroy '(:id 1))
    (ewwm-manage--on-destroy '(:id 3))
    (should (= (ewwm--surface-count) 0))))

;;; e2e-full-stack-test.el ends here
