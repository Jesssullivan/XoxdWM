;;; e2e-vr-mode-test.el --- E2E VR mode tests  -*- lexical-binding: t -*-

;; End-to-end tests for VR mode: OpenXR state, scene graph, DRM lease,
;; stereo rendering, VR interaction, and display management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-vr)
(require 'ewwm-vr-scene)
(require 'ewwm-vr-display)
(require 'ewwm-vr-input)

;; Forward-declare dynamic variables
(defvar ewwm-ipc--event-handlers)
(defvar ewwm-vr-session-state)
(defvar ewwm-vr-hmd-name)
(defvar ewwm-vr-hmd-info)
(defvar ewwm-vr-headless)
(defvar ewwm-vr-frame-stats)
(defvar ewwm-vr-enabled)
(defvar ewwm-vr-session-state-hook)
(defvar ewwm-vr-display-mode)
(defvar ewwm-vr-display-hmd)
(defvar ewwm-vr-display-connector)

;; ── Module loading ──────────────────────────────────────────

(ert-deftest e2e-vr/all-modules-loaded ()
  "All VR modules provide their features."
  (dolist (feat '(ewwm-vr
                  ewwm-vr-scene
                  ewwm-vr-display
                  ewwm-vr-input))
    (should (featurep feat))))

(ert-deftest e2e-vr/module-count ()
  "At least 4 VR modules loaded."
  (let ((count 0))
    (dolist (feat '(ewwm-vr ewwm-vr-scene
                    ewwm-vr-display ewwm-vr-input))
      (when (featurep feat)
        (cl-incf count)))
    (should (>= count 4))))

;; ── OpenXR state ────────────────────────────────────────────

(ert-deftest e2e-vr/openxr-state-variables ()
  "OpenXR state variables are initialized."
  (should (boundp 'ewwm-vr-session-state))
  (should (boundp 'ewwm-vr-hmd-name))
  (should (boundp 'ewwm-vr-hmd-info))
  (should (boundp 'ewwm-vr-headless))
  (should (boundp 'ewwm-vr-frame-stats))
  (should (boundp 'ewwm-vr-enabled)))

(ert-deftest e2e-vr/session-state-handler ()
  "Session state event handler updates state."
  (let ((ewwm-vr-session-state nil)
        (ewwm-vr-headless nil)
        (ewwm-vr-session-state-hook nil))
    (ewwm-vr--on-session-state '(:state :focused :headless nil))
    (should (eq ewwm-vr-session-state :focused))
    (should-not ewwm-vr-headless)))

(ert-deftest e2e-vr/system-discovered-handler ()
  "System discovered event updates HMD info."
  (let ((ewwm-vr-hmd-name nil)
        (ewwm-vr-hmd-info nil))
    (ewwm-vr--on-system-discovered
     '(:system-name "Valve Index"
       :max-resolution (:w 2880 :h 1600)
       :orientation-tracking t
       :position-tracking t))
    (should (equal ewwm-vr-hmd-name "Valve Index"))
    (should (= (plist-get ewwm-vr-hmd-info :max-width) 2880))
    (should (= (plist-get ewwm-vr-hmd-info :max-height) 1600))))

;; ── Frame timing ────────────────────────────────────────────

(ert-deftest e2e-vr/frame-timing-infrastructure ()
  "Frame timing functions exist."
  (should (fboundp 'ewwm-vr-frame-timing))
  (should (fboundp 'ewwm-vr--on-frame-stats)))

(ert-deftest e2e-vr/frame-stats-handler ()
  "Frame stats event updates stats plist."
  (let ((ewwm-vr-frame-stats nil))
    (ewwm-vr--on-frame-stats
     '(:fps 90.0 :total-p50 11.0 :total-p99 15.0
       :missed-pct 0.5 :total-frames 10000
       :missed-frames 50 :wait-p50 2.0
       :render-p50 8.0 :submit-p50 1.0))
    (should (= (plist-get ewwm-vr-frame-stats :fps) 90.0))
    (should (= (plist-get ewwm-vr-frame-stats :total-p50) 11.0))))

;; ── Scene graph API ─────────────────────────────────────────

(ert-deftest e2e-vr/scene-graph-functions ()
  "Scene graph API functions exist."
  (should (fboundp 'ewwm-vr-scene-status))
  (should (fboundp 'ewwm-vr-scene-set-layout))
  (should (fboundp 'ewwm-vr-scene-set-ppu))
  (should (fboundp 'ewwm-vr-scene-set-background))
  (should (fboundp 'ewwm-vr-scene-focus))
  (should (fboundp 'ewwm-vr-scene-move)))

(ert-deftest e2e-vr/scene-layout-options ()
  "Scene layout options are configurable."
  (should (boundp 'ewwm-vr-scene-default-layout))
  (should (memq ewwm-vr-scene-default-layout
                '(arc grid stack freeform))))

(ert-deftest e2e-vr/scene-init-teardown ()
  "Scene init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (should (fboundp 'ewwm-vr-scene-init))
    (should (fboundp 'ewwm-vr-scene-teardown))
    (ewwm-vr-scene-init)
    (ewwm-vr-scene-teardown)))

;; ── DRM lease lifecycle ─────────────────────────────────────

(ert-deftest e2e-vr/drm-lease-functions ()
  "DRM lease lifecycle functions exist."
  (should (fboundp 'ewwm-vr-display-info))
  (should (fboundp 'ewwm-vr-display-set-mode))
  (should (fboundp 'ewwm-vr-display-select-hmd))
  (should (fboundp 'ewwm-vr-display-set-refresh-rate))
  (should (fboundp 'ewwm-vr-display-auto-detect))
  (should (fboundp 'ewwm-vr-display-list-connectors)))

(ert-deftest e2e-vr/drm-init-teardown ()
  "DRM display init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-display-init)
    (ewwm-vr-display-teardown)
    (should-not ewwm-vr-display-mode)
    (should-not ewwm-vr-display-hmd)
    (should-not ewwm-vr-display-connector)))

;; ── Stereo rendering config ─────────────────────────────────

(ert-deftest e2e-vr/stereo-rendering-defcustoms ()
  "Stereo rendering configuration exists."
  ;; VR scene PPU controls pixel density
  (should (boundp 'ewwm-vr-scene-default-ppu))
  (should (integerp ewwm-vr-scene-default-ppu))
  ;; Scene projection function
  (should (fboundp 'ewwm-vr-scene-set-projection)))

;; ── VR interaction functions ────────────────────────────────

(ert-deftest e2e-vr/interaction-functions ()
  "VR interaction functions exist."
  (should (fboundp 'ewwm-vr-click))
  (should (fboundp 'ewwm-vr-grab))
  (should (fboundp 'ewwm-vr-grab-release))
  (should (fboundp 'ewwm-vr-pull-closer))
  (should (fboundp 'ewwm-vr-push-away)))

(ert-deftest e2e-vr/interaction-defcustoms ()
  "VR interaction defcustoms are configured."
  (should (boundp 'ewwm-vr-gaze-ray-offset))
  (should (boundp 'ewwm-vr-depth-step))
  (should (boundp 'ewwm-vr-cursor-scale))
  (should (boundp 'ewwm-vr-show-ray))
  (should (boundp 'ewwm-vr-show-hit-point)))

;; ── Display mode selection ──────────────────────────────────

(ert-deftest e2e-vr/display-mode-options ()
  "Display mode options are correctly defined."
  (should (boundp 'ewwm-vr-display-default-mode))
  (should (memq ewwm-vr-display-default-mode
                '(auto headset preview headless off))))

;; ── HMD manager ─────────────────────────────────────────────

(ert-deftest e2e-vr/hmd-manager-functions ()
  "HMD manager functions exist."
  (should (fboundp 'ewwm-vr-display-auto-detect))
  (should (fboundp 'ewwm-vr-display-select-hmd))
  (should (boundp 'ewwm-vr-display-auto-select-hmd)))

;; ── VR status IPC ───────────────────────────────────────────

(ert-deftest e2e-vr/vr-status-command-format ()
  "VR status command is an interactive command."
  (should (fboundp 'ewwm-vr-status))
  (should (commandp 'ewwm-vr-status)))

;; ── Head-gaze ray ───────────────────────────────────────────

(ert-deftest e2e-vr/head-gaze-ray-config ()
  "Head-gaze ray intersection parameters exist."
  (should (listp ewwm-vr-gaze-ray-offset))
  (should (= (length ewwm-vr-gaze-ray-offset) 3))
  (should (boundp 'ewwm-vr-emacs-follow-mode)))

;; ── Texture manager ─────────────────────────────────────────

(ert-deftest e2e-vr/texture-manager-rust-file ()
  "Texture manager Rust module exists."
  (let ((root (locate-dominating-file default-directory ".git")))
    (should (file-exists-p
             (expand-file-name
              "compositor/src/vr/texture.rs" root)))))

;; ── VR renderer ─────────────────────────────────────────────

(ert-deftest e2e-vr/vr-renderer-rust-file ()
  "VR renderer Rust module exists."
  (let ((root (locate-dominating-file default-directory ".git")))
    (should (file-exists-p
             (expand-file-name
              "compositor/src/vr/vr_renderer.rs" root)))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest e2e-vr/event-handlers-registered ()
  "VR event handlers are registered."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr--register-events)
    (should (assq :vr-session-state ewwm-ipc--event-handlers))
    (should (assq :vr-system-discovered ewwm-ipc--event-handlers))
    (should (assq :vr-frame-stats ewwm-ipc--event-handlers))))

;; ── Mode-line ───────────────────────────────────────────────

(ert-deftest e2e-vr/mode-line-strings ()
  "VR mode-line strings reflect session state."
  (let ((ewwm-vr-mode-line t)
        (ewwm-vr-session-state :focused))
    (should (equal (ewwm-vr-mode-line-string)
                   " [VR:FOCUSED]")))
  (let ((ewwm-vr-mode-line t)
        (ewwm-vr-session-state :headless))
    (should (equal (ewwm-vr-mode-line-string)
                   " [VR:HEADLESS]"))))

;; ── VR defcustom groups ─────────────────────────────────────

(ert-deftest e2e-vr/defcustom-groups-exist ()
  "All VR defcustom groups exist."
  (dolist (grp '(ewwm-vr ewwm-vr-scene
                 ewwm-vr-display ewwm-vr-input))
    (should (get grp 'custom-group))))

;; ── Init/teardown pair ──────────────────────────────────────

(ert-deftest e2e-vr/init-teardown-pair ()
  "VR init and teardown are callable without error."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-init)
    (ewwm-vr-teardown)
    (should-not ewwm-vr-session-state)
    (should-not ewwm-vr-hmd-name)
    (should-not ewwm-vr-frame-stats)))

;;; e2e-vr-mode-test.el ends here
