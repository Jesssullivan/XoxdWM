;;; ewwm-vr-eye.el --- Eye tracking integration  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Eye tracking for ewwm: OpenXR XR_EXT_eye_gaze_interaction, Pupil Labs
;; ZMQ client, unified gaze model, gaze-surface intersection, visualization,
;; calibration, health monitoring, and simulated gaze for development.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-vr-eye nil
  "Eye tracking settings."
  :group 'ewwm-vr)

(defcustom ewwm-vr-gaze-source 'auto
  "Preferred gaze data source.
`auto': prefer OpenXR, fall back to Pupil Labs, then simulated.
`openxr': force OpenXR eye gaze extension.
`pupil-labs': force Pupil Labs ZMQ client.
`simulated': use simulated gaze for development.
`none': disable eye tracking."
  :type '(choice (const auto)
                 (const openxr)
                 (const pupil-labs)
                 (const simulated)
                 (const none))
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-gaze-smoothing 0.3
  "EMA alpha for gaze smoothing.
0 = maximum smooth (high latency), 1 = no smoothing (jittery)."
  :type 'number
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-gaze-visualization 'dot
  "Visualization mode for the gaze indicator.
`dot': small circle at intersection point.
`crosshair': thin crosshair lines.
`spotlight': larger area with gaussian falloff.
`none': no visualization."
  :type '(choice (const dot)
                 (const crosshair)
                 (const spotlight)
                 (const none))
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-gaze-simulate nil
  "Simulated gaze mode for development without eye tracker hardware.
nil to disable simulation.
`mouse': mouse position drives gaze.
`scripted': replay recorded gaze trace.
`random-walk': random saccade-like gaze wandering.
`pattern': cycle through surfaces dwelling 2 seconds each."
  :type '(choice (const nil)
                 (const mouse)
                 (const scripted)
                 (const random-walk)
                 (const pattern))
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-gaze-confidence-min 0.6
  "Minimum gaze confidence to update target.
Samples below this threshold are ignored."
  :type 'number
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-pupil-port 50020
  "Pupil Capture IPC port for ZMQ connection."
  :type 'integer
  :group 'ewwm-vr-eye)

(defcustom ewwm-vr-gaze-mode-line t
  "Non-nil to show gaze target in mode-line."
  :type 'boolean
  :group 'ewwm-vr-eye)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-vr-gaze-surface nil
  "Surface ID currently under gaze, or nil.")

(defvar ewwm-vr-gaze-position nil
  "Cons of (x . y) pixel coordinates of gaze on current surface.")

(defvar ewwm-vr-gaze-confidence 0.0
  "Current gaze confidence [0.0, 1.0].")

(defvar ewwm-vr-gaze-source-active nil
  "Symbol for the currently active gaze source, or nil.")

(defvar ewwm-vr-gaze-tracking-p nil
  "Non-nil when eye tracking is active and producing data.")

(defvar ewwm-vr-gaze-calibrated-p nil
  "Non-nil when eye tracker has been calibrated.")

(defvar ewwm-vr-eye--tracking-active nil
  "Non-nil when the eye tracking subsystem is running.")

(defvar ewwm-vr-eye--gaze-position nil
  "Current gaze position as (X . Y) in screen coordinates.")

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-vr-gaze-target-change-hook nil
  "Hook run when gaze moves to a different surface.
Functions receive (SURFACE-ID PREV-SURFACE-ID).")

(defvar ewwm-vr-gaze-fixation-hook nil
  "Hook run on detected gaze fixation.
Functions receive (SURFACE-ID X Y DURATION-MS).")

(defvar ewwm-vr-gaze-tracking-lost-hook nil
  "Hook run when eye tracking is lost.
Functions receive (SOURCE DURATION-MS).")

(defvar ewwm-vr-gaze-calibration-drift-hook nil
  "Hook run when calibration drift is detected.
Functions receive (ERROR-DEG).")

;; ── IPC event handlers ──────────────────────────────────────

(defun ewwm-vr-eye--on-gaze-data (msg)
  "Handle :gaze-data event MSG."
  (let ((sid (plist-get msg :surface-id))
        (x (plist-get msg :x))
        (y (plist-get msg :y))
        (conf (plist-get msg :confidence))
        (source (plist-get msg :source)))
    (setq ewwm-vr-gaze-surface sid
          ewwm-vr-gaze-position (when (and x y) (cons x y))
          ewwm-vr-gaze-confidence (or conf 0.0)
          ewwm-vr-gaze-source-active source
          ewwm-vr-gaze-tracking-p t
          ewwm-vr-eye--gaze-position (when (and x y) (cons x y)))))

(defun ewwm-vr-eye--on-gaze-target-changed (msg)
  "Handle :gaze-target-changed event MSG."
  (let ((sid (plist-get msg :surface-id))
        (prev (plist-get msg :prev-surface-id)))
    (setq ewwm-vr-gaze-surface sid)
    (run-hook-with-args 'ewwm-vr-gaze-target-change-hook sid prev)))

(defun ewwm-vr-eye--on-gaze-fixation (msg)
  "Handle :gaze-fixation event MSG."
  (let ((sid (plist-get msg :surface-id))
        (x (plist-get msg :x))
        (y (plist-get msg :y))
        (dur (plist-get msg :duration-ms)))
    (run-hook-with-args 'ewwm-vr-gaze-fixation-hook sid x y dur)))

(defun ewwm-vr-eye--on-gaze-saccade (_msg)
  "Handle :gaze-saccade event MSG (informational).")

(defun ewwm-vr-eye--on-gaze-tracking-lost (msg)
  "Handle :gaze-tracking-lost event MSG."
  (let ((source (plist-get msg :source))
        (dur (plist-get msg :duration-ms)))
    (setq ewwm-vr-gaze-tracking-p nil)
    (run-hook-with-args 'ewwm-vr-gaze-tracking-lost-hook source dur)
    (message "ewwm-vr-eye: tracking lost (%s, %dms)" source (or dur 0))))

(defun ewwm-vr-eye--on-gaze-calibration-drift (msg)
  "Handle :gaze-calibration-drift event MSG."
  (let ((err (plist-get msg :error-deg)))
    (run-hook-with-args 'ewwm-vr-gaze-calibration-drift-hook err)
    (message "ewwm-vr-eye: calibration drift detected (%.1f deg)" (or err 0))))

;; ── Interactive commands ────────────────────────────────────

(defun ewwm-vr-calibrate-eyes (&optional points)
  "Start interactive gaze calibration with POINTS targets (default 5)."
  (interactive "P")
  (let ((n (or points 5)))
    (if (not (fboundp 'ewwm-ipc-send))
        (message "ewwm-vr-eye: IPC not available")
      (when (and (fboundp 'ewwm-ipc-connected-p)
                 (ewwm-ipc-connected-p))
        (ewwm-ipc-send
         `(:type :gaze-calibrate-start :points ,n))
        (message "ewwm-vr-eye: calibration started (%d points) — look at target 1" n)))))

(defun ewwm-vr-calibrate-eye-point ()
  "Confirm the current calibration target point."
  (interactive)
  (if (not (fboundp 'ewwm-ipc-send-sync))
      (message "ewwm-vr-eye: IPC not available")
    (condition-case err
        (let ((resp (ewwm-ipc-send-sync '(:type :gaze-calibrate-point))))
          (if (eq (plist-get resp :status) :ok)
              (let ((cal (plist-get resp :calibration)))
                (cond
                 ((eq cal :complete)
                  (setq ewwm-vr-gaze-calibrated-p t)
                  (message "ewwm-vr-eye: calibration complete! RMS error: %.1f degrees"
                           (or (plist-get resp :rms-error) 0)))
                 ((eq cal :point-recorded)
                  (message "ewwm-vr-eye: point recorded, look at target %d"
                           (1+ (or (plist-get resp :next) 0))))))
            (message "ewwm-vr-eye: calibration error: %s"
                     (plist-get resp :reason))))
      (error (message "ewwm-vr-eye: %s" (error-message-string err))))))

(defun ewwm-vr-gaze-health ()
  "Display eye tracking health dashboard."
  (interactive)
  (if (not (fboundp 'ewwm-ipc-send-sync))
      (message "ewwm-vr-eye: IPC not available")
    (condition-case err
        (let ((resp (ewwm-ipc-send-sync '(:type :gaze-health))))
          (if (eq (plist-get resp :status) :ok)
              (let ((h (plist-get resp :health)))
                (message "ewwm-vr-eye: rate=%s/%sHz conf=%.2f lost=%s cal-err=%s"
                         (or (plist-get h :rate) "?")
                         (or (plist-get h :expected-rate) "?")
                         (or (plist-get h :confidence) 0)
                         (if (plist-get h :tracking-lost) "YES" "no")
                         (or (plist-get h :calibration-error) "n/a")))
            (message "ewwm-vr-eye: health query failed")))
      (error (message "ewwm-vr-eye: %s" (error-message-string err))))))

(defun ewwm-vr-set-gaze-source (source)
  "Set gaze SOURCE preference.
SOURCE is a symbol: `auto', `openxr', `pupil-labs', `simulated', or `none'."
  (interactive
   (list (intern (completing-read "Gaze source: "
                                  '("auto" "openxr" "pupil-labs" "simulated" "none")
                                  nil t))))
  (unless (memq source '(auto openxr pupil-labs simulated none))
    (error "Invalid gaze source: %s" source))
  (when (and (fboundp 'ewwm-ipc-connected-p)
             (ewwm-ipc-connected-p))
    (ewwm-ipc-send
     `(:type :gaze-set-source :source ,(symbol-name source))))
  (message "ewwm-vr-eye: gaze source set to %s" source))

(defun ewwm-vr-set-gaze-visualization (mode)
  "Set gaze visualization MODE.
MODE is a symbol: `dot', `crosshair', `spotlight', or `none'."
  (interactive
   (list (intern (completing-read "Visualization: "
                                  '("dot" "crosshair" "spotlight" "none")
                                  nil t))))
  (unless (memq mode '(dot crosshair spotlight none))
    (error "Invalid visualization mode: %s" mode))
  (when (and (fboundp 'ewwm-ipc-connected-p)
             (ewwm-ipc-connected-p))
    (ewwm-ipc-send
     `(:type :gaze-set-visualization :mode ,(symbol-name mode))))
  (message "ewwm-vr-eye: visualization set to %s" mode))

(defun ewwm-vr-set-gaze-smoothing (alpha)
  "Set gaze EMA smoothing ALPHA (0.0-1.0)."
  (interactive "nSmoothing alpha (0.0=max smooth, 1.0=none): ")
  (let ((a (max 0.0 (min 1.0 alpha))))
    (setq ewwm-vr-gaze-smoothing a)
    (when (and (fboundp 'ewwm-ipc-connected-p)
               (ewwm-ipc-connected-p))
      (ewwm-ipc-send
       `(:type :gaze-set-smoothing :alpha ,(round (* a 100)))))
    (message "ewwm-vr-eye: smoothing alpha set to %.2f" a)))

(defun ewwm-vr-gaze-simulate (mode)
  "Set simulated gaze MODE for development.
MODE is nil (off), `mouse', `scripted', `random-walk', or `pattern'."
  (interactive
   (list (let ((choice (completing-read "Simulate mode: "
                                        '("off" "mouse" "scripted" "random-walk" "pattern")
                                        nil t)))
           (if (string= choice "off") nil (intern choice)))))
  (when (and (fboundp 'ewwm-ipc-connected-p)
             (ewwm-ipc-connected-p))
    (ewwm-ipc-send
     `(:type :gaze-simulate :mode ,(if mode (symbol-name mode) "off"))))
  (message "ewwm-vr-eye: simulation %s" (if mode (symbol-name mode) "off")))

(defun ewwm-vr-gaze-status ()
  "Query and display current gaze tracking state."
  (interactive)
  (if (not (fboundp 'ewwm-ipc-send-sync))
      (message "ewwm-vr-eye: IPC not available")
    (condition-case err
        (let ((resp (ewwm-ipc-send-sync '(:type :gaze-status))))
          (if (eq (plist-get resp :status) :ok)
              (let ((g (plist-get resp :gaze)))
                (message "ewwm-vr-eye: source=%s active=%s conf=%.2f vis=%s smooth=%.2f"
                         (or (plist-get g :source) "?")
                         (or (plist-get g :active) "?")
                         (or (plist-get g :confidence) 0)
                         (or (plist-get g :visualization) "?")
                         (or (plist-get g :smoothing) 0)))
            (message "ewwm-vr-eye: status query failed")))
      (error (message "ewwm-vr-eye: %s" (error-message-string err))))))

(defun ewwm-vr-gaze-at-point-p ()
  "Return non-nil if gaze is on the current buffer's surface."
  (when ewwm-vr-gaze-surface
    (let ((buf (current-buffer)))
      (when (ewwm--surface-buffer-p buf)
        (eq (buffer-local-value 'ewwm-surface-id buf)
            ewwm-vr-gaze-surface)))))

;; ── Mode-line ────────────────────────────────────────────────

(defun ewwm-vr-eye-mode-line-string ()
  "Return a mode-line string for gaze state."
  (when (and ewwm-vr-gaze-mode-line ewwm-vr-gaze-tracking-p)
    (if ewwm-vr-gaze-surface
        (format " [Gaze:%d]" ewwm-vr-gaze-surface)
      " [Gaze:---]")))

;; ── Event registration ──────────────────────────────────────

(defun ewwm-vr-eye--register-events ()
  "Register eye tracking event handlers with IPC event dispatch."
  (when (boundp 'ewwm-ipc--event-handlers)
    (let ((handlers
           '((:gaze-data             . ewwm-vr-eye--on-gaze-data)
             (:gaze-target-changed   . ewwm-vr-eye--on-gaze-target-changed)
             (:gaze-fixation         . ewwm-vr-eye--on-gaze-fixation)
             (:gaze-saccade          . ewwm-vr-eye--on-gaze-saccade)
             (:gaze-tracking-lost    . ewwm-vr-eye--on-gaze-tracking-lost)
             (:gaze-calibration-drift . ewwm-vr-eye--on-gaze-calibration-drift))))
      (dolist (handler handlers)
        (unless (assq (car handler) ewwm-ipc--event-handlers)
          (push handler ewwm-ipc--event-handlers))))))

;; ── Minor mode ───────────────────────────────────────────────

(define-minor-mode ewwm-vr-eye-mode
  "Minor mode for eye tracking integration."
  :lighter " VR-Eye"
  :group 'ewwm-vr-eye
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c e c") #'ewwm-vr-calibrate-eyes)
            (define-key map (kbd "C-c e h") #'ewwm-vr-gaze-health)
            (define-key map (kbd "C-c e s") #'ewwm-vr-gaze-status)
            map))

;; ── Init / teardown ─────────────────────────────────────────

(defun ewwm-vr-eye-init ()
  "Initialize eye tracking."
  (ewwm-vr-eye--register-events)
  (setq ewwm-vr-eye--tracking-active t))

(defun ewwm-vr-eye-teardown ()
  "Clean up eye tracking state."
  (setq ewwm-vr-gaze-surface nil
        ewwm-vr-gaze-position nil
        ewwm-vr-gaze-confidence 0.0
        ewwm-vr-gaze-source-active nil
        ewwm-vr-gaze-tracking-p nil
        ewwm-vr-gaze-calibrated-p nil
        ewwm-vr-eye--tracking-active nil
        ewwm-vr-eye--gaze-position nil))

(provide 'ewwm-vr-eye)
;;; ewwm-vr-eye.el ends here
