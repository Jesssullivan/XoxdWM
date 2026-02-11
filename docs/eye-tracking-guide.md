# EXWM-VR: Eye Tracking Guide

**Version 0.1.0** | Eye tracking setup, calibration, and usage

---

## Table of Contents

1. [Supported Trackers](#supported-trackers)
2. [Pupil Capture Setup](#pupil-capture-setup)
3. [Calibration Procedure](#calibration-procedure)
4. [Gaze Focus](#gaze-focus)
5. [Wink Detection](#wink-detection)
6. [Gaze Zones](#gaze-zones)
7. [Gaze Scrolling](#gaze-scrolling)
8. [Link Hints](#link-hints)
9. [Fatigue Monitoring](#fatigue-monitoring)
10. [Privacy Considerations](#privacy-considerations)

---

## Supported Trackers

EWWM supports multiple eye tracking sources through a unified gaze model.

| Tracker | Type | Connection | Refresh Rate | Status |
|---------|------|-----------|-------------|--------|
| OpenXR native | HMD-integrated | XR_EXT_eye_gaze_interaction | HMD-dependent | Primary |
| Pupil Labs Core | External (clip-on) | USB + ZMQ | 200 Hz | Tested |
| Pupil Labs Neon | External (glasses) | USB + ZMQ | 200 Hz | Recommended |
| Simulated (mouse) | Development | Internal | 60 Hz | For testing |
| Simulated (scripted) | Development | Internal | Configurable | For testing |
| Head-gaze fallback | No hardware | VR head tracking | HMD-dependent | Always available |

### Source Priority (auto mode)

When `ewwm-vr-gaze-source` is set to `auto` (default), the system selects
the best available source in this order:

1. OpenXR native eye tracking (if HMD supports it)
2. Pupil Labs ZMQ connection (if Pupil Capture is running)
3. Head-gaze ray from VR interaction (if VR is active)
4. Simulated gaze (if configured)

### Selecting a Source

```elisp
;; Auto-select best available
(ewwm-vr-set-gaze-source 'auto)

;; Force a specific source
(ewwm-vr-set-gaze-source 'openxr)
(ewwm-vr-set-gaze-source 'pupil-labs)
(ewwm-vr-set-gaze-source 'simulated)
(ewwm-vr-set-gaze-source 'none)  ; Disable eye tracking
```

---

## Pupil Capture Setup

### Installation

1. Install Pupil Capture from https://pupil-labs.com/products/core
2. Connect the Pupil Labs eye cameras via USB
3. Launch Pupil Capture

### ZMQ Configuration

Pupil Capture exposes a ZMQ IPC interface on port 50020 (default).

```elisp
;; Set the Pupil Capture IPC port (if non-default)
(setq ewwm-vr-pupil-port 50020)
```

### Verifying the Connection

```elisp
M-x ewwm-vr-gaze-status
;; Should show: source=pupil-labs active=t conf=0.85 vis=dot smooth=0.30
```

### Pupil Capture Settings

For best results with EWWM:

1. Enable "Network API" in Pupil Capture settings
2. Set "Gaze Mapping" to "3D Gaze Mapper" (for VR) or "2D Gaze Mapper" (for monitor)
3. Enable "ZMQ Push" for gaze data
4. Calibrate within Pupil Capture first, then refine in EWWM

---

## Calibration Procedure

### Starting Calibration

```elisp
;; Start 5-point calibration (default)
M-x ewwm-vr-calibrate-eyes

;; Or with a custom number of points
(ewwm-vr-calibrate-eyes 9)  ; 9-point for higher accuracy
```

### Calibration Steps

1. A calibration target appears in VR space
2. Look directly at the target and hold your gaze steady
3. Press `M-x ewwm-vr-calibrate-eye-point` (or use a bound key: `C-c e c`)
4. The system records your gaze direction and the target position
5. Repeat for each calibration point
6. On completion, the system reports RMS error in degrees

### Interpreting Calibration Quality

| RMS Error | Quality | Action |
|-----------|---------|--------|
| < 1.0 deg | Excellent | No action needed |
| 1.0-2.0 deg | Good | Acceptable for most use |
| 2.0-3.0 deg | Fair | Consider recalibrating |
| > 3.0 deg | Poor | Recalibrate; check hardware |

### Calibration Drift Detection

The compositor monitors calibration drift over time. When drift is detected:

```elisp
;; Hook fires when drift exceeds threshold
(add-hook 'ewwm-vr-gaze-calibration-drift-hook
          (lambda (error-deg)
            (when (> error-deg 3.0)
              (message "Recalibration recommended: %.1f deg drift" error-deg))))
```

### IPC Commands

```elisp
;; Start calibration
(:type :gaze-calibrate-start :points 5)

;; Confirm a calibration point
(:type :gaze-calibrate-point :target-x 0 :target-y 0 :target-z -200)

;; Response:
;; (:type :response :status :ok :calibration :point-recorded :next 2)
;; or
;; (:type :response :status :ok :calibration :complete :rms-error 1.2)
```

---

## Gaze Focus

Gaze focus is the core feature: your eyes control which window receives
keyboard input.

### Focus Policies

| Policy | Behavior |
|--------|----------|
| `gaze-only` | Gaze dwell is the sole method to switch focus |
| `gaze-primary` | Gaze dwell switches focus; keyboard/mouse also work |
| `gaze-assist` | Gaze highlights target; requires confirmation to focus |
| `disabled` | Traditional focus only; gaze has no effect |

```elisp
(ewwm-vr-eye-set-focus-policy 'gaze-primary)
```

### Dwell Mechanics

The dwell pipeline:

1. **Saccade filter**: Fast eye movements (> 300 deg/s) are classified as
   saccades and ignored
2. **Reading detector**: Horizontal scan patterns are classified as reading
   and suppress focus changes
3. **Confidence filter**: Gaze samples below `ewwm-vr-gaze-confidence-min`
   (default 0.6) are discarded
4. **Dwell timer**: Stable gaze on a surface starts a timer
   (default 200ms)
5. **Jitter check**: If gaze moves > `ewwm-vr-eye-max-jitter-px` (50px)
   during dwell, the timer resets
6. **Cooldown**: After a focus switch, further switches are blocked for
   `ewwm-vr-eye-cooldown-ms` (500ms)
7. **Hysteresis**: If ping-pong focus switching is detected, dwell
   threshold is automatically increased

### Tuning Dwell

```elisp
;; Faster switching (experienced users)
(setq ewwm-vr-eye-dwell-ms 150)
(setq ewwm-vr-eye-cooldown-ms 300)

;; Slower switching (reduce false positives)
(setq ewwm-vr-eye-dwell-ms 400)
(setq ewwm-vr-eye-cooldown-ms 800)

;; Disable saccade filtering (not recommended)
(setq ewwm-vr-eye-saccade-threshold 9999)
```

### Dwell Progress Indicator

When `ewwm-vr-eye-show-dwell-progress` is `t`, the mode-line shows a
progress bar during dwell:

```
[Eye:===__ 3]    ; 60% dwell on surface 3
[Eye:===== 3]    ; Dwell complete, focus switches
```

### Focus Exceptions

Override the focus policy for specific surfaces:

```elisp
(setq ewwm-vr-eye-focus-exceptions
      '((;; Never gaze-focus video players
         (lambda (buf)
           (string-match-p "mpv" (buffer-local-value 'ewwm-app-id buf)))
         . disabled)
        ;; Require confirmation for terminals
        (lambda (buf)
           (string-match-p "foot\\|alacritty" (buffer-local-value 'ewwm-app-id buf)))
         . gaze-assist)))
```

### Focus History

Navigate back through gaze focus history:

```elisp
M-x ewwm-vr-eye-focus-back  ; Return to previously focused surface
```

The focus ring stores the last 10 gaze-focused surfaces.

### Analytics

```elisp
M-x ewwm-vr-eye-analytics
```

Displays:
- Total focus switches
- False positives
- Saccade suppressions
- Cooldown blocks
- Reading suppressions
- Switches per minute

---

## Wink Detection

Wink detection enables hands-free binary input: left wink and right wink
each trigger a configurable action.

### Calibration

```elisp
;; Start wink calibration wizard
(:type :wink-calibrate-start :eye :left)
(:type :wink-calibrate-start :eye :right)
```

The wizard collects blink/wink samples and computes per-eye thresholds.

### Configuration

```elisp
;; Wink IPC commands
(:type :wink-status)
(:type :wink-config)
(:type :wink-set-confidence :confidence 0.8)
```

### Usage via ewwm-vr-wink.el

The `ewwm-vr-wink.el` module provides:
- 9 `defcustom` variables for wink detection tuning
- Wink dispatch to configurable Emacs commands
- Calibration wizard UI
- Minor mode with mode-line indicator

---

## Gaze Zones

The visual field is divided into 9 zones. Gazing at a zone for the
configured dwell time triggers a zone action.

### Zone Layout

```
+--------+--------+--------+
|  TL    |   TC   |   TR   |
| top-   |  top-  |  top-  |
| left   | center | right  |
+--------+--------+--------+
|  ML    |   MC   |   MR   |
| mid-   |  mid-  |  mid-  |
| left   | center | right  |
+--------+--------+--------+
|  BL    |   BC   |   BR   |
| bot-   |  bot-  |  bot-  |
| left   | center | right  |
+--------+--------+--------+
```

### Layout Presets

`ewwm-vr-gaze-zone.el` provides 4 built-in presets:

1. **Default**: 9-zone grid with equal sizes
2. **Wide center**: Larger center zone for reading
3. **Programmer**: Larger left zone for code, thin right for docs
4. **Presentation**: Large center, edge zones for navigation

### Zone-to-Modifier Injection

Map gaze zones to Emacs modifiers for hands-free modifier keys:

```elisp
;; Looking at top-left zone acts as Meta modifier
;; Looking at top-right zone acts as Control modifier
```

### IPC Commands

```elisp
(:type :gaze-zone-status)
(:type :gaze-zone-config)
(:type :gaze-zone-set-dwell :zone :top-left :dwell-ms 500)
```

---

## Gaze Scrolling

Edge-zone gaze scrolling in Qutebrowser: look near the top or bottom
edge of the viewport to scroll.

### How It Works

1. The compositor monitors gaze position within the Qutebrowser surface
2. When gaze enters the top or bottom edge zone, scrolling begins
3. Scroll speed increases the closer to the edge
4. Scrolling stops when gaze returns to the center

### Configuration

```elisp
;; IPC commands
(:type :gaze-scroll-status)
(:type :gaze-scroll-config :top-zone 0.1 :bottom-zone 0.1 :max-speed 500)
(:type :gaze-scroll-set-speed :speed 300)
```

The `gaze_scroll.rs` module (396 lines) handles:
- Edge zone detection
- Speed ramping based on distance from edge
- Smooth acceleration/deceleration
- Per-surface scroll state

---

## Link Hints

Dwell-to-confirm link hint overlay for Qutebrowser.

### How It Works

1. Activate link hints: `(:type :link-hints-load :url "...")`
2. The compositor renders hint labels over clickable elements
3. Gaze at a hint label and dwell to confirm
4. The link is activated

### Usage

```elisp
;; Load hints for current page
(:type :link-hints-load :url "current")

;; Confirm the gazed hint
(:type :link-hints-confirm)

;; Clear hint overlay
(:type :link-hints-clear)

;; Check status
(:type :link-hints-status)
```

The `link_hints.rs` module (479 lines) provides:
- Link element extraction via JavaScript userscript
- Hint label rendering with distinct visual labels
- Dwell-to-confirm with configurable threshold
- Automatic cleanup on navigation

---

## Fatigue Monitoring

Eye fatigue monitoring tracks physiological signs of fatigue and provides
multi-level alerts.

### Fatigue Levels

| Level | Trigger | Action |
|-------|---------|--------|
| Normal | Baseline metrics | None |
| Mild | Blink rate increase > 20% | Mode-line warning |
| Moderate | Saccade amplitude decrease + blink rate | Suggestion to rest |
| Severe | Prolonged moderate + tracking quality drop | Auto-save + alert |

### Configuration via ewwm-vr-fatigue.el

```elisp
;; 6 defcustom variables for fatigue detection
;; CSV logging of fatigue metrics
;; Multi-level alert system
```

### IPC Commands

```elisp
(:type :fatigue-status)    ; Current fatigue level
(:type :fatigue-config)    ; Get configuration
(:type :fatigue-metrics)   ; Detailed metrics
(:type :fatigue-reset)     ; Reset fatigue tracking
```

### CSV Logging

When enabled, fatigue metrics are logged to CSV for post-session analysis:

```
timestamp,blink_rate,saccade_amplitude,tracking_quality,fatigue_level
1705312345.123,18.5,12.3,0.92,normal
1705312350.456,22.1,10.1,0.88,mild
```

---

## Privacy Considerations

EWWM-VR eye tracking is designed with privacy as a core principle:

### Local-Only Processing

- All gaze data is processed locally in the compositor (Rust)
- No gaze data is transmitted over the network
- No cloud services are involved
- Pupil Labs ZMQ connection is localhost-only

### Data Retention

- Gaze data is ephemeral: processed in real-time, not stored
- Fatigue CSV logs are stored locally and auto-cleaned per retention policy
- Calibration data is stored in compositor memory only (lost on restart)
- No eye images are captured or stored by EWWM (Pupil Capture handles its own)

### Secure Input Mode

During credential entry (`ewwm-vr-secure-input-mode`):
- Eye tracking data is paused
- Gaze visualization is hidden
- Wink detection is disabled
- BCI acquisition is paused
- No biometric data can leak into logs or traces

### Gaze-Away Detection

The `ewwm-secrets-gaze-away.el` module monitors whether the user is looking
at the screen during credential entry. If the user looks away, credential
display is paused to prevent shoulder-surfing.

### Recommendations

1. Use full-disk encryption to protect fatigue CSV logs
2. Regularly clean old session data: `(setq ewwm-bci-data-retention-days 30)`
3. Disable IPC trace mode in production: `(ewwm-ipc-trace-mode -1)`
4. Review Pupil Capture's own privacy settings separately

### Gaze Smoothing

The EMA smoothing filter (`ewwm-vr-gaze-smoothing`) intentionally blurs
precise gaze location, providing a form of differential privacy for the
gaze signal. Higher smoothing (lower alpha) provides more privacy but
reduced responsiveness.
