# EXWM-VR: Brain-Computer Interface Guide

**Version 0.1.0** | BCI setup, calibration, and usage

---

## Table of Contents

1. [Overview](#overview)
2. [Hardware Requirements](#hardware-requirements)
3. [Electrode Placement](#electrode-placement)
4. [Impedance Checking](#impedance-checking)
5. [BrainFlow Daemon Setup](#brainflow-daemon-setup)
6. [Attention Monitoring](#attention-monitoring)
7. [SSVEP Workspace Selection](#ssvep-workspace-selection)
8. [P300 Confirmation](#p300-confirmation)
9. [Motor Imagery](#motor-imagery)
10. [Neurofeedback Training](#neurofeedback-training)
11. [Multi-Modal Fusion](#multi-modal-fusion)
12. [EEG Fatigue Monitoring](#eeg-fatigue-monitoring)
13. [Signal Troubleshooting](#signal-troubleshooting)
14. [Privacy and Data Retention](#privacy-and-data-retention)

---

## Overview

EWWM-VR integrates EEG-based brain-computer interface (BCI) input alongside
gaze tracking and hand gestures. The BCI pipeline uses BrainFlow for hardware
abstraction, with a daemon process bridging the C++ BrainFlow library to the
Rust compositor via shared memory.

The BCI system is built from seven Elisp modules:

| Module | Purpose |
|--------|---------|
| `ewwm-bci-core.el` | Daemon lifecycle, board connection, signal quality |
| `ewwm-bci-attention.el` | Engagement index from EEG band power ratios |
| `ewwm-bci-ssvep.el` | Frequency-tagged workspace selection |
| `ewwm-bci-p300.el` | Oddball paradigm confirmation dialog |
| `ewwm-bci-mi.el` | Motor imagery classification (left/right/foot) |
| `ewwm-bci-nfb.el` | Neurofeedback training with real-time display |
| `ewwm-bci-multimodal.el` | Cross-modal fusion (gaze + EEG + gesture) |

---

## Hardware Requirements

### Supported EEG Devices

| Device | Board ID | Channels | Sample Rate | Notes |
|--------|----------|----------|-------------|-------|
| OpenBCI Cyton | 1 | 8 | 250 Hz | Primary target |
| OpenBCI CytonDaisy | 22 | 16 | 125 Hz | Full 10-20 coverage |
| OpenBCI Ganglion | 2 | 4 | 200 Hz | Budget option |
| Muse 2 | 45 | 4 | 256 Hz | Consumer-grade |
| BrainBit | 38 | 4 | 250 Hz | Wireless |
| Synthetic | 0 | 8 | 250 Hz | Testing only |

### Recommended Setup

For full BCI functionality (all paradigms):

- **OpenBCI Cyton** (8 channels, 250 Hz) or **CytonDaisy** (16 channels)
- **Ultracortex Mark IV** headset (dry electrode, compatible with VR HMD)
- USB Bluetooth dongle (OpenBCI RFduino)
- Or serial connection via `/dev/ttyUSB0`

For attention monitoring only:

- Any 4-channel device covering frontal and parietal sites
- Muse 2 is sufficient for basic attention tracking

### Configuring the Board

```elisp
;; OpenBCI Cyton (8-channel)
(setq ewwm-bci-board-id 1)
(setq ewwm-bci-serial-port "/dev/ttyUSB0")
(setq ewwm-bci-sample-rate 250)

;; OpenBCI CytonDaisy (16-channel)
(setq ewwm-bci-board-id 22)
(setq ewwm-bci-serial-port "/dev/ttyUSB0")
(setq ewwm-bci-sample-rate 125)

;; Synthetic board (for testing without hardware)
(setq ewwm-bci-board-id 0)

;; Power line filter (60 Hz for North America, 50 Hz for Europe/Asia)
(setq ewwm-bci-notch-frequency 60)

;; Enable artifact rejection
(setq ewwm-bci-artifact-rejection t)
```

---

## Electrode Placement

Electrode sites follow the International 10-20 system. The required
channels depend on which BCI paradigms you intend to use.

### 10-20 System Reference (8-Channel Cyton)

```
          Nasion
            |
     Fp1----Fpz---Fp2
    /   \         /   \
   F7    F3--Fz--F4    F8
   |    / |       | \    |
   T3  C3  Cz   C4  T4
   |    \ |       | /    |
   P3    P3--Pz--P4    T6
    \   /         \   /
     O1----Oz----O2
            |
          Inion
```

### Recommended Channel Assignments

**8-Channel (Cyton):**

| Channel | Site | Purpose |
|---------|------|---------|
| 1 | Fp1 | Frontal left (attention) |
| 2 | Fp2 | Frontal right (attention) |
| 3 | C3 | Central left (motor imagery left hand) |
| 4 | C4 | Central right (motor imagery right hand) |
| 5 | P3 | Parietal left (P300, SSVEP) |
| 6 | P4 | Parietal right (P300, SSVEP) |
| 7 | O1 | Occipital left (SSVEP, alpha) |
| 8 | O2 | Occipital right (SSVEP, alpha) |
| REF | A1 | Left earlobe reference |
| GND | A2 | Right earlobe ground |

**16-Channel (CytonDaisy) adds:**

| Channel | Site | Purpose |
|---------|------|---------|
| 9 | F3 | Frontal left (attention detail) |
| 10 | F4 | Frontal right (attention detail) |
| 11 | T3 | Temporal left (artifact detection) |
| 12 | T4 | Temporal right (artifact detection) |
| 13 | Cz | Central midline (motor imagery foot) |
| 14 | Pz | Parietal midline (P300) |
| 15 | Fz | Frontal midline (attention) |
| 16 | Oz | Occipital midline (SSVEP) |

### Minimum Channels per Paradigm

| Paradigm | Minimum Channels | Required Sites |
|----------|------------------|----------------|
| Attention | 2 | Fp1, Fp2 |
| SSVEP | 2 | O1, O2 |
| P300 | 4 | P3, P4, Pz, Cz |
| Motor imagery | 3 | C3, C4, Cz |
| Neurofeedback | 2 | Depends on protocol |

---

## Impedance Checking

Good electrode contact is critical for signal quality. Target impedance
is below 10 kOhm for wet electrodes, below 50 kOhm for dry electrodes.

### Checking Signal Quality

```elisp
;; Start BCI and check per-channel quality
M-x ewwm-bci-start
M-x ewwm-bci-signal-quality

;; Output:
;; ewwm-bci signal quality:
;;   ch1: good
;;   ch2: good
;;   ch3: fair
;;   ch4: poor       <-- needs attention
;;   ch5: good
;;   ...
```

### Quality Levels

| Level | Impedance | Action |
|-------|-----------|--------|
| `good` | < 10 kOhm | No action needed |
| `fair` | 10-30 kOhm | Acceptable for most use |
| `poor` | 30-100 kOhm | Reposition electrode, add gel |
| `disconnected` | > 100 kOhm | Check cable and electrode |

### Hooks for Quality Monitoring

```elisp
(add-hook 'ewwm-bci-quality-change-hook
          (lambda (channel-quality)
            ;; channel-quality is a plist: (:1 good :2 fair :3 poor ...)
            (message "Signal quality updated: %S" channel-quality)))
```

---

## BrainFlow Daemon Setup

The BrainFlow daemon is a separate process that bridges the BrainFlow C++
library to the compositor. It handles board communication, signal
preprocessing, and data streaming.

### NixOS Module

```nix
# In your NixOS configuration
services.exwm-vr = {
  enable = true;
  bci = {
    enable = true;
    boardId = 1;         # OpenBCI Cyton
    serialPort = "/dev/ttyUSB0";
    sampleRate = 250;
  };
};
```

### Manual Setup

```bash
# Install the daemon (built with the compositor)
nix build .#compositor

# Run manually
exwm-vr-brainflow-daemon \
  --board-id 1 \
  --serial-port /dev/ttyUSB0 \
  --sample-rate 250 \
  --notch 60 \
  --artifact-rejection
```

### Daemon Configuration in Elisp

```elisp
;; Path to daemon binary
(setq ewwm-bci-daemon-path "exwm-vr-brainflow-daemon")

;; Extra arguments
(setq ewwm-bci-daemon-args '("--log-level" "debug"))

;; Auto-reconnect on connection loss
(setq ewwm-bci-auto-reconnect t)
(setq ewwm-bci-reconnect-delay 5)  ; seconds
```

### Starting and Stopping

```elisp
;; Start BCI (launches daemon + sends start to compositor)
M-x ewwm-bci-start

;; Stop BCI (stops daemon + streaming)
M-x ewwm-bci-stop

;; Restart BCI
M-x ewwm-bci-restart

;; Check status
M-x ewwm-bci-status
;; ewwm-bci: state=connected stream=on frames=12345 dur=42m err=none
```

### IPC Commands

```elisp
;; Compositor-level BCI commands
(:type :bci-status)
(:type :bci-start)
(:type :bci-stop)
(:type :bci-restart)
(:type :bci-signal-quality)
(:type :bci-config :board-id 1 :sample-rate 250)
(:type :bci-inject-synthetic :pattern "alpha")  ; testing
(:type :bci-data-list)
(:type :bci-data-delete :session-id "20260211-143000")
```

---

## Attention Monitoring

The attention system tracks cognitive engagement using EEG band power ratios.
The engagement index is computed as `beta / (alpha + theta)`, normalized
to [0.0, 1.0].

### Attention States

| State | Score Range | WM Behavior |
|-------|------------|-------------|
| `neutral` | 0.2 - 0.6 | Normal operation |
| `focused` | 0.6 - 0.8 | Focus indicator in mode-line |
| `deep-focus` | > 0.8 | Auto-DND: notifications suppressed |
| `drowsy` | < 0.2 | Break alert, auto-save buffers |

### Configuration

```elisp
;; Enable attention tracking (on by default)
(setq ewwm-bci-attention-enabled t)

;; Score thresholds
(setq ewwm-bci-attention-threshold 0.6)       ; focused
(setq ewwm-bci-attention-dnd-threshold 0.8)    ; deep focus / auto-DND
(setq ewwm-bci-attention-drowsy-threshold 0.2) ; drowsy

;; Break suggestion after N minutes of drowsiness
(setq ewwm-bci-attention-break-minutes 5)

;; Auto-save buffers when transitioning to drowsy
(setq ewwm-bci-attention-auto-save t)

;; Debounce interval (prevent rapid state oscillation)
(setq ewwm-bci-attention-update-interval 2.0)
```

### Calibration

Attention calibration establishes your personal baseline. Sit relaxed
with eyes open for 30 seconds:

```elisp
M-x ewwm-bci-attention-calibrate
;; ewwm-bci: calibrating -- relax with eyes open for 30s
```

### Mode-Line Indicator

When active, the mode-line shows:

```
[Att:DEEP 92%]    ; Deep focus at 92% engagement
[Att:FOCUS 71%]   ; Focused state
[Att:DROWSY 15%]  ; Drowsiness detected
```

### Hooks

```elisp
(add-hook 'ewwm-bci-attention-change-hook
          (lambda (old-state new-state score)
            (message "Attention: %s -> %s (%.0f%%)"
                     old-state new-state (* 100 score))))
```

### IPC Commands

```elisp
(:type :bci-attention-status)
(:type :bci-attention-config :threshold 0.6 :dnd-threshold 0.8)
(:type :bci-attention-calibrate-start)
(:type :bci-attention-calibrate-finish)
```

---

## SSVEP Workspace Selection

Steady-State Visually Evoked Potentials (SSVEP) allow workspace switching
by attending to flickering visual beacons. Each workspace displays a beacon
at a unique frequency; the system uses FFT to detect which frequency the
user is attending.

### Photosensitivity Warning

SSVEP uses flickering visual stimuli. Users with photosensitive epilepsy
or a family history of seizures should NOT enable this feature. SSVEP is
disabled by default and must be explicitly enabled.

### Configuration

```elisp
;; Enable SSVEP (disabled by default for safety)
(setq ewwm-bci-ssvep-enabled t)

;; Workspace-to-frequency mapping
;; Frequencies should be > 6 Hz and separated by >= 2 Hz
(setq ewwm-bci-ssvep-frequencies
      '((1 . 12.0)   ; workspace 1 at 12 Hz
        (2 . 15.0)   ; workspace 2 at 15 Hz
        (3 . 20.0)   ; workspace 3 at 20 Hz
        (4 . 24.0))) ; workspace 4 at 24 Hz

;; FFT analysis window (longer = more accurate, higher latency)
(setq ewwm-bci-ssvep-window-seconds 3.0)

;; Classification confidence threshold
(setq ewwm-bci-ssvep-min-confidence 0.7)

;; Cooldown between switches
(setq ewwm-bci-ssvep-cooldown-ms 2000)
```

### Usage

```elisp
;; Activate SSVEP mode (starts beacons and classification)
M-x ewwm-bci-ssvep-mode

;; Check status
M-x ewwm-bci-ssvep-status
;; ewwm-bci-ssvep: active=yes cls=5 last=ws2(85%)

;; Send configuration to compositor
M-x ewwm-bci-ssvep-configure
```

### Mode-Line

```
[SSVEP:ws2]   ; Last detected workspace
[SSVEP:--]    ; Active but no detection yet
```

### IPC Commands

```elisp
(:type :bci-ssvep-status)
(:type :bci-ssvep-config :frequencies ((1 . 12.0) (2 . 15.0)))
(:type :bci-ssvep-start)
(:type :bci-ssvep-stop)
```

---

## P300 Confirmation

The P300 event-related potential is a brain response that occurs ~300ms
after perceiving a rare (target) stimulus among frequent (non-target)
stimuli. EWWM uses the oddball paradigm for BCI-driven confirmation
dialogs and menu selection.

### How It Works

1. Multiple visual targets are displayed (e.g., workspace icons, menu items)
2. Targets flash in random order (oddball paradigm)
3. The user mentally counts when the desired target flashes
4. The P300 response is detected in the EEG over parietal electrodes
5. The target with the strongest P300 response is selected

### Configuration

```elisp
;; Enable P300 detection
(setq ewwm-bci-p300-enabled t)

;; Stimulus repetitions (more = higher accuracy, more time)
(setq ewwm-bci-p300-repetitions 5)

;; Confidence threshold
(setq ewwm-bci-p300-min-confidence 0.7)

;; Stimulus timing
(setq ewwm-bci-p300-soa-ms 200)            ; stimulus onset asynchrony
(setq ewwm-bci-p300-flash-duration-ms 100)  ; flash duration

;; Trial timeout
(setq ewwm-bci-p300-timeout-ms 30000)  ; 30 seconds
```

### Async Callback API

The P300 system uses an asynchronous callback API:

```elisp
;; Start a P300 confirmation
(ewwm-bci-p300-confirm
 "Select workspace"                 ; prompt
 '("ws1" "ws2" "ws3" "ws4")       ; targets
 (lambda (target confidence)        ; callback
   (if target
       (message "Selected %s (%.0f%%)" target (* 100 confidence))
     (message "No selection (timeout/cancel)"))))
```

The callback receives `(nil nil)` on timeout or cancellation.

### Interactive Commands

```elisp
;; Start a demo trial
M-x ewwm-bci-p300-start

;; Cancel active trial
M-x ewwm-bci-p300-stop

;; Check status
M-x ewwm-bci-p300-status
;; ewwm-bci-p300: active=yes trials=3 last=ws2(82%)
```

### IPC Commands

```elisp
(:type :bci-p300-status)
(:type :bci-p300-config :repetitions 5 :soa-ms 200)
(:type :bci-p300-start :prompt "Select" :targets ("a" "b" "c"))
(:type :bci-p300-stop)
```

---

## Motor Imagery

Motor imagery (MI) classifies imagined hand and foot movements from EEG
mu/beta desynchronization patterns over the sensorimotor cortex. Each
class maps to a configurable Emacs command.

### Classes

| Class | Electrode Sites | Default Action |
|-------|----------------|----------------|
| `left` | C3 (contralateral) | `previous-buffer` |
| `right` | C4 (contralateral) | `next-buffer` |
| `foot` | Cz (midline) | `ewwm-workspace-switch` |

### Configuration

```elisp
;; Enable motor imagery (requires calibration first)
(setq ewwm-bci-mi-enabled t)

;; Action bindings
(setq ewwm-bci-mi-left-action #'previous-buffer)
(setq ewwm-bci-mi-right-action #'next-buffer)
(setq ewwm-bci-mi-foot-action
      (lambda ()
        (ewwm-workspace-switch nil)))  ; cycle workspaces

;; Classification threshold
(setq ewwm-bci-mi-min-confidence 0.6)

;; Cooldown between dispatches
(setq ewwm-bci-mi-cooldown-ms 1500)

;; Show feedback on classification
(setq ewwm-bci-mi-feedback t)
```

### Calibration

MI requires per-user calibration. The calibration wizard presents visual
cues and the user performs imagined movements:

```elisp
M-x ewwm-bci-mi-calibrate
;; ewwm-bci-mi: calibration started -- follow cues
;; ... visual cues for left hand, right hand, foot ...
;; ewwm-bci-mi: calibration 25%
;; ewwm-bci-mi: calibration 50%
;; ewwm-bci-mi: calibration 75%
;; ewwm-bci-mi: calibration complete
```

Tips for effective MI calibration:

1. Vividly imagine the physical movement (not just think about it)
2. For left hand: imagine squeezing a ball with your left hand
3. For right hand: imagine squeezing a ball with your right hand
4. For foot: imagine pressing a pedal with your right foot
5. Relax during rest periods (do not imagine any movement)

### Mode-Line

```
[MI:L]   ; Last classification: left hand
[MI:R]   ; Last classification: right hand
[MI:F]   ; Last classification: foot
```

### IPC Commands

```elisp
(:type :bci-mi-status)
(:type :bci-mi-config :min-confidence 0.6 :cooldown-ms 1500)
(:type :bci-mi-calibrate-start)
(:type :bci-mi-calibrate-finish)
```

---

## Neurofeedback Training

Neurofeedback (NFB) provides real-time EEG band power display in a
dedicated Emacs buffer, with multiple training protocols and CSV
data export.

### Training Protocols

| Protocol | Target Band | Inhibit Band | Goal |
|----------|-------------|--------------|------|
| `alpha-trainer` | Alpha (8-12 Hz) | Theta (4-8 Hz) | Relaxation, creativity |
| `beta-trainer` | Beta (12-30 Hz) | Theta (4-8 Hz) | Focus, alertness |
| `mi-trainer` | Mu (8-13 Hz, motor) | Beta (12-30 Hz) | Motor imagery skill |

### Starting a Session

```elisp
;; Start with protocol selection
M-x ewwm-bci-nfb-start
;; Prompts: NFB protocol: alpha-trainer

;; Or programmatically
(ewwm-bci-nfb-start 'alpha-trainer)
```

### Display Buffer

The `*ewwm-bci-neurofeedback*` buffer shows real-time data:

```
EWWM Neurofeedback -- alpha-trainer
==================================================

Session: 20260211-143000  Duration: 5.2m  Frames: 312

Score: 72.5%
[##############################----------]

Band Powers:
  delta     12.3 uV^2  [##--------]
  theta      8.1 uV^2  [#---------]
  alpha     45.7 uV^2  [#####-----]
  beta      15.2 uV^2  [##--------]
  gamma      3.4 uV^2  [----------]

Channels:
  ch1: 23.4 uV
  ch2: 21.8 uV
  ...
```

### Data Export

Session data is exported to CSV automatically on stop (when
`ewwm-bci-nfb-auto-export` is non-nil), or manually:

```elisp
;; Manual export
M-x ewwm-bci-nfb-export-session

;; Export directory
(setq ewwm-bci-nfb-session-dir
      "~/.local/share/exwm-vr/neurofeedback/")
```

CSV format:

```
timestamp,delta,theta,alpha,beta,gamma,score
1707660600.123,12.3,8.1,45.7,15.2,3.4,0.7250
1707660601.456,11.8,7.9,47.2,14.8,3.6,0.7510
```

### Configuration

```elisp
;; Log interval in seconds
(setq ewwm-bci-nfb-log-interval 1.0)

;; Number of channels to display
(setq ewwm-bci-nfb-display-channels 8)

;; Bar chart width
(setq ewwm-bci-nfb-bar-width 40)

;; Auto-export on session stop
(setq ewwm-bci-nfb-auto-export t)
```

### Hooks

```elisp
(add-hook 'ewwm-bci-nfb-start-hook
          (lambda (protocol session-id)
            (message "NFB started: %s (%s)" protocol session-id)))

(add-hook 'ewwm-bci-nfb-stop-hook
          (lambda (session-id frame-count)
            (message "NFB stopped: %s (%d frames)"
                     session-id frame-count)))
```

---

## Multi-Modal Fusion

Multi-modal fusion combines EEG, gaze, and hand gesture inputs for
higher accuracy and richer interaction. Three fusion modes are available.

### 1. Adaptive Dwell

Automatically adjusts gaze dwell time based on attention level:

```elisp
(setq ewwm-bci-multimodal-enabled t)
(setq ewwm-bci-multimodal-adaptive-dwell t)

;; Dwell times per attention level
(setq ewwm-bci-multimodal-dwell-focused-ms 150)   ; deep focus: fast
(setq ewwm-bci-multimodal-dwell-default-ms 250)   ; neutral: normal
(setq ewwm-bci-multimodal-dwell-relaxed-ms 400)   ; drowsy: slow

;; Attention thresholds for dwell adjustment
(setq ewwm-bci-multimodal-focused-threshold 0.7)
(setq ewwm-bci-multimodal-relaxed-threshold 0.4)
```

When deeply focused, dwell time shortens (faster interaction). When
drowsy, dwell time lengthens (fewer false positives).

### 2. Two-Factor: Gaze + Motor Imagery

Gaze selects a target; motor imagery confirms it. This eliminates
accidental activations from natural eye movements:

```elisp
(setq ewwm-bci-multimodal-two-factor t)

;; Timeout for MI confirmation after gaze select
(setq ewwm-bci-multimodal-two-factor-timeout-ms 3000)
```

Workflow:

1. Look at target (gaze selects)
2. "Think to confirm" prompt appears
3. Perform any MI class (left/right/foot) to confirm
4. If no MI within timeout, selection is canceled

### 3. Three-Factor: Gaze + P300 + Pinch

High-assurance mode for security-critical actions (credential entry,
destructive commands). Requires three independent confirmations:

```elisp
(setq ewwm-bci-multimodal-three-factor-security t)
```

API for three-factor verification:

```elisp
(ewwm-bci-multimodal-three-factor-verify
 "delete-workspace"
 (lambda (action success)
   (if success
       (message "Verified: %s" action)
     (message "Verification failed"))))
```

The verification requires:

1. **Gaze fixation** on the confirmation target
2. **P300 confirmation** via oddball paradigm
3. **Pinch gesture** as physical confirmation

All three must succeed for the action to proceed.

### Hooks

```elisp
(add-hook 'ewwm-bci-multimodal-fusion-hook
          (lambda (action modalities)
            (message "Fused action via %s"
                     (mapconcat #'symbol-name modalities "+"))))
```

---

## EEG Fatigue Monitoring

EEG-based fatigue monitoring complements the eye-tracking fatigue system
(see Eye Tracking Guide) with physiological measures from the EEG signal.

### Fatigue Indicators

| Indicator | Source | Interpretation |
|-----------|--------|----------------|
| Theta/alpha ratio increase | Frontal (Fp1, Fp2) | Cognitive fatigue |
| Alpha power increase | Occipital (O1, O2) | Drowsiness |
| Beta power decrease | Frontal (F3, F4) | Reduced alertness |
| Blink rate increase | Fp1, Fp2 (artifact channel) | Eye strain |

### Configuration

```elisp
;; IPC commands for EEG fatigue
(:type :bci-fatigue-eeg-status)
(:type :bci-fatigue-eeg-config :threshold 0.7 :window-sec 300)
```

EEG fatigue integrates with the main fatigue system in `ewwm-vr-fatigue.el`,
contributing to the composite fatigue index alongside eye-tracking metrics.

---

## Signal Troubleshooting

### Common Issues

**High noise floor (50/60 Hz):**

- Check that `ewwm-bci-notch-frequency` matches your local power grid
- Move away from monitors and power supplies
- Use shielded cables
- Ensure reference electrode has good contact

**Drifting baseline:**

- Allow 2-3 minutes for electrodes to stabilize after application
- Apply electrode gel to dry electrodes
- Check for loose electrode connections
- Reduce cable movement (use clips to secure cables)

**Low classification accuracy:**

- Recalibrate with fresh electrode gel
- Verify channel-to-site mapping matches your montage
- Increase `ewwm-bci-p300-repetitions` for P300
- Increase `ewwm-bci-ssvep-window-seconds` for SSVEP
- Run `M-x ewwm-bci-signal-quality` and fix `poor`/`disconnected` channels

**Frequent artifact rejection:**

- Minimize jaw clenching and facial muscle tension
- Keep eyes relatively still during MI classification
- Verify temporal channels (T3, T4) are included for artifact detection
- Reduce `ewwm-bci-artifact-rejection` sensitivity if too aggressive

### Hardware Check

```elisp
;; Run comprehensive hardware diagnostics
M-x ewwm-bci-hardware-check
;; Sends :bci-hardware-check to compositor, reports board connectivity,
;; firmware version, battery level, and per-channel impedance
```

### Synthetic Testing

Test BCI pipelines without hardware:

```elisp
;; Use synthetic board
(setq ewwm-bci-board-id 0)
M-x ewwm-bci-start

;; Inject specific test patterns
(:type :bci-inject-synthetic :pattern "alpha")
(:type :bci-inject-synthetic :pattern "p300-target")
(:type :bci-inject-synthetic :pattern "mi-left")
```

---

## Privacy and Data Retention

### Local-Only Processing

- All EEG data is processed locally in the compositor and daemon
- No neural data is transmitted over the network
- No cloud services are involved
- Raw EEG signals are never persisted (processed in real-time only)

### Data Storage

| Data Type | Location | Retention |
|-----------|----------|-----------|
| Neurofeedback CSV | `~/.local/share/exwm-vr/neurofeedback/` | Per policy |
| Calibration data | Compositor memory | Lost on restart |
| Session metadata | `ewwm-bci-data-list` | Per policy |
| Raw EEG | Not stored | Ephemeral |

### Retention Policy

```elisp
;; Auto-cleanup after N days (0 = never clean)
(setq ewwm-bci-data-retention-days 90)

;; Manually delete a session
(:type :bci-data-delete :session-id "20260211-143000")
```

### Secure Input Mode

During credential entry, all BCI acquisition is paused:

```elisp
;; In ewwm-vr-secure-input.el
;; When secure input mode activates:
;; - BCI streaming is paused
;; - Attention tracking is paused
;; - No EEG data can leak into logs
;; - Neurofeedback display is frozen
```

### Recommendations

1. Use full-disk encryption to protect neurofeedback CSV files
2. Set a reasonable retention policy: `(setq ewwm-bci-data-retention-days 30)`
3. Disable IPC trace mode in production: `(ewwm-ipc-trace-mode -1)`
4. Review BrainFlow daemon logs for unintended data exposure
5. Keep the daemon socket permissions restricted (same user only)
