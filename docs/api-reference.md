# EXWM-VR: API Reference

**Version 0.1.0** | Public functions, variables, hooks, and IPC messages

---

## Table of Contents

1. [ewwm-core.el](#ewwm-coreel)
2. [ewwm-ipc.el](#ewwm-ipcel)
3. [ewwm-vr.el](#ewwm-vrel)
4. [ewwm-vr-eye.el](#ewwm-vr-eyeel)
5. [ewwm-vr-wink.el](#ewwm-vr-winkel)
6. [ewwm-vr-gaze-zone.el](#ewwm-vr-gaze-zoneel)
7. [ewwm-vr-fatigue.el](#ewwm-vr-fatigueel)
8. [ewwm-vr-hand.el](#ewwm-vr-handel)
9. [ewwm-vr-gesture.el](#ewwm-vr-gestureel)
10. [ewwm-vr-keyboard.el](#ewwm-vr-keyboardel)
11. [ewwm-bci-core.el](#ewwm-bci-coreel)
12. [ewwm-bci-attention.el](#ewwm-bci-attentionel)
13. [ewwm-bci-ssvep.el](#ewwm-bci-ssvepel)
14. [ewwm-bci-p300.el](#ewwm-bci-p300el)
15. [ewwm-bci-mi.el](#ewwm-bci-miel)
16. [ewwm-bci-nfb.el](#ewwm-bci-nfbel)
17. [ewwm-bci-multimodal.el](#ewwm-bci-multimodalel)
18. [IPC Message Types](#ipc-message-types)

---

## ewwm-core.el

Core data structures and surface-as-buffer model.

### Defcustom Variables

*None (core uses defvar for runtime state).*

### Global Variables

| Variable | Type | Description |
|----------|------|-------------|
| `ewwm--surface-buffer-alist` | alist | Central `(surface-id . buffer)` mapping |
| `ewwm--compositor-process` | process | Compositor subprocess |
| `ewwm--ipc-connection` | process | IPC network process |
| `ewwm--compositor-socket` | string | IPC socket path |

### Buffer-Local Variables

All have `permanent-local` property.

| Variable | Type | Description |
|----------|------|-------------|
| `ewwm-surface-id` | integer | Wayland surface ID |
| `ewwm-app-id` | string | Application ID (from xdg_toplevel) |
| `ewwm-title` | string | Surface title |
| `ewwm-class-name` | string | Alias for `ewwm-app-id` |
| `ewwm-instance-name` | string | Instance name |
| `ewwm-surface-state` | symbol | `managed`, `floating`, or `fullscreen` |
| `ewwm-geometry` | plist | `(:x N :y N :w N :h N)` |
| `ewwm-workspace` | integer | Workspace index |
| `ewwm-x11-p` | boolean | Non-nil for XWayland surfaces |
| `ewwm-x11-class` | string | X11 WM_CLASS |
| `ewwm-x11-instance` | string | X11 WM_CLASS instance |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm--get-buffer` | `(surface-id)` | Get buffer for surface ID |
| `ewwm--set-buffer` | `(surface-id buffer)` | Associate surface with buffer |
| `ewwm--remove-buffer` | `(surface-id)` | Remove association |
| `ewwm--all-surfaces` | `()` | List all managed surface IDs |
| `ewwm--all-buffers` | `()` | List all managed buffers |
| `ewwm--surface-count` | `()` | Count of managed surfaces |
| `ewwm--clear-surfaces` | `()` | Remove all associations |
| `ewwm--create-surface-buffer` | `(surface-id app-id title)` | Create buffer for surface |
| `ewwm--destroy-surface-buffer` | `(surface-id)` | Destroy buffer for surface |
| `ewwm--update-surface-title` | `(surface-id title)` | Update surface title |
| `ewwm--update-surface-geometry` | `(surface-id geometry)` | Update geometry plist |
| `ewwm--buffers-on-workspace` | `(workspace)` | Buffers on workspace N |
| `ewwm--surface-buffer-p` | `(buffer)` | Non-nil if buffer is ewwm surface |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-mode-hook` | -- | After entering `ewwm-mode` |

---

## ewwm-ipc.el

IPC client for compositor communication.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-ipc-socket-path` | `nil` | Socket path (nil = auto-detect) |
| `ewwm-ipc-reconnect-max-delay` | `30` | Max reconnect delay (seconds) |
| `ewwm-ipc-sync-timeout` | `2` | Sync request timeout (seconds) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-ipc-connect` | `(&optional socket-path)` | Connect to compositor |
| `ewwm-ipc-disconnect` | `()` | Disconnect from compositor |
| `ewwm-ipc-connected-p` | `()` | Non-nil if connected |
| `ewwm-ipc-send` | `(request &optional callback)` | Send async request |
| `ewwm-ipc-send-sync` | `(request &optional timeout)` | Send sync request |
| `ewwm-ipc-ping` | `()` | Ping with latency report |
| `ewwm-ipc-status` | `()` | Display connection status |
| `ewwm-ipc-benchmark` | `(&optional count)` | Benchmark round-trip latency |
| `ewwm-surface-list` | `()` | Query all surfaces |
| `ewwm-surface-focus` | `(surface-id)` | Focus surface |
| `ewwm-surface-close` | `(surface-id)` | Close surface |
| `ewwm-surface-move` | `(surface-id x y)` | Move surface |
| `ewwm-surface-resize` | `(surface-id w h)` | Resize surface |
| `ewwm-workspace-switch` | `(n)` | Switch to workspace N |
| `ewwm-workspace-list` | `()` | Query workspaces |
| `ewwm-key-grab` | `(key)` | Register global key grab |
| `ewwm-key-ungrab` | `(key)` | Release key grab |

### Minor Modes

| Mode | Lighter | Description |
|------|---------|-------------|
| `ewwm-ipc-trace-mode` | ` IPC-Trace` | Log all IPC to trace buffer |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-ipc-connected-hook` | -- | After IPC connection established |
| `ewwm-ipc-disconnected-hook` | -- | After IPC connection lost |

---

## ewwm-vr.el

VR session management and HMD state tracking.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-reference-space` | `local` | OpenXR reference space (`local`/`stage`/`view`) |
| `ewwm-vr-mode-line` | `t` | Show VR status in mode-line |
| `ewwm-vr-frame-stats-interval` | `5` | Seconds between frame stat events |

### State Variables

| Variable | Type | Description |
|----------|------|-------------|
| `ewwm-vr-session-state` | keyword | `:idle`, `:ready`, `:focused`, etc. |
| `ewwm-vr-hmd-name` | string | Discovered HMD name |
| `ewwm-vr-hmd-info` | plist | HMD capabilities |
| `ewwm-vr-headless` | boolean | Headless mode flag |
| `ewwm-vr-frame-stats` | plist | Frame timing (fps, p50, p99, missed) |
| `ewwm-vr-enabled` | boolean | VR feature enabled in compositor |

---

## ewwm-vr-eye.el

Eye tracking and gaze-based focus.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-gaze-source` | `auto` | Gaze source (`auto`/`openxr`/`pupil-labs`/`simulated`/`none`) |
| `ewwm-vr-gaze-smoothing` | `0.3` | EMA alpha (0=smooth, 1=raw) |
| `ewwm-vr-gaze-visualization` | `dot` | Visualization (`dot`/`crosshair`/`spotlight`/`none`) |
| `ewwm-vr-gaze-simulate` | `nil` | Simulation mode (`mouse`/`scripted`/`random-walk`/`pattern`) |
| `ewwm-vr-gaze-confidence-min` | `0.6` | Minimum gaze confidence |
| `ewwm-vr-pupil-port` | `50020` | Pupil Capture ZMQ port |
| `ewwm-vr-gaze-mode-line` | `t` | Show gaze in mode-line |
| `ewwm-vr-eye-enable` | `t` | Master switch for gaze focus |
| `ewwm-vr-eye-focus-policy` | `gaze-primary` | Focus policy (`gaze-only`/`gaze-primary`/`gaze-assist`/`disabled`) |
| `ewwm-vr-eye-dwell-ms` | `200` | Dwell time (ms) |
| `ewwm-vr-eye-cooldown-ms` | `500` | Cooldown after focus switch (ms) |
| `ewwm-vr-eye-max-jitter-px` | `50` | Max jitter during dwell (px) |
| `ewwm-vr-eye-saccade-threshold` | `300` | Saccade velocity threshold (deg/s) |
| `ewwm-vr-eye-show-dwell-progress` | `nil` | Show dwell progress in mode-line |
| `ewwm-vr-eye-focus-exceptions` | `nil` | Per-surface focus policy overrides |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-vr-set-gaze-source` | `(source)` | Set gaze data source |
| `ewwm-vr-gaze-status` | `()` | Display gaze status |
| `ewwm-vr-calibrate-eyes` | `(&optional points)` | Start calibration |
| `ewwm-vr-calibrate-eye-point` | `()` | Record calibration point |
| `ewwm-vr-eye-set-focus-policy` | `(policy)` | Set focus policy |
| `ewwm-vr-eye-focus-back` | `()` | Return to previous surface |
| `ewwm-vr-eye-analytics` | `()` | Display focus analytics |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-vr-gaze-calibration-drift-hook` | `(error-deg)` | Calibration drift detected |

---

## ewwm-vr-wink.el

Wink-based hands-free interaction.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-wink-enable` | `t` | Master switch |
| `ewwm-vr-wink-left-action` | `#'previous-buffer` | Left wink command |
| `ewwm-vr-wink-right-action` | `#'next-buffer` | Right wink command |
| `ewwm-vr-wink-double-left-action` | `#'mode-line-other-buffer` | Double left wink |
| `ewwm-vr-wink-double-right-action` | `#'delete-window` | Double right wink |
| `ewwm-vr-blink-confidence-min` | `0.7` | Minimum blink confidence |
| `ewwm-vr-wink-feedback` | `t` | Mode-line feedback on wink |
| `ewwm-vr-wink-sequences` | `nil` | Custom multi-wink sequences |
| `ewwm-vr-wink-sequence-timeout-ms` | `1500` | Sequence timeout (ms) |

---

## ewwm-vr-gaze-zone.el

9-region gaze zone modifier system.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-gaze-zone-enable` | `t` | Master switch |
| `ewwm-vr-gaze-zone-dwell-ms` | `200` | Zone activation dwell (ms) |
| `ewwm-vr-gaze-zone-layout` | `default` | Layout preset (`default`/`vim-like`/`spacemacs`/`custom`) |
| `ewwm-vr-gaze-zone-custom-map` | `nil` | Custom zone-to-modifier alist |
| `ewwm-vr-gaze-zone-overlay-alpha` | `0.15` | Zone overlay opacity |

---

## ewwm-vr-fatigue.el

Eye fatigue monitoring and alerting.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-fatigue-enable` | `t` | Master switch |
| `ewwm-vr-fatigue-alert-threshold` | `25` | Blink rate threshold (blinks/min) |
| `ewwm-vr-fatigue-perclos-threshold` | `0.15` | PERCLOS threshold |
| `ewwm-vr-fatigue-log-enabled` | `t` | Enable CSV logging |
| `ewwm-vr-fatigue-log-file` | `~/.local/share/exwm-vr/fatigue-log.csv` | Log file path |
| `ewwm-vr-fatigue-check-interval` | `60` | Check interval (seconds) |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-vr-fatigue-alert-hook` | `(level metrics-plist)` | Fatigue alert triggered |

---

## ewwm-vr-hand.el

Hand tracking integration (OpenXR XR_EXT_hand_tracking).

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-hand-enable` | `t` | Master switch |
| `ewwm-vr-hand-min-confidence` | `0.5` | Minimum tracking confidence |
| `ewwm-vr-hand-smoothing` | `0.3` | EMA alpha for smoothing |
| `ewwm-vr-hand-prediction-ms` | `20.0` | Prediction lookahead (ms) |
| `ewwm-vr-hand-show-skeleton` | `nil` | Debug skeleton visualization |
| `ewwm-vr-hand-dominant` | `right` | Dominant hand (`left`/`right`) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-vr-hand-status` | `()` | Display tracking status |
| `ewwm-vr-hand-toggle` | `()` | Toggle hand tracking |
| `ewwm-vr-hand-configure` | `()` | Send config to compositor |
| `ewwm-vr-hand-mode-line-string` | `()` | Mode-line string |

### Minor Modes

| Mode | Lighter | Keymap |
|------|---------|--------|
| `ewwm-vr-hand-mode` | ` VR-Hand` | `C-c h s` status, `C-c h t` toggle, `C-c h c` configure |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-vr-hand-tracking-started-hook` | `(hand)` | Hand tracking started |
| `ewwm-vr-hand-tracking-lost-hook` | `(hand)` | Hand tracking lost |

---

## ewwm-vr-gesture.el

Gesture recognition and binding system.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-gesture-enable` | `t` | Master switch |
| `ewwm-vr-gesture-pinch-threshold` | `0.02` | Pinch distance (meters) |
| `ewwm-vr-gesture-grab-threshold` | `0.04` | Grab distance (meters) |
| `ewwm-vr-gesture-swipe-min-velocity` | `0.5` | Swipe velocity (m/s) |
| `ewwm-vr-gesture-debounce-ms` | `200` | Debounce time (ms) |
| `ewwm-vr-gesture-verbose` | `nil` | Log gestures to *Messages* |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-vr-gesture-bind` | `(hand gesture command)` | Bind gesture to command |
| `ewwm-vr-gesture-unbind` | `(hand gesture)` | Remove gesture binding |
| `ewwm-vr-gesture-default-bindings` | `()` | Return default bindings alist |
| `ewwm-vr-gesture-status` | `()` | Display gesture status |
| `ewwm-vr-gesture-toggle` | `()` | Toggle gesture recognition |
| `ewwm-vr-gesture-list-bindings` | `()` | List current bindings |

### Binding Format

Bindings are `((HAND . GESTURE) . COMMAND)` where:
- HAND: `left` or `right`
- GESTURE: `pinch`, `grab`, `point`, `swipe-left`, `swipe-right`, `swipe-up`, `swipe-down`
- COMMAND: any function

### Default Bindings

| Hand | Gesture | Command |
|------|---------|---------|
| right | pinch | select/click |
| right | grab | grab/move |
| right | point | aim ray |
| left | swipe-left | `previous-buffer` |
| left | swipe-right | `next-buffer` |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-vr-gesture-hook` | `(hand gesture command)` | After gesture dispatch |

---

## ewwm-vr-keyboard.el

VR virtual keyboard.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-vr-keyboard-layout` | `qwerty` | Layout (`qwerty`/`dvorak`/`colemak`) |
| `ewwm-vr-keyboard-key-size` | `0.03` | Key size in meters |
| `ewwm-vr-keyboard-haptic` | `t` | Haptic feedback |
| `ewwm-vr-keyboard-auto-show` | `nil` | Auto-show on text input focus |
| `ewwm-vr-keyboard-auto-capitalize` | `t` | Auto-capitalize after punctuation |
| `ewwm-vr-keyboard-prediction` | `nil` | Predictive text |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-vr-keyboard-show` | `()` | Show keyboard |
| `ewwm-vr-keyboard-hide` | `()` | Hide keyboard |
| `ewwm-vr-keyboard-toggle` | `()` | Toggle keyboard |
| `ewwm-vr-keyboard-set-layout` | `(layout)` | Set layout |
| `ewwm-vr-keyboard-status` | `()` | Display status |
| `ewwm-vr-keyboard-mode-line-string` | `()` | Mode-line string |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-vr-keyboard-show-hook` | -- | Keyboard shown |
| `ewwm-vr-keyboard-hide-hook` | -- | Keyboard hidden |

---

## ewwm-bci-core.el

BCI daemon lifecycle and board connection.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-board-id` | `0` | BrainFlow board ID (0=synthetic, 1=Cyton, 22=CytonDaisy) |
| `ewwm-bci-serial-port` | `"/dev/openbci"` | Serial port |
| `ewwm-bci-sample-rate` | `250` | Sample rate (Hz) |
| `ewwm-bci-notch-frequency` | `60` | Notch filter (50 or 60 Hz) |
| `ewwm-bci-artifact-rejection` | `t` | Auto artifact rejection |
| `ewwm-bci-data-retention-days` | `90` | Data retention (days, 0=never clean) |
| `ewwm-bci-daemon-path` | `"exwm-vr-brainflow-daemon"` | Daemon binary path |
| `ewwm-bci-daemon-args` | `nil` | Extra daemon arguments |
| `ewwm-bci-auto-reconnect` | `t` | Auto-reconnect on loss |
| `ewwm-bci-reconnect-delay` | `5` | Reconnect delay (seconds) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-start` | `()` | Start BCI acquisition |
| `ewwm-bci-stop` | `()` | Stop BCI and daemon |
| `ewwm-bci-restart` | `()` | Restart BCI |
| `ewwm-bci-status` | `()` | Display BCI status |
| `ewwm-bci-signal-quality` | `()` | Per-channel signal quality |
| `ewwm-bci-hardware-check` | `()` | Run hardware diagnostics |
| `ewwm-bci-mode-line-string` | `()` | Mode-line string |
| `ewwm-bci-init` | `()` | Initialize BCI core |
| `ewwm-bci-teardown` | `()` | Clean up BCI state |

### State Variables

| Variable | Type | Description |
|----------|------|-------------|
| `ewwm-bci--connection-state` | symbol | `disconnected`/`connecting`/`connected`/`error` |
| `ewwm-bci--streaming` | boolean | Streaming active |
| `ewwm-bci--channel-quality` | plist | Per-channel quality |
| `ewwm-bci--frames-received` | integer | Total frames received |
| `ewwm-bci--session-start` | float | Session start time |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-connected-hook` | -- | Board connected |
| `ewwm-bci-disconnected-hook` | `(reason)` | Board disconnected |
| `ewwm-bci-quality-change-hook` | `(channel-quality)` | Signal quality changed |
| `ewwm-bci-error-hook` | `(error-msg)` | BCI error |

---

## ewwm-bci-attention.el

Attention state tracking from EEG band power ratios.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-attention-enabled` | `t` | Master switch |
| `ewwm-bci-attention-threshold` | `0.6` | Focused state threshold |
| `ewwm-bci-attention-dnd-threshold` | `0.8` | Deep focus / auto-DND threshold |
| `ewwm-bci-attention-drowsy-threshold` | `0.2` | Drowsiness threshold |
| `ewwm-bci-attention-break-minutes` | `5` | Minutes of drowsiness before break alert |
| `ewwm-bci-attention-auto-save` | `t` | Auto-save on attention loss |
| `ewwm-bci-attention-update-interval` | `2.0` | Debounce interval (seconds) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-attention-status` | `()` | Display attention state |
| `ewwm-bci-attention-calibrate` | `()` | Start baseline calibration |
| `ewwm-bci-attention-toggle` | `()` | Toggle attention tracking |
| `ewwm-bci-attention-mode-line-string` | `()` | Mode-line string |

### States

| State | Score Range | WM Behavior |
|-------|------------|-------------|
| `neutral` | 0.2 - 0.6 | Normal operation |
| `focused` | 0.6 - 0.8 | Mode-line indicator |
| `deep-focus` | > 0.8 | Auto-DND enabled |
| `drowsy` | < 0.2 | Break alert, auto-save |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-attention-change-hook` | `(old-state new-state score)` | State transition |

---

## ewwm-bci-ssvep.el

SSVEP frequency-tagged workspace selection.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-ssvep-enabled` | `nil` | Master switch (off by default) |
| `ewwm-bci-ssvep-frequencies` | `((1 . 12.0) (2 . 15.0) (3 . 20.0) (4 . 24.0))` | Workspace-frequency map |
| `ewwm-bci-ssvep-window-seconds` | `3.0` | FFT analysis window (seconds) |
| `ewwm-bci-ssvep-min-confidence` | `0.7` | Classification threshold |
| `ewwm-bci-ssvep-cooldown-ms` | `2000` | Switch cooldown (ms) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-ssvep-status` | `()` | Display SSVEP status |
| `ewwm-bci-ssvep-configure` | `()` | Send config to compositor |
| `ewwm-bci-ssvep-mode-line-string` | `()` | Mode-line string |

### Minor Modes

| Mode | Lighter | Description |
|------|---------|-------------|
| `ewwm-bci-ssvep-mode` | ` SSVEP` | Start/stop SSVEP classification |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-ssvep-select-hook` | `(workspace-id confidence)` | Workspace selected |

---

## ewwm-bci-p300.el

P300 oddball paradigm confirmation system.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-p300-enabled` | `nil` | Master switch |
| `ewwm-bci-p300-repetitions` | `5` | Stimulus repetitions per trial |
| `ewwm-bci-p300-min-confidence` | `0.7` | Detection threshold |
| `ewwm-bci-p300-soa-ms` | `200` | Stimulus onset asynchrony (ms) |
| `ewwm-bci-p300-flash-duration-ms` | `100` | Flash duration (ms) |
| `ewwm-bci-p300-timeout-ms` | `30000` | Trial timeout (ms) |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-p300-confirm` | `(prompt targets callback)` | Async P300 confirmation |
| `ewwm-bci-p300-start` | `()` | Start demo trial |
| `ewwm-bci-p300-stop` | `()` | Cancel active trial |
| `ewwm-bci-p300-status` | `()` | Display P300 status |

### Callback Signature

```elisp
(lambda (target-id confidence)
  ;; target-id: selected target string/symbol, or nil on timeout
  ;; confidence: float [0.0, 1.0], or nil on timeout
  )
```

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-p300-detect-hook` | `(target-id confidence)` | Target detected |

---

## ewwm-bci-mi.el

Motor imagery classification (left/right/foot).

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-mi-enabled` | `nil` | Master switch |
| `ewwm-bci-mi-left-action` | `#'previous-buffer` | Left hand MI action |
| `ewwm-bci-mi-right-action` | `#'next-buffer` | Right hand MI action |
| `ewwm-bci-mi-foot-action` | *(lambda cycling workspaces)* | Foot MI action |
| `ewwm-bci-mi-min-confidence` | `0.6` | Classification threshold |
| `ewwm-bci-mi-cooldown-ms` | `1500` | Cooldown between dispatches (ms) |
| `ewwm-bci-mi-feedback` | `t` | Show feedback on classification |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-mi-calibrate` | `()` | Start MI calibration |
| `ewwm-bci-mi-status` | `()` | Display MI status |
| `ewwm-bci-mi-toggle` | `()` | Toggle MI classification |
| `ewwm-bci-mi-mode-line-string` | `()` | Mode-line string |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-mi-classify-hook` | `(class confidence)` | Classification event |

---

## ewwm-bci-nfb.el

Neurofeedback training with real-time display.

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-nfb-session-dir` | `~/.local/share/exwm-vr/neurofeedback/` | Session data directory |
| `ewwm-bci-nfb-log-interval` | `1.0` | Data log interval (seconds) |
| `ewwm-bci-nfb-display-channels` | `8` | Channels to display |
| `ewwm-bci-nfb-bar-width` | `40` | Bar chart width (chars) |
| `ewwm-bci-nfb-auto-export` | `t` | Auto-export CSV on stop |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-nfb-start` | `(protocol)` | Start session (`alpha-trainer`/`beta-trainer`/`mi-trainer`) |
| `ewwm-bci-nfb-stop` | `()` | Stop session |
| `ewwm-bci-nfb-status` | `()` | Display session status |
| `ewwm-bci-nfb-export-session` | `()` | Manual CSV export |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-nfb-start-hook` | `(protocol session-id)` | Session started |
| `ewwm-bci-nfb-stop-hook` | `(session-id frame-count)` | Session stopped |

---

## ewwm-bci-multimodal.el

Multi-modal fusion (gaze + EEG + gesture).

### Defcustom Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `ewwm-bci-multimodal-enabled` | `nil` | Master switch |
| `ewwm-bci-multimodal-adaptive-dwell` | `t` | Attention-based dwell adjustment |
| `ewwm-bci-multimodal-dwell-focused-ms` | `150` | Dwell when focused (ms) |
| `ewwm-bci-multimodal-dwell-relaxed-ms` | `400` | Dwell when relaxed (ms) |
| `ewwm-bci-multimodal-dwell-default-ms` | `250` | Default dwell (ms) |
| `ewwm-bci-multimodal-focused-threshold` | `0.7` | Attention score for focused dwell |
| `ewwm-bci-multimodal-relaxed-threshold` | `0.4` | Attention score for relaxed dwell |
| `ewwm-bci-multimodal-two-factor` | `nil` | Gaze + MI confirmation |
| `ewwm-bci-multimodal-two-factor-timeout-ms` | `3000` | MI confirm timeout (ms) |
| `ewwm-bci-multimodal-three-factor-security` | `nil` | Gaze + P300 + pinch |

### Public Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `ewwm-bci-multimodal-status` | `()` | Display fusion status |
| `ewwm-bci-multimodal-toggle` | `()` | Toggle multi-modal |
| `ewwm-bci-multimodal-three-factor-verify` | `(action callback)` | Three-factor verification |

### Hooks

| Hook | Arguments | Description |
|------|-----------|-------------|
| `ewwm-bci-multimodal-fusion-hook` | `(action modalities)` | Fused action completed |

---

## IPC Message Types

### Core Events (Compositor -> Emacs)

| Event | Fields | Description |
|-------|--------|-------------|
| `:surface-created` | `:id :app-id :title` | New surface |
| `:surface-destroyed` | `:id` | Surface closed |
| `:surface-title-changed` | `:id :title` | Title updated |
| `:surface-focused` | `:id` | Focus changed |
| `:surface-geometry-changed` | `:id :geometry` | Geometry changed |
| `:workspace-changed` | `:workspace` | Workspace switched |
| `:key-pressed` | `:key :modifiers :timestamp` | Grabbed key pressed |
| `:output-usable-area-changed` | `:x :y :w :h` | Layer-shell area changed |

### VR Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:vr-session-state` | `:state :headless` | Session state change |
| `:vr-frame-stats` | `:fps :total-p50 ...` | Frame timing update |

### Eye Tracking Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:gaze-update` | `:x :y :z :confidence :surface-id` | Gaze position update |
| `:gaze-focus-changed` | `:surface-id :old-surface-id` | Gaze focus switch |
| `:gaze-calibration-complete` | `:rms-error` | Calibration done |
| `:gaze-calibration-drift` | `:error-deg` | Drift detected |

### Hand Tracking Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:hand-tracking-started` | `:hand` | Hand detected |
| `:hand-tracking-lost` | `:hand` | Hand lost |
| `:hand-confidence` | `:hand :confidence` | Confidence update |

### Gesture Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:gesture-started` | `:hand :gesture` | Gesture recognized |
| `:gesture-swipe` | `:hand :direction` | Swipe direction |
| `:gesture-ended` | `:hand :gesture` | Gesture ended |

### Keyboard Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:keyboard-text-input` | `:text` | Text from VR keyboard |
| `:keyboard-special-key` | `:key` | Special key (backspace, etc.) |
| `:keyboard-visibility` | `:visible` | Keyboard show/hide |
| `:keyboard-layout-changed` | `:layout` | Layout changed |

### BCI Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:bci-connected` | `:board-id :sample-rate` | Board connected |
| `:bci-disconnected` | `:reason` | Board disconnected |
| `:bci-quality` | `:channels` | Signal quality update |
| `:bci-error` | `:message` | BCI error |
| `:bci-frame` | *(data)* | EEG data frame |
| `:bci-attention` | `:score :band-powers` | Attention update |
| `:bci-ssvep` | `:workspace :frequency :confidence` | SSVEP classification |
| `:bci-p300` | `:target-id :confidence :latency-ms :status` | P300 detection |
| `:bci-mi` | `:class :confidence :band-power` | MI classification |
| `:bci-mi-calibration` | `:status :progress :reason` | MI calibration progress |
| `:bci-nfb-frame` | `:bands :channels :score` | NFB data frame |
| `:bci-multimodal` | `:subtype ...` | Multi-modal fusion event |

### Fatigue Events

| Event | Fields | Description |
|-------|--------|-------------|
| `:fatigue-update` | `:level :blink-rate :perclos :saccade-jitter` | Eye fatigue |
| `:bci-fatigue-eeg` | `:level :theta-alpha-ratio :alpha-power` | EEG fatigue |

### Wire Format Summary

```
Request:  (:type MESSAGE-TYPE :id N &rest PAYLOAD)
Response: (:type :response :id N :status :ok|:error &rest PAYLOAD)
Event:    (:type :event :event EVENT-TYPE &rest PAYLOAD)
```

Transport: 4-byte big-endian length prefix + UTF-8 s-expression payload
over Unix domain stream socket at `$XDG_RUNTIME_DIR/ewwm-ipc.sock`.
