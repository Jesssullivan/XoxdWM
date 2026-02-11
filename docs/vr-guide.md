# EXWM-VR: VR Setup and Usage Guide

**Version 0.1.0** | VR subsystem documentation for EWWM

---

## Table of Contents

1. [Supported HMDs](#supported-hmds)
2. [Monado Installation and Configuration](#monado-installation-and-configuration)
3. [DRM Lease Setup](#drm-lease-setup)
4. [WiVRn Wireless Streaming](#wivrn-wireless-streaming)
5. [Scene Configuration](#scene-configuration)
6. [Head-Gaze Interaction](#head-gaze-interaction)
7. [Hand Tracking](#hand-tracking)
8. [Virtual Keyboard](#virtual-keyboard)
9. [Performance Tuning](#performance-tuning)
10. [Troubleshooting](#troubleshooting)

---

## Supported HMDs

EWWM-VR uses the OpenXR standard via the Monado runtime. Any HMD supported
by Monado should work. The following have been tested or are expected to work:

| HMD | Connection | Status | Notes |
|-----|-----------|--------|-------|
| Valve Index | DisplayPort + USB | Tested | Full support including eye tracking (via Pupil Labs) |
| Meta Quest 3 | USB / WiVRn wireless | Expected | Via Monado Quest driver or WiVRn |
| HP Reverb G2 | DisplayPort + USB | Expected | WMR driver in Monado |
| Pimax Crystal | DisplayPort + USB | Experimental | Requires Pimax Monado fork |
| Bigscreen Beyond | DisplayPort + USB | Experimental | DRM lease required |
| Somnium VR1 | DisplayPort + USB | Aspirational | Moving target, Monado support pending |
| HTC Vive Pro 2 | DisplayPort + USB | Expected | Via Monado lighthouse driver |
| Samsung Odyssey+ | DisplayPort + USB | Expected | WMR driver in Monado |

### Architecture Limitations

| Architecture | VR Support |
|-------------|------------|
| x86_64 | Full (DRM lease, 90fps+) |
| aarch64 | No (no ARM VR runtime) |
| s390x | No (headless only) |

---

## Monado Installation and Configuration

### NixOS

Use the provided NixOS module:

```nix
{
  services.exwm-vr.vr.monado = {
    enable = true;
    headset = "auto";         # auto, index, quest3, reverb-g2
    socketActivation = true;  # Start on first OpenXR API call
  };
}
```

The module configures:
- Monado service unit (`monado.service`)
- OpenXR runtime JSON (`/etc/xdg/openxr/1/active_runtime.json`)
- `XR_RUNTIME_JSON` environment variable
- udev rules for HMD USB devices

### Manual Installation

```bash
# Install Monado
git clone https://gitlab.freedesktop.org/monado/monado.git
cd monado && mkdir build && cd build
cmake .. -DCMAKE_INSTALL_PREFIX=/usr/local \
         -DXRT_HAVE_VULKAN=ON \
         -DXRT_HAVE_OPENGL=ON
make -j$(nproc) && sudo make install

# Set as active OpenXR runtime
sudo mkdir -p /etc/xdg/openxr/1
cat > /tmp/active_runtime.json <<EOF
{
  "file_format_version": "1.0.0",
  "runtime": {
    "name": "Monado",
    "library_path": "/usr/local/lib/libopenxr_monado.so"
  }
}
EOF
sudo mv /tmp/active_runtime.json /etc/xdg/openxr/1/active_runtime.json

# Set environment variable
export XR_RUNTIME_JSON=/etc/xdg/openxr/1/active_runtime.json
```

### Monado Configuration

Monado reads configuration from `~/.config/monado/config_v0.json`:

```json
{
  "tracking": {
    "tracking_override": {
      "enabled": false
    }
  },
  "remote": {
    "port": 4242
  }
}
```

### Verify Monado

```bash
# Check Monado is running
monado-cli info

# List available devices
monado-cli devices
```

In Emacs:

```elisp
M-x ewwm-vr-status
;; Should show: session=ready hmd="Valve Index" headless=no
```

---

## DRM Lease Setup

DRM leasing allows the compositor to hand exclusive GPU connector control
to the VR runtime for direct-mode rendering.

### How It Works

1. The compositor (`drm_lease.rs`) scans DRM connectors on startup
2. Connectors with the `non_desktop` property set to 1 are identified as HMDs
3. A DRM lease is created, giving Monado exclusive control of the HMD connector
4. The compositor continues rendering the desktop on the primary connector

### Connector Detection

```elisp
;; List all DRM connectors
(:type :vr-display-list-connectors)

;; Response:
;; (:type :response :id N :status :ok :connectors
;;   ((:id 1 :name "DP-1" :non-desktop nil :connected t)
;;    (:id 2 :name "DP-2" :non-desktop t :connected t :hmd t)))
```

### Auto-Detection

```elisp
;; Auto-detect HMD and create lease
(ewwm-ipc-send '(:type :vr-display-auto-detect))
```

The `HmdManager` in the compositor:
- Filters connectors by `non_desktop` property
- Debounces hotplug events (500ms)
- Negotiates refresh rate with the HMD
- Falls back to headless mode if no HMD found

### Manual HMD Selection

```elisp
;; Select a specific connector as the HMD
(ewwm-ipc-send '(:type :vr-display-select-hmd :connector-id 2))

;; Set display mode
(ewwm-ipc-send '(:type :vr-display-set-mode :mode :headset))
;; Modes: headset, preview, headless, off
```

### Refresh Rate

```elisp
;; Set target refresh rate
(ewwm-ipc-send '(:type :vr-display-set-refresh-rate :rate 90))

;; Response includes actual negotiated rate:
;; (:type :response :status :ok :target 90 :actual 90)
```

### udev Rules

The project includes udev rules at `/etc/udev/rules.d/99-exwm-vr.rules`
to ensure correct permissions on HMD devices.

---

## WiVRn Wireless Streaming

WiVRn enables wireless VR streaming to standalone headsets (Quest 3, etc.)
over Wi-Fi.

### Setup

1. Install WiVRn server:
   ```bash
   # NixOS: included in the monado module
   # Manual: build from https://github.com/WiVRn/WiVRn
   ```

2. Configure Monado to use WiVRn as a driver:
   ```bash
   export XRT_COMPOSITOR_FORCE_WAYLAND=1
   ```

3. Pair your headset:
   - Install the WiVRn client APK on your Quest 3
   - Connect to the same network
   - Enter the pairing code

### Performance Considerations

- Use 5GHz Wi-Fi or Wi-Fi 6E for best latency
- Wired Ethernet backhaul recommended for the host
- Target: < 20ms motion-to-photon additional latency
- Encode resolution should match HMD native resolution

---

## Scene Configuration

The VR scene graph manages 3D placement of Wayland surfaces in VR space.

### Layout Modes

| Layout | Description |
|--------|-------------|
| `arc` | Surfaces arranged in a curved arc around the user |
| `stack` | Surfaces stacked vertically |
| `grid` | Surfaces in an N-column grid |
| `freeform` | Manually positioned surfaces |

```elisp
;; Set layout
(ewwm-ipc-send '(:type :vr-scene-set-layout :layout :arc))
(ewwm-ipc-send '(:type :vr-scene-set-layout :layout :grid :columns 3))
```

### Pixels Per Unit (PPU)

PPU controls the rendering resolution of surfaces in VR space.

```elisp
;; Set global PPU
(ewwm-ipc-send '(:type :vr-scene-set-ppu :ppu 2000))

;; Set per-surface PPU
(ewwm-ipc-send '(:type :vr-scene-set-ppu :surface-id 1 :ppu 3000))
```

Higher PPU = sharper text, more GPU load. Recommended: 2000-3000 for
readable text.

### Background

```elisp
;; Set VR environment background
(ewwm-ipc-send '(:type :vr-scene-set-background :background :dark))
;; Options: dark, gradient, grid, passthrough
```

`passthrough` uses the HMD's cameras for mixed reality (if supported).

### Surface Projection

Surfaces can be projected as flat planes or cylindrical surfaces:

```elisp
(ewwm-ipc-send '(:type :vr-scene-set-projection :surface-id 1 :projection :cylinder))
;; Options: flat, cylinder
```

Cylindrical projection reduces distortion at the edges and is recommended
for wide surfaces.

### Moving Surfaces in 3D

```elisp
;; Position in centimeters (converted to meters internally)
(ewwm-ipc-send '(:type :vr-scene-move :surface-id 1 :x 0 :y 150 :z -200))

;; Adjust depth (positive = closer)
(ewwm-ipc-send '(:type :vr-adjust-depth :surface-id 1 :delta -20))
```

### Focus and Follow

```elisp
;; Focus a surface in VR
(ewwm-ipc-send '(:type :vr-scene-focus :surface-id 1))

;; Set follow mode
(ewwm-ipc-send '(:type :vr-set-follow :surface-id 1 :mode :lazy))
;; Modes: none, lazy, sticky, locked
```

Follow modes:
- `none`: Surface stays fixed in world space
- `lazy`: Surface slowly drifts to stay in view
- `sticky`: Surface follows head rotation with damping
- `locked`: Surface is head-locked (HUD-style)

---

## Head-Gaze Interaction

When no eye tracker is available, head gaze provides interaction via
the head-forward direction (Xray pattern).

### How It Works

1. A ray is cast from the head position in the forward direction
2. The ray intersects with surfaces in the VR scene
3. The intersected surface receives pointer events
4. Dwell on a surface to focus it

### Calibration

```elisp
;; Start head-gaze calibration (5 points)
(ewwm-ipc-send '(:type :vr-calibrate-confirm))
```

The calibration collects head poses at predefined target positions and
computes an offset correction.

### Gaze Offset

Fine-tune the gaze ray offset:

```elisp
;; Offset in centimeters
(ewwm-ipc-send '(:type :vr-set-gaze-offset :x 15 :y -10 :z -5))
```

---

## Hand Tracking

EWWM supports OpenXR hand tracking via `XR_EXT_hand_tracking` (26-joint
skeleton per hand).

### Enabling

```elisp
(setq ewwm-vr-hand-enable t)
(ewwm-vr-hand-configure)
```

### Configuration

```elisp
(setq ewwm-vr-hand-min-confidence 0.5)  ; Reject low-confidence data
(setq ewwm-vr-hand-smoothing 0.3)       ; Position smoothing
(setq ewwm-vr-hand-prediction-ms 20.0)  ; Lookahead for latency comp
(setq ewwm-vr-hand-dominant 'right)     ; For asymmetric bindings
```

### Gesture Recognition

Gestures are recognized from hand joint positions:

| Gesture | Description | Default Action |
|---------|-------------|----------------|
| `pinch` | Thumb-index finger contact (< 2cm) | Select/click |
| `grab` | All fingers curled (< 4cm) | Grab/move surface |
| `point` | Index finger extended, others curled | Aim ray |
| `swipe-left` | Hand sweep left (> 0.5 m/s) | Previous buffer |
| `swipe-right` | Hand sweep right (> 0.5 m/s) | Next buffer |
| `swipe-up` | Hand sweep up | (unbound) |
| `swipe-down` | Hand sweep down | (unbound) |

### Custom Gesture Bindings

```elisp
;; Bind right-hand pinch to a custom command
(ewwm-vr-gesture-bind 'right 'pinch #'my-custom-select)

;; Bind left-hand swipe-up to workspace switch
(ewwm-vr-gesture-bind 'left 'swipe-up
  (lambda () (ewwm-workspace-switch 0)))

;; Unbind a gesture
(ewwm-vr-gesture-unbind 'left 'swipe-down)

;; List all bindings
M-x ewwm-vr-gesture-list-bindings
```

### Gesture Tuning

```elisp
(setq ewwm-vr-gesture-pinch-threshold 0.02)    ; 2cm for pinch
(setq ewwm-vr-gesture-grab-threshold 0.04)      ; 4cm for grab
(setq ewwm-vr-gesture-swipe-min-velocity 0.5)   ; 0.5 m/s for swipe
(setq ewwm-vr-gesture-debounce-ms 200)           ; 200ms debounce
```

### IPC Commands

```elisp
;; Query hand tracking state
(:type :hand-tracking-status)
(:type :hand-tracking-joint :hand :left :joint :index-tip)
(:type :hand-tracking-skeleton :hand :right)
(:type :hand-tracking-distance :hand :right :joint-a :thumb-tip :joint-b :index-tip)

;; Query/manage gestures
(:type :gesture-status)
(:type :gesture-config :pinch-threshold 0.02 :grab-threshold 0.04)
(:type :gesture-bind :hand :right :gesture :pinch :action "select")
(:type :gesture-unbind :hand :right :gesture :pinch)
(:type :gesture-bindings)
```

---

## Virtual Keyboard

EWWM includes a VR virtual keyboard for text input when physical keyboards
are not accessible.

### Usage

```elisp
M-x ewwm-vr-keyboard-show     ; Show keyboard
M-x ewwm-vr-keyboard-hide     ; Hide keyboard
M-x ewwm-vr-keyboard-toggle   ; Toggle visibility
```

### Layouts

```elisp
(ewwm-vr-keyboard-set-layout 'qwerty)   ; QWERTY
(ewwm-vr-keyboard-set-layout 'dvorak)   ; Dvorak
(ewwm-vr-keyboard-set-layout 'colemak)  ; Colemak
```

### Configuration

```elisp
(setq ewwm-vr-keyboard-key-size 0.03)       ; Key size in meters
(setq ewwm-vr-keyboard-haptic t)             ; Haptic feedback
(setq ewwm-vr-keyboard-auto-show nil)        ; Auto-show on text focus
(setq ewwm-vr-keyboard-auto-capitalize t)    ; Auto-capitalize
(setq ewwm-vr-keyboard-prediction nil)       ; Predictive text (stub)
```

### IPC Commands

```elisp
(:type :keyboard-show)
(:type :keyboard-hide)
(:type :keyboard-toggle)
(:type :keyboard-layout :layout "dvorak")
(:type :keyboard-status)
```

---

## Performance Tuning

### Frame Timing

Monitor frame performance:

```elisp
M-x ewwm-vr-frame-timing
```

This displays:
- Wait, render, and submit times (p50)
- Total frame time (p50, p99)
- FPS and missed frame percentage

### Performance Targets

| Metric | Target | Critical |
|--------|--------|----------|
| Total frame time p50 | < 8ms | < 11ms (@ 90Hz) |
| Missed frames | < 1% | < 5% |
| IPC round-trip | < 1ms | < 5ms |

### Optimization Flags

```bash
# Maximum performance (native CPU, LTO)
export RUSTFLAGS="-C target-cpu=native -C lto=fat"

# AVX2 baseline (recommended for signal processing)
export RUSTFLAGS="-C target-cpu=x86-64-v3"
```

### GPU Profiling

```bash
# AMD
GALLIUM_HUD=fps,gpu-load,cpu+gpu ~/.local/bin/ewwm-compositor

# Intel
INTEL_DEBUG=perf ewwm-compositor

# NVIDIA
NVML_MONITOR=1 ewwm-compositor
```

### Scene Complexity

Reduce VR scene load:
- Use `stack` layout instead of `arc` for fewer transforms
- Lower PPU for non-focused surfaces
- Use `flat` projection instead of `cylinder`
- Reduce the number of visible surfaces

---

## Troubleshooting

### Black screen in HMD

1. Check DRM lease: `M-x ewwm-vr-display-info`
2. Verify connector: `(:type :vr-display-list-connectors)`
3. Check Monado: `monado-cli info`
4. Try headless mode: `(:type :vr-display-set-mode :mode :headless)`

### Tracking loss

1. Ensure lighthouse base stations have line-of-sight (Index/Vive)
2. Check USB bandwidth (use USB 3.0 ports)
3. Verify tracking in Monado: `monado-cli tracking`

### Frame drops

1. Check frame timing: `M-x ewwm-vr-frame-timing`
2. Reduce PPU: `(:type :vr-scene-set-ppu :ppu 1500)`
3. Switch to simpler layout: `(:type :vr-scene-set-layout :layout :stack)`
4. Check GPU temperature and throttling
5. Close unnecessary surfaces

### Hand tracking jitter

1. Increase smoothing: `(setq ewwm-vr-hand-smoothing 0.2)`
2. Increase confidence threshold: `(setq ewwm-vr-hand-min-confidence 0.7)`
3. Ensure good lighting (for camera-based tracking)
4. Keep hands within tracking volume

### VR session stuck in "idle"

1. Restart VR: `M-x ewwm-vr-restart`
2. Check Monado service: `systemctl --user restart monado`
3. Verify OpenXR runtime: `echo $XR_RUNTIME_JSON`
4. Check environment: `M-x ewwm-environment-check`

### Reference space issues

- `local`: Origin at initial head position; reset with `ewwm-vr-restart`
- `stage`: Origin at floor center; requires room setup in Monado
- `view`: Head-locked; useful for HUD overlays
