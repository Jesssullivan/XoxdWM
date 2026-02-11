# EXWM-VR User Guide

**Version 0.1.0** | **EWWM (Emacs Wayland Window Manager)**

A VR-first transhuman Emacs window manager built on Smithay, OpenXR, and
the full Emacs ecosystem.

---

## Table of Contents

1. [System Requirements](#system-requirements)
2. [NixOS Installation](#nixos-installation)
3. [Rocky Linux Installation](#rocky-linux-installation)
4. [First Boot Walkthrough](#first-boot-walkthrough)
5. [Configuration Reference](#configuration-reference)
6. [Keyboard Shortcuts](#keyboard-shortcuts)
7. [Workspace Management](#workspace-management)
8. [Window Tiling and Floating](#window-tiling-and-floating)
9. [Application Integration](#application-integration)
10. [Theming](#theming)
11. [Headless Mode](#headless-mode)
12. [Troubleshooting FAQ](#troubleshooting-faq)

---

## System Requirements

### Hardware

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | x86_64 (SSE4.2) | x86_64-v3 (AVX2) |
| RAM | 4 GB | 16 GB |
| GPU | Intel UHD 630 / AMD RX 560 | NVIDIA RTX 3070+ / AMD RX 6700+ |
| GPU Driver | Mesa 23.0+ (radeonsi, i915) | Mesa 24.0+ or NVIDIA 535+ |
| Display | Any Wayland-capable | 2560x1440+ |
| VR HMD | Valve Index (optional) | See [VR Guide](vr-guide.md) |
| Eye Tracker | Pupil Labs Core (optional) | Pupil Labs Neon |
| EEG | OpenBCI Cyton 8ch (optional) | Cyton+Daisy 16ch |

### Supported Operating Systems

| OS | Architecture | Support Level |
|----|-------------|---------------|
| NixOS unstable | x86_64 | Primary |
| NixOS unstable | aarch64 | 2D only (no VR) |
| Rocky Linux 9/10 | x86_64 | Secondary |
| Rocky Linux 9/10 | aarch64 | 2D only |
| Any Linux (s390x) | s390x | Headless only |

### Software Dependencies

- Emacs 30.x (pgtk build with native-comp)
- Wayland compositor libraries (wayland, libdrm, libinput, seatd)
- Monado OpenXR runtime (for VR)
- Qutebrowser (optional, for browser integration)
- KeePassXC (optional, for secrets management)

---

## NixOS Installation

### Flake-based Installation

Add EXWM-VR to your flake inputs:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    exwm-vr.url = "github:Jesssullivan/XoxdWM";
  };

  outputs = { self, nixpkgs, exwm-vr }: {
    nixosConfigurations.myhost = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        exwm-vr.nixosModules.exwm-vr
        exwm-vr.nixosModules.monado
        ./configuration.nix
      ];
    };
  };
}
```

### NixOS Module Configuration

In your `configuration.nix`:

```nix
{
  services.exwm-vr = {
    enable = true;
    compositor.enable = true;
    vr.enable = true;            # Enable VR support
    vr.monado.enable = true;     # Enable Monado runtime
    vr.monado.headset = "auto";  # auto, index, quest3, reverb-g2
    secrets.keepassxc = true;    # KeePassXC integration
    qutebrowser.enable = true;   # Browser integration
  };
}
```

### Home-Manager Module

For per-user configuration:

```nix
{
  imports = [ exwm-vr.homeManagerModules.exwm-vr ];

  programs.exwm-vr = {
    enable = true;
    config = {
      workspace-number = 6;
      layout-default = "tiling";
      layout-master-ratio = 0.55;
      vr-reference-space = "local";
      gaze-source = "auto";
      gaze-dwell-ms = 200;
      qutebrowser-auto-sync-theme = true;
    };
  };
}
```

### Building from Flake

```bash
# Build the compositor
nix build .#packages.x86_64-linux.compositor

# Build headless variant (s390x, CI)
nix build .#packages.x86_64-linux.compositor-headless

# Enter development shell
nix develop

# Build OCI container image
nix build .#packages.x86_64-linux.oci-image
```

---

## Rocky Linux Installation

### RPM Installation

```bash
# Enable EPEL and EXWM-VR repository
sudo dnf install epel-release
sudo dnf config-manager --add-repo https://rpm.exwm-vr.dev/rocky-9.repo

# Install main package
sudo dnf install exwm-vr

# Install optional subpackages
sudo dnf install exwm-vr-vr           # VR support
sudo dnf install exwm-vr-qutebrowser  # Browser integration
sudo dnf install exwm-vr-bci          # BCI support
sudo dnf install exwm-vr-selinux      # SELinux policy
sudo dnf install exwm-vr-headless     # Headless compositor
```

### SELinux Configuration

The SELinux policy confines three domains: the compositor, the BrainFlow
daemon, and the Monado runtime.

```bash
# Install and load SELinux policy
sudo dnf install exwm-vr-selinux
sudo semodule -i /usr/share/selinux/packages/exwm-vr.pp

# Verify policy is loaded
sudo semodule -l | grep exwm
```

### systemd Services

EXWM-VR uses user-level systemd units:

```bash
# Enable and start the compositor
systemctl --user enable ewwm-compositor.service
systemctl --user start ewwm-compositor.service

# Enable VR (if applicable)
systemctl --user enable monado.service
systemctl --user start monado.service

# Start the full stack
systemctl --user start ewwm.target
```

### Desktop Session

Select "EWWM" from your display manager (GDM, SDDM) session list. The
session wrapper at `/usr/share/wayland-sessions/exwm-vr.desktop` handles
environment setup, compositor launch, and Emacs startup.

---

## First Boot Walkthrough

1. **Log in** via display manager selecting the EWWM session
2. **Compositor starts**: the Smithay compositor launches and creates the
   Wayland display socket at `$XDG_RUNTIME_DIR/ewwm-ipc.sock`
3. **Emacs connects**: Emacs (pgtk) starts and `ewwm-ipc.el` connects
   to the compositor via the Unix domain socket
4. **Hello handshake**: Emacs sends `(:type :hello :version 1 :client "ewwm.el")`
   and receives feature flags (VR, XWayland status)
5. **Workspace ready**: 4 workspaces are initialized; workspace 0 is active
6. **Launch applications**: `s-r` opens `ewwm-launch`, type an application name

### Verify the Connection

```elisp
;; In *scratch* buffer or M-:
(ewwm-ipc-status)     ; Shows connected/disconnected, msg counts
(ewwm-ipc-ping)       ; Round-trip latency measurement
(ewwm-surface-list)   ; List all managed surfaces
(ewwm-workspace-list) ; List workspace state
```

### Environment Validation

Run the environment checker to verify all subsystems:

```elisp
M-x ewwm-environment-check
```

This validates: Wayland session, compositor binary, OpenXR runtime,
Monado service, BrainFlow daemon, serial ports, and GPU capabilities.

---

## Configuration Reference

All configuration is via Emacs `defcustom` variables. Set them in your
`init.el` or via `M-x customize-group RET ewwm RET`.

### Core Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-workspace-number` | integer | 4 | Number of workspaces |
| `ewwm-layout-default` | symbol | `tiling` | Default layout: tiling, monocle, grid, floating |
| `ewwm-layout-master-ratio` | float | 0.55 | Master window width ratio |
| `ewwm-ipc-socket-path` | string/nil | nil | IPC socket path (nil = auto-detect) |
| `ewwm-ipc-reconnect-max-delay` | integer | 30 | Max reconnect backoff (seconds) |
| `ewwm-ipc-sync-timeout` | number | 2 | Sync request timeout (seconds) |

### VR Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-vr-reference-space` | symbol | `local` | OpenXR reference space: local, stage, view |
| `ewwm-vr-mode-line` | boolean | t | Show VR status in mode-line |
| `ewwm-vr-frame-stats-interval` | integer | 5 | Frame stats interval (seconds) |

### Eye Tracking Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-vr-gaze-source` | symbol | `auto` | Gaze source: auto, openxr, pupil-labs, simulated, none |
| `ewwm-vr-gaze-smoothing` | number | 0.3 | EMA smoothing alpha (0=smooth, 1=raw) |
| `ewwm-vr-gaze-visualization` | symbol | `dot` | Visualization: dot, crosshair, spotlight, none |
| `ewwm-vr-eye-focus-policy` | symbol | `gaze-primary` | Policy: gaze-only, gaze-primary, gaze-assist, disabled |
| `ewwm-vr-eye-dwell-ms` | integer | 200 | Dwell time before focus switch (ms) |
| `ewwm-vr-eye-cooldown-ms` | integer | 500 | Cooldown between focus changes (ms) |
| `ewwm-vr-eye-saccade-threshold` | integer | 300 | Saccade velocity threshold (deg/s) |
| `ewwm-vr-eye-reading-detection` | boolean | t | Detect reading to prevent false focus |
| `ewwm-vr-pupil-port` | integer | 50020 | Pupil Capture ZMQ port |

### Hand Tracking Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-vr-hand-enable` | boolean | t | Enable hand tracking |
| `ewwm-vr-hand-min-confidence` | number | 0.5 | Minimum tracking confidence |
| `ewwm-vr-hand-smoothing` | number | 0.3 | Position smoothing alpha |
| `ewwm-vr-hand-dominant` | symbol | `right` | Dominant hand: left, right |

### BCI Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-bci-board-id` | integer | 0 | BrainFlow board ID (0=synthetic, 1=Cyton) |
| `ewwm-bci-serial-port` | string | `/dev/openbci` | Serial port for EEG board |
| `ewwm-bci-sample-rate` | integer | 250 | Sample rate (Hz) |
| `ewwm-bci-notch-frequency` | integer | 60 | Power line notch (50 or 60 Hz) |
| `ewwm-bci-data-retention-days` | integer | 90 | Days to retain session data |

### Qutebrowser Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-qutebrowser-app-id` | string | `org.qutebrowser.qutebrowser` | Wayland app_id |
| `ewwm-qutebrowser-command` | string | `qutebrowser` | Launch command |
| `ewwm-qutebrowser-ipc-method` | symbol | `fifo` | IPC method: fifo, socket |
| `ewwm-qutebrowser-auto-sync-theme` | boolean | t | Auto-sync Emacs theme |

---

## Keyboard Shortcuts

### Default Key Bindings

All global bindings use the Super key prefix (`s-`).

| Key | Action |
|-----|--------|
| `s-r` | Launch application |
| `s-1` through `s-4` | Switch to workspace 1-4 |
| `s-S-1` through `s-S-4` | Move surface to workspace 1-4 |
| `s-RET` | Open terminal |
| `s-q` | Close focused surface |
| `s-f` | Toggle floating |
| `s-F` | Toggle fullscreen |
| `s-l` | Cycle layout (tiling -> monocle -> grid) |
| `s-Tab` | Cycle focus to next surface |

### ewwm-mode Buffer Keys

When focus is on an ewwm-mode buffer:

| Key | Action |
|-----|--------|
| `q` | Close surface |
| `f` | Toggle floating |
| `F` | Toggle fullscreen |
| `m` | Move to workspace (prompts) |
| `i` | Surface info |

### Eye Tracking Keys (ewwm-vr-eye-mode)

| Key | Action |
|-----|--------|
| `C-c e c` | Start gaze calibration |
| `C-c e h` | Eye tracking health dashboard |
| `C-c e s` | Gaze status |
| `C-c e b` | Focus back (previous gaze target) |
| `C-c e p` | Set focus policy |
| `C-c e d` | Set dwell threshold |
| `C-c e a` | Gaze analytics dashboard |
| `C-c e C` | Focus configuration |

### Hand Tracking Keys (ewwm-vr-hand-mode)

| Key | Action |
|-----|--------|
| `C-c h s` | Hand tracking status |
| `C-c h t` | Toggle hand tracking |
| `C-c h c` | Send configuration |

---

## Workspace Management

EWWM provides a window-configuration-based workspace system. Each workspace
maintains its own Emacs window layout and surface assignments.

### Switching Workspaces

```elisp
(ewwm-workspace-switch 0)  ; Switch to workspace 0
(ewwm-workspace-switch 3)  ; Switch to workspace 3
```

Or use `s-1` through `s-4` for the default 4 workspaces.

### Moving Surfaces Between Workspaces

From an ewwm-mode buffer, press `m` and enter the target workspace number.

Programmatically:

```elisp
(ewwm-workspace-move-surface surface-id 2)
```

### Workspace Names

Workspaces are named `ws-0` through `ws-3` by default. Rename with:

```elisp
(aset ewwm-workspace--names 0 "code")
(aset ewwm-workspace--names 1 "web")
```

### Hooks

```elisp
(add-hook 'ewwm-workspace-switch-hook
          (lambda (from to)
            (message "Switched from ws %d to ws %d" from to)))
```

---

## Window Tiling and Floating

### Layout Modes

| Layout | Description |
|--------|-------------|
| `tiling` | Master-stack: primary window on left, stack on right |
| `monocle` | Single window fills frame |
| `grid` | Even grid of all windows |
| `floating` | No automatic tiling |

### Cycle Layouts

Press `s-l` to cycle through layouts, or:

```elisp
(ewwm-layout-cycle)
(ewwm-layout-set 'grid)
```

### Floating Windows

Toggle floating with `s-f` or `f` in ewwm-mode. Floating windows are
excluded from tiling layouts.

### Master Ratio

Adjust the master/stack split:

```elisp
(setq ewwm-layout-master-ratio 0.6)  ; 60% master, 40% stack
```

---

## Application Integration

### Qutebrowser

EWWM provides deep integration with Qutebrowser:

```elisp
;; Launch qutebrowser
(ewwm-qutebrowser-launch "https://example.com")

;; Navigate in focused qutebrowser
(ewwm-qutebrowser-open-url "https://emacs.org")
(ewwm-qutebrowser-open-url-new-tab "https://gnu.org")
(ewwm-qutebrowser-back)
(ewwm-qutebrowser-forward)
(ewwm-qutebrowser-reload)

;; Tab management (ewwm-qutebrowser-tabs.el)
;; Each qutebrowser tab is represented as an Emacs buffer

;; Consult integration (ewwm-qutebrowser-consult.el)
;; Fuzzy search bookmarks, history, and quickmarks

;; Gaze integration (ewwm-qutebrowser-gaze.el)
;; Edge-zone gaze scrolling and dwell-to-confirm link hints
```

### KeePassXC

Secrets integration via D-Bus Secret Service and NaCl browser protocol:

```elisp
;; Secrets are available via auth-source
(auth-source-search :host "github.com" :user "me")

;; Auto-type into focused surface
(ewwm-secrets-autotype-at-point)

;; Secure input mode (pauses biometrics during credential entry)
(ewwm-vr-secure-input-mode)
```

### Terminal Emulators

Launch any Wayland-native terminal:

```elisp
(ewwm-launch "foot")
(ewwm-launch "alacritty")
(ewwm-launch "kitty")
```

---

## Theming

### Emacs to Qutebrowser Theme Sync

When `ewwm-qutebrowser-auto-sync-theme` is `t`, Emacs theme colors are
automatically synced to qutebrowser on theme change. The sync covers:

- Background and foreground colors
- Status bar colors
- Tab bar colors
- Completion widget colors
- Hint styling

Manual sync:

```elisp
M-x ewwm-qutebrowser-sync-theme
```

### Reader Mode

Inject custom CSS for distraction-free reading via `ewwm-qutebrowser-reader.el`.

### Ad Blocking

Generate filter list configurations via `ewwm-qutebrowser-adblock.el`.

---

## Headless Mode

EXWM-VR supports headless operation for s390x, CI pipelines, and remote
sessions.

### Enabling Headless Mode

```elisp
;; Auto-detected in terminal Emacs
(ewwm-headless-mode)
```

Or build the headless compositor:

```bash
cargo build --release --no-default-features --features headless
```

### Headless IPC Commands

```elisp
;; Query headless status
(:type :headless-status)

;; Add/remove virtual outputs
(:type :headless-add-output)
(:type :headless-remove-output)

;; Set resolution
(:type :headless-set-resolution :w 1920 :h 1080)
```

### Use Cases

- **CI/CD**: Run ERT tests with workspace management
- **s390x mainframes**: Terminal Emacs with IPC-only workspace management
- **Remote sessions**: Emacs pgtk over VNC/SPICE
- **Automated testing**: IPC protocol validation

---

## Troubleshooting FAQ

### 1. Compositor fails to start: "Permission denied"

Ensure your user is in the `video` and `input` groups:

```bash
sudo usermod -aG video,input $USER
```

Then log out and back in.

### 2. Black screen on login

Check compositor logs:

```bash
journalctl --user -u ewwm-compositor -n 50
```

Common causes: missing GPU driver, wrong `WLR_RENDERER` setting, or
seatd not running.

### 3. IPC connection fails

Verify the socket exists:

```bash
ls -la $XDG_RUNTIME_DIR/ewwm-ipc.sock
```

Check that the compositor is running. In Emacs: `M-x ewwm-ipc-connect`.

### 4. Emacs shows "ewwm-ipc: connection failed"

The compositor may not be running yet. EWWM uses exponential backoff
reconnection (1s, 2s, 4s, ... up to 30s). Wait or manually reconnect.

### 5. VR headset not detected

Run `M-x ewwm-environment-check` to diagnose. Common fixes:

- Ensure Monado is running: `systemctl --user status monado`
- Check `XR_RUNTIME_JSON` environment variable
- Verify DRM lease support: check for non-desktop connectors

### 6. Eye tracking shows "tracking lost"

- Verify the eye tracker is connected (USB or wireless)
- Check Pupil Capture is running and ZMQ is enabled
- Run `M-x ewwm-vr-gaze-health` for diagnostics
- Try recalibrating: `M-x ewwm-vr-calibrate-eyes`

### 7. Gaze focus keeps switching too fast

Increase the dwell threshold and cooldown:

```elisp
(setq ewwm-vr-eye-dwell-ms 400)
(setq ewwm-vr-eye-cooldown-ms 800)
```

Or switch to `gaze-assist` policy which requires confirmation.

### 8. BCI board not connecting

- Check serial port: `ls /dev/ttyUSB*`
- Create a udev rule or symlink: `/dev/openbci -> /dev/ttyUSB0`
- Verify BrainFlow daemon: `M-x ewwm-bci-status`
- Try synthetic mode first: `(setq ewwm-bci-board-id 0)`

### 9. Qutebrowser commands not working

- Verify qutebrowser is running and detected: `M-x ewwm-qutebrowser-current-surface`
- Check FIFO exists: `ls $XDG_RUNTIME_DIR/qutebrowser/`
- Ensure `ewwm-qutebrowser-app-id` matches your qutebrowser's `app_id`

### 10. KeePassXC secrets not available

- Verify KeePassXC has Secret Service integration enabled
  (Settings > Secret Service Integration > Enable)
- Check D-Bus: `M-x ewwm-secrets-status`
- Ensure the database is unlocked

### 11. High latency / frame drops

- Check compositor frame timing: `M-x ewwm-vr-frame-timing`
- Verify GPU driver: `glxinfo | grep "OpenGL renderer"`
- Reduce VR scene complexity: `(:type :vr-scene-set-layout :layout :stack)`
- Profile IPC: `M-x ewwm-ipc-benchmark`

### 12. XWayland applications not appearing

Verify XWayland is enabled in the compositor hello response:

```elisp
;; Check hello response features
;; Should show :xwayland t
```

### 13. Pre-commit hook byte-compile errors

Ensure the load-path includes all module directories:

```bash
emacs --batch -L lisp/core -L lisp/vr -L lisp/ext -f batch-byte-compile FILE
```

### 14. Layout not updating after surface changes

Force a layout refresh:

```elisp
(ewwm-layout--apply-current
 (ewwm--buffers-on-workspace ewwm-workspace-current-index))
```

### 15. Compositor crash recovery

The IPC client automatically reconnects with exponential backoff. If the
compositor crashes, Emacs preserves all workspace state and reconnects
when the compositor restarts. Surface buffers remain in Emacs; they are
re-associated on reconnection.

### 16. Mode-line cluttered with status indicators

Disable individual indicators:

```elisp
(setq ewwm-vr-mode-line nil)        ; Hide VR status
(setq ewwm-vr-gaze-mode-line nil)   ; Hide gaze status
(setq ewwm-vr-hand-enable nil)      ; Hide hand status
```
