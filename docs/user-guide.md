# XoxdWM User Guide

Reference guide for installation, operation, and subsystem workflows.

This document is broader than the currently validated support surface. For
named-host truth and Rocky / Linux build authority, read
[support-matrix.md](support-matrix.md) and [status.md](status.md) first.
For the external authority split and the current repo-owned remote workflow map,
read [remote-build-authority.md](remote-build-authority.md) and
[remote-proof-lanes.md](remote-proof-lanes.md).

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

Add XoxdWM to your flake inputs:

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

### Linux Build Outputs From The Flake

These commands are for Linux targets, CI, or explicit remote build lanes. Do
not treat them as the default authority path from `neo`.

```bash
# Build the compositor on a Linux build surface
nix build .#packages.x86_64-linux.compositor

# Build headless variant
nix build .#packages.x86_64-linux.compositor-headless

# Enter the Linux-oriented development shell
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
sudo dnf install exwm-vr-selinux      # optional SELinux hardening, when published
sudo dnf install exwm-vr-headless     # Headless compositor
```

### SELinux Configuration

The SELinux policy confines three domains: the compositor, the BrainFlow
daemon, and the Monado runtime. On the current Rocky MVP lane, this is a
separate hardening follow-on step rather than a gate on the base compositor
RPM.

```bash
# Install and load SELinux policy
sudo dnf install exwm-vr-selinux
sudo semodule -i /usr/share/selinux/packages/exwm-vr.pp

# Verify policy is loaded
sudo semodule -l | grep exwm
```

### systemd Services

The RPM-packaged Rocky session uses user-level `exwm-vr-*` systemd units:

```bash
# Enable and start the compositor
systemctl --user enable exwm-vr-compositor.service
systemctl --user start exwm-vr-compositor.service

# Enable VR (if Monado integration is installed)
systemctl --user enable exwm-vr-monado.service
systemctl --user start exwm-vr-monado.service

# Start the full stack
systemctl --user start exwm-vr.target
```

On the packaged Rocky lane, host-specific direct-mode settings now have a
supported user-scoped config surface instead of requiring arbitrary unit edits:

```bash
mkdir -p ~/.config/exwm-vr

hmd_connector=$(/usr/libexec/exwm-vr/hmd-connector --format name) || {
  echo "No live HMD connector resolved; set EXWM_VR_HMD_CONNECTOR=DP-n and rerun" >&2
  exit 1
}

cat > ~/.config/exwm-vr/compositor.env <<EOF
EWWM_DRM_LEASE_CONNECTORS=${hmd_connector}
EOF

cat > ~/.config/exwm-vr/monado.env <<EOF
# Optional for hosts that still use a local Monado build:
# MONADO_SERVICE_BIN=/usr/local/bin/monado-service
XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1
XRT_COMPOSITOR_WAYLAND_CONNECTOR=${hmd_connector}
WAYLAND_DISPLAY=wayland-0
STEAMVR_LH_ENABLE=1
XRT_COMPOSITOR_COMPUTE=1
LH_OVERRIDE_IPD_MM=64
EOF

systemctl --user daemon-reload
```

The packaged `exwm-vr-compositor.service` reads
`~/.config/exwm-vr/compositor.env`, and the packaged
`exwm-vr-monado.service` reads `~/.config/exwm-vr/monado.env`. Its launcher
defaults to `/usr/bin/monado-service`, but `MONADO_SERVICE_BIN` can point the
service at a local `/usr/local/bin/monado-service` build on hosts like
`honey`.

### Desktop Session

Select "EXWM-VR" from your display manager (GDM, SDDM) session list. The
session wrapper at `/usr/share/wayland-sessions/exwm-vr.desktop` handles
environment setup and compositor launch. Emacs/eGreg is an optional
application/control layer; set `EXWM_VR_START_EMACS=1` when the packaged
Emacs compatibility service should also start.

On `yoga`, this greeter-driven session path has now been smoke-validated once
through SDDM on `seat0` using the installed package surface, and the packaged
`SuccessExitStatus=15` stop-path fix is now present on-host without a separate
unit override.

On the packaged Rocky session lane, optional Emacs startup uses the dedicated
`/usr/share/exwm-vr/exwm-vr-session-init.el` bootstrap. That session entrypoint
avoids ambient `~/.emacs` / `init.el` state and optionally loads
`~/.config/exwm-vr/config.el` instead.

---

## First Boot Walkthrough

1. **Log in** via display manager selecting the EXWM-VR session
2. **Compositor starts**: the Smithay compositor launches and creates the
   Wayland display socket at `$XDG_RUNTIME_DIR/wayland-0`
3. **IPC socket appears**: the compositor also binds its control socket at
   `$XDG_RUNTIME_DIR/ewwm-ipc.sock`
4. **Optional Emacs/eGreg connects**: if `EXWM_VR_START_EMACS=1` or an
   external eGreg daemon is used, Emacs (pgtk) starts with
   `WAYLAND_DISPLAY=wayland-0` and `ewwm-ipc.el` connects to the compositor
   via the Unix domain socket
5. **Optional hello handshake**: Emacs sends
   `(:type :hello :version 1 :client "ewwm.el")` and receives feature flags
   (VR, XWayland status)
6. **Workspace ready**: 4 workspaces are initialized; workspace 0 is active
7. **Launch applications**: `s-RET` launches the configured terminal target;
   optional native autostart launches named targets when enabled in compositor
   config

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

Configuration is split between native compositor startup settings and the
Emacs/eGreg application layer. Native compositor settings are loaded before the
Wayland backend starts; Emacs `defcustom` variables remain for editor,
diagnostic, and compatibility-client behavior.

### Native Compositor Config

The compositor loads native JSON config from
`$XDG_CONFIG_HOME/exwm-vr/compositor.json`, or
`~/.config/exwm-vr/compositor.json` when `XDG_CONFIG_HOME` is unset. A missing
default config file is deterministic: the compositor logs the missing path and
uses built-in defaults.

Use `--config /path/to/compositor.json` to load an explicit file. An explicit
config path must exist and parse successfully; this prevents typoed operator
paths from silently falling back to defaults.

IPC socket precedence is:

1. `ewwm-compositor --ipc-socket /path/to/socket`
2. native JSON `ipc_socket_path`
3. `$XDG_RUNTIME_DIR/ewwm-ipc.sock`

Minimal native config:

```json
{
  "ipc_socket_path": "/run/user/1000/ewwm-ipc.sock",
  "workspace_count": 4,
  "active_workspace": 0,
  "layout_default": "tiling",
  "layout_master_ratio": 0.55,
  "workspace_app_rules": "firefox=1,foot=0",
  "floating_app_ids": "pavucontrol,org.keepassxc.KeePassXC",
  "key_action_bindings": "s-1=workspace:0,s-2=workspace:1,s-RET=launch:terminal,s-SPC=layout:cycle,s-j=focus:next,s-k=focus:previous,s-r=compositor:reload,s-q=compositor:exit",
  "app_launch_commands": "terminal=foot,browser=firefox,launcher=rofi -show drun",
  "autostart_enabled": false,
  "autostart_targets": "",
  "session_lock_command": "swaylock",
  "session_idle_enabled": false,
  "session_idle_command": "",
  "vr_enabled": false,
  "passthrough_blend_mode": "opaque",
  "passthrough_opacity": 1.0,
  "gaze_zone_layout": "default",
  "gaze_zone_custom_map": "",
  "default_scale": 1.0,
  "cursor_size": 24
}
```

### Emacs Application Config

On the packaged Rocky session lane, place Emacs/eGreg application-layer
settings in `~/.config/exwm-vr/config.el`. For non-packaged development flows,
you can still set them in your regular `init.el` or via
`M-x customize-group RET ewwm RET`.

### Core Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `ewwm-workspace-number` | integer | 4 | Emacs app-layer workspace count; native compositor startup uses JSON `workspace_count` |
| `ewwm-layout-default` | symbol | `tiling` | Emacs app-layer default layout; native compositor startup uses JSON `layout_default` |
| `ewwm-layout-master-ratio` | float | 0.55 | Emacs app-layer master ratio; native compositor startup uses JSON `layout_master_ratio` |
| `ewwm-ipc-socket-path` | string/nil | nil | IPC socket path (nil = auto-detect) |
| `ewwm-ipc-reconnect-max-delay` | integer | 30 | Max reconnect backoff (seconds) |
| `ewwm-ipc-sync-timeout` | number | 2 | Sync request timeout (seconds) |

### Native Manage Policy

The native compositor config supports an early exact-match policy surface for
common app placement. `workspace_app_rules` is a comma-separated
`app-id=workspace` list. `floating_app_ids` is a comma-separated list matched
against Wayland app IDs and XWayland class/instance strings.

```json
{
  "workspace_app_rules": "firefox=1,foot=0",
  "floating_app_ids": "pavucontrol,org.keepassxc.KeePassXC"
}
```

This is intentionally smaller than the historical Lisp predicate rule engine.
Use it for stable startup policy while richer native rule matching is built.

### Native Workspace Visibility

Workspace membership is native compositor state. Inactive workspace surfaces are
removed from the compositor space, and active workspace surfaces are remapped
before layout reflow. This means `workspace-switch` and
`workspace-move-surface` affect what the compositor displays even when no
Emacs/eGreg IPC client is running.

Floating and manually moved surfaces retain their last compositor geometry for
workspace visibility changes. Tiled surfaces are then placed by the active
native layout mode.

### Native Layout Policy

The compositor reflows non-floating surfaces on the active workspace without
requiring Emacs/eGreg in the WM path. Native reflow runs after new Wayland or
XWayland windows map, workspace switches, surface workspace moves, float
toggles, layout changes, and native config reloads.

`layout_default` may be `tiling`, `monocle`, `grid`, or `floating`. `tiling`
uses `layout_master_ratio` for the master area and stacks remaining windows on
the right. `monocle` sizes each tiled surface to the usable output area. `grid`
uses a deterministic row/column layout. `floating` leaves surface geometry under
explicit move/resize or client policy.

Emacs/eGreg can still observe and request layout changes through IPC, but the
basic placement behavior is native compositor policy.
The `ewwm-layout-set` and `ewwm-layout-cycle` helpers request native
`:layout-set` / `:layout-cycle` when connected, and mirror native
`:layout-changed` events into their app-layer state.

### Native Key Actions

Native key actions are compositor-owned startup policy. They are handled before
IPC key grabs so core WM behavior does not require Emacs/eGreg in the input
path.

```json
{
  "key_action_bindings": "s-1=workspace:0,s-2=workspace:1,s-RET=launch:terminal,s-SPC=layout:cycle,s-j=focus:next,s-k=focus:previous,s-r=compositor:reload,s-q=compositor:exit",
  "app_launch_commands": "terminal=foot,browser=firefox,launcher=rofi -show drun"
}
```

Supported native actions are `workspace:N`, `focus:next`, `focus:previous`,
`layout:cycle`, `launch:NAME`, `compositor:reload`, and `compositor:exit`.
`launch:NAME` resolves through `app_launch_commands` and runs from the
compositor process; keep commands short and user-scoped. Emacs/eGreg can still
register IPC key grabs for app-layer behavior, but those grabs are no longer the
only route for core workspace, focus, launch, layout, and exit behavior.

The bundled Emacs/eGreg default key helpers also prefer native IPC for core
workspace switches, surface moves, layout cycling, and config reload when a
compositor connection exists. Their historical Lisp commands remain disconnected
fallbacks for app-layer debugging and editor-only sessions.

The same launch table is available to IPC clients through configured targets:

```elisp
(:type :app-launch-list :id 14)
(:type :app-launch :name "terminal")
```

The compositor launches only names present in `app_launch_commands`; arbitrary
shell command strings stay outside this IPC surface.
Emacs/eGreg can use `ewwm-launch-native-target` and
`ewwm-launch-native-target-list` for this native target surface while keeping
`ewwm-launch` as an app-layer arbitrary command fallback.

Native config reload is also available over IPC:

```elisp
(:type :config-reload :id 16)
```

The compositor reloads `~/.config/exwm-vr/compositor.json`, reapplies native
workspace/layout/app policy, and emits a `config-reloaded` event. Emacs/eGreg
may request this as an app-layer client, but the resulting policy remains
compositor-owned. Missing config reloads built-in defaults; invalid config
returns an IPC error instead of silently replacing runtime policy.

### Native Autostart Policy

Native autostart is compositor-owned session launch policy for named targets in
`app_launch_commands`. It is disabled by default so headless checks and smoke
runs do not launch applications unexpectedly.

```json
{
  "app_launch_commands": "terminal=foot,browser=firefox,launcher=rofi -show drun",
  "autostart_enabled": true,
  "autostart_targets": "terminal,browser"
}
```

The real-session DRM and winit backends run enabled autostart targets after
the Wayland display is ready. The compositor records launched targets for the
session and skips duplicates unless IPC explicitly requests a forced run.

```elisp
(:type :autostart-list :id 17)
(:type :autostart-run :id 18)
(:type :autostart-run :id 19 :force t)
```

This deliberately does not replace the full historical XDG `.desktop`
autostart parser in `ewwm-autostart.el` yet. That Lisp code remains
compatibility and app-layer scaffolding until a native desktop-entry parser is
planned and tested.

### Native Session Lock

The compositor owns the session-lock protocol and exposes a small native lock
launcher so Emacs/eGreg can request a lock without owning the session command
path.

```json
{
  "session_lock_command": "swaylock"
}
```

```elisp
(:type :session-status :id 20)
(:type :session-lock :id 21)
(:type :session-logout :id 22)
```

`:session-lock` launches `session_lock_command`; the actual locked state is
reported by the Wayland session-lock protocol when a locker client takes the
lock. `:session-logout` is a compatibility alias for compositor shutdown and
sets the native runtime shutdown flag. Shutdown, reboot, suspend, and
hibernate remain app-layer/operator policy until XoxdWM has a mediated native
power-management contract.

### Native Idle Supervision

The compositor can supervise one configured idle daemon process for real
sessions. This is disabled by default; enable it only with an explicit
user-scoped command.

```json
{
  "session_idle_enabled": true,
  "session_idle_command": "swayidle -w timeout 300 swaylock"
}
```

```elisp
(:type :session-idle-status :id 23)
(:type :session-idle-start :id 24)
(:type :session-idle-stop :id 25)
```

The DRM and winit backends start the configured idle daemon after the Wayland
display is ready when `session_idle_enabled` is true. Emacs/eGreg
`ewwm-session-start-idle`, `ewwm-session-stop-idle`, and `ewwm-session-status`
request the native IPC surface when connected and keep their existing fallback
behavior for app-layer use.

DPMS remains native compositor IPC through `:dpms-get` and `:dpms-set`.
The Emacs compatibility functions `ewwm-dpms-get` and `ewwm-dpms-set` are IPC
clients rather than local display-power policy.

### VR Settings

Native compositor config includes the passthrough defaults used by the VR
scene IPC compatibility surface:

```json
{
  "passthrough_blend_mode": "opaque",
  "passthrough_opacity": 1.0
}
```

The Emacs/eGreg passthrough commands are app-layer IPC clients; the runtime
scene background, blend mode, and opacity state live in the compositor.

Gaze-zone layout is also compositor-owned at runtime. Set
`gaze_zone_layout` to `default`, `vim-like`, `spacemacs`, or `custom`. For a
custom layout, provide `gaze_zone_custom_map` as comma-separated
`zone=modifier` entries:

```json
{
  "gaze_zone_layout": "custom",
  "gaze_zone_custom_map": "top-left=SPC,top-right=M-x,bottom-left=C-,bottom-right=M-,center="
}
```

Spatial anchor commands are compositor-local scene anchors. The compositor
stores named surface poses and can reapply/focus them at runtime; this is not
XR_EXT spatial-anchor persistence. The Emacs/eGreg app layer may still persist
the anchor list as JSON and restore it over IPC.

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
| `s-1` through `s-4` | Switch to workspace 1-4 |
| `s-S-1` through `s-S-4` | Move surface to workspace 1-4 |
| `s-RET` | Open terminal |
| `s-b` | Open browser |
| `s-SPC` | Cycle layout |
| `s-j` / `s-k` | Focus next / previous surface |
| `s-r` | Reload native compositor config |
| `s-q` | Exit compositor |
| IPC `:surface-close` | Close focused or selected surface |
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

Native XoxdWM owns the current layout mode. Emacs/eGreg can still request
changes as a control client:

```elisp
(ewwm-ipc-send '(:type :layout-get))
(ewwm-ipc-send '(:type :layout-set :layout :grid))
(ewwm-layout-cycle)
(ewwm-layout-set 'grid)
```

### Floating Windows

Toggle floating with `s-f` or `f` in ewwm-mode. Floating windows are
excluded from tiling layouts.

Emacs/eGreg sends explicit `:enable` values for `:surface-float` and
`:surface-fullscreen` requests, then mirrors native
`:surface-float-changed` and `:surface-workspace-changed` events back into the
surface buffers. Treat the buffer state as an app-layer view of native
compositor state, not the source of WM truth.

### Master Ratio

Adjust the native startup default in `~/.config/exwm-vr/compositor.json`:

```json
{
  "layout_default": "tiling",
  "layout_master_ratio": 0.6
}
```

Emacs app-layer layout helpers can also use their local ratio:

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

XoxdWM supports headless operation for s390x, CI pipelines, and remote
sessions.

### Enabling Headless Mode

```elisp
;; Auto-detected in terminal Emacs
(ewwm-headless-mode)
```

Or, on a Linux target, build the headless compositor directly:

```bash
cargo build --release --no-default-features
```

The native-authority smoke proves the compositor can start as a headless
Wayland process without an Emacs or EXWM service owning startup:

```bash
just boot-without-emacs-smoke 1
```

That smoke is a process/startup gate, not a visual-product gate. It should pass
before treating Emacs/eGreg as optional application clients rather than the
WM authority.

For the Linux visual/runtime gate, use the native-authority proof lane:

```bash
just native-authority-proof
```

On an installed Rocky package, the same helper is available as:

```bash
xoxdwm-native-authority-proof
```

To run the proof over the existing remote host wrapper:

```bash
just native-authority-proof-remote honey 0  # read-only IPC proof
just native-authority-proof-remote yoga 1   # mutating proof while observing UI
just native-authority-proof-remote yoga 1 terminal  # also request app launch
```

That command probes the live compositor IPC surface, verifies the Emacs service
is not the WM authority path, and exercises native workspace/layout commands.
Record human-visible app launch, focus, workspace visibility, and layout reflow
in [native-authority-runtime-proof-template.md](native-authority-runtime-proof-template.md).

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
journalctl --user -u exwm-vr-compositor.service -n 50
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
