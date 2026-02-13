# Rocky Linux 10 + Nix Deployment Guide

Deploy EXWM-VR (XoxdWM) on Rocky Linux 10 with Nix managing the compositor,
Emacs, and user configuration. Rocky provides the stable kernel, drivers, and
SELinux policy; Nix manages reproducible packages and systemd user services.

## Prerequisites

### Tier 1: Headless (s390x, CI, containers)
- Rocky Linux 10 minimal install
- 2 GB RAM, 10 GB disk for Nix store
- Network access for initial Nix package fetches

### Tier 2: Desktop (flat Wayland)
- Rocky Linux 10 minimal or workstation
- GPU with Wayland support (Intel i915+, AMD amdgpu, NVIDIA 535+)
- `mesa-dri-drivers`, `libinput`, `libseat` system packages
- 4 GB RAM recommended

### Tier 3: VR
- Everything in Tier 2
- USB-connected HMD (Valve Index, Meta Quest via Link, etc.)
- Kernel with DRM lease support (Rocky 10 default kernel is sufficient)
- Monado runtime or WiVRn for standalone headsets

## 1. Install Nix

### Single-user install (desktop use)

```bash
curl -L https://nixos.org/nix/install | sh -s -- --no-daemon
source ~/.nix-profile/etc/profile.d/nix.sh
```

### Multi-user daemon install (servers, shared systems)

```bash
curl -L https://nixos.org/nix/install | sh -s -- --daemon
# Follow the prompts; the installer creates nix-daemon.service
```

### Enable flakes

```bash
mkdir -p ~/.config/nix
echo "experimental-features = nix-command flakes" > ~/.config/nix/nix.conf
```

### Verify

```bash
nix --version
nix flake show github:Jesssullivan/XoxdWM
```

## 2. Install home-manager

### Standalone install (recommended for Rocky)

```bash
nix-channel --add \
  https://github.com/nix-community/home-manager/archive/master.tar.gz \
  home-manager
nix-channel --update
nix-shell '<home-manager>' -A install
```

### Verify

```bash
home-manager --version
```

## 3. Configure home-manager

### Basic configuration

Create `~/.config/home-manager/home.nix`:

```nix
{ config, pkgs, ... }:

let
  exwm-vr = builtins.getFlake "github:Jesssullivan/XoxdWM";
in {
  imports = [
    exwm-vr.homeManagerModules.exwm-vr
  ];

  programs.exwm-vr = {
    enable = true;
    theme = "modus-vivendi";

    # Nix-to-Elisp config mapping (snake_case -> kebab-case)
    config = {
      ewwm_workspace_number = 4;
      ewwm_floating_border_width = 2;
      ewwm_vr_enable = false;
    };
  };

  home.username = "youruser";
  home.homeDirectory = "/home/youruser";
  home.stateVersion = "24.05";
}
```

### Full configuration with systemd services

This is the recommended setup for Rocky Linux — home-manager generates
compositor and Emacs systemd user services so `systemctl --user` manages
the full session lifecycle:

```nix
{ config, pkgs, ... }:

let
  exwm-vr = builtins.getFlake "github:Jesssullivan/XoxdWM";
in {
  imports = [
    exwm-vr.homeManagerModules.exwm-vr
  ];

  programs.exwm-vr = {
    enable = true;
    theme = "modus-vivendi";

    config = {
      ewwm_workspace_number = 4;
      ewwm_floating_border_width = 2;
      ewwm_vr_enable = false;
    };

    # Compositor binary from the flake
    compositor.package = exwm-vr.packages.x86_64-linux.compositor;
    compositor.extraArgs = [ ];  # e.g. [ "--backend" "drm" ]

    # Elisp load-path package
    elisp.package = exwm-vr.packages.x86_64-linux.ewwm-elisp;

    # Generate systemd user services
    systemd.enable = true;
  };

  home.username = "youruser";
  home.homeDirectory = "/home/youruser";
  home.stateVersion = "24.05";
}
```

### Apply

```bash
home-manager switch
```

This generates:
- `~/.config/exwm-vr/config.el` — Elisp configuration
- `~/.config/qutebrowser/exwm-vr-theme.py` — Qutebrowser theme
- `~/.config/systemd/user/ewwm-compositor.service` (if systemd.enable)
- `~/.config/systemd/user/ewwm-emacs.service` (if systemd.enable)
- `~/.config/systemd/user/ewwm-session.target` (if systemd.enable)

## 4. Config variable mapping

Nix option keys use `snake_case` and are converted to Elisp `kebab-case`:

| Nix key                        | Elisp variable                   | Type    | Default |
|--------------------------------|----------------------------------|---------|---------|
| `ewwm_workspace_number`        | `ewwm-workspace-number`          | int     | 4       |
| `ewwm_floating_border_width`   | `ewwm-floating-border-width`     | int     | 2       |
| `ewwm_vr_enable`               | `ewwm-vr-enable`                 | bool    | false   |
| `ewwm_vr_reference_space`      | `ewwm-vr-reference-space`        | string  | "local" |
| `ewwm_headless_resolution`     | `ewwm-headless-resolution`       | string  | "1920x1080" |
| `ewwm_bci_enable`              | `ewwm-bci-enable`                | bool    | false   |

Any `snake_case` key in `programs.exwm-vr.config` is automatically converted.

## 5. Build packages manually

If not using systemd services, build packages directly:

```bash
# Full compositor (GPU + VR)
nix build github:Jesssullivan/XoxdWM#compositor
./result/bin/ewwm-compositor --backend drm

# Headless only (no GPU required)
nix build github:Jesssullivan/XoxdWM#compositor-headless
./result/bin/ewwm-compositor --headless

# Elisp package (for Emacs load-path)
nix build github:Jesssullivan/XoxdWM#ewwm-elisp
ls result/share/emacs/site-lisp/ewwm/{core,vr,ext}/
```

## 6. SELinux

### Loading the Nix store path policy

Rocky Linux enforces SELinux by default. Nix store binaries need appropriate
file context labels:

```bash
# Install policy build tools
sudo dnf install -y selinux-policy-devel policycoreutils-python-utils

# Build and install the Nix policy module
just selinux-build
just selinux-install
```

### Label the compositor binary

Use the provided helper script to label the Nix store path:

```bash
sudo ./packaging/selinux/label-nix-compositor.sh
# Or specify a path explicitly:
sudo ./packaging/selinux/label-nix-compositor.sh /nix/store/abc123-ewwm-compositor/bin/ewwm-compositor
```

### Verify labels

```bash
ls -Z ~/.nix-profile/bin/ewwm-compositor
# Should show: system_u:object_r:exwm_vr_nix_exec_t:s0
```

### Fallback: audit2allow

If you encounter AVC denials not covered by the policy:

```bash
# Check recent denials
sudo ausearch -m AVC -ts recent | grep ewwm

# Generate and install a custom policy from denials
sudo audit2allow -a -M exwm-vr-local
sudo semodule -i exwm-vr-local.pp

# Or temporarily set permissive for debugging
sudo setenforce 0
# ... reproduce the issue ...
sudo ausearch -m AVC -ts recent
sudo setenforce 1
```

## 7. Systemd user services

### With home-manager (recommended)

After `home-manager switch` with `systemd.enable = true`:

```bash
# Reload systemd user daemon
systemctl --user daemon-reload

# Start the full session
systemctl --user start ewwm-session.target

# Check status
systemctl --user status ewwm-compositor.service
systemctl --user status ewwm-emacs.service

# View logs
journalctl --user -u ewwm-compositor.service -f
journalctl --user -u ewwm-emacs.service -f

# Stop everything
systemctl --user stop ewwm-session.target

# Enable auto-start on login
systemctl --user enable ewwm-session.target
```

### Without home-manager

Copy the packaged service files:

```bash
mkdir -p ~/.config/systemd/user
cp packaging/systemd/exwm-vr-compositor.service ~/.config/systemd/user/ewwm-compositor.service
cp packaging/systemd/exwm-vr-emacs.service ~/.config/systemd/user/ewwm-emacs.service
cp packaging/systemd/exwm-vr.target ~/.config/systemd/user/ewwm-session.target

# Edit ExecStart paths to point to Nix store results
vim ~/.config/systemd/user/ewwm-compositor.service

systemctl --user daemon-reload
systemctl --user start ewwm-session.target
```

## 8. Headless server deployment

### SSH-forwarded headless compositor

Useful for s390x mainframes or remote development:

```bash
# On the server
nix build github:Jesssullivan/XoxdWM#compositor-headless
./result/bin/ewwm-compositor --headless --headless-outputs 2

# From your workstation
ssh -L /tmp/ewwm-remote.sock:$XDG_RUNTIME_DIR/ewwm-ipc.sock user@server
```

### OCI container deployment

```bash
# Build the headless OCI image
nix build .#oci-headless

# Load into podman/docker
./result | podman load
podman run --rm -it ewwm-compositor-headless:latest
```

### s390x cross-compilation

The flake provides cross-compiled headless binaries:

```bash
nix build .#packages.s390x-linux.compositor-headless
```

## 9. Upgrading

### Flake-based update

```bash
# Update the flake input to latest
nix flake update --flake ~/.config/home-manager

# Rebuild and switch
home-manager switch

# Restart services to pick up new binaries
systemctl --user restart ewwm-session.target
```

### Pinning a version

In your `home.nix`, pin to a specific commit or tag:

```nix
let
  exwm-vr = builtins.getFlake "github:Jesssullivan/XoxdWM/v0.5.0";
in { ... }
```

## 10. Troubleshooting

### SELinux denials

```bash
# Most common: Nix store binary not labeled
sudo ausearch -m AVC -ts recent | grep ewwm
sudo ./packaging/selinux/label-nix-compositor.sh

# If denials persist after labeling
sudo audit2allow -a | grep ewwm
```

### Socket permissions

```bash
# Verify IPC socket exists and has correct ownership
ls -la "$XDG_RUNTIME_DIR/ewwm-ipc.sock"

# If XDG_RUNTIME_DIR is unset
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
```

### Emacs load-path issues

```bash
# Verify ewwm-elisp package contents
nix build .#ewwm-elisp
ls result/share/emacs/site-lisp/ewwm/core/
ls result/share/emacs/site-lisp/ewwm/vr/

# Test loading manually
emacs --batch \
  -L result/share/emacs/site-lisp/ewwm/core \
  -L result/share/emacs/site-lisp/ewwm/vr \
  -L result/share/emacs/site-lisp/ewwm/ext \
  --eval '(require (quote ewwm-core))'
```

### Missing native libraries

```bash
# Common missing deps on Rocky 10
sudo dnf install -y \
  mesa-dri-drivers \
  libinput \
  libseat \
  libxkbcommon \
  libdrm

# For VR
sudo dnf install -y \
  vulkan-loader \
  mesa-vulkan-drivers
```

### Compositor won't start

```bash
# Check the journal for errors
journalctl --user -u ewwm-compositor.service --no-pager -n 50

# Common issues:
# 1. No DRM device access -> add user to video group
sudo usermod -aG video $USER

# 2. No seat access -> ensure seatd/logind is running
loginctl show-session $(loginctl | grep $USER | awk '{print $1}') -p Seat

# 3. Headless mode sanity check
./result/bin/ewwm-compositor --headless &
sleep 2
test -S "$XDG_RUNTIME_DIR/ewwm-ipc.sock" && echo "OK" || echo "FAIL"
kill %1
```

### Home-manager switch fails

```bash
# Check for Nix evaluation errors
home-manager switch --show-trace

# Verify flake is accessible
nix flake show github:Jesssullivan/XoxdWM

# Clear Nix evaluation cache
nix store gc
home-manager switch
```
