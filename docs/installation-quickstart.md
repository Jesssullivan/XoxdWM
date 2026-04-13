# XoxdWM Installation Quickstart

This quickstart is intentionally narrower than the feature inventory.

- For current support claims, read [Support Matrix](support-matrix.md).
- For the repo’s current operational status, read [Status](status.md).
- As of 2026-04-13, the Rocky base compositor package lane is public in `v0.5.1`.
- `yoga` validated the Rocky RPM install path: the compositor package installs, links cleanly on-host, and can be started in a bounded named-host run with `seatd`.
- The remaining Rocky follow-on is local session ergonomics on `yoga`, not the base RPM artifact itself.
- Monado integration and full VR enablement remain separate follow-on work after the base Rocky compositor package path.
- SELinux hardening and the BrainFlow BCI virtualenv remain separate opt-in package concerns so the base Rocky compositor RPM stays shippable.

## NixOS (Declarative)

Add to your `flake.nix` inputs:
```nix
inputs.xoxdwm.url = "github:Jesssullivan/XoxdWM";
```

Enable in your NixOS configuration:
```nix
{ inputs, ... }: {
  imports = [ inputs.xoxdwm.nixosModules.exwm-vr ];
  services.exwm-vr.enable = true;
  services.exwm-vr.monado.enable = true;  # optional: VR runtime
}
```

Home Manager (user config):
```nix
{ inputs, ... }: {
  imports = [ inputs.xoxdwm.homeManagerModules.exwm-vr ];
  programs.exwm-vr = {
    enable = true;
    compositor.backend = "auto";
  };
}
```

## Rocky Linux / Fedora (RPM)

This describes the released compositor package path. It does not, by itself, provision a full VR stack. The current public Rocky release is the native non-`vr` compositor package path; SELinux policy packaging, Monado integration, and the BrainFlow BCI virtualenv remain separate follow-on paths.

Download from GitHub Releases:
```bash
curl -LO https://github.com/Jesssullivan/XoxdWM/releases/latest/download/exwm-vr-compositor-*.x86_64.rpm
sudo dnf install ./exwm-vr-compositor-*.x86_64.rpm
```

Current status:

- `v0.5.1` is public and ships the corrected native Rocky compositor RPM.
- `yoga` validated package install and bounded runtime on Rocky 10.
- A polished local login/session path on `yoga` is still follow-on work after the base package lane.
- Full VR/OpenXR enablement on Rocky is still a separate follow-on step after the base compositor package path.
- SELinux policy packaging and the BrainFlow BCI virtualenv are separate follow-on steps after the base compositor package path.

Install Emacs and enable the session:
```bash
sudo dnf install emacs dbus-daemon seatd xorg-x11-server-Xwayland
sudo systemctl enable --now seatd
sudo usermod -aG seat "$USER"
# Log out and back in so the seat group reaches the local session/user manager.
# The display-manager or local-launch path is still being hardened on yoga.
```

## Debian / Ubuntu (DEB)

```bash
curl -LO https://github.com/Jesssullivan/XoxdWM/releases/latest/download/ewwm-compositor_*_amd64.deb
sudo apt install ./ewwm-compositor_*_amd64.deb
```

## From Source (Any Wayland System)

```bash
git clone https://github.com/Jesssullivan/XoxdWM.git
cd XoxdWM

# Build compositor (requires Rust 1.70+, wayland-devel, mesa, libinput, libxkbcommon)
cargo build --release --manifest-path compositor/Cargo.toml

# Install
sudo install -Dm755 compositor/target/release/ewwm-compositor /usr/local/bin/
sudo install -Dm644 packaging/systemd/exwm-vr-compositor.service /usr/lib/systemd/user/
sudo install -Dm644 packaging/desktop/exwm-vr.desktop /usr/share/wayland-sessions/exwm-vr.desktop
sudo install -Dm755 packaging/desktop/exwm-vr-session /usr/share/exwm-vr/exwm-vr-session

# Emacs packages
sudo mkdir -p /usr/share/emacs/site-lisp/exwm{,-vr}
sudo cp lisp/core/*.el /usr/share/emacs/site-lisp/exwm/
sudo cp lisp/vr/*.el /usr/share/emacs/site-lisp/exwm-vr/
```

## Headless / Server Mode

For environments without a GPU or display:
```bash
cargo build --release --manifest-path compositor/Cargo.toml --no-default-features
ewwm-compositor --backend headless --headless-outputs 2 --headless-resolution 1920x1080
```

## Verification

```bash
# Check compositor version
ewwm-compositor --version

# Test headless mode (exits after 5 seconds)
ewwm-compositor --backend headless --headless-exit-after 5

# Run ERT test suite
emacs --batch -L lisp/core -L lisp/vr -L lisp/ext -l test/run-tests.el
```

## Named-Host Guidance

- `yoga`: Rocky 10 package install is validated; local session ergonomics are the active follow-on.
- `honey`: target VR smoke host, but not currently documented here as a proven XoxdWM deployment.
