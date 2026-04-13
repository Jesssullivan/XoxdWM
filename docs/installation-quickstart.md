# XoxdWM Installation Quickstart

This quickstart is intentionally narrower than the feature inventory.

- For current support claims, read [Support Matrix](support-matrix.md).
- For the repo’s current operational status, read [Status](status.md).
- As of 2026-04-12, the Rocky path is not yet a claimed working named-host install path.
- The current public `v0.5.0` RPM failed on `honey`: its metadata requires bare `wayland` on Rocky 10 and the packaged compositor binary points at a `/nix/store/.../ld-linux...` interpreter.
- Until a corrected release is cut, treat the Rocky RPM section as release-surface documentation, not as a supported install path.
- The active `0.5.1` repair lane is targeting a native Rocky compositor RPM without the `vr` Cargo feature; full VR enablement remains a separate source/Nix path for now.

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

This describes the released compositor package path. It does not, by itself, provision a proven full VR stack, and the current public Rocky RPM still needs a packaging fix before it is a usable host install path. The repair lane is currently converging on a native non-`vr` compositor RPM first.

Download from GitHub Releases:
```bash
curl -LO https://github.com/Jesssullivan/XoxdWM/releases/latest/download/exwm-vr-compositor-*.x86_64.rpm
sudo dnf install ./exwm-vr-compositor-*.x86_64.rpm
```

Current status:

- `v0.5.0` RPM publication exists, but failed on `honey` in this audit.
- A corrected native Rocky RPM or a validated source-build path is still required before this section can be promoted beyond `Design`.
- Full VR/OpenXR enablement on Rocky is still a separate follow-on step after the native compositor RPM works on a named host.

Install Emacs and enable the session:
```bash
sudo dnf install emacs
# Log out, select "EXWM-VR" session at login screen if the session file is installed
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

- `yoga`: target desktop/dev host for the next reproducible Rocky 10 install path.
- `honey`: target VR smoke host, but not currently documented here as a proven XoxdWM deployment.
