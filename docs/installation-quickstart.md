# XoxdWM Installation Quickstart

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

Download from GitHub Releases:
```bash
curl -LO https://github.com/Jesssullivan/XoxdWM/releases/latest/download/ewwm-compositor-*.x86_64.rpm
sudo dnf install ./ewwm-compositor-*.x86_64.rpm
```

Install Emacs and enable the session:
```bash
sudo dnf install emacs
# Log out, select "EWWM" session at login screen
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
sudo install -Dm644 packaging/desktop/ewwm.desktop /usr/share/wayland-sessions/

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
