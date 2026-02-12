# QEMU Testing Guide

Test the full XoxdWM stack without VR hardware using QEMU virtual machines
and Monado's headless simulation mode.

## Why QEMU?

- Validate the full boot sequence (compositor + Emacs + IPC)
- Test packaging (RPM/DEB installation)
- CI-friendly (no GPU required with headless backend)
- Safe environment for development

## NixOS QEMU VM

### Quick Start

```bash
# Build and launch the test VM
nix run .#nixosConfigurations.test-vm.config.system.build.vm

# Or with helper script
./scripts/qemu-nixos-test.sh
```

### VM Configuration

The test VM includes:
- XoxdWM compositor (headless backend)
- Emacs 30 with pgtk
- Monado in headless mode
- Auto-login to EWWM session

## Rocky Linux QEMU VM

### Quick Start

```bash
# Download Rocky Linux cloud image
./scripts/qemu-rocky-test.sh

# Manual approach
curl -LO https://dl.rockylinux.org/pub/rocky/10/images/x86_64/Rocky-10-GenericCloud.latest.x86_64.qcow2
qemu-system-x86_64 \
  -m 4G -smp 2 -enable-kvm \
  -drive file=Rocky-10-GenericCloud.latest.x86_64.qcow2,format=qcow2 \
  -nographic \
  -cloud-init seed.img
```

## Monado Headless Simulation

Test VR features without a real HMD:

```bash
# Inside the VM or any Linux system
export XRT_COMPOSITOR_FORCE_HEADLESS=1
export MONADO_DRIVER_SIMULATION=1

# Launch compositor with VR enabled
ewwm-compositor --backend headless
```

## Simulated Input

Use IPC commands to inject synthetic input data:

```bash
# Connect to IPC socket
export EWWM_SOCK="${XDG_RUNTIME_DIR}/ewwm-ipc.sock"

# Simulate gaze (requires socat or a simple IPC client)
# From Emacs:
# (ewwm-ipc-send '(:type :gaze-simulate :id 1 :yaw 0.0 :pitch 0.0))

# Inject synthetic BCI data
# (ewwm-ipc-send '(:type :bci-inject-synthetic :id 1 :channel 0 :value 0.8))

# Configure hand tracking
# (ewwm-ipc-send '(:type :hand-tracking-config :id 1 :smoothing 0.5))
```

## CI Integration

### GitHub Actions with QEMU

```yaml
# Use uraimo/run-on-arch-action for cross-architecture testing
- uses: uraimo/run-on-arch-action@v3
  with:
    arch: aarch64
    distro: ubuntu_latest
    run: |
      cargo build --no-default-features --manifest-path compositor/Cargo.toml
      cargo test --no-default-features --manifest-path compositor/Cargo.toml
```

### Container-Based Testing

```bash
# Rocky Linux 10 container (no QEMU overhead)
podman run --rm -v .:/workspace:Z rockylinux:10 bash -c '
  dnf install -y gcc wayland-devel mesa-libEGL-devel libinput-devel libxkbcommon-devel emacs-nox curl
  curl --proto "=https" --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  source ~/.cargo/env
  cd /workspace
  cargo build --manifest-path compositor/Cargo.toml --no-default-features
  cargo test --manifest-path compositor/Cargo.toml --no-default-features
  emacs --batch -L lisp/core -L lisp/vr -L lisp/ext -l ert $(find test -name "*-test.el" -printf "-l %p ") -f ert-run-tests-batch-and-exit
'
```
