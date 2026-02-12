#!/usr/bin/env bash
# Launch a NixOS QEMU VM for testing XoxdWM
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Building NixOS test VM..."
nix build "${PROJECT_ROOT}#nixosConfigurations.test-vm.config.system.build.vm" \
  --out-link "${PROJECT_ROOT}/result-vm"

echo "Launching VM (press Ctrl-A X to exit)..."
echo "  - Compositor: headless backend"
echo "  - Emacs: pgtk with EWWM loaded"
echo "  - IPC: auto-connected"
"${PROJECT_ROOT}/result-vm/bin/run-test-vm-vm" -m 4G -smp 2
