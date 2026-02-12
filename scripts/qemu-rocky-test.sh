#!/usr/bin/env bash
# Launch a Rocky Linux QEMU VM for testing XoxdWM
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
WORK_DIR="${PROJECT_ROOT}/.qemu-rocky"

mkdir -p "${WORK_DIR}"

IMAGE_URL="https://dl.rockylinux.org/pub/rocky/10/images/x86_64/Rocky-10-GenericCloud.latest.x86_64.qcow2"
IMAGE="${WORK_DIR}/rocky-10.qcow2"

if [ ! -f "${IMAGE}" ]; then
    echo "Downloading Rocky Linux 10 cloud image..."
    curl -L -o "${IMAGE}" "${IMAGE_URL}"
fi

# Create cloud-init seed
cat > "${WORK_DIR}/meta-data" << 'EOF'
instance-id: xoxdwm-test
local-hostname: xoxdwm-test
EOF

cat > "${WORK_DIR}/user-data" << 'EOF'
#cloud-config
password: test
chpasswd: { expire: False }
ssh_pwauth: True
packages:
  - gcc
  - gcc-c++
  - wayland-devel
  - mesa-libEGL-devel
  - libinput-devel
  - libxkbcommon-devel
  - emacs-nox
  - curl
  - git
runcmd:
  - curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
  - echo "Rocky Linux QEMU test VM ready"
EOF

# Generate seed ISO (requires genisoimage or mkisofs)
if command -v genisoimage &>/dev/null; then
    genisoimage -output "${WORK_DIR}/seed.img" -volid cidata -joliet -rock \
        "${WORK_DIR}/meta-data" "${WORK_DIR}/user-data"
elif command -v mkisofs &>/dev/null; then
    mkisofs -output "${WORK_DIR}/seed.img" -volid cidata -joliet -rock \
        "${WORK_DIR}/meta-data" "${WORK_DIR}/user-data"
else
    echo "Error: genisoimage or mkisofs required for cloud-init"
    exit 1
fi

echo "Launching Rocky Linux 10 VM (press Ctrl-A X to exit)..."
echo "  Login: root / test"
qemu-system-x86_64 \
    -m 4G -smp 2 \
    -enable-kvm \
    -drive "file=${IMAGE},format=qcow2" \
    -drive "file=${WORK_DIR}/seed.img,format=raw" \
    -nographic
