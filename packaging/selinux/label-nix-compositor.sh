#!/bin/bash
# label-nix-compositor.sh — Apply SELinux labels to Nix-managed compositor binary.
#
# Usage:
#   sudo ./label-nix-compositor.sh [path]
#
# If no path given, resolves ~/.nix-profile/bin/ewwm-compositor.
# Requires: semanage, restorecon (policycoreutils-python-utils).

set -euo pipefail

LABEL="exwm_vr_nix_exec_t"

if [ $# -ge 1 ]; then
    BIN_PATH="$1"
else
    BIN_PATH="$(readlink -f "${HOME}/.nix-profile/bin/ewwm-compositor" 2>/dev/null || true)"
    if [ -z "$BIN_PATH" ] || [ ! -f "$BIN_PATH" ]; then
        echo "ERROR: ewwm-compositor not found in ~/.nix-profile/bin/" >&2
        echo "Usage: $0 [/nix/store/.../bin/ewwm-compositor]" >&2
        exit 1
    fi
fi

echo "Labeling: $BIN_PATH -> system_u:object_r:${LABEL}:s0"

# Add persistent file context rule
semanage fcontext -a -t "$LABEL" "$BIN_PATH" 2>/dev/null \
    || semanage fcontext -m -t "$LABEL" "$BIN_PATH"

# Apply the label
restorecon -v "$BIN_PATH"

echo "Done. Verify with: ls -Z $BIN_PATH"
