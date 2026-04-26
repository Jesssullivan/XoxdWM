# `honey` OpenXR Fresh-Boot Runbook - 2026-04-26

This runbook is the next evidence lane for `TIN-346` and GitHub issue `#20`.
It turns the current clean service-cycle proof into an attended fresh-boot
check without moving reset, power, or display-topology authority out of
[`Dell-7810`](https://github.com/Jesssullivan/Dell-7810).

Read this with:

- [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md)
- [remote-proof-lanes.md](remote-proof-lanes.md)
- [support-matrix.md](support-matrix.md)
- [status.md](status.md)

## Boundary

- This runbook does not choose or automate the reboot/reset action.
- `just honey-openxr-fresh-boot-check` does not reboot `honey`.
- Do not stop or restart `rke2` as part of this XoxdWM evidence lane.
- If the boot/reset behavior itself is under test, record that in Dell-7810,
  not as an XoxdWM product proof.
- A passing CLI smoke is fresh-boot OpenXR smoke, not in-goggles first-frame
  proof unless a human records a visible frame in the headset.

## Required Presence

- A `neo` operator in `/Users/jess/git/XoxdWM`.
- A lab operator present at `honey`, with access to the Dell management display
  and the Bigscreen Beyond.
- A known display topology written into the run notes:
  - Dell HDMI display connected or disconnected.
  - Bigscreen Beyond DP path connected or disconnected.
  - Whether the boot action was warm reboot, hard reset, or power cycle.

## Preconditions

From `neo`:

```sh
git checkout main
git pull --ff-only
git status --short --branch
just --list | rg -n "honey-openxr"
```

On `honey`, the checkout should also be current and clean enough for operator
work:

```sh
just honey-run honey -- 'git checkout main && git pull --ff-only && git status --short --branch && git rev-parse --short HEAD'
```

Before the attended boot action, capture a non-disruptive baseline:

```sh
log="/tmp/honey-openxr-fresh-boot-$(date -u +%Y%m%dT%H%M%SZ).log"

{
  echo "== pre-boot status =="
  date -u
  git rev-parse --short HEAD
  just honey-run honey -- 'set -euo pipefail
    echo "host=$(hostname)"
    echo "head=$(git rev-parse --short HEAD)"
    echo "boot_id=$(cat /proc/sys/kernel/random/boot_id 2>/dev/null || echo unknown)"
    echo "uptime_seconds=$(cut -d" " -f1 /proc/uptime 2>/dev/null || echo unknown)"
    printf "rke2-server="
    systemctl is-active rke2-server 2>/dev/null || true
    printf "rke2-agent="
    systemctl is-active rke2-agent 2>/dev/null || true
    ./packaging/scripts/exwm-vr-openxr-smoke --status-only'
} | tee -a "${log}"
```

## Attended Boot Step

Perform exactly one attended boot action from the lab. Do not let this runbook
decide whether that action is a warm reboot, hard reset, or power cycle.

Record:

- action type
- local display result on the Dell panel
- whether the Bigscreen Beyond appears powered and connected
- whether `honey` returns over Tailscale and/or local IPv4
- any visible GPU reset, display training, or no-signal symptom

If `honey` does not return, stop the XoxdWM lane and move the evidence to the
Dell reset/power tracker.

## Post-Boot Check

After `honey` is reachable again:

```sh
{
  echo "== post-boot fresh-boot check =="
  date -u
  just honey-run honey -- 'git checkout main && git pull --ff-only && git status --short --branch && git rev-parse --short HEAD'
  just honey-openxr-fresh-boot-check honey 1 20
} | tee -a "${log}"
```

Only run a second fresh-boot cycle after another attended boot action. Repeating
`just honey-openxr-fresh-boot-check` without a new boot proves another clean
service cycle, not another fresh-boot cycle.

## Pass Criteria

Fresh-boot OpenXR smoke passes only if all of these are true:

- The post-boot `boot_id` differs from the pre-boot `boot_id`.
- `honey` is on `main` at the expected repo head.
- `rke2-server` remains `active` after the check.
- The OpenXR wrapper selects `/usr/libexec/exwm-vr/hello_xr -g Vulkan`.
- The smoke reaches Monado / Bigscreen Beyond.
- Two eye swapchains are created at the expected `3561x3561` size.
- The wrapper reports `openxr_smoke=passed`.
- No stale IPC socket prevents service startup or client connection.

In-goggles first-frame proof additionally requires a human note that a visible
frame appeared in the headset. CLI success alone is not enough.

## Failure Classification

Classify the first failure before rerunning:

- Host/substrate: `honey` fails to boot, disappears from SSH/Tailscale/local
  IPv4, loses the Dell display, or shows GPU reset/display-training symptoms.
- Packaging/deployment: packaged paths are missing, `main` is not synced, or
  `/usr/libexec/exwm-vr/hello_xr` is not selected.
- Compositor/runtime: XoxdWM, Monado, DRM lease, IPC, or OpenXR client startup
  fails after the host is otherwise healthy.
- Product/visual: CLI smoke passes, but the headset does not present a visible
  first frame.

## Tracker Updates

After the run:

- Comment on GitHub issue `#20` with the log path or selected output.
- Update Linear `TIN-346`.
- If the blocker is host/reset/display/power, update the appropriate Dell-7810
  tracker instead of promoting XoxdWM support.
- Update [support-matrix.md](support-matrix.md) only after the evidence passes
  and the status label remains honest.

