# Honey Watchman Read-Only Audit - 2026-05-17

## Scope

This runbook turns the current Bigscreen Beyond retained panel-state hypothesis
into a repo-owned, read-only evidence lane for `honey`.

The May 2026 lab evidence already proves the stronger machine side of the path:
Monado can reacquire a real DP-1 lease, select Bigscreen Beyond, create eye
swapchains, reach `BEGIN_SESSION`, and hold active DP/DSC state. The blocker is
now below OpenXR session proof and above product visual proof: the `35bd:0101`
panel controller accepts the known Bigscreen HID commands, but `video_state`
stays at `0:DP Init` and backlight readback stays `0.0`.

Local research also records that the Valve / Watchman `28de:2300` lane owns
video configuration and HMD display-state traffic. This audit captures the
current USB/hidraw/descriptor surface for that lane without sending any feature
reports or replaying unknown commands.

## Hard Boundaries

- Do not write Watchman feature reports.
- Do not write debugfs.
- Do not retrain DisplayPort.
- Do not restart XoxdWM, Monado, display-manager, or other services.
- Do not stop, restart, drain, or otherwise touch `rke2`.
- Do not install a kernel, change a boot entry, or reboot `honey`.
- Do not promote P4 from this audit. P4 still requires human-observed visible
  non-black headset output with `visual_observed=yes`.

## Command

Run from `neo`:

```sh
just honey-watchman-readonly-audit honey
```

The target streams the repo-owned helper to `honey` over plain SSH:

```sh
packaging/scripts/exwm-vr-watchman-readonly-audit
```

It intentionally does not use `sudo`, `nix develop`, `honey-run`, or
`honey-sudo-run`.

## What It Captures

The helper records:

- host, kernel, boot ID, and UID
- matching USB devices for:
  - `28de:2300` Watchman / tracked display-state lane
  - `35bd:0101` Bigscreen Beyond panel controller
  - `28de:2102` Valve VR radio dongles when present
- matching hidraw nodes, sysfs paths, ownership, and permissions
- HID `uevent` fields
- report descriptor size and SHA-256 when readable
- selected `udevadm` properties when available
- runtime socket presence for `wayland-0`, `ewwm-ipc.sock`, and
  `monado_comp_ipc`

## What It Does Not Capture

This lane does not read the current Watchman video state. That state is expected
to require device-specific HID feature-report traffic, and local code should not
start sending or replaying Watchman reports until the report IDs and side
effects are understood.

This lane also does not replace the existing `35bd:0101` panel probe history.
Known panel-probe facts remain in
[honey-beyond-black-display-history-2026-05-16.md](research/honey-beyond-black-display-history-2026-05-16.md)
and
[honey-kernel-dsc-truth-2026-05-10.md](research/honey-kernel-dsc-truth-2026-05-10.md).

## Evidence Packet

Use this packet when posting to GitHub `#49`, GitHub `#20`, or Linear
`TIN-346`.

```markdown
Honey Watchman read-only audit:

- XoxdWM commit:
- honey kernel:
- honey boot ID:
- command: `just honey-watchman-readonly-audit honey`
- Watchman `28de:2300` present:
- Bigscreen `35bd:0101` present:
- Valve VR radio `28de:2102` present:
- matching hidraw nodes:
- report descriptor hashes:
- `wayland-0`:
- `ewwm-ipc.sock`:
- `monado_comp_ipc`:
- forbidden mutations confirmed:
  - Watchman writes: no
  - debugfs writes: no
  - DP retrain: no
  - service restart: no
  - rke2 operation: no
  - kernel/default boot change/reboot: no
- P4 classification: not promoted

Notes:
```

## First Read-Only Audit Result

Captured from `neo` against `honey` on 2026-05-17:

```sh
just honey-watchman-readonly-audit honey
```

Observed host baseline:

- `host=honey`
- `kernel=6.19.5-11.xr.el10`
- `boot_id=1e6ac407-ad70-44bc-abd8-59568fdd8461`
- `mode=read-only`
- `watchman_audit=inventory_only`
- `video_state_capture=not_attempted`
- `p4_promotion=not_allowed`

Observed USB matches:

- `28de:2102` Valve VR Radio, two devices
- `35bd:0101` Bigscreen Beyond, one device
- `28de:2300` Valve Tundra Tracker / Watchman lane, one device

Observed hidraw matches:

| Node | Device | Permissions | Report descriptor SHA-256 |
| --- | --- | --- | --- |
| `/dev/hidraw2` | `28de:2102` Valve VR Radio | `660 root:video` | `6a43817089a1f0626fedc4c883599f5933c08f8295328aa3859ea10ab0f046ed` |
| `/dev/hidraw3` | `35bd:0101` Bigscreen Beyond | `660 root:video` | `4d708df676961b8e1ea0062750ee822a9d6f81075c178e0b46ddfde400a0d18b` |
| `/dev/hidraw4` | `28de:2102` Valve VR Radio | `660 root:video` | `6a43817089a1f0626fedc4c883599f5933c08f8295328aa3859ea10ab0f046ed` |
| `/dev/hidraw5` | `28de:2300` Watchman interface 0 | `660 root:video` | `806233f25525576bacfa11368672faad1c8b3b7b53a07000a598a117d5ca75bd` |
| `/dev/hidraw6` | `28de:2300` Watchman interface 1 | `660 root:video` | `4bf992ba17777cdae0f726436e8374204817dba36ea15e35c834b8d709447276` |
| `/dev/hidraw7` | `28de:2300` Watchman interface 2 | `660 root:video` | `5179e76f49c602356230ffd5c5c78c73c901d3f30960f42c0da103a424f7cce7` |

Runtime boundary at capture time:

- `wayland-0=missing`
- `ewwm-ipc.sock=missing`
- `monado_comp_ipc=missing`

Interpretation:

- This confirms the Watchman inventory lane exists on the current xr11-booted
  `honey` host and is reachable without sudo or HID writes.
- This does not prove an active XoxDWM/OpenXR session, because the runtime
  sockets were missing during the audit.
- This does not read Watchman video state and does not promote P4.

## Next Decision

Use this audit to decide whether the next attended lab pass needs:

1. more descriptor/permission evidence only,
2. a documented external trace of Watchman display-state reports, or
3. a separately approved mutating Watchman experiment.

The default answer remains no mutation. Any future Watchman write/replay lane
needs a separate runbook, explicit operator approval, and a rollback story.
This audit does not promote P4.
