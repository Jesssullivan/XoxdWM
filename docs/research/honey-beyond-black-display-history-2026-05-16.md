# Honey Bigscreen Black Display History - 2026-05-16

## Scope

This note records the local evidence trail for GitHub `#49` and Linear
`TIN-1086` after the May 16 lab observation that the Bigscreen Beyond goggles
showed a brief edge-light/backlight event for less than one second, apparently
coincident with Dell display initialization, then returned to no visible
illumination.

This is not P4 visual first-frame proof. It is panel-power/display-state
evidence that should be kept separate from `visual_observed=yes`.

## Current Classification

- Dell management display path: physically proven. The operator observed the
  Dell output changing through the compositor color-block probe, matching the
  earlier HDMI framebuffer readback.
- HMD machine path: P3 is strong for this boot. Monado reacquired a real DP-1
  lease after restarting only `exwm-vr-monado.service`, selected the Bigscreen
  Beyond display, created four `3561x3561` color/depth swapchains, and logged
  `BEGIN_SESSION`.
- Kernel/display path: read-only DSC truth on `6.19.5-10.xr.el10` reports
  `debugfs_dsc_bits_per_pixel=128`, active `5088x2544` geometry, and completed
  link training during the held session.
- Panel state: the `35bd:0101` probe acknowledges `OnHidOpen` and accepts
  `SetVideoConfig dp_training=1`, backlight gain, and unmute, but
  `video_state` remains `0:DP Init` and backlight readback remains `0.0`.
- Visual gate: P4 remains failed. The transient edge-light/backlight flash is
  useful, but it is not visible non-black retained headset output.

## Failure Classes

### Missing wake or proprietary HID sequence

The original local bootstrap analysis centered on SteamVR-for-Linux `#610`:
tracked/connected headset state with mirrored compositor output, but the HMD
display never wakes. That issue's packet analysis identified a missing five
packet wake sequence:

```text
810600220000
810600220000
810600220000
810600220100
810600220200
```

Local code corrected this path in stages:

- `d7fa0c7` documented the wake-packet theory and native HID strategy.
- `e52468a` corrected the report ID assumption from `0x06` to implicit `0x00`,
  but still over-claimed DP/display effect in the commit message.
- `876c7e0` moved `SetWorkState` from byte 2 to byte 1, matching the command
  layout used by brightness, fan, and LED reports.
- `9515541` fixed native IPC queue draining so mutating Beyond HID IPC commands
  actually flush to hidraw instead of reporting success while leaving commands
  pending.

Current interpretation: the corrected `35bd:0101` power/brightness/fan path is
reachable and sends packets, but it is probably not the whole display-state
sequence. Local research already warns that `28de:2300` Watchman owns video
configuration and HMD display state, so complete panel bring-up may require both
the Bigscreen proprietary lane and the Watchman lane.

### Non-desktop or DRM lease failure

This was a real earlier blocker, but the May 16 pass moved past it. After the
stale Monado service was restarted, the compositor granted a DP-1 lease, Monado
selected the headset mode, sysfs showed DP-1 connected/enabled/DPMS On, and the
OpenXR client reached session/swapchain evidence.

Current interpretation: no longer the leading explanation for the black goggles
on this boot.

### DSC, BPP, or rainbow-static class

External Linux VR guidance separates black-display bring-up from the AMD DSC
artifact class: AMD users need Bigscreen Beyond kernel patches, and the DSC fix
prevents rainbow/static-style visual artifacts. Local Honey truth now proves the
live kernel carries the DisplayID DSC BPP parser and BPP=8 QP/rate-control
fixes, with `debugfs_dsc_bits_per_pixel=128` during active presentation.

Current interpretation: DSC remains worth auditing with the PPS diagnostic
kernel, but the observed symptom is not rainbow/static. A fully black panel with
`video_state=0:DP Init` points more strongly at retained panel/display-state
bring-up than at decoded-but-corrupt DSC content.

### Mode-selection mismatch

LVRA documents `XRT_COMPOSITOR_DESIRED_MODE` for choosing exposed Bigscreen
Beyond modes when Monado lists both 75 Hz and 90 Hz modes. The May 16 canary
temporarily set `XRT_COMPOSITOR_DESIRED_MODE=1`, confirmed Monado selected
`3840x1920@90`, held a real `hello_xr -g Vulkan2` session, and then restored the
default mode.

Current interpretation: forcing mode 1 did not move the panel controller out of
`DP Init`; it is not sufficient to fix P4.

### Retained panel/display-state failure

This is now the leading hypothesis. The transient edge-light/backlight event
means the headset display path can be briefly energized during display
initialization, but the state is not retained. That lines up with the machine
evidence: real lease, session, active DP/DSC, and accepted `35bd:0101` commands,
while the panel controller remains at `DP Init` with backlight `0.0`.

## Safe Next Evidence

Read-only or low-risk checks:

1. Confirm Monado stayed restored to the default mode environment after the
   canary.
2. Capture current `35bd:0101` and `28de:2300` hidraw inventory, descriptors,
   ownership, and permissions without writing feature reports to Watchman.
3. Preserve each human observation as a P4 packet field: no backlight, transient
   edge-light, green/static, sustained backlight, or visible non-black image.

Operator-approved checks only:

1. Install and boot the diagnostic PPS kernel artifact, then capture read-only
   PPS bytes during an active session.
2. Audit Watchman `28de:2300` video/display-state traffic before attempting any
   writes. Do not replay unknown Watchman reports from local code until the
   report IDs and side effects are understood.
3. Keep debugfs link-setting writes, DP retrain canaries, and DSC sweeps out of
   the default path unless the operator explicitly approves them.

## Sources

- Local HID and wake-packet history:
  [beyond-2e-bootstrap-analysis.md](beyond-2e-bootstrap-analysis.md) and
  [beyond-amd-kernel-patches.md](beyond-amd-kernel-patches.md)
- Local kernel/panel evidence:
  [honey-kernel-dsc-truth-2026-05-10.md](honey-kernel-dsc-truth-2026-05-10.md)
- P4 evidence gate:
  [../honey-p4-visual-first-frame-evidence-template.md](../honey-p4-visual-first-frame-evidence-template.md)
- External Linux VR mode and kernel-patch context:
  <https://wiki.vronlinux.org/docs/hardware/bigscreen-beyond/>
- External wake/display failure class:
  <https://github.com/ValveSoftware/SteamVR-for-Linux/issues/610>
