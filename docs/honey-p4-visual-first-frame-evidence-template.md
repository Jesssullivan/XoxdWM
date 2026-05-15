# `honey` P4 Visual First-Frame Evidence Template

Use this template for GitHub issue `#49` and Linear `TIN-1086` when the lab is
attempting to prove a visible, non-black Bigscreen Beyond frame on `honey`.

This is not the P5 fresh-boot repeatability template. If the same attended run
also tests boot repeatability, fill out
[honey-fresh-boot-evidence-template.md](honey-fresh-boot-evidence-template.md)
and link it here. P4 only proves that a human observed visible headset output
during an active OpenXR/Monado session.

## Hard Boundaries

- Do not install a kernel, change the default boot entry, reboot `honey`,
  restart XoxdWM/Monado services, write debugfs, retrain DisplayPort, or launch
  an OpenXR client without explicit attended operator approval.
- Do not stop, restart, drain, or otherwise touch `rke2`.
- CLI state such as `READY`, `VISIBLE`, `FOCUSED`, or created swapchains is P3
  OpenXR Session evidence. It is not P4 unless a human records visible,
  non-black headset output.
- PPS/DSC capture is diagnostic evidence only. It does not promote P4 unless
  `visual_observed=yes` is recorded by the lab observer.

## Run Identity

- Date/time UTC:
- Date/time local:
- Operator on `neo`:
- Lab observer at `honey`:
- XoxdWM commit:
- `honey` repo commit:
- GitHub issue:
- Linear issue:
- Related Dell-7810 artifact:
- Related `linux-xr` artifact or release:

## Host And Display Baseline

Record read-only host state before the attended run:

```sh
git status --short --branch
just honey-openxr-status honey
just honey-kernel-dsc-truth honey auto
```

Required fields:

- `honey` kernel release:
- `honey` boot ID:
- `rke2-server`:
- `rke2-agent`:
- resolved HMD connector:
- connector source:
  - `non_desktop` / `edid-bigscreen` / explicit override / other
- Dell management display:
  - connected / disconnected / unknown
- Bigscreen Beyond DP path:
  - connected / disconnected / unknown
- OpenXR client path:
- Monado runtime JSON:
- `exwm-vr-compositor.service`:
- `exwm-vr-monado.service`:
- `monado_comp_ipc`:

## Active Session Evidence

Command:

```sh
just honey-openxr-smoke honey -- --timeout 120
```

If the lab observer sees visible non-black headset output during the active
session, rerun or launch the smoke with the explicit P4 confirmation gate:

```sh
EXWM_VR_VISUAL_OBSERVED=yes \
EXWM_VR_VISUAL_OBSERVER=<observer-id> \
EXWM_VR_VISUAL_CONFIRMATION=VISIBLE_NON_BLACK \
just honey-openxr-smoke honey -- --timeout 120
```

Paste the selected output:

```text
```

Required fields:

- runtime:
- headset:
- session states reached:
- eye swapchain count:
- eye swapchain size:
- `openxr_smoke`:
- `proof_ladder`:
- `visual_observed` from wrapper:
- `visual_observer` from wrapper:
- `visual_confirmation` from wrapper:
- `visual_first_frame` from wrapper:

## Optional PPS / DSC Capture

Use this section only when the diagnostic kernel exposes the read-only PPS
debugfs surface and the lab has approved capture during or immediately after the
active run.

Command:

```sh
just honey-kernel-dsc-truth honey auto
```

Required fields:

- kernel release:
- resolved connector:
- debugfs connector:
- `debugfs_dsc_bits_per_pixel`:
- `debugfs_dsc_pic_width`:
- `debugfs_dsc_pic_height`:
- `debugfs_dsc_slice_width`:
- `debugfs_dsc_slice_height`:
- `pps_available`:
- `pps_sha256`:
- `pps_bits_per_pixel_x16`:
- `pps_pic_width` / `pps_pic_height`:
- `pps_slice_width` / `pps_slice_height`:
- `pps_rc_ranges_bpp8_444_patched`:
- `pps_rc_ranges_bpp8_444_stock`:

## Human Visual Observation

The lab observer must be watching the headset during the active OpenXR session.
The wrapper rejects `visual_observed=yes` unless `EXWM_VR_VISUAL_OBSERVER` is
non-empty and `EXWM_VR_VISUAL_CONFIRMATION=VISIBLE_NON_BLACK` is set before the
client launches.

Select exactly one:

- `visual_observed=yes`: visible non-black headset output was observed.
- `visual_observed=no`: OpenXR/Monado reached session proof, but the goggles
  stayed black. The wrapper should report `visual_first_frame=P4_FAILED`.
- `visual_observed=not_observed`: no person was watching the headset.
  The wrapper should report `visual_first_frame=P4_UNOBSERVED`.

Required fields:

- Observer:
- Observation time:
- Was the headset worn or directly inspected:
- Visible non-black output:
  - yes / no / not observed
- Description of visible content or black-screen symptom:
- Any panel power, backlight, static, flicker, or link-training symptom:
- If photographed or filmed, artifact link:

## Classification

Select exactly one primary classification:

- P4 pass: visible non-black first frame observed
- P3 pass / P4 fail: focused OpenXR session, black headset
- P3 pass / P4 unobserved: focused OpenXR session, no human observation
- Host/substrate blocker
- Packaging/deployment blocker
- Compositor/runtime blocker
- Inconclusive / rerun needed

Rationale:

## Tracker Comment Draft

```markdown
Honey P4 visual-first-frame evidence:

- XoxdWM commit:
- honey commit:
- Dell artifact:
- linux-xr artifact:
- kernel:
- resolved connector:
- OpenXR client:
- runtime/headset:
- session states:
- eye swapchains:
- openxr_smoke:
- proof_ladder:
- visual_observed:
- visual_observer:
- visual_confirmation:
- visual_first_frame:
- PPS available:
- rke2-server:
- classification:

Notes:
```

Before posting a P4 pass tracker comment, save the filled packet or draft
comment locally and run:

```sh
just honey-p4-evidence-check path/to/filled-p4-packet.md
./packaging/scripts/exwm-vr-p4-evidence-check --require-p4 path/to/filled-p4-packet.md
```

The checker is local/tracker-side only. It does not touch `honey`, but it rejects
P4 promotion claims unless `visual_observed=yes`, a non-placeholder
`visual_observer`, `visual_confirmation=VISIBLE_NON_BLACK`, and
`visual_first_frame=P4_OBSERVED` are all present.

## Promotion Decision

- Update [support-matrix.md](support-matrix.md):
  - yes / no
- Update [status.md](status.md):
  - yes / no
- Keep as issue-only evidence:
  - yes / no
- Reason:
