# `honey` PPS Diagnostic Runbook - 2026-05-12

## Scope

Use this runbook to capture the packed AMD DSC Picture Parameter Set (PPS)
for the Bigscreen Beyond stream on `honey` after the `linux-xr` read-only PPS
debugfs carry is available in a diagnostic kernel.

This is evidence capture only. It does not prove a visible first frame, does
not promote `honey` beyond the current P3 OpenXR Session pass / P4 Visual First
Frame fail classification, and does not move host/kernel authority into
XoxdWM.

## Current Artifact Lane

- Kernel repo: `tinyland-inc/linux-xr`
- PPS carry PR: <https://github.com/tinyland-inc/linux-xr/pull/69>
- Merge commit: `dbfcd3938a2f3`
- Post-merge CI: <https://github.com/tinyland-inc/linux-xr/actions/runs/25710732827>
- Artifact-only diagnostic build: <https://github.com/tinyland-inc/linux-xr/actions/runs/25710987473>
- Artifact ID: `6937251018`
- Artifact name: `kernel-xr-rpms-generic`
- Realized RPM release: `6.19.5-12.xr.el10`, generic variant

Use a successful generic RPM artifact from that run or a later equivalent run.
Do not treat a workflow artifact as a tagged release unless `linux-xr` cuts an
actual release tag.

The `25710987473` artifact was inspected on `neo` on `2026-05-13` at
`/tmp/xoxdwm-linux-xr-25710987473`:

| RPM | SHA-256 |
| --- | --- |
| `kernel-xr-6.19.5-12.xr.el10.x86_64.rpm` | `57f469a4b2ddebd5097c22606e1671eaa54a7860ff40feeca1e31ee09acfc2cb` |
| `kernel-xr-devel-6.19.5-12.xr.el10.x86_64.rpm` | `53f4b02bb89bbb905b96c50a274aa800f7529ad452b63b1dc22ccd8f8fbe93f0` |
| `kernel-xr-headers-6.19.5-12.xr.el10.x86_64.rpm` | `489c1406734856eb8edab3ec78d0e198265a9eede732612eab34bc11a694f16d` |

## Hard Boundaries

- Do not install a kernel, change the default boot entry, reboot `honey`, or
  restart XoxdWM/Monado services without explicit attended operator approval.
- Do not stop, restart, drain, or otherwise touch `rke2`.
- Do not write debugfs as part of this PPS capture. `just honey-kernel-dsc-truth`
  copies a helper to `/tmp` and reads sysfs/debugfs through the Honey sudo lane;
  it must not retrain the link or write connector state.
- Keep Dell reset, BIOS, SMI, power, and management-display facts in the
  Dell-7810 evidence surface. Link those artifacts from the final tracker
  comment instead of turning them into XoxdWM product claims.

## Read-Only Baseline

Before any diagnostic kernel install or reboot, capture the current state:

```sh
gh run view 25710987473 -R tinyland-inc/linux-xr --json status,conclusion,url
just honey-openxr-status honey
just honey-kernel-dsc-truth honey auto
```

Record:

- current kernel from `kernel_release`
- resolved connector and source
- OpenXR client path
- service/socket state
- `debugfs_dsc_bits_per_pixel`
- `debugfs_dsc_pic_width`
- `debugfs_dsc_pic_height`
- `debugfs_dsc_slice_width`
- `debugfs_dsc_slice_height`
- `pps_available`

On the pre-diagnostic `6.19.5-10.xr.el10` host, the known result is
`pps_available=false` because the live kernel does not expose
`dsc_pic_parameter_set`.

## Diagnostic Kernel Activation

Only after operator approval:

1. Download the generic RPM artifact from the selected `linux-xr` Actions run.
2. Record the artifact URL, artifact name, RPM filenames, and checksums.
3. Install the RPM through the approved Honey sudo/become path.
4. Record the previous default kernel and the new default kernel.
5. Reboot only with an attended operator present and a rollback path available.
6. After boot, verify `uname -r` before starting any visual or OpenXR proof.

For tagged `linux-xr` releases, `just beyond-kernel-install honey <tag>` is the
repo-owned release helper. For workflow artifacts, use the explicit downloaded
RPM paths instead of pretending there is a release tag.

## Active PPS Capture

The useful sample is the live HMD stream while Monado/OpenXR has driven the
Bigscreen Beyond path into active presentation.

Use two shells from `neo`:

```sh
just honey-openxr-status honey
just honey-openxr-smoke honey -- --timeout 120
```

While the smoke client is still running or immediately after it reaches
`FOCUSED`, capture DSC/PPS truth:

```sh
just honey-kernel-dsc-truth honey auto
```

Required PPS fields:

- `pps_available=true`
- `pps_length=128`
- `pps_sha256`
- `pps_hex_0_127`
- `pps_bits_per_pixel_x16=128`
- `pps_pic_width=5088`
- `pps_pic_height=2544`
- `pps_slice_width=1272`
- `pps_slice_height=159`
- `pps_rc_ranges_bpp8_444_patched=true`
- `pps_rc_ranges_bpp8_444_stock=false`

If `pps_available=false`, the run is still useful evidence, but the PPS gap is
not closed. Record the kernel release, debugfs connector, debugfs file list if
available, OpenXR state, and whether the stream was active at capture time.

## Visual Gate

Record the human-visible result separately:

- `visual_observed=yes`: visible non-black HMD output was observed by the lab
  operator during the active run.
- `visual_observed=no`: OpenXR/Monado reached session proof but goggles stayed
  black.
- `visual_observed=not_observed`: no person was watching the headset.

Do not promote #49 unless `visual_observed=yes` is backed by an attended
evidence packet.

## Tracker Draft

```markdown
PPS diagnostic evidence:

- linux-xr source:
- artifact run:
- artifact name/checksum:
- Honey kernel before:
- Honey kernel after:
- boot ID:
- resolved connector:
- OpenXR client:
- runtime/headset:
- session state:
- `pps_available`:
- `pps_sha256`:
- `pps_bits_per_pixel_x16`:
- `pps_pic_width` / `pps_pic_height`:
- `pps_slice_width` / `pps_slice_height`:
- `pps_rc_ranges_bpp8_444_patched`:
- `pps_rc_ranges_bpp8_444_stock`:
- `visual_observed`:
- `rke2-server` state:
- classification:

Notes:
```
