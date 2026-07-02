# `honey` P4 Visual First-Frame Evidence Template

Use this template only for the product/visual gate on `honey`: visible
non-black headset output from the XoxdWM/OpenXR path.

This is narrower than the fresh-boot runbook. A CLI OpenXR run that reaches
`READY`, `VISIBLE`, `FOCUSED`, or creates eye swapchains is P3 session evidence
unless a human observer records a visible frame in the headset.

## Run Identity

- Date/time UTC:
- Date/time local:
- XoxdWM commit:
- `honey` repo head:
- GitHub issue:
- Linear issue:
- Lab observer:
- Remote operator:

## Preflight

Paste the read-only status output:

```text
just honey-openxr-status honey
```

`rke2-server` is observed, not disturbed. Do not stop, restart, mask, or drain
the Honey Kubernetes substrate while collecting this P4 evidence packet.

Required fields:

- `rke2-server`:
- `rke2-agent`:
- `wayland_socket`:
- `ewwm_ipc`:
- `monado_ipc`:
- `runtime_name`:
- `runtime_library_path`:
- `openxr_client`:
- `visual_observed`:

## Display Topology

Keep host/reset/topology authority in Dell-7810 and link it here.

- Dell-7810 artifact:
- Dell management display:
  - connected / disconnected
  - result:
- Bigscreen Beyond connector:
- Bigscreen Beyond power/HID state:
- Kernel:
- GPU:
- Notes:

## P3 Session Output

Paste the bounded OpenXR run:

```text
just honey-openxr-smoke honey -- --timeout 20
```

Required fields:

- OpenXR client:
- Runtime:
- Headset:
- Session state reached:
- Eye swapchain size:
- `openxr_smoke`:
- `proof_ladder`:
- `visual_first_frame`:

## Renderer Diagnostics

Paste the compositor/OpenXR renderer diagnostics used to debug black headset
output:

```text
vr-diagnostics
```

Required fields:

- renderer backend:
- frame_wait_count:
- frame_begin_count:
- frame_end_count:
- last_readback_hash:
- last_swapchain_width:
- last_swapchain_height:
- black-screen diagnostic notes:

## P4 Human Observation

P4 requires explicit human observation. Do not promote P4 from logs alone.

- Human observed visible non-black frame:
  - yes / no
- Observer:
- Observation time:
- In-goggles result:
- If visible, describe what was visible:
- If black, record `visual_observed=no` and classify as P3 pass / P4 fail:

## Acceptance

P4 passes only when all of these are true:

- The OpenXR client reaches the Bigscreen Beyond runtime path.
- The run reaches P3 session evidence (`proof_ladder=P3_OPENXR_SESSION`).
- A human observer records visible non-black headset output.
- The tracker comment includes `visual_observed=yes`.
- The evidence does not rely on a Dell reset/power claim as XoxdWM-owned proof.

## Classification

Select exactly one:

- P4 pass: visible first frame observed
- P3 pass / P4 fail: session reached but headset stayed black
- Host/topology blocker
- Runtime/compositor blocker
- Inconclusive / rerun needed

## Tracker Comment Draft

```markdown
Honey P4 visual first-frame run:

- XoxdWM commit:
- honey repo head:
- Dell-7810 artifact:
- runtime/headset:
- eye swapchains:
- renderer diagnostics:
- openxr_smoke:
- proof_ladder:
- visual_first_frame:
- visual_observed:
- human observer:
- in-goggles result:
- classification:

Notes:
```
