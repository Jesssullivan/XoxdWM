# `honey` Fresh-Boot Evidence Template

Use this template after running
[honey-fresh-boot-runbook-2026-04-26.md](honey-fresh-boot-runbook-2026-04-26.md).
It is meant for GitHub issue `#20`, Linear `TIN-346`, and any follow-up doc
promotion.

Do not use this template to record reset, PSU, BIOS, SMI, or display-topology
truth as if it were XoxdWM-owned evidence. Link those Dell-7810 artifacts here
and keep this packet focused on downstream compositor/OpenXR behavior.

## Run Identity

- Date/time UTC:
- Date/time local:
- Operator on `neo`:
- Lab operator at `honey`:
- XoxdWM commit:
- `honey` commit:
- GitHub issue:
- Linear issue:

## Dell-7810 Host Evidence Links

- Dell reset/power/display artifact:
- Boot action type:
  - warm reboot / hard reset / power cycle / other
- Dell HDMI management display:
  - connected / disconnected
  - result:
- Bigscreen Beyond DP path:
  - connected / disconnected
  - result:
- Tailscale return:
  - yes / no
- Local IPv4 return:
  - yes / no
- Host/kernel notes from Dell-7810:

## XoxdWM Pre-Boot Baseline

Paste the pre-boot status-only output:

```text
```

Required fields:

- pre-boot `boot_id`:
- pre-boot `rke2-server`:
- pre-boot `rke2-agent`:
- pre-boot OpenXR client:
- pre-boot IPC/socket state:

## Post-Boot Fresh-Boot Check

Command:

```sh
just honey-openxr-fresh-boot-check honey 1 20
```

Paste the post-boot output:

```text
```

Required fields:

- post-boot `boot_id`:
- boot ID changed:
  - yes / no
- `honey` repo head:
- `rke2-server` after check:
- `rke2-agent` after check:
- OpenXR client:
- runtime:
- headset:
- eye swapchain size:
- `openxr_smoke`:

## In-Goggles Observation

CLI smoke is not first-frame proof by itself.

- Human observed visible frame:
  - yes / no / not observed
- Observer:
- Headset/display note:
- If no visible frame, classify as product/visual even if CLI smoke passed.

## Classification

Select exactly one primary classification:

- Pass: fresh-boot OpenXR smoke
- Host/substrate blocker
- Packaging/deployment blocker
- Compositor/runtime blocker
- Product/visual blocker
- Inconclusive / rerun needed

Rationale:

## Tracker Comment Draft

```markdown
Fresh-boot evidence run:

- XoxdWM commit:
- honey commit:
- Dell artifact:
- boot action:
- boot ID changed:
- rke2-server after check:
- packaged client:
- runtime/headset:
- eye swapchains:
- openxr_smoke:
- in-goggles frame:
- classification:

Notes:
```

## Promotion Decision

- Update [support-matrix.md](support-matrix.md):
  - yes / no
- Update [status.md](status.md):
  - yes / no
- Keep as issue-only evidence:
  - yes / no
- Reason:

