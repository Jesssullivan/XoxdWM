# XoxdWM Support Matrix

This matrix is the operational truth for the repo.

Status vocabulary:

- `Proven`: repeatably validated on a named host or in stable automation.
- `Smoke`: packaged or manually validated once, but not yet a stable supported lane.
- `Design`: code/docs/research exist, but the flow is not yet claimed as working on a named target.

Date baseline: 2026-04-11.

## Named Hosts

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel | Proven | Running `6.19.5-5.xr.el10`. |
| `honey` XoxdWM compositor install | Design | No deployed XoxdWM packages found. |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs` present; `monado.service` exists but is disabled. |
| `honey` VR session | Design | Not yet claimed as working end-to-end. |
| `yoga` kernel | Proven | Running stock Rocky 10 kernel. |
| `yoga` XoxdWM install | Design | No XoxdWM/OpenXR packages found. |
| `petting-zoo-mini` | Out of scope | Not a current Linux XR validation target. |

## Packaging And Install Surfaces

| Surface | Status | Notes |
| --- | --- | --- |
| GitHub release RPM | Smoke | Public release exists and ships `ewwm-compositor`. |
| GitHub release DEB | Smoke | Public release exists. |
| Nix flake outputs | Smoke | Build surfaces exist; critical push CI is not green yet. |
| Rocky 10 quickstart | Smoke | Documented as compositor/package path only. |
| NixOS / Home Manager module | Smoke | Present in repo, but not claimed as proven in this audit. |

## Runtime Areas

| Area | Status | Notes |
| --- | --- | --- |
| Headless compositor | Smoke | Explicit build/test path exists. |
| Desktop 2D compositor path | Smoke | Packaged, but not yet named-host validated in this pass. |
| DRM backend on AMD | Smoke | Code and workflows exist; host validation remains pending. |
| VR session lifecycle | Design | Not yet claimed as working on `honey`. |
| DRM lease / HMD non-desktop handling | Design | Code exists; validation still pending. |
| Eye tracking | Design | Documented and implemented in part; no named-host support claim. |
| Hand tracking / gestures | Design | Same. |
| BCI / BrainFlow | Design | Same. |

## CI

| Workflow Area | Status | Notes |
| --- | --- | --- |
| Lightweight CI on code changes | Smoke | Kept as primary CI surface. |
| Rocky Linux test | Smoke | Workflow exists; current failure was isolated to optional `sccache` setup. |
| Self-hosted fast CI | Smoke | Still useful, but no longer triggered for docs-only changes. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Proven` or `Smoke`, treat it as `Design` regardless of how detailed the subsystem docs are.
