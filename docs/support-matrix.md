# XoxdWM Support Matrix

This matrix is the operational truth for the repo.

Status vocabulary:

- `Proven`: repeatably validated on a named host or in stable automation.
- `Smoke`: packaged or manually validated once, but not yet a stable supported lane.
- `Design`: code/docs/research exist, but the flow is not yet claimed as working on a named target.

Date baseline: 2026-04-12.

## Named Hosts

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel generic lane | Proven | Running `6.19.5-7.xr.el10`; this is the persistent default. |
| `honey` kernel RT lane | Smoke | One-time boot into `6.19.5-rt1-8.xr.el10` succeeded and live `PREEMPT_RT` was verified, but RT is not the default lane. |
| `honey` XoxdWM compositor install | Design | No `xoxdwm` binary or host checkout was found. |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-service`, and `/usr/local/share/openxr/1/openxr_monado.json` are present. |
| `honey` VR session | Design | No active DRM connector or obvious HMD path was observed, and no OpenXR client tools were installed. |
| `yoga` kernel-xr boot path | Smoke | One-time boot into `6.19.5-8.xr.el10` succeeded; normal reboot returned to the saved stock Rocky default. |
| `yoga` XoxdWM install | Design | No XoxdWM binary or package install was found. |
| `yoga` OpenXR userspace prereqs | Design | No Monado/OpenXR/wlroots runtime packages or runtime manifest were found. |
| `petting-zoo-mini` | Out of scope | Not a current Linux XR validation target. |

## Packaging And Install Surfaces

| Surface | Status | Notes |
| --- | --- | --- |
| GitHub release RPM | Design | Public release exists, but the current `v0.5.0` Rocky RPM failed on `honey`: it depends on bare `wayland` and ships a `/nix/store`-linked compositor binary. |
| GitHub release DEB | Smoke | Public release exists. |
| Nix flake outputs | Smoke | Build surfaces exist; critical push CI is not green yet. |
| Rocky 10 quickstart | Design | Keep as packaging guidance only until a corrected native RPM or validated source-build path succeeds on a named Rocky host. |
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
| Self-hosted fast CI | Smoke | Still useful, but now skips cleanly on `Jesssullivan/XoxdWM` when no matching self-hosted runner scope exists. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Proven` or `Smoke`, treat it as `Design` regardless of how detailed the subsystem docs are.
