# XoxdWM Support Matrix

This matrix, together with [status.md](status.md) and
[reality-check-2026-04-22.md](reality-check-2026-04-22.md), is the current
support surface for the repo.

Status vocabulary:

- `Proven`: repeatably validated on a named host or in stable automation.
- `Smoke`: packaged or manually validated once, but not yet a stable supported lane.
- `Design`: code/docs/research exist, but the flow is not yet claimed as working on a named target.

Date baseline: 2026-04-22.

## Named Hosts

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel generic lane | Proven | Running `6.19.5-7.xr.el10`; this is the persistent default. |
| `honey` kernel RT lane | Smoke | One-time boot into `6.19.5-rt1-8.xr.el10` succeeded and live `PREEMPT_RT` was verified, but RT is not the default lane. |
| `honey` XoxdWM compositor install | Design | No deployed XoxdWM binary or named-host compositor install is currently claimed on the host. |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-service`, and `/usr/local/share/openxr/1/openxr_monado.json` are present. |
| `honey` VR session | Design | Recent host recovery work has shown a stable Dell HDMI path and a `DP-2` display path, but there is still no deployed XoxdWM stack, no OpenXR client tools, and no recorded XoxdWM VR smoke path. |
| `yoga` kernel-xr boot path | Smoke | One-time boot into `6.19.5-8.xr.el10` succeeded; normal reboot returned to the saved stock Rocky default. |
| `yoga` XoxdWM install | Smoke | `yoga` now has installed `0.5.4-1.el10` `exwm-vr-*` RPMs from Actions run `24768509226`, and the real installed units pass a named-host proof with explicit `drm` backend and dedicated Emacs bootstrap. The repo-side `SuccessExitStatus=15` stop-path fix is active on-host via `/etc/systemd/user/exwm-vr-emacs.service.d/10-success-exit.conf`. `sddm` is now installed and active, and `state.conf` preselects `jsullivan2` plus `exwm-vr.desktop`, so the remaining local-login gap is the first physical `EXWM-VR` proof from the greeter path; see [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md). |
| `yoga` OpenXR userspace prereqs | Design | No Monado/OpenXR/wlroots runtime packages or runtime manifest were found. |
| `petting-zoo-mini` | Out of scope | Not a current Linux XR validation target. |

## Packaging And Install Surfaces

| Surface | Status | Notes |
| --- | --- | --- |
| GitHub release RPM | Smoke | `v0.5.1` publishes the repaired native Rocky compositor RPM, and the base package lane is named-host validated on `yoga`. This does not imply full VR enablement or a polished local session path. |
| Branch-scoped Rocky RPM | Smoke | GitHub Actions packaging run `24768509226` produced `0.5.4-1.el10` RPMs for `codex/reality-authority-surface`, and that payload is now both installed and named-host validated on `yoga`. It is still not claimed as a public release. |
| GitHub release DEB | Smoke | Public release exists. |
| Nix flake outputs | Smoke | Linux-oriented build surfaces exist. Local Darwin flake evaluation now works, but Darwin is still not the authoritative runtime or build target for this repo. |
| Rocky 10 quickstart | Smoke | Valid for the base compositor package lane on `yoga`. Full VR/OpenXR enablement, SELinux hardening, and BrainFlow BCI packaging remain separate follow-on paths. |
| NixOS / Home Manager module | Design | Modules exist, but the published examples currently drift from the module option shapes and there is no named-host proof in this pass. |

## Runtime Areas

| Area | Status | Notes |
| --- | --- | --- |
| Headless compositor | Smoke | Explicit build/test path exists. |
| Desktop 2D compositor path | Smoke | Earlier bounded startup was proved on `yoga`, and the installed `0.5.4` user-unit path now reaches compositor plus Emacs initialization on the host. It is still not yet a polished local desktop/session lane. |
| DRM backend on AMD | Smoke | Code exists and bounded named-host startup is recorded on `yoga`; full `honey` VR smoke remains pending. |
| VR session lifecycle | Design | Not yet claimed as working on `honey`. |
| DRM lease / HMD non-desktop handling | Design | Code exists; validation still pending. |
| Eye tracking | Design | Documented and implemented in part; no named-host support claim. |
| Hand tracking / gestures | Design | Same. |
| BCI / BrainFlow | Design | Same. |

## CI

| Workflow Area | Status | Notes |
| --- | --- | --- |
| Lightweight CI on code changes | Smoke | Kept as primary CI surface. |
| Rocky Linux test | Smoke | Workflow exists; recent repo work has focused on reducing optional cache/bootstrap failures, but live Actions state should still be checked before calling the lane green. |
| Self-hosted fast CI | Smoke | Shared `xoxdwm-nix` lane now runs on `Jesssullivan/XoxdWM`; repo-level runner inventory is still opaque, but the path is proven. |
| VR hardware-in-the-loop CI | Design | Honey-backed jobs now require explicit `USE_VR_HARDWARE=true` instead of a fork-shaped repo-name gate. The lane is not yet claimed as active on the canonical repo by default. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health; the VR runner check is explicit opt-in. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Proven` or `Smoke`, treat it as `Design` regardless of how detailed the subsystem docs are.
