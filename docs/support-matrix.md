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

Hardware setup, reset behavior, BIOS/SMI characterization, and kernel baseline
validation for `honey` (Dell Precision 7810) are documented in the companion
[`Dell-7810`](https://github.com/Jesssullivan/Dell-7810) repository. See the
[authority map](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/authority-map.md)
for which surfaces each repo owns.

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel generic lane | Proven | Running `6.19.5-7.xr.el10`; this is the persistent default. Baseline validated in [`Dell-7810/docs/platform/host-kernel-baseline.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/host-kernel-baseline.md). |
| `honey` kernel RT lane | Smoke | One-time boot into `6.19.5-rt1-8.xr.el10` succeeded and live `PREEMPT_RT` was verified, but RT is not the default lane. |
| `honey` XoxdWM compositor install | Smoke | `honey` now has branch-scoped `exwm-vr-0.5.4-1.el10` packages installed, and `systemctl --user start exwm-vr.target` reached a bounded named-host startup with active compositor plus Emacs, `DP-2` at `5088x2544@75Hz`, `HDMI-A-2` at `1920x1080@60Hz`, and `ewwm: initialized`. A follow-up staged binary from packaging run `24776900393` then initialized `wp_drm_lease_v1`, reserved `DP-2`, and granted a real DRM lease to Monado; see [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md). |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-service`, `/usr/local/share/openxr/1/openxr_monado.json`, and `/etc/xdg/openxr/1/active_runtime.json` are present. `monado-cli probe` can now identify the Bigscreen Beyond when the host uses the explicit `DP-2`/SteamVR environment. |
| `honey` VR session | Smoke | On `2026-04-22`, a staged `ewwm-compositor` binary from commit `3cae58e` granted a real `DP-2` DRM lease to Monado on `honey`, and `hello_xr -g Vulkan` reached `READY` with two eye swapchains at `3561x3561`. This is still a one-shot manually staged proof that depends on user-unit overrides and a literal IPC shim for the local `hello_xr` build, so it is not yet a repeatable installed operator lane. |
| `yoga` kernel-xr boot path | Smoke | One-time boot into `6.19.5-8.xr.el10` succeeded; normal reboot returned to the saved stock Rocky default. |
| `yoga` XoxdWM install | Smoke | `yoga` now has refreshed installed `0.5.4-1.el10` `exwm-vr-*` RPMs from the current branch, and the real installed units pass a named-host proof with explicit `drm` backend and dedicated Emacs bootstrap. Actions run `24771056471` carried the packaged `SuccessExitStatus=15` stop-path fix onto the host, so no `/etc/systemd/user` drop-in is still required. `sddm` is installed and active, `state.conf` preselects `jsullivan2` plus `exwm-vr.desktop`, and a controlled `sddm-autologin` run reached a real `wayland` `EXWM-VR` user session on `seat0`; see [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md). |
| `yoga` OpenXR userspace prereqs | Design | No Monado/OpenXR/wlroots runtime packages or runtime manifest were found. |
| `petting-zoo-mini` | Out of scope | Not a current Linux XR validation target. |

## Packaging And Install Surfaces

| Surface | Status | Notes |
| --- | --- | --- |
| GitHub release RPM | Smoke | `v0.5.1` publishes the repaired native Rocky compositor RPM, and the base package lane is named-host validated on `yoga`. This does not imply full VR enablement or a polished local session path. |
| Branch-scoped Rocky RPM | Smoke | GitHub Actions packaging runs `24768509226`, `24771056471`, and `24776900393` produced branch-scoped `0.5.4-1.el10` RPMs for `codex/reality-authority-surface`; the refreshed payload is installed and named-host validated on `yoga`, and the `24776900393` artifact produced the staged `honey` direct-mode lease proof. It is still not claimed as a public release. |
| GitHub release DEB | Smoke | Public release exists. |
| Nix flake outputs | Smoke | Linux-oriented build surfaces exist. Local Darwin flake evaluation now works, but Darwin is still not the authoritative runtime or build target for this repo. |
| Rocky 10 quickstart | Smoke | Valid for the base compositor package lane on `yoga`. Full VR/OpenXR enablement, SELinux hardening, and BrainFlow BCI packaging remain separate follow-on paths. |
| NixOS / Home Manager module | Design | Modules exist, but the published examples currently drift from the module option shapes and there is no named-host proof in this pass. |

## Runtime Areas

| Area | Status | Notes |
| --- | --- | --- |
| Headless compositor | Smoke | Explicit build/test path exists. |
| Desktop 2D compositor path | Smoke | Earlier bounded startup was proved on `yoga`, and the installed `0.5.4` user-unit path now reaches compositor plus Emacs initialization on the host. It is still not yet a polished local desktop/session lane. |
| DRM backend on AMD | Smoke | Code exists and bounded named-host startup is now recorded on both `yoga` and `honey`; `honey` now also has a one-shot direct-mode lease proof with Monado plus `hello_xr`, but it is not yet a repeated installed operator lane. |
| VR session lifecycle | Smoke | `honey` now has a one-time direct-mode session proof where `hello_xr -g Vulkan` reached `READY` and created eye swapchains. This is still not claimed as a stable repeated run or first-frame proof. |
| DRM lease / HMD non-desktop handling | Smoke | The live `honey` direct-mode probe now initializes `wp_drm_lease_v1`, keeps `DP-2` out of the desktop output map, and grants a real lease to Monado. The remaining gap is packaging and repeatability, not missing lease support. |
| Eye tracking | Design | Documented and implemented in part; no named-host support claim. |
| Hand tracking / gestures | Design | Same. |
| BCI / BrainFlow | Design | Same. |

## CI

| Workflow Area | Status | Notes |
| --- | --- | --- |
| Lightweight CI on code changes | Smoke | Kept as primary CI surface. |
| Rocky Linux test | Smoke | Workflow exists and the current branch proof lane is green: run `24776895510` passed both `Rocky Linux 10 Build & Test` and `Rocky Linux 10 + Nix` on `2026-04-22`. |
| Self-hosted fast CI | Smoke | Shared `xoxdwm-nix` lane now runs on `Jesssullivan/XoxdWM`; repo-level runner inventory is still opaque, but the path is proven. |
| VR hardware-in-the-loop CI | Design | Honey-backed jobs now require explicit `USE_VR_HARDWARE=true` instead of a fork-shaped repo-name gate. The lane is not yet claimed as active on the canonical repo by default. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health; the VR runner check is explicit opt-in. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Proven` or `Smoke`, treat it as `Design` regardless of how detailed the subsystem docs are.
