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

For PREEMPT_RT specifically, use the Dell claim ladder:

- Dell-7810 owns C1/C2/C3 for `honey` RT host evidence and workstation acceptance
- XoxdWM should only claim C4, meaning a demonstrated downstream software benefit under RT

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel generic lane | Proven | Running `6.19.5-7.xr.el10`; this is the persistent default. Baseline validated in [`Dell-7810/docs/platform/host-kernel-baseline.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/host-kernel-baseline.md). |
| `honey` kernel RT lane | Smoke | Dell-7810 has one-time RT boot proof and host-posture validation for `6.19.5-rt1-8.xr.el10`, but XoxdWM does not yet claim a downstream XR/software RT benefit on `honey`. |
| `honey` XoxdWM compositor install | Smoke | `honey` now has branch-scoped `exwm-vr-0.5.4-1.el10` packages installed, and `systemctl --user start exwm-vr.target` reached a bounded named-host startup with active compositor plus Emacs, `DP-2` at `5088x2544@75Hz`, `HDMI-A-2` at `1920x1080@60Hz`, and `ewwm: initialized`. Reinstalling the `24776900393` compositor RPM put the lease-capable `/usr/bin/ewwm-compositor` onto the host, and the installed package surface now grants a real DRM lease to Monado when `DP-2` is designated via the supported `~/.config/exwm-vr/compositor.env` surface; see [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md). |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-beyond-0.0.1-1.20260310git.el10`, `/usr/bin/monado-service`, `/usr/share/openxr/1/openxr_monado.json`, and `/etc/xdg/openxr/1/active_runtime.json` are present. `monado-cli probe` can now identify the Bigscreen Beyond when the host uses the explicit `DP-2`/SteamVR environment. |
| `honey` VR session | Smoke | On `2026-04-22`, the installed `ewwm-compositor` from the reinstalled `24776900393` RPM artifact granted a real `DP-2` DRM lease to Monado on `honey`, and `hello_xr -g Vulkan` reached `READY` with two eye swapchains at `3561x3561`. The compositor-side lease designation now uses `~/.config/exwm-vr/compositor.env`, and a repo-owned `exwm-vr-monado.service` now uses `~/.config/exwm-vr/monado.env` without service drop-ins. A later staged `monado-beyond` companion RPM proof from run `24804821792` reached active Monado plus eye swapchain creation, and the installed-host follow-up from run `24807084915` now reaches the same direct-mode proof on `honey` with `/usr/bin/monado-service` and no `MONADO_SERVICE_BIN` override. The repo now carries `exwm-vr-openxr-smoke` plus `just honey-openxr-status` / `just honey-openxr-smoke`; the remaining gap is packaged client provenance and repeated runs, not an operator command shape. |
| `yoga` kernel-xr boot path | Smoke | One-time boot into `6.19.5-8.xr.el10` succeeded; normal reboot returned to the saved stock Rocky default. |
| `yoga` XoxdWM install | Smoke | `yoga` now has refreshed installed `0.5.4-1.el10` `exwm-vr-*` RPMs from the current branch, and the real installed units pass a named-host proof with explicit `drm` backend and dedicated Emacs bootstrap. Actions run `24771056471` carried the packaged `SuccessExitStatus=15` stop-path fix onto the host, so no `/etc/systemd/user` drop-in is still required. `sddm` is installed and active, `state.conf` preselects `jsullivan2` plus `exwm-vr.desktop`, and a controlled `sddm-autologin` run reached a real `wayland` `EXWM-VR` user session on `seat0`; see [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md). |
| `yoga` OpenXR userspace prereqs | Design | No Monado/OpenXR/wlroots runtime packages or runtime manifest were found. |
| `petting-zoo-mini` | Out of scope | Not a current Linux XR validation target. |

## Packaging And Install Surfaces

| Surface | Status | Notes |
| --- | --- | --- |
| GitHub release RPM | Smoke | `v0.5.1` publishes the repaired native Rocky compositor RPM, and the base package lane is named-host validated on `yoga`. This does not imply full VR enablement or a polished local session path. |
| Branch-scoped Rocky RPM | Smoke | GitHub Actions packaging runs `24768509226`, `24771056471`, and `24776900393` produced branch-scoped `0.5.4-1.el10` RPMs for `codex/reality-authority-surface`; the refreshed payload is installed and named-host validated on `yoga`, and the `24776900393` artifact produced the staged `honey` direct-mode lease proof. It is still not claimed as a public release. |
| Monado companion Rocky RPM | Smoke | GitHub Actions runs `24804821792` and `24807084915` produced `monado-beyond-0.0.1-1.20260310git.el10.x86_64.rpm`. The first run proved a staged non-root path on `honey`; the second was installed on-host with normal dependency resolution, switched the active runtime to `/usr/share/openxr/1/openxr_monado.json`, removed the `MONADO_SERVICE_BIN` host override, and reached active direct-mode Monado plus eye swapchain creation through `/usr/bin/monado-service`. |
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
| VR session lifecycle | Smoke | `honey` now has a one-time direct-mode session proof where `hello_xr -g Vulkan` reached `READY` and created eye swapchains from the installed compositor package surface. It also has a staged Monado companion RPM proof that reached active direct-mode Monado plus eye swapchain creation. The repo-owned OpenXR smoke wrapper makes the client invocation repeatable, but neither path is yet claimed as a stable repeated run or first-frame proof. |
| DRM lease / HMD non-desktop handling | Smoke | The live `honey` direct-mode probe now initializes `wp_drm_lease_v1`, keeps `DP-2` out of the desktop output map when explicitly designated, and grants a real lease to Monado. The remaining gap is supported host configuration and repeatability, not missing lease support. |
| Eye tracking | Design | Documented and implemented in part; no named-host support claim. |
| Hand tracking / gestures | Design | Same. |
| BCI / BrainFlow | Design | Same. |

## CI

| Workflow Area | Status | Notes |
| --- | --- | --- |
| Lightweight CI on code changes | Smoke | Kept as primary CI surface. |
| Rocky Linux test | Smoke | Workflow exists and the current branch proof lane is green: run `24776895510` passed both `Rocky Linux 10 Build & Test` and `Rocky Linux 10 + Nix` on `2026-04-22`. |
| Self-hosted fast CI | Smoke | The stale `xoxdwm-nix` repo-shaped lane is no longer treated as current authority. Self-hosted Nix workflows target the shared `tinyland-nix` GloriousFlywheel capability lane only when both `USE_SELFHOSTED` and `GF_SHARED_RUNNERS_REACHABLE` are true; otherwise they fall back to hosted Linux or skip self-hosted-only jobs while enrollment/scheduling is tracked separately from product proof. |
| VR hardware-in-the-loop CI | Design | Honey-backed jobs now require explicit `USE_VR_HARDWARE=true` instead of a fork-shaped repo-name gate. The lane is not yet claimed as active on the canonical repo by default. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health; the VR runner check is explicit opt-in. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Proven` or `Smoke`, treat it as `Design` regardless of how detailed the subsystem docs are.
