# XoxdWM Support Matrix

This matrix, together with [status.md](status.md) and
[reality-check-2026-04-22.md](reality-check-2026-04-22.md), is the current
support surface for the repo.

Status vocabulary:

- `Product`: repeatably validated on a named host or stable automation and
  ready to present as a supported product surface.
- `Smoke`: packaged or manually validated once, but not yet a stable supported lane.
- `Prototype`: active implementation exists, but the path still needs named-host
  proof before it becomes a support claim.
- `Synthetic`: code/static/headless proof exists, but no named visual runtime proof.
- `Design`: code/docs/research exist, but the flow is not yet claimed as working on a named target.

Date baseline: 2026-05-10.

## Honey XR Proof Ladder

Use this ladder for `honey` VR claims. A higher level includes the levels below
it, and CLI smoke never substitutes for the human observation required at P4.

| Level | Name | Required Evidence |
| --- | --- | --- |
| P0 | Inventory | Host, kernel, GPU, DP/HDMI topology, USB `35bd`, hidraw, runtime JSON, service state, and rke2 state captured read-only. |
| P1 | Host Substrate | Kernel, firmware, permissions, active runtime, and package paths are present without disturbing rke2 or display topology. |
| P2 | Lease/Runtime | The compositor advertises the live headset connector for DRM lease and Monado selects the Bigscreen Beyond runtime path. |
| P3 | OpenXR Session | An OpenXR client reaches the HMD, creates eye swapchains, and reaches `READY`/`VISIBLE`/`FOCUSED` or equivalent session markers. |
| P4 | Visual First Frame | A human observer records visible non-black headset output. `visual_observed=yes` must be explicit. |
| P5 | Fresh-Boot Repeatability | P4 survives an attended fresh boot with changed `boot_id` and rke2 still active. |
| P6 | Operator Stability | Repeated use is stable enough for an operator workflow, not only bounded smoke. |

Current `honey` classification: P3 pass / P4 fail. The May 2026 session reached
`FOCUSED` with swapchains, but `visual_observed=no` because the goggles stayed
black. Use
[honey-p4-visual-first-frame-evidence-template.md](honey-p4-visual-first-frame-evidence-template.md)
for any #49 promotion attempt; the fresh-boot template is the separate P5
repeatability surface.

## Named Hosts

Hardware setup, reset behavior, BIOS/SMI characterization, and kernel baseline
validation for `honey` (Dell Precision 7810) are documented in the companion
[`Dell-7810`](https://github.com/Jesssullivan/Dell-7810) repository. See the
[authority map](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/authority-map.md)
for which surfaces each repo owns.

For PREEMPT_RT specifically, use the Dell claim ladder:

- Dell-7810 owns C1/C2/C3 for `honey` RT host evidence and workstation acceptance
- XoxdWM should only claim C4, meaning a demonstrated downstream software benefit under RT
- Current Dell evidence treats RT as an experimental lane, not a default requirement; see
  [`Dell-7810/docs/publication/rt-benefit-decision-framework-2026-04-26.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/publication/rt-benefit-decision-framework-2026-04-26.md)

| Target | Status | Notes |
| --- | --- | --- |
| `honey` kernel generic lane | Smoke | The May 2026 lab pass was on `6.19.5-10.xr.el10`; generic XR kernel boot remains the default lane. Baseline validation belongs in [`Dell-7810/docs/platform/host-kernel-baseline.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/host-kernel-baseline.md). |
| `honey` kernel RT lane | Smoke | Dell-7810 has RT boot proof, host-posture validation, and a cautionary repeated RT Chapel packet for `6.19.5-rt1-8.xr.el10`, but XoxdWM does not yet claim a downstream XR/software RT benefit on `honey`. |
| `honey` XoxdWM compositor install | Smoke | `honey` has branch-scoped `exwm-vr-0.5.4-1.el10` packages installed, and `systemctl --user start exwm-vr.target` reached bounded named-host startup with active compositor plus Emacs and `ewwm: initialized`. Reinstalling the `24776900393` compositor RPM put the lease-capable `/usr/bin/ewwm-compositor` onto the host, and the installed package surface grants a real DRM lease to Monado when the live headset connector is designated via `~/.config/exwm-vr/compositor.env`; see [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md). May 2026 live topology is `card0-DP-1` for BS2E and `card0-HDMI-A-1` for Dell, superseding the April `DP-2`/`HDMI-A-2` sample. |
| `honey` OpenXR userspace prereqs | Smoke | `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-beyond-0.0.1-1.20260310git.el10`, `/usr/bin/monado-service`, `/usr/share/openxr/1/openxr_monado.json`, and `/etc/xdg/openxr/1/active_runtime.json` are present. `monado-cli`/OpenXR clients can identify the Bigscreen Beyond when the host uses the explicit live connector and SteamVR environment. |
| `honey` VR session | Smoke | The installed compositor can grant a real DRM lease to Monado on `honey`, and the repo-owned `exwm-vr-monado.service`, `exwm-vr-openxr-smoke`, and installed smoke-client package now prove the packaged client lane. On `2026-05-09`, with BS2E on `card0-DP-1`, `hello_xr -g Vulkan` selected Monado / Bigscreen Beyond, created `3561x3561` eye swapchains, and reached `READY -> SYNCHRONIZED -> VISIBLE -> FOCUSED`. The headset remained black to the human observer, so the current blocker is product/visual first-frame after successful session bring-up. The corrected `beyond-power-on` helper must also be refreshed on-host before its service result is treated as panel-power evidence. |
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
| OpenXR smoke-client Rocky RPM | Smoke | GitHub Actions run `24938791255` produced `exwm-vr-openxr-smoke-client-0.0.1-1.20260425git.el10.x86_64.rpm`; the package is installed on `honey`, provides `/usr/bin/exwm-vr-hello-xr -> ../libexec/exwm-vr/hello_xr`, and three bounded runs plus three clean stop/start cycles on `2026-04-25` EDT selected `/usr/libexec/exwm-vr/hello_xr -g Vulkan` successfully. This proves packaged-client runtime smoke across clean user-service cycles, not fresh-boot stability by itself; use the Honey Fresh-Boot Runbook before promoting that claim. |
| GitHub release DEB | Smoke | Public release exists. |
| Nix flake outputs | Smoke | Linux-oriented build surfaces exist. Local Darwin flake evaluation now works, but Darwin is still not the authoritative runtime or build target for this repo. |
| Rocky 10 quickstart | Smoke | Valid for the base compositor package lane on `yoga`. Full VR/OpenXR enablement, SELinux hardening, and BrainFlow BCI packaging remain separate follow-on paths. |
| NixOS / Home Manager module | Design | Modules exist, but the published examples currently drift from the module option shapes and there is no named-host proof in this pass. |

## Runtime Areas

| Area | Status | Notes |
| --- | --- | --- |
| Headless compositor | Smoke | Explicit build/test path exists. |
| Desktop 2D compositor path | Smoke | Earlier bounded startup was proved on `yoga`, and the installed `0.5.4` user-unit path now reaches compositor plus Emacs initialization on the host. It is still not yet a polished local desktop/session lane. |
| Native WM authority | Smoke | Rust owns static/configured workspace, layout, key-action, app-launch, config-reload, autostart, session, idle, and DPMS IPC surfaces. The `2026-05-10` Honey live-session proof exercised native IPC hello, workspace list, focused surface, layout cycle, workspace switch, and configured app launch with `exwm-vr-emacs.service` inactive; #45 closed after merged PR #78. The package/Nix default `xoxdwm.target` now starts the native compositor without pulling Emacs, while `exwm-vr.target` remains explicit compatibility. This is bounded authority proof, not a polished daily-driver desktop claim. |
| DRM backend on AMD | Smoke | Code exists and bounded named-host startup is now recorded on both `yoga` and `honey`; `honey` now also has direct-mode lease proof with Monado plus `hello_xr`, including packaged-client smoke across clean user-service cycles. It is not yet a fresh-boot operator lane. |
| VR session lifecycle | Smoke | `honey` now has a direct-mode session proof where `hello_xr -g Vulkan` reached `FOCUSED` and created eye swapchains from the installed compositor package surface. The repo-owned OpenXR smoke wrapper plus installed smoke-client package give repeated smoke evidence, but the packaged-client path is not yet claimed as stable first-frame, long-running, or daily-driver proof because the current in-goggles result is black. |
| DRM lease / HMD non-desktop handling | Smoke | The live `honey` direct-mode probe initializes `wp_drm_lease_v1`, keeps the explicitly designated headset connector out of the desktop output map, and grants a real lease to Monado. In May 2026 the live connector is `DP-1`; the kernel still does not expose a useful `non_desktop` sysfs property locally, so explicit connector designation remains required. |
| Eye tracking | Design | Documented and implemented in part; no named-host support claim. |
| Hand tracking / gestures | Design | Same. |
| BCI / BrainFlow | Design | Same. |

## Multimodal Input Reality Ladder

This ladder is the claim boundary for eye, head-gaze, hand, HID, mouth/voice,
and BCI input. Upgrade any row only after the named acquisition proof in its
gate exists; synthetic tests and subsystem inventories do not count as real
acquisition proof or promote product support.

| Surface | Class | Current Proof | Upgrade Gate |
| --- | --- | --- | --- |
| Eye tracking / gaze control | Design | OpenXR, Pupil Labs, and simulated gaze paths are documented and partially implemented, but no named-host packet records real eye-gaze acquisition. | Record a dated `honey`/`yoga` acquisition packet from OpenXR `XR_EXT_eye_gaze_interaction` or Pupil ZMQ with device, host, confidence/rate, and compositor/client consumer. |
| Head-gaze fallback | Prototype | Head-pose and gaze-zone code exists as a fallback interaction design, but it is not evidence for eye tracking. | Record a VR pose acquisition plus target-selection proof on a named host and keep it separate from eye-gaze claims. |
| Hand tracking / gestures | Design | Hand/gesture modules and tests exist; no current named-host hand-tracking acquisition proof is claimed. | Capture an OpenXR hand-tracking or controller/gesture stream on a named host. |
| HMD HID helpers | Synthetic | The Bigscreen Beyond HID helper is corrected and statically guarded, but installed-host refresh and observed device effect are still separate evidence. | Refresh the helper on `honey`, capture the hidraw device transaction, and record the observed panel/service effect. |
| BCI / BrainFlow acquisition | Design | BrainFlow/OpenBCI docs and Lisp modules exist; current automated coverage uses synthetic board/data paths rather than a real EEG stream. | Record a BrainFlow/OpenBCI acquisition packet with board ID, serial path, sample rate, channel quality, and consumer path on a named host. |
| Synthetic gaze / BCI injection | Synthetic | Simulated gaze and BrainFlow board `0` exist for tests and development only. | Never promote directly; replace with a real acquisition packet before any smoke/product claim. |
| Mouth / voice input | Design | Voice and mouth input are archived roadmap/design material; no active XoxdWM input path is claimed. | Add an active acquisition pipeline, tests, and named-host capture before claiming prototype or smoke support. |

## CI

| Workflow Area | Status | Notes |
| --- | --- | --- |
| Lightweight CI on code changes | Smoke | Kept as primary CI surface. |
| Rocky Linux test | Smoke | Workflow exists and the current branch proof lane is green: run `24776895510` passed both `Rocky Linux 10 Build & Test` and `Rocky Linux 10 + Nix` on `2026-04-22`. |
| Self-hosted fast CI | Smoke | The stale `xoxdwm-nix` repo-shaped lane is no longer treated as current authority. Self-hosted Nix workflows target the shared `tinyland-nix` GloriousFlywheel capability lane only when both `USE_SELFHOSTED` and `GF_SHARED_RUNNERS_REACHABLE` are true; otherwise they fall back to hosted Linux or skip self-hosted-only jobs while enrollment/scheduling is tracked separately from product proof. |
| VR hardware-in-the-loop CI | Design | Honey-backed jobs now require explicit `USE_VR_HARDWARE=true` instead of a fork-shaped repo-name gate. The lane is not yet claimed as active on the canonical repo by default. |
| Scheduled runner health | Smoke | Reduced to weekly to avoid daily noise dominating repo health; the VR runner check is explicit opt-in. |

## Interpretation

The feature matrix is a subsystem inventory, not a support promise. If a capability is not listed here as `Product` or `Smoke`, treat it as `Design` unless a narrower row explicitly labels it `Prototype` or `Synthetic`.
