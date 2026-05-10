# XoxdWM Status

Snapshot date: 2026-05-09

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- `v0.5.1` is publicly released and publishes the repaired Rocky compositor RPM and the DEB artifact.
- The base Rocky RPM gate is complete: the public package now installs on Rocky 10 without the earlier bare-`wayland` metadata or `/nix/store` runtime-linking problems.
- The release lane now treats Monado integration, SELinux hardening, and the BrainFlow BCI virtualenv as separate opt-in package paths so the base Rocky compositor package can stay shippable.
- `honey` is not currently a stable deployed XoxdWM VR stack.
- `honey` now has a proven `linux-xr` generic default; Dell-7810 owns the current RT host-validation ledger, while XoxdWM still treats RT benefit as unproven on the software side.
- `honey` now has refreshed `exwm-vr-0.5.4-1.el10`, `exwm-vr-compositor-0.5.4-1.el10`, and `exwm-vr-elisp-0.5.4-1.el10` installed from branch-scoped RPMs.
- `honey` has XR prereqs plus explicit runtime activation: `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-beyond-0.0.1-1.20260310git.el10`, `/usr/bin/monado-service`, `/usr/share/openxr/1/openxr_monado.json`, and `/etc/xdg/openxr/1/active_runtime.json`.
- `honey` does not currently expose `openxr-info`, `xrgears`, or `xoxdwm`.
- `honey` has a bounded named-host XoxdWM compositor startup: `exwm-vr.target`, `exwm-vr-compositor.service`, and `exwm-vr-emacs.service` reached `active`, IPC initialized, and Emacs reached `ewwm: initialized`; see [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md).
- `honey` now has direct-mode client proof from the installed package surface too: after reinstalling `exwm-vr-compositor-0.5.4-1.el10` from packaging run `24776900393`, `/usr/bin/ewwm-compositor` initialized `wp_drm_lease_v1`, read the explicit headset connector from `~/.config/exwm-vr/compositor.env`, kept that connector out of the desktop output map, and granted a real DRM lease to Monado.
- `honey` now also has a repo-owned Monado service proof on three runtime surfaces: `/usr/lib/systemd/user/exwm-vr-monado.service` plus `~/.config/exwm-vr/monado.env` reached direct-mode proof first with the older local `/usr/local/bin/monado-service` lane, then with a staged `monado-beyond` companion RPM tree from GitHub Actions run `24804821792`, and now with an installed `monado-beyond` host package from run `24807084915`.
- `hello_xr -g Vulkan` on `honey` now reaches `xrCreateInstance`, `xrGetSystem`, `Bigscreen Beyond`, two eye swapchains at `3561x3561`, and, in the May 2026 current-boot lab pass, `READY -> SYNCHRONIZED -> VISIBLE -> FOCUSED`.
- Current live Honey topology from the May 2026 lab pass is `card0-DP-1` for BS2E and `card0-HDMI-A-1` for the Dell management panel, not the older April `DP-2`/`HDMI-A-2` mapping. Host config must follow the live connector sample before attempting goggles proof.
- The repo now carries `packaging/scripts/exwm-vr-hmd-connector` so setup/status/proof helpers resolve the headset as connected DP `non_desktop=1`, then BIG EDID `0x1234`/`0x5095`, then explicit override, instead of assuming Honey is always `DP-2`.
- The May 2026 lab pass still failed human-visible first frame: the OpenXR session reached `FOCUSED`, but the goggles remained black. Treat this as `P3 OpenXR Session` pass / `P4 Visual First Frame` fail after successful host visibility, service lifecycle, DRM lease routing, runtime selection, and OpenXR session bring-up.
- The packaged `beyond-power-on` helper had drifted from the corrected Rust HID implementation by placing the SetWorkState command at byte 2 instead of byte 1. The repo copy is now corrected and guarded by a substrate test; any installed Honey helper should be refreshed before using the service as panel-power evidence.
- Native XoxdWM authority is now a code/static and headless-proofable lane: Rust owns configured workspace count, layout reflow, workspace visibility, native key actions, configured app-launch IPC, config reload, autostart, lock/logout, idle supervision, and DPMS IPC. It is not yet a named-host product proof until #45 records app launch, focus, workspace movement, and layout in a real Linux session without Emacs in the WM path.
- The active blockers on `honey` are now productization and repeatability, not missing lease support:
  - compositor-side headset connector designation and Monado direct-mode settings now both have supported host config surfaces, and `monado-beyond` is installed as a real host package on `honey`
  - the package-default `exwm-vr-monado.service` now runs `/usr/bin/monado-service` without `MONADO_SERVICE_BIN`, and the packaged launcher clears dead `monado_comp_ipc` sockets before launching when no `monado-service` is active
  - the repo now carries `packaging/scripts/exwm-vr-openxr-smoke`, plus `just honey-openxr-status`, `just honey-openxr-smoke`, `just honey-openxr-clean-cycle`, and `just honey-openxr-fresh-boot-check`, so the OpenXR client invocation, clean service-cycle check, and post-boot capture lane are repo-owned instead of undocumented SSH one-liners
  - the `exwm-vr-openxr-smoke-client` RPM from GitHub Actions run `24938791255` is now installed on `honey`, and `just honey-openxr-status` resolves the packaged client path `/usr/libexec/exwm-vr/hello_xr -g Vulkan`
  - on `2026-04-25` EDT, three bounded packaged-client smoke passes succeeded in one active user-service session, each selecting Bigscreen Beyond and creating two `3561x3561` eye swapchains through Monado
  - three clean stop/start cycles also succeeded: two from the explicit shell sequence and one through `just honey-openxr-clean-cycle`; `rke2-server` stayed active
  - `just honey-openxr-fresh-boot-check` is prepared for attended/manual fresh-boot evidence; it does not reboot `honey`, and a current-boot focused session still is not first-frame proof while the headset is black
  - the attended fresh-boot procedure is captured in [honey-fresh-boot-runbook-2026-04-26.md](honey-fresh-boot-runbook-2026-04-26.md)
  - the current proof is current-boot OpenXR/session evidence, but it is not yet in-goggles first-frame, long-running operator, or daily-driver proof
- `yoga` now has `exwm-vr-0.5.4-1.el10`, `exwm-vr-compositor-0.5.4-1.el10`, and `exwm-vr-elisp-0.5.4-1.el10` installed.
- `yoga` validated the earlier named-host package line: the compositor binary links cleanly, a bounded runtime succeeds with `seatd`, and Wayland/DRM/libinput startup is confirmed.
- The branch-scoped `0.5.4-1.el10` session payload from GitHub Actions run `24768509226` now has both a staged proof and a real installed-package proof on `yoga`; the installed units reached `active` / `active` / `active`, `ewwm-compositor v0.5.4 starting`, `backend: drm`, `ewwm-ipc: connected`, and `ewwm: initialized` without the old ambient dotfile contamination.
- The exact evidence and boundary for that staged proof live in [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md).
- `yoga` now has a one-time greeter-path local session proof. A controlled SDDM autologin run created a real `wayland` user session on `seat0` with `Service=sddm-autologin`, launched `/usr/share/exwm-vr/exwm-vr-session`, and kept `exwm-vr.target`, `exwm-vr-compositor.service`, and `exwm-vr-emacs.service` active with the same bounded proof markers seen over SSH.
- GitHub Actions packaging run `24771056471` produced a refreshed branch-scoped `0.5.4-1.el10` RPM payload, that payload was reinstalled on `yoga`, the packaged `SuccessExitStatus=15` stop-path fix is now present in `/usr/lib/systemd/user/exwm-vr-emacs.service`, and the temporary `/etc/systemd/user/exwm-vr-emacs.service.d/10-success-exit.conf` host drop-in has been removed.
- `neo` is useful as an orchestration and documentation machine, but it is not a product target.
  - XoxdWM is not expected to run as a macOS desktop or VR environment.
  - this repo does not vendor the external Bazel or remote-build definitions for the Rocky build authority
  - the current external authority surfaces are documented in [remote-build-authority.md](remote-build-authority.md)
  - the current repo-owned workflow and host proof lanes are documented in [remote-proof-lanes.md](remote-proof-lanes.md)
  - authoritative build and runtime validation should stay on Rocky / Linux remote lanes rather than being re-centered on Darwin
- The local Apple control-plane surface is now serviceable for repo work:
  - `just truth-lint` passes locally
  - `just test` passes locally
  - `nix flake check --no-build` passes locally
  - local byte-compile and headless Rust checks can pass, but they are auxiliary repo-maintenance checks rather than the default build authority path
  - this does not turn macOS into a supported runtime target; it only reduces local repo-management friction
- Recent push CI failures were concentrated in three places:
  - Rocky test: optional `sccache` setup step
  - Nix cache workflow: attempting to install Nix on a pre-provisioned self-hosted runner
  - self-hosted fast CI: heavy checks on all pushes, including non-code work
- Shared self-hosted CI is being reconciled against the GloriousFlywheel runner
  contract:
  - `Jesssullivan/XoxdWM#29` removed the shared-path fork gate and passed on an ephemeral `xoxdwm-nix` runner; merged PR #34 migrated non-hardware self-hosted jobs to the shared `tinyland-nix` capability lane
  - non-hardware self-hosted Nix workflows now target the shared `tinyland-nix` capability lane
  - PR #34 merged without unexplained `tinyland-nix` queues on its final head; `tinyland-inc/GloriousFlywheel#413` remains the shared-lane reachability/enrollment tracker for turning that capability back on intentionally
  - fresh workflow selection now requires `GF_SHARED_RUNNERS_REACHABLE=true` before choosing `tinyland-nix`; until that proof variable is set, PR checks fall back to hosted Linux or skip self-hosted-only jobs instead of creating known-unreachable queues
  - `Cross-compile aarch64` and `Cross-compile s390x` are classified as secondary-target informational lanes while the hosted/shared cache path is too slow; x86_64 Rocky/runtime proof remains the release-critical lane
  - the remaining Honey / VR hardware lanes now key off explicit `USE_VR_HARDWARE` opt-in instead of `github.repository == 'tinyland-inc/XoxdWM'`
  - runner inventory and repo enrollment are still separate infrastructure facts, not XoxdWM runtime support claims
  - the named remote lane map is now explicit: `runner-health.yml`, `self-hosted-fast.yml`, `rocky-test.yml`, `packaging.yml`, and `vr-hardware.yml`

## What This Repo Is Good For Right Now

- tracking compositor and packaging work in public
- publishing experimental package artifacts
- carrying the userspace side of Rocky 10 / Wayland / VR integration work
- serving as the research and implementation repo for the `honey` and `yoga` MVP paths now that both hosts have validated kernel lanes
- carrying a real public Rocky compositor package that has been validated on `yoga`
- carrying a named-host `honey` direct lease plus OpenXR session proof and the remaining productization gaps to close

## What This Repo Is Not Claiming Right Now

- daily-driver VR on `honey`
- a complete turnkey Rocky 10 VR deployment
- a polished, repeatedly exercised local login/session lane on `yoga`
- a repeatable installed named-host VR session on `honey`
- proven eye tracking, hand tracking, or BCI support on current named lab hosts
- macOS as a real runtime target for XoxdWM
- Darwin as the authoritative build or validation surface

## Immediate Priorities

1. Preserve the now-explicit `monado_comp_ipc` cleanup path in the launcher and keep the `honey` OpenXR status wrapper as the safe preflight.
2. Preserve the installed `monado-beyond` host lane on `honey`, refresh the corrected `beyond-power-on` helper on-host, and resolve the black first-frame blocker before promoting OpenXR smoke to product evidence.
3. Keep using the installed `exwm-vr-openxr-smoke-client` path for honey OpenXR smoke runs and record whether failures are host/substrate, packaging/deployment, or compositor/runtime blockers.
4. Keep the Rocky base package lane green while leaving Monado, SELinux, and BCI as separate follow-on concerns.
5. Preserve the now-proven `yoga` package/session lane as the 2D reference host while packaging continues to evolve.
