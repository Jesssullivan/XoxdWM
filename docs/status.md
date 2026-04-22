# XoxdWM Status

Snapshot date: 2026-04-22

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- `v0.5.1` is publicly released and publishes the repaired Rocky compositor RPM and the DEB artifact.
- The base Rocky RPM gate is complete: the public package now installs on Rocky 10 without the earlier bare-`wayland` metadata or `/nix/store` runtime-linking problems.
- The release lane now treats Monado integration, SELinux hardening, and the BrainFlow BCI virtualenv as separate opt-in package paths so the base Rocky compositor package can stay shippable.
- `honey` is not currently a stable deployed XoxdWM VR stack.
- `honey` now has a proven `linux-xr` kernel default and a one-time verified PREEMPT_RT boot, but the VR userspace is still incomplete.
- `honey` now has refreshed `exwm-vr-0.5.4-1.el10`, `exwm-vr-compositor-0.5.4-1.el10`, and `exwm-vr-elisp-0.5.4-1.el10` installed from branch-scoped RPMs.
- `honey` has XR prereqs plus explicit runtime activation: `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `monado-beyond-0.0.1-1.20260310git.el10`, `/usr/bin/monado-service`, `/usr/share/openxr/1/openxr_monado.json`, and `/etc/xdg/openxr/1/active_runtime.json`.
- `honey` does not currently expose `openxr-info`, `xrgears`, or `xoxdwm`.
- `honey` now has a bounded named-host XoxdWM compositor startup: `exwm-vr.target`, `exwm-vr-compositor.service`, and `exwm-vr-emacs.service` all reached `active`, `DP-2` came up at `5088x2544@75Hz`, `HDMI-A-2` came up at `1920x1080@60Hz`, IPC initialized, and Emacs reached `ewwm: initialized`; see [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md).
- `honey` now has a one-shot direct-mode client proof from the installed package surface too: after reinstalling `exwm-vr-compositor-0.5.4-1.el10` from packaging run `24776900393`, `/usr/bin/ewwm-compositor` initialized `wp_drm_lease_v1`, read `EWWM_DRM_LEASE_CONNECTORS=DP-2` from `~/.config/exwm-vr/compositor.env`, kept `DP-2` out of the desktop output map, and granted a real DRM lease to Monado.
- `honey` now also has a repo-owned Monado service proof on three runtime surfaces: `/usr/lib/systemd/user/exwm-vr-monado.service` plus `~/.config/exwm-vr/monado.env` reached direct-mode proof first with the older local `/usr/local/bin/monado-service` lane, then with a staged `monado-beyond` companion RPM tree from GitHub Actions run `24804821792`, and now with an installed `monado-beyond` host package from run `24807084915`.
- `hello_xr -g Vulkan` on `honey` now reaches `xrCreateInstance`, `xrGetSystem`, `Bigscreen Beyond`, `READY`, and two eye swapchains at `3561x3561`.
- The active blockers on `honey` are now productization and repeatability, not missing lease support:
  - compositor-side `DP-2` designation and Monado direct-mode settings now both have supported host config surfaces, and `monado-beyond` is installed as a real host package on `honey`
  - the package-default `exwm-vr-monado.service` now runs `/usr/bin/monado-service` without `MONADO_SERVICE_BIN`, but stale `monado_comp_ipc` cleanup should still be explicit in launcher and operator docs
  - the current client-tool proof still uses a local `/usr/local/bin/hello_xr` build rather than a packaged Rocky operator tool
  - the proof has been captured once, not yet as a repeated operator lane
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
- Shared self-hosted CI now proves out on the canonical repo:
  - `Jesssullivan/XoxdWM#29` removed the shared-path fork gate and passed on an ephemeral `xoxdwm-nix` runner
  - the remaining Honey / VR hardware lanes now key off explicit `USE_VR_HARDWARE` opt-in instead of `github.repository == 'tinyland-inc/XoxdWM'`
  - the repo-level Actions runners API still reports `0` accessible runners on both repos, so runner inventory is still somewhat opaque even though the live shared path works
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

1. Make stale `monado_comp_ipc` cleanup or prevention explicit in launcher behavior and `honey` operator docs.
2. Preserve the new installed `monado-beyond` host lane on `honey` and rerun it enough times to call it repeatable instead of one-shot.
3. Replace the current local `/usr/local/bin/hello_xr` proof tool on `honey` with a packaged or otherwise durable Rocky-facing client-tool path.
4. Keep the Rocky base package lane green while leaving Monado, SELinux, and BCI as separate follow-on concerns.
5. Preserve the now-proven `yoga` package/session lane as the 2D reference host while packaging continues to evolve.
