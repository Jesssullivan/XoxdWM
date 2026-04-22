# XoxdWM Status

Snapshot date: 2026-04-22

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- `v0.5.1` is publicly released and publishes the repaired Rocky compositor RPM and the DEB artifact.
- The base Rocky RPM gate is complete: the public package now installs on Rocky 10 without the earlier bare-`wayland` metadata or `/nix/store` runtime-linking problems.
- The release lane now treats Monado integration, SELinux hardening, and the BrainFlow BCI virtualenv as separate opt-in package paths so the base Rocky compositor package can stay shippable.
- `honey` is not currently running a deployed XoxdWM stack.
- `honey` now has a proven `linux-xr` kernel default and a one-time verified PREEMPT_RT boot, but the VR userspace is still incomplete.
- `honey` has partial XR prereqs only: `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `/usr/local/bin/monado-service`, and `/usr/local/share/openxr/1/openxr_monado.json`.
- `honey` does not currently expose `openxr-info`, `xrgears`, or `xoxdwm`.
- Recent live host investigation on `honey` showed a stable Dell HDMI management path and a `DP-2` display path on recovered boots, but that is still host-level evidence, not a deployed XoxdWM VR smoke path.
- `yoga` now has `exwm-vr-0.5.4-1.el10`, `exwm-vr-compositor-0.5.4-1.el10`, and `exwm-vr-elisp-0.5.4-1.el10` installed.
- `yoga` validated the earlier named-host package line: the compositor binary links cleanly, a bounded runtime succeeds with `seatd`, and Wayland/DRM/libinput startup is confirmed.
- The branch-scoped `0.5.4-1.el10` session payload from GitHub Actions run `24768509226` now has both a staged proof and a real installed-package proof on `yoga`; the installed units reached `active` / `active` / `active`, `ewwm-compositor v0.5.4 starting`, `backend: drm`, `ewwm-ipc: connected`, and `ewwm: initialized` without the old ambient dotfile contamination.
- The exact evidence and boundary for that staged proof live in [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md).
- `yoga` still does not have a polished local login/session path, and a controlled stop still leaves `exwm-vr-emacs.service` in `failed` state after the target is stopped, so stop-path ergonomics remain follow-on work.
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

## What This Repo Is Not Claiming Right Now

- daily-driver VR on `honey`
- a complete turnkey Rocky 10 VR deployment
- a polished local login/session launch on `yoga`
- a working named-host XoxdWM compositor install on `honey`
- proven eye tracking, hand tracking, or BCI support on current named lab hosts
- macOS as a real runtime target for XoxdWM
- Darwin as the authoritative build or validation surface

## Immediate Priorities

1. Finish the local login/session path on `yoga` now that the real installed `0.5.4-1.el10` units are proved.
2. Put a real XoxdWM/Monado/OpenXR client-tool install path on `honey`, not just a bare Monado runtime manifest.
3. Produce the first named-host compositor startup on `honey`.
4. Keep the Rocky base package lane green while leaving Monado, SELinux, and BCI as separate follow-on concerns.
5. Make the repo explicit that `neo` is orchestration-only and Rocky / Linux remote lanes are the authoritative build and runtime surface.
