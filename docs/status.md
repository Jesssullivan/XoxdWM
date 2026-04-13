# XoxdWM Status

Snapshot date: 2026-04-13

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- `v0.5.1` is publicly released and publishes the repaired Rocky compositor RPM and the DEB artifact.
- The base Rocky RPM gate is complete: the public package now installs on Rocky 10 without the earlier bare-`wayland` metadata or `/nix/store` runtime-linking problems.
- The release lane now treats Monado integration, SELinux hardening, and the BrainFlow BCI virtualenv as separate opt-in package paths so the base Rocky compositor package can stay shippable.
- `honey` is not currently running a deployed XoxdWM stack.
- `honey` now has a proven `linux-xr` kernel default and a one-time verified PREEMPT_RT boot, but the VR userspace is still incomplete.
- `honey` has partial XR prereqs only: `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `/usr/local/bin/monado-service`, and `/usr/local/share/openxr/1/openxr_monado.json`.
- `honey` does not currently expose `openxr-info`, `xrgears`, or `xoxdwm`, and no active DRM connector or obvious HMD path was present during the audit.
- `yoga` now has the public Rocky package installed: `exwm-vr`, `exwm-vr-compositor`, and `exwm-vr-elisp`.
- `yoga` validated the named-host package line: the compositor binary links cleanly, a bounded runtime succeeds with `seatd`, and Wayland/DRM/libinput startup is confirmed.
- `yoga` still does not have a polished local login/session path. The remaining follow-on is session ergonomics: display-manager or local-launch flow, seat backend propagation into user services, and a fresh-login/user-manager path after seat-group changes.
- Recent push CI failures were concentrated in three places:
  - Rocky test: optional `sccache` setup step
  - Nix cache workflow: attempting to install Nix on a pre-provisioned self-hosted runner
  - self-hosted fast CI: heavy checks on all pushes, including non-code work
- Current self-hosted runner scope does not match the current public repo:
  - historical self-hosted evidence on `honey` points at `tinyland-inc/XoxdWM`
  - live runners we found are registered to `tinyland-inc`, `Jesssullivan/cmux`, `Jesssullivan/outbot-ci`, and `Jesssullivan/chapel`
  - we did not find a runner registration that can accept jobs from `Jesssullivan/XoxdWM`

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

## Immediate Priorities

1. Finish the local login/session path on `yoga` so the named-host result is not limited to a bounded ssh-driven compositor smoke.
2. Put a real XoxdWM/Monado/OpenXR client-tool install path on `honey`, not just a bare Monado runtime manifest.
3. Produce the first named-host compositor startup on `honey`.
4. Keep the Rocky base package lane green while leaving Monado, SELinux, and BCI as separate follow-on concerns.
