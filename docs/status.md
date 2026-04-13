# XoxdWM Status

Snapshot date: 2026-04-12

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- The repo now has a root README and status surface, but the named-host runtime reality still trails the packaging story.
- The current public Rocky RPM failed on `honey`: package metadata requires bare `wayland`, and the installed compositor binary points at a `/nix/store/.../ld-linux...` interpreter.
- The active `0.5.1` repair lane is now building toward a native Rocky compositor RPM without the `vr` Cargo feature; full VR packaging is still blocked on a real `libuvc` path.
- The latest native Rocky RPM blocker is SELinux policy portability, not the compositor build itself. The repair lane is now treating SELinux hardening as a separate opt-in package path so it does not block the base Rocky compositor release.
- `honey` is not currently running a deployed XoxdWM stack.
- `honey` now has a proven `linux-xr` kernel default and a one-time verified PREEMPT_RT boot, but the VR userspace is still incomplete.
- `honey` has partial XR prereqs only: `openxr-libs`, `openxr-devel`, `wlroots`, `wlroots-devel`, `/usr/local/bin/monado-service`, and `/usr/local/share/openxr/1/openxr_monado.json`.
- `honey` does not currently expose `openxr-info`, `xrgears`, or `xoxdwm`, and no active DRM connector or obvious HMD path was present during the audit.
- `yoga` is not currently running a deployed XoxdWM stack.
- `yoga` now has a one-time validated `kernel-xr` boot path, but stock Rocky remains the persistent default and no XR userspace stack is installed.
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

## What This Repo Is Not Claiming Right Now

- daily-driver VR on `honey`
- a complete turnkey Rocky 10 VR deployment
- a working named-host XoxdWM compositor install on `honey` or `yoga`
- proven eye tracking, hand tracking, or BCI support on current named lab hosts

## Immediate Priorities

1. Finish the corrected public Rocky RPM release path so it ships a host-runnable native compositor binary and correct dependency metadata, without making SELinux policy compilation a release gate.
2. Put a real XoxdWM/Monado/OpenXR client-tool install path on `honey`, not just a bare Monado runtime manifest.
3. Produce the first named-host compositor startup on either `honey` or `yoga`.
4. Produce one reproducible Rocky 10 XR userspace install on `yoga`.
