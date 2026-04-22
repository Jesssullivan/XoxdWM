# XoxdWM Roadmap: Q2 2026

This document is the execution-side counterpart to the repo truth surface.
For the full audit-to-execution model, also read
[reality-driven-development-arc-2026-q2.md](reality-driven-development-arc-2026-q2.md).
For the milestone and iteration split that separates `yoga` 2D closure from
`honey` substrate work and the later XoxdWM smoke, also read
[grounded-milestone-plan-2026-q2.md](grounded-milestone-plan-2026-q2.md).

## Epic 1: Reality Audit And Support Surface

Goal: keep the public repo honest.

Acceptance:

- root README exists and points to current status docs
- support matrix, status, and reality-check docs stay aligned
- install docs distinguish package availability from named-host validation
- aspirational subsystems are not described as supported by default

## Epic 2: CI Recovery

Goal: restore a reliable critical path for code changes.

Acceptance:

- docs-only pushes do not trigger heavy self-hosted workflows
- Rocky test no longer fails in optional cache setup
- self-hosted Nix workflows stop reinstalling Nix on provisioned runners
- weekly runner-health remains available without dominating repo signal
- shared self-hosted jobs run from `Jesssullivan/XoxdWM` without fork-specific repo-name gates
- Honey / VR hardware lanes use explicit `USE_VR_HARDWARE` opt-in instead of repo-name policy

## Epic 3: Rocky 10 Desktop/Dev MVP On `yoga`

Goal: produce a documented install path for the compositor on a real Rocky 10 lab machine.

Current reality:

- `yoga` has a validated one-time `kernel-xr` boot path.
- `yoga` now has the public Rocky packages installed and a bounded compositor startup path with `seatd`.
- `yoga` now has a one-time SDDM greeter-path proof via `sddm-autologin` on
  `seat0`; the refreshed packaged unit now carries the `SuccessExitStatus=15`
  stop-path fix, so the remaining follow-on is repeatability and, if desired,
  recording a manual session selection pass.
- Monado/OpenXR userspace on `yoga` is not the current gate for the desktop/dev MVP.

Acceptance:

- a corrected native RPM or documented source-build path exists for Rocky 10
- `yoga` can install the public package or build from source with documented steps
- compositor launch path is documented and repeatable
- rollback path is documented if package install fails
- SELinux hardening is tracked as a follow-on package path rather than a gate on the first named-host compositor RPM

## Epic 4: `honey` XR Substrate And XoxdWM Smoke

Goal: prove the XR substrate on the current kernel host, then record the first
honest XoxdWM smoke on that substrate.

Current reality:

- `honey` has a proven generic `linux-xr` default and a one-time verified RT lane.
- `honey` now has branch-scoped `exwm-vr-0.5.4-1.el10` packages installed and a bounded named-host XoxdWM compositor startup.
- `honey` now has an explicit active OpenXR runtime file plus a host-local Monado service override for the Beyond / `DP-2` path.
- `hello_xr` now reaches Monado runtime selection, Bigscreen Beyond selection, and Vulkan device creation on `honey` in the current fallback Wayland-window path, but the real direct-mode probe now fails earlier because `ewwm-compositor` does not expose DRM lease support to Monado. The fallback path still crashes at `xrCreateSession` with `VK_ERROR_SURFACE_LOST_KHR`.
- the Honey-backed workflows are now capability-gated, but the canonical repo is still not claiming an always-on hardware runner surface by default

Acceptance:

- substrate acceptance:
  - Monado and required userspace are installed and documented
  - an OpenXR client tool path exists and is documented on-host
  - HMD enumeration is observed as expected, or the lack of attached VR hardware is explicitly recorded as the blocker
  - the trusted bridge path on `honey` is explicit
- smoke acceptance:
  - compositor starts on the target host
  - one smoke path for DRM lease / OpenXR startup is recorded in repo docs
