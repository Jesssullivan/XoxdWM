# XoxdWM Roadmap: Q2 2026

This document is the execution-side counterpart to the support matrix.

## Epic 1: Reality Audit And Support Surface

Goal: keep the public repo honest.

Acceptance:

- root README exists and points to current status docs
- support matrix is maintained
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
- `yoga` still has no installed Monado/OpenXR/XoxdWM userspace stack.
- the current public `v0.5.0` Rocky RPM is not yet a usable named-host install path

Acceptance:

- a corrected native RPM or documented source-build path exists for Rocky 10
- `yoga` can install the public package or build from source with documented steps
- compositor launch path is documented and repeatable
- rollback path is documented if package install fails
- SELinux hardening is tracked as a follow-on package path rather than a gate on the first named-host compositor RPM

## Epic 4: `honey` VR Smoke Path

Goal: prove a minimal VR lifecycle on the current kernel host.

Current reality:

- `honey` has a proven generic `linux-xr` default and a one-time verified RT lane.
- `honey` has a Monado runtime manifest and `monado-service`, but not the OpenXR client tooling or compositor deployment needed for a real smoke path.
- the Honey-backed workflows are now capability-gated, but the canonical repo is still not claiming an always-on hardware runner surface by default

Acceptance:

- Monado and required userspace are installed and documented
- an OpenXR client tool path exists and is documented on-host
- HMD enumeration is observed as expected, or the lack of attached VR hardware is explicitly recorded as the blocker
- compositor starts on the target host
- one smoke path for DRM lease / OpenXR startup is recorded in repo docs
