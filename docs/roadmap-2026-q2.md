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

## Epic 3: Rocky 10 Desktop/Dev MVP On `yoga`

Goal: produce a documented install path for the compositor on a real Rocky 10 lab machine.

Acceptance:

- `yoga` can install the public package or build from source with documented steps
- compositor launch path is documented and repeatable
- rollback path is documented if package install fails

## Epic 4: `honey` VR Smoke Path

Goal: prove a minimal VR lifecycle on the current kernel host.

Acceptance:

- Monado and required userspace are installed and documented
- HMD enumeration is observed as expected
- compositor starts on the target host
- one smoke path for DRM lease / OpenXR startup is recorded in repo docs
