# XoxdWM

XoxdWM is an experimental Wayland compositor plus Emacs window-management layer with VR, eye-tracking, hand-tracking, and BCI research surfaces.

This repository is the canonical public home for the compositor, packaging, releases, and status tracking. It is not a claim that every documented subsystem is proven on lab hardware today.

## Current State

As of 2026-04-12:

| Area | Status | Notes |
| --- | --- | --- |
| Release artifacts | Smoke | `v0.5.0` publishes RPM and DEB artifacts, but the Rocky RPM is not yet a proven host-runnable install path. |
| Headless compositor path | Smoke | Build and test surfaces exist, but not re-validated in this pass. |
| Rocky 10 package install | Design | The current public `v0.5.0` RPM failed on `honey`: it requires bare `wayland` metadata on Rocky and ships a `/nix/store`-linked compositor binary. The active `0.5.1` repair lane is targeting a native non-`vr` compositor RPM first. |
| `honey` VR session | Design | `honey` has partial Monado/OpenXR prereqs, but no deployed XoxdWM stack or working OpenXR client-tool path. |
| `yoga` desktop/dev target | Design | `yoga` has validated kernel work, but still has no XoxdWM userspace install. |
| Eye tracking / hand tracking / BCI | Design | Documented and partially implemented, but not currently claimed as proven on named lab hosts. |

## Start Here

- [Support Matrix](docs/support-matrix.md)
- [Status](docs/status.md)
- [Q2 2026 Roadmap](docs/roadmap-2026-q2.md)
- [Installation Quickstart](docs/installation-quickstart.md)
- [VR Guide](docs/vr-guide.md)

## Scope

The repo contains four different kinds of work:

- compositor and Emacs WM code
- packaging for Rocky/Nix/systemd/SELinux
- hardware and upstream patch research
- aspirational feature inventory

Only the support matrix and status docs should be read as the current operational truth.

## Releases

Latest public release:

- [`v0.5.0`](https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.5.0)

Current public install artifacts:

- `exwm-vr-compositor-*.x86_64.rpm`
- `ewwm-compositor_*_amd64.deb`

These artifacts currently package the compositor path. They do not, by themselves, establish a proven full VR deployment on `honey` or `yoga`, and the Rocky RPM release lane is being repaired as a native non-`vr` compositor package before any full VR claim is made.

## Near-Term Goal

The next 12 weeks are aimed at one honest MVP:

- `yoga`: reproducible Rocky 10 desktop/dev install
- `honey`: VR smoke path with Monado, non-desktop HMD detection, and compositor launch

Everything else remains secondary until those two named-host outcomes are green.
