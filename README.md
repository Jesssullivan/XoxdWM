# XoxdWM

XoxdWM is an experimental Wayland compositor plus Emacs window-management layer with VR, eye-tracking, hand-tracking, and BCI research surfaces.

This repository is the canonical public home for the compositor, packaging, releases, and status tracking. It is not a claim that every documented subsystem is proven on lab hardware today.

## Current State

As of 2026-04-11:

| Area | Status | Notes |
| --- | --- | --- |
| Release artifacts | Smoke | `v0.5.0` publishes RPM and DEB artifacts. |
| Headless compositor path | Smoke | Build and test surfaces exist, but not re-validated in this pass. |
| Rocky 10 package install | Smoke | Packaging exists; quickstart now documents it as a compositor/package path, not a full VR deployment. |
| `honey` VR session | Design | `honey` has OpenXR libs and disabled Monado units, but no deployed XoxdWM stack. |
| `yoga` desktop/dev target | Design | `yoga` is still on stock Rocky 10 and has no XoxdWM install. |
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

- `ewwm-compositor-*.x86_64.rpm`
- `ewwm-compositor_*_amd64.deb`

These artifacts currently package the compositor path. They do not, by themselves, establish a proven full VR deployment on `honey` or `yoga`.

## Near-Term Goal

The next 12 weeks are aimed at one honest MVP:

- `yoga`: reproducible Rocky 10 desktop/dev install
- `honey`: VR smoke path with Monado, non-desktop HMD detection, and compositor launch

Everything else remains secondary until those two named-host outcomes are green.
