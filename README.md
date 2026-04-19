# XoxdWM

XoxdWM is an experimental Wayland compositor plus Emacs window-management layer with VR, eye-tracking, hand-tracking, and BCI research surfaces.

This repository is the canonical public home for the compositor, packaging, releases, and status tracking. It is not a claim that every documented subsystem is proven on lab hardware today.

## Current State

As of 2026-04-19:

| Area | Status | Notes |
| --- | --- | --- |
| Release artifacts | Smoke | `v0.5.1` publishes RPM and DEB artifacts. The Rocky base compositor RPM is now public and host-installable. |
| Headless compositor path | Smoke | Build and test surfaces exist, but not re-validated in this pass. |
| Rocky 10 package install | Smoke | `v0.5.1` ships a native non-`vr` compositor RPM. `yoga` validated package install, clean runtime linking, and bounded compositor startup. Local session ergonomics are still follow-on work. |
| `honey` VR session | Design | `honey` has partial Monado/OpenXR prereqs, but no deployed XoxdWM stack or working OpenXR client-tool path. |
| `yoga` desktop/dev target | Smoke | `yoga` now has the released Rocky package installed and a named-host bounded compositor result, but not yet a polished local login/session path. |
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

Shared self-hosted CI now runs from this canonical repo. Honey-backed hardware lanes remain explicit opt-in via `USE_VR_HARDWARE` rather than implicit fork-only behavior.

## Releases

Latest public release:

- [`v0.5.0`](https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.5.0)
- [`v0.5.1`](https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.5.1)

Current public install artifacts:

- `exwm-vr-compositor-*.x86_64.rpm`
- `ewwm-compositor_*_amd64.deb`

These artifacts currently package the compositor path. They do not, by themselves, establish a proven full VR deployment on `honey`, and they do not yet mean `yoga` has a polished local login/session experience. SELinux hardening, Monado integration, and the BrainFlow BCI virtualenv remain separate follow-on package or host-integration paths instead of blocking the base Rocky compositor release.

## Near-Term Goal

The next 12 weeks are aimed at one honest MVP:

- `yoga`: reproducible Rocky 10 desktop/dev install
- `honey`: VR smoke path with Monado, non-desktop HMD detection, and compositor launch

Everything else remains secondary until those two named-host outcomes are green.
