# XoxdWM

XoxdWM is an experimental Wayland compositor plus Emacs window-management layer with VR, eye-tracking, hand-tracking, and BCI research surfaces.

This repository is the canonical public home for the compositor, packaging, releases, and status tracking. It is not a claim that every documented subsystem is proven on lab hardware today.

## Current State

As of 2026-04-22:

| Area | Status | Notes |
| --- | --- | --- |
| Release artifacts | Smoke | `v0.5.1` publishes RPM and DEB artifacts. The Rocky base compositor RPM is public and host-installable, and branch-scoped `0.5.4-1.el10` RPMs from Actions run `24768509226` are now installed and revalidated on `yoga`. |
| Headless compositor path | Smoke | Build and test surfaces exist, but not re-validated in this pass. |
| Rocky 10 package install | Smoke | `yoga` now has refreshed installed `0.5.4-1.el10` `exwm-vr-*` RPMs from the current branch, the real installed units pass a named-host bounded proof, and a controlled SDDM autologin run reached a real `EXWM-VR` Wayland user session on `seat0`. The packaged `SuccessExitStatus=15` stop-path fix is now on-host; the remaining follow-on is repeatability and operator polish, not package repair. |
| `honey` compositor/substrate path | Smoke | `honey` now has installed branch-scoped `exwm-vr-0.5.4-1.el10` packages and a bounded named-host `exwm-vr.target` startup with `DP-2` plus `HDMI-A-2` configured. |
| `honey` VR session | Design | Monado can now identify the Bigscreen Beyond and `hello_xr` reaches OpenXR runtime and HMD selection in the current forced Wayland-window fallback. The true Wayland-direct probe currently fails because `ewwm-compositor` does not expose DRM lease support to Monado, and the fallback path still crashes at `xrCreateSession` with `VK_ERROR_SURFACE_LOST_KHR`; see [Honey Substrate Proof](docs/honey-substrate-proof-2026-04-22.md). |
| `yoga` desktop/dev target | Smoke | `yoga` now has an installed `0.5.4` session proof with explicit `drm` backend and dedicated Emacs bootstrap, plus a one-time SDDM greeter-path proof via `sddm-autologin` on `seat0`. The remaining work is repeatability and session polish, not basic launch viability or packaged stop-path repair. |
| Eye tracking / hand tracking / BCI | Design | Documented and partially implemented, but not currently claimed as proven on named lab hosts. |

## Start Here

- [Support Matrix](docs/support-matrix.md)
- [Remote Build Authority](docs/remote-build-authority.md)
- [Remote Proof Lanes](docs/remote-proof-lanes.md)
- [Reality Check](docs/reality-check-2026-04-22.md)
- [Reality-Driven Development Arc](docs/reality-driven-development-arc-2026-q2.md)
- [Grounded Milestone Plan](docs/grounded-milestone-plan-2026-q2.md)
- [Yoga Session Proof](docs/yoga-session-proof-2026-04-22.md)
- [Honey Substrate Proof](docs/honey-substrate-proof-2026-04-22.md)
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

Use the reality check, status, and support matrix together as the current truth surface.
Subsystem docs and feature inventories can still be more aspirational than present support.
`neo` and other Darwin machines are control-plane surfaces only. Authoritative
build and runtime validation belongs on Rocky / Linux remote lanes and named-host
proof; see [Remote Build Authority](docs/remote-build-authority.md) and
[Remote Proof Lanes](docs/remote-proof-lanes.md).

Shared self-hosted CI now runs from this canonical repo. Honey-backed hardware lanes remain explicit opt-in via `USE_VR_HARDWARE` rather than implicit fork-only behavior.

## Releases

Latest public release:

- [`v0.5.0`](https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.5.0)
- [`v0.5.1`](https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.5.1)

Current public install artifacts:

- `exwm-vr-compositor-*.x86_64.rpm`
- `ewwm-compositor_*_amd64.deb`

These artifacts currently package the compositor path. They do not, by themselves, establish a proven full VR deployment on `honey`, and they do not yet mean `yoga` has a polished or repeatedly exercised local login/session lane. Right now `yoga` has the installed session entry, refreshed packaged user units, an active SDDM greeter, and a one-time `sddm-autologin` Wayland user session proof on `seat0`. SELinux hardening, Monado integration, and the BrainFlow BCI virtualenv remain separate follow-on package or host-integration paths instead of blocking the base Rocky compositor release.

## Near-Term Goal

The next 12 weeks are aimed at one honest, ordered MVP:

- `yoga`: reproducible Rocky 10 desktop/dev install with a real local session path
- `honey` substrate: stable XR bridge path with kernel, connector, runtime, and client-tool truth
- `honey` smoke: first XoxdWM launch on the true substrate

Everything else remains secondary until those three named outcomes are green.
