# XoxdWM

XoxdWM is an experimental native Wayland WM/DE authority with optional Emacs/eGreg app-layer clients plus VR, eye-tracking, hand-tracking, and BCI research surfaces.

This repository is the canonical public home for the compositor, packaging, releases, and status tracking. It is not a claim that every documented subsystem is proven on lab hardware today.

## Current State

As of 2026-05-12:

| Area | Status | Notes |
| --- | --- | --- |
| Release artifacts | Smoke | `v0.5.1` publishes RPM and DEB artifacts. The Rocky base compositor RPM is public and host-installable, and branch-scoped `0.5.4-1.el10` RPMs from Actions run `24768509226` are now installed and revalidated on `yoga`. |
| Headless compositor path | Smoke | Build and test surfaces exist, but not re-validated in this pass. |
| Rocky 10 package install | Smoke | `yoga` now has refreshed installed `0.5.4-1.el10` `exwm-vr-*` RPMs from the current branch, the real installed units pass a named-host bounded proof, and a controlled SDDM autologin run reached a real `EXWM-VR` Wayland user session on `seat0`. The packaged `SuccessExitStatus=15` stop-path fix is now on-host; the remaining follow-on is repeatability and operator polish, not package repair. |
| `honey` compositor/substrate path | Smoke | `honey` now has installed branch-scoped `exwm-vr-0.5.4-1.el10` packages, a bounded named-host `exwm-vr.target` startup, and a direct-mode lease proof from the installed `/usr/bin/ewwm-compositor` after reinstalling the branch RPM artifact from run `24776900393`. |
| `honey` VR session | Smoke | On `2026-04-22`, the installed `ewwm-compositor` on `honey` initialized `wp_drm_lease_v1`, reserved the headset connector, and granted a real DRM lease to Monado. The repo-owned `exwm-vr-monado.service`, installed `monado-beyond` host package, `exwm-vr-openxr-smoke-client` RPM, and `just honey-openxr-*` wrappers now prove the packaged OpenXR client lane. On `2026-05-09`, the current Honey topology was `card0-DP-1` for the BS2E and `card0-HDMI-A-1` for the Dell panel, with `rke2-server` still active. After correcting the host config from the old `DP-2` target to `DP-1`, repeated `/usr/libexec/exwm-vr/hello_xr -g Vulkan` runs selected Monado / Bigscreen Beyond, created two `3561x3561` eye swapchains, and reached `READY -> SYNCHRONIZED -> VISIBLE -> FOCUSED`. The human-visible headset output remained black, so the current classification is P3 OpenXR Session pass / P4 Visual First Frame fail. `linux-xr` PR #69 now provides the next read-only PPS diagnostic artifact lane, but that is observability only until installed and attended on `honey`. |
| `yoga` desktop/dev target | Smoke | `yoga` now has an installed `0.5.4` session proof with explicit `drm` backend and dedicated Emacs bootstrap, plus a one-time SDDM greeter-path proof via `sddm-autologin` on `seat0`. The remaining work is repeatability and session polish, not basic launch viability or packaged stop-path repair. |
| Multimodal input surfaces | Design | Eye tracking, hand tracking, HID helpers, mouth/voice, and BCI are documented or partially implemented, with synthetic paths labeled test-only. None are currently claimed as named-host product input support. |

## Start Here

- [Support Matrix](docs/support-matrix.md)
- [Remote Build Authority](docs/remote-build-authority.md)
- [Remote Proof Lanes](docs/remote-proof-lanes.md)
- [Hygiene Mini-Sprint](docs/hygiene-minisprint-2026-04-25.md)
- [Reality Check](docs/reality-check-2026-04-22.md)
- [Reality-Driven Development Arc](docs/reality-driven-development-arc-2026-q2.md)
- [Grounded Milestone Plan](docs/grounded-milestone-plan-2026-q2.md)
- [Yoga Session Proof](docs/yoga-session-proof-2026-04-22.md)
- [Yoga Local Session Runbook](docs/yoga-local-session-runbook.md)
- [Honey Substrate Proof](docs/honey-substrate-proof-2026-04-22.md)
- [Honey Fresh-Boot Runbook](docs/honey-fresh-boot-runbook-2026-04-26.md)
- [Honey Fresh-Boot Evidence Template](docs/honey-fresh-boot-evidence-template.md)
- [Honey P4 Visual First-Frame Evidence Template](docs/honey-p4-visual-first-frame-evidence-template.md)
- [Honey PPS Diagnostic Runbook](docs/honey-pps-diagnostic-runbook-2026-05-12.md)
- [Emacs/eGreg App-Layer Contract](docs/emacs-egreg-app-layer.md)
- [Status](docs/status.md)
- [Q2 2026 Roadmap](docs/roadmap-2026-q2.md)
- [Installation Quickstart](docs/installation-quickstart.md)
- [VR Guide](docs/vr-guide.md)

## Scope

The repo contains four different kinds of work:

- native compositor and Emacs/eGreg app-layer client code
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

## Related repositories

Platform-specific work for the Dell Precision 7810 (`honey` host) lives in
[`Jesssullivan/Dell-7810`](https://github.com/Jesssullivan/Dell-7810):

- Hardware design, reset behavior, power paths, BIOS/SMI characterization:
  [`Dell-7810/docs/platform/`](https://github.com/Jesssullivan/Dell-7810/tree/main/docs/platform)
- `honey` reset matrix and power characterization:
  [`Dell-7810/docs/research/`](https://github.com/Jesssullivan/Dell-7810/tree/main/docs/research)
- Ownership boundary between repos:
  [`Dell-7810/docs/platform/xoxdwm-boundary-audit.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/xoxdwm-boundary-audit.md)
- Chapel/NUMA host characterization and property-based testing:
  [`Dell-7810/analysis/`](https://github.com/Jesssullivan/Dell-7810/tree/main/analysis)

XoxdWM proves that software works on a prepared host. Dell-7810 proves the
host is prepared.

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
- `honey` smoke: convert the current-boot direct-mode session proof into visible first-frame and repeatable installed-lane evidence

Everything else remains secondary until those three named outcomes are green.
