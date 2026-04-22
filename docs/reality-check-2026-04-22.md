# XoxdWM Reality Check - 2026-04-22

This note is a reset on what XoxdWM is, what it is not, and what the repo can honestly claim right now.

## What XoxdWM actually is

XoxdWM is currently three things at once:

- an experimental Wayland compositor plus Emacs window-management layer
- a packaging and release lane for Rocky, Debian, and Nix surfaces
- a large research and subsystem inventory for VR, eye tracking, hand tracking, and BCI work

That means the repo contains more implementation and documentation surface than the named-host validation currently justifies.

## What is true today

- There is a real public release lane.
  - `v0.5.0` and `v0.5.1` are tagged and public.
- The Rocky base compositor package lane is real.
  - current repo docs claim `v0.5.1` is the public repaired Rocky compositor RPM
  - current repo docs claim `yoga` validated package install, clean runtime linking, and a bounded compositor start with `seatd`
- `honey` is not currently running a deployed XoxdWM stack.
  - there is still no current named-host claim for a working XoxdWM + Monado + OpenXR client-tool path on `honey`
- `yoga` is not yet a polished local desktop/session target.
  - the host now has an active SDDM greeter lane in addition to the installed
    `exwm-vr.desktop` session entry and user-unit stack
  - a one-time `sddm-autologin` run has now proved the real greeter path can
    launch `EXWM-VR` on `seat0`
  - the refreshed packaged unit now carries the validated `SuccessExitStatus=15`
    stop-path fix, so the remaining follow-on is repeated manual/operator
    evidence if we want it
- The Emacs-side test surface is substantial and currently passes locally on this checkout.
  - `just test` passed: `1891/1891`
- The repo contains real compositor, Elisp, Nix, packaging, and CI work, not just ideas.

## What is also true, but easy to miss

- A lot of the repo is research or inventory, not support promise.
- The current `honey` hardware investigation shows partial host reality, not product reality.
  - current live host evidence shows a Dell HDMI management path and a Bigscreen-connected `DP-2` path can both appear after hard reset
  - that does not equal a deployed XoxdWM VR smoke path
- `neo` is not a product target.
  - XoxdWM is not expected to run on macOS as a desktop or VR environment.
  - the authoritative build and runtime surface should remain Rocky / Linux remote lanes
  - the current external control-plane split is documented in [remote-build-authority.md](remote-build-authority.md)
- The local Apple control-plane surface is now workable for repo management.
  - `nix flake check --no-build` passes locally
  - `nix develop -c just build` passes locally
  - `cargo test --manifest-path compositor/Cargo.toml --no-default-features` passes locally
  - that improvement is about orchestration and headless logic work, not about making macOS a deployment target

## What is not true

- XoxdWM is not a proven daily-driver VR desktop on `honey`.
- XoxdWM is not currently a polished local Rocky desktop/session on `yoga`.
- Eye tracking, hand tracking, gesture, and BCI support are not currently proven on named lab hosts.
- The repo still contains large reference docs and partial inventories that can drift away from the truth surface unless they are actively maintained.

## What is still design or speculation

- broad HMD support claims in [vr-guide.md](vr-guide.md)
- named-host readiness for eye tracking, hand tracking, and BCI flows
- end-to-end Nix module usability from the published examples
- any claim that the large feature inventory equals present support

Two specific examples of drift that still need active guardrails:

- large reference docs had been carrying early-version headers, missing-file references, and stale inventory counts
- the repo needs cheap truth-surface checks so doc and CI drift becomes visible before release work starts leaning on it

## The honest near-term goal

XoxdWM does not need a bigger vision statement right now. It needs an honest MVP:

1. `yoga`: reproducible Rocky 10 desktop/dev install with a real local session path
2. `honey`: deployed XoxdWM + Monado + OpenXR client-tool path plus one recorded compositor smoke path
3. repo truth surface: docs that clearly separate validated support from research and speculation
4. primary developer surface: keep `neo` usable as an orchestration machine while making Rocky / Linux remote build authority explicit

## Decision rule

Until the repo is reconciled, treat claims this way:

- if it is validated on `yoga`, `honey`, or stable CI, call it real
- if it is packaged once or manually smoke-tested once, call it smoke only
- if it lives mainly in subsystem docs, feature matrices, or research notes, call it design/speculation unless proven otherwise
