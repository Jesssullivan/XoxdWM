# COSMIC Reference Pins - 2026-05-10

This is a research ledger for native XoxdWM authority work. It does not vendor
COSMIC code, add a COSMIC runtime dependency, or turn COSMIC packaging into a
current product target.

## Epoch Pin

- Upstream: `pop-os/cosmic-epoch`
- Release: `epoch-1.0.12`
- Release URL: <https://github.com/pop-os/cosmic-epoch/releases/tag/epoch-1.0.12>
- Published: 2026-05-05 21:23:48 UTC
- Annotated tag: `e0246cb61c4570464597727645b0684f5cc440a8`
- Checked-out commit: `4412bb00d7b9924131d49738164e80597c041ea6`
- Release note items relevant to XoxdWM: Smithay update in `cosmic-comp`,
  resume DPMS cleanup, Rust 1.93 toolchain bump, and dependency updates across
  the Epoch graph.

## Module Commits Worth Studying

| Module | Commit | XoxdWM relevance |
| --- | --- | --- |
| `cosmic-comp` | `b5a1a6d3179810627fa0bffac7bd5d78c7df4fa0` | Smithay compositor policy, workspace/layout/session boundaries. |
| `cosmic-session` | `17cf4485a917c5e7490c0e1a26cdf348f06bf486` | Session startup, lock/logout boundary, environment handoff. |
| `cosmic-idle` | `c95d066b5b640509a6369634b669ca60dc50e168` | Idle supervision and DPMS-related behavior. |
| `cosmic-randr` | `6e8e795970fa06d434af22775e415b517f7552d3` | Output configuration patterns and user-facing display state. |
| `cosmic-panel` | `2358f0473bf68b79f54a0906994a218de211de34` | App-layer shell client behavior, not compositor authority. |
| `cosmic-launcher` | `296e5cb66c77159840d2039540ede315bcd51ab0` | Launcher UX as client surface; XoxdWM keeps launch authority in native config. |
| `cosmic-settings-daemon` | `716da6d6af0b252e2f78aba2ad72ee19ae0241e0` | Settings daemon split from compositor policy. |
| `xdg-desktop-portal-cosmic` | `db8ec7cf496ed0f2028c67f4eec7ffdc2cbcf145` | Portal boundary reference for later capture/session work. |

## Mapping To XoxdWM Trackers

| XoxdWM issue | COSMIC reference area | Boundary |
| --- | --- | --- |
| #51, #54, #59, #65 | `cosmic-comp`, `cosmic-settings-daemon` | Native config and reload authority stay in Rust. |
| #60, #61 | `cosmic-comp`, `cosmic-workspaces-epoch` | Study layout/workspace behavior; do not copy implementation. |
| #67, #70 | `cosmic-session`, `cosmic-launcher` | Startup and launch targets stay configured native policy. |
| #68, #69 | `cosmic-session`, `cosmic-idle` | Lock/logout/idle/DPMS boundaries stay narrow and explicit. |
| #71, #72, #73 | `cosmic-panel`, applets | Emacs/eGreg remains an app-layer/control client, analogous to shell clients. |

## Explicit Non-Goals

- Do not vendor COSMIC source into this repository.
- Do not pin arbitrary module `HEAD` commits outside the coherent Epoch release
  graph for authority work.
- Do not use COSMIC visual assets, app examples, or shell UX as product proof
  for XoxdWM.
- Do not promote this ledger beyond research/design evidence until a separate
  runtime or packaging task exists.
