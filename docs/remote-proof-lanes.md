# XoxdWM Remote Proof Lanes

Snapshot date: 2026-04-22

This document names the remote workflow and host lanes that actually matter when
you are operating XoxdWM from `neo`.

Read this with [remote-build-authority.md](remote-build-authority.md). That file
explains which repos own which kinds of truth. This file explains which current
XoxdWM-side remote lanes are worth looking at before you make a support or
runtime claim.

## Core Distinction

Not every remote workflow is authoritative in the same way.

- Repo-owned CI and cache lanes prove that this repo still builds, evaluates, or
  runs tests on the shared remote substrate.
- Rocky container lanes provide bounded Rocky userspace smoke, but they are not
  the same as named-host proof on `yoga` or `honey`.
- Honey hardware lanes provide bounded host-side GPU and VR evidence, but they
  still do not imply a stable daily-driver VR desktop.
- Named-host claims should still be grounded in explicit `yoga` or `honey`
  evidence reflected back into [status.md](status.md) and
  [support-matrix.md](support-matrix.md).

## Current Remote Lanes

| Lane | Workflow / Surface | Runner | What It Proves | What It Does Not Prove |
| --- | --- | --- | --- | --- |
| Shared runner health | [.github/workflows/runner-health.yml](../.github/workflows/runner-health.yml) | `xoxdwm-nix`, optional `honey` | repo-owned self-hosted runner, Nix, and Attic path are alive | Rocky desktop runtime or named-host compositor success |
| Shared fast CI | [.github/workflows/self-hosted-fast.yml](../.github/workflows/self-hosted-fast.yml) | `xoxdwm-nix`, optional `honey` | shared self-hosted fast path still works on the canonical repo | full Rocky support or a stable VR deployment |
| Nix build and cache | [.github/workflows/nix-cache.yml](../.github/workflows/nix-cache.yml), [.github/workflows/cache-warm.yml](../.github/workflows/cache-warm.yml) | `xoxdwm-nix` | key Nix outputs still realize on the repo-owned remote lane | named-host runtime truth |
| Rocky userspace smoke | [.github/workflows/rocky-test.yml](../.github/workflows/rocky-test.yml) | `ubuntu-latest` with `rockylinux:10` container | bounded Rocky userspace and headless build smoke | `yoga`, `honey`, or custom Rockies build authority |
| Release packaging | [.github/workflows/packaging.yml](../.github/workflows/packaging.yml) | mixed: `xoxdwm-nix` and hosted Rocky container jobs | release artifact production and bounded RPM metadata validation | deployed host truth or full VR package integration |
| Honey VR hardware | [.github/workflows/vr-hardware.yml](../.github/workflows/vr-hardware.yml) and `fast-vr` in [.github/workflows/self-hosted-fast.yml](../.github/workflows/self-hosted-fast.yml) | `[self-hosted, honey]` | GPU, DRM, Monado, and bounded VR smoke when hardware lanes are explicitly enabled | stable deployed XoxdWM VR desktop on `honey` |
| External Rocky control plane | sibling repos [rockies](</Users/jess/git/rockies>), [linux-xr](</Users/jess/git/linux-xr>), and [GloriousFlywheel](</Users/jess/git/GloriousFlywheel>) | external | Rocky composition policy, kernel packaging, and runner/cache substrate | XoxdWM implementation truth by themselves |

## Default Sequence From `neo`

1. Edit code and docs here.
2. Run cheap local sanity only: `just truth-lint`, `just test`, and optionally
   `nix flake check --no-build`.
3. Inspect the live remote surface with `just remote-proof-surface` and
   `just remote-proof-runs`.
4. Dispatch bounded remote checks when needed:
   - `just remote-runner-health`
   - `just remote-cache-warm`
   - `just remote-vr-smoke smoke`
5. Treat `yoga` and `honey` host evidence as the only basis for support-matrix
   or status promotion.

## Operator Notes

- The `gh`-driven helper commands in [justfile](../justfile) are intentionally
  thin wrappers around the real GitHub Actions lanes.
- `rocky-test.yml` is useful, but it is still a containerized Rocky smoke lane,
  not a replacement for host truth.
- `packaging.yml` is a release lane, not a support claim.
- `vr-hardware.yml` should stay opt-in and capability-gated. A green run there
  is strong evidence, but it is still not the same thing as a repeatable named-host
  user session.
