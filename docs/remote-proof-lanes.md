# XoxdWM Remote Proof Lanes

Snapshot date: 2026-04-25

This document names the remote workflow and host lanes that actually matter when
you are operating XoxdWM from `neo`.

Read this with [remote-build-authority.md](remote-build-authority.md). That file
explains which repos own which kinds of truth. This file explains which current
XoxdWM-side remote lanes are worth looking at before you make a support or
runtime claim.

For the next attended `honey` fresh-boot proof, use
[honey-fresh-boot-runbook-2026-04-26.md](honey-fresh-boot-runbook-2026-04-26.md).

## Core Distinction

Not every remote workflow is authoritative in the same way.

- Repo-side CI and cache lanes prove that this repo still builds, evaluates, or
  runs tests on the shared GloriousFlywheel remote substrate.
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
| Shared runner health | [.github/workflows/runner-health.yml](../.github/workflows/runner-health.yml) | `tinyland-nix`, optional `honey` | shared GloriousFlywheel runner, Nix, and Attic path are alive | Rocky desktop runtime or named-host compositor success |
| Shared fast CI | [.github/workflows/self-hosted-fast.yml](../.github/workflows/self-hosted-fast.yml) | `tinyland-nix`, optional `honey` | shared self-hosted fast path still works on the canonical repo | full Rocky support or a stable VR deployment |
| Nix build and cache | [.github/workflows/nix-cache.yml](../.github/workflows/nix-cache.yml), [.github/workflows/cache-warm.yml](../.github/workflows/cache-warm.yml) | `tinyland-nix` | key Nix outputs still realize on the shared remote lane | named-host runtime truth |
| Rocky userspace smoke | [.github/workflows/rocky-test.yml](../.github/workflows/rocky-test.yml) | `ubuntu-latest` with `rockylinux:10` container | bounded Rocky userspace and headless build smoke | `yoga`, `honey`, or custom Rockies build authority |
| Release packaging | [.github/workflows/packaging.yml](../.github/workflows/packaging.yml) | mixed: `tinyland-nix` and hosted Rocky container jobs | release artifact production and bounded RPM metadata validation | deployed host truth or full VR package integration |
| Monado companion packaging | [.github/workflows/monado-companion.yml](../.github/workflows/monado-companion.yml) | hosted Rocky container job | bounded Rocky RPM production for the companion Monado runtime lane | named-host proof or proof that the base compositor RPM now bundles Monado |
| OpenXR smoke client packaging | [.github/workflows/openxr-smoke-client.yml](../.github/workflows/openxr-smoke-client.yml) | hosted Rocky container job | bounded Rocky RPM production for the Khronos `hello_xr` smoke-client lane | repeated named-host runtime proof; run `24938791255` has since been installed on `honey` and selected by status preflight |
| Honey VR hardware | [.github/workflows/vr-hardware.yml](../.github/workflows/vr-hardware.yml) and `fast-vr` in [.github/workflows/self-hosted-fast.yml](../.github/workflows/self-hosted-fast.yml) | `[self-hosted, honey]` | GPU, DRM, Monado, and bounded VR smoke when hardware lanes are explicitly enabled | stable deployed XoxdWM VR desktop on `honey` |
| External Rocky control plane | sibling repos [rockies](</Users/jess/git/rockies>), [linux-xr](</Volumes/linux-xr-cs/linux-xr>), and [GloriousFlywheel](</Users/jess/git/GloriousFlywheel>) | external | Rocky composition policy, kernel packaging, and runner/cache substrate | XoxdWM implementation truth by themselves |

## Default Sequence From `neo`

1. Edit code and docs here.
2. Run cheap local sanity only: `just truth-lint`, `just test`, and optionally
   `nix flake check --no-build`.
3. Use the explicit remote host lane when the task is really about `honey`:
   - `just honey-shell`
   - `just honey-devshell`
   - `just honey-run honey -- <command...>`
   - `just honey-proof-env`
   - `just honey-openxr-status`
   - `just honey-openxr-smoke`
   - `just honey-openxr-clean-cycle`
   - `just honey-openxr-fresh-boot-check`
4. Inspect the live remote surface with `just remote-proof-surface` and
   `just remote-proof-runs`.
5. Dispatch bounded remote checks when needed:
   - `just remote-runner-health`
   - `just remote-cache-warm`
   - `just remote-monado-package`
   - `just remote-openxr-smoke-client-package`
   - `just remote-vr-smoke smoke`
6. Treat `yoga` and `honey` host evidence as the only basis for support-matrix
   or status promotion.

## Shared-Runner Reachability Gate

For non-hardware Nix lanes, `USE_SELFHOSTED=true` is no longer enough by
itself. XoxdWM only selects `tinyland-nix` when
`GF_SHARED_RUNNERS_REACHABLE=true` is also set.

Until that proof variable is set:

- mixed workflows use hosted Linux fallback instead of queuing forever
- self-hosted-only fast lanes skip
- hosted-capable jobs use the local `.github/actions/ensure-nix` shim instead
  of resolving GloriousFlywheel private/external actions directly
- GloriousFlywheel issue `#413` remains the owner-boundary and ARC
  reachability tracker
- this repo must not recreate repo-shaped `xoxdwm-*` runner labels to make the
  queue disappear

## `neo` To `honey` Operator Lane

The repo now has a thin explicit remote-dev/operator lane for working from
`neo` against the live `honey` host.

- `just honey-shell`
  - open a plain login shell on `honey` in `~/XoxdWM`
- `just honey-devshell`
  - open `nix develop` directly on `honey` in `~/XoxdWM`
- `just honey-run honey -- <command...>`
  - run one command through the remote `nix develop` lane
  - this also defaults `XDG_RUNTIME_DIR` to `/run/user/$(id -u)` on the host
- `just honey-proof-env`
  - print the remote repo path, runtime dir, key IPC sockets, and current
    user-service activity
- `just honey-openxr-status`
  - run the repo-owned OpenXR wrapper in status-only mode on `honey`
  - this does not start services or launch an OpenXR client
- `just honey-openxr-smoke`
  - run the repo-owned OpenXR wrapper against the active Monado runtime
  - this can launch `hello_xr` or another OpenXR client, so use it only when
    the compositor/Monado path is intentionally active
- `just honey-openxr-clean-cycle`
  - stop and restart only the XoxdWM/Monado user services, then run the
    packaged OpenXR smoke client
  - this intentionally exercises the clean service-cycle lane and does not
    stop or restart `rke2`
- `just honey-openxr-fresh-boot-check`
  - print post-boot host identity, repo head, boot ID, uptime, kernel, `rke2`
    state, and OpenXR status, then run `just honey-openxr-clean-cycle`
  - this target does not reboot `honey`; run it only after an attended/manual
    fresh boot has completed
  - use [honey-fresh-boot-runbook-2026-04-26.md](honey-fresh-boot-runbook-2026-04-26.md)
    for the evidence checklist and pass/fail criteria

This lane is for live host work such as:

- Monado and OpenXR probing
- systemd unit inspection
- DRM, Wayland, and IPC debugging
- running repo commands on the real Linux host from `neo`

It is not a replacement for the external Bazel control plane in `rockies`.
Use `rockies` when the work is really Rocky graph/orchestration policy rather
than direct `honey` host runtime proof.

## Hardware Authority for `honey`

The `honey` host platform (Dell Precision 7810) has its own measurement,
validation, and design work in
[`Jesssullivan/Dell-7810`](https://github.com/Jesssullivan/Dell-7810). Before
interpreting `honey` test results as XR software proofs, verify the host is
healthy using the
[reset matrix](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/research/honey-reset-matrix-2026-04-22.md)
and the
[platform validation scripts](https://github.com/Jesssullivan/Dell-7810/tree/main/scripts/platform).

## Operator Notes

- The `gh`-driven helper commands in [justfile](../justfile) are intentionally
  thin wrappers around the real GitHub Actions lanes.
- `rocky-test.yml` is useful, but it is still a containerized Rocky smoke lane,
  not a replacement for host truth.
- `packaging.yml` is a release lane, not a support claim.
- `monado-companion.yml` is the Rocky companion-runtime packaging lane. A green
  run there means the Monado RPM is buildable, not that `honey` is done.
- `openxr-smoke-client.yml` is the Rocky companion-client packaging lane. A
  green run there means the smoke client RPM is buildable. The artifact from
  run `24938791255` has since been installed on `honey` and selected by
  `just honey-openxr-status`; `just honey-openxr-clean-cycle` now proves clean
  user-service-cycle smoke, and `just honey-openxr-fresh-boot-check` is the
  prepared post-boot capture lane. Fresh-boot and in-goggles first-frame proof
  remain separate named-host evidence until that target is run after an
  attended/manual boot.
- `vr-hardware.yml` should stay opt-in and capability-gated. A green run there
  is strong evidence, but it is still not the same thing as a repeatable named-host
  user session.
