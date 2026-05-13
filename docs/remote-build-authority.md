# XoxdWM Remote Build Authority

Snapshot date: 2026-04-25

This repo is the source and truth surface for XoxdWM implementation work. It is
not the Rocky build farm, not the Bazel control plane, and not the runner/cache
substrate.

## Core Rule

When working from `neo`:

- treat Darwin as control-plane only
- do not treat local `cargo build`, `nix develop`, or local package assembly as
  Rocky build authority
- use Rocky / Linux remote lanes for build, package, and runtime proof

Local work on `neo` is still useful for:

- editing code and docs
- `just truth-lint`
- `just test`
- workflow and repo-shape review

If you need the live repo-owned workflow map, read
[remote-proof-lanes.md](remote-proof-lanes.md).

## Direnv, Remote Devshells, And Bazel

The shell layers now have an explicit split:

- local `.envrc` in this repo is intentionally minimal:
  - `use flake`
- on `neo`, `direnv` is a control-plane convenience for editing, docs, and
  cheap repo checks
- `ssh honey` does not inherit that local shell contract
- for live host work on `honey`, prefer explicit remote `nix develop`
  entrypoints over assuming `direnv` is installed or allowed there
- if you need a remote host shell or command lane from `neo`, use the repo
  operator helpers:
  - `just honey-shell`
  - `just honey-devshell`
  - `just honey-run honey -- <command...>`
  - `just honey-proof-env`

This keeps the contract honest:

- `direnv` is local convenience
- remote `nix develop` is the live `neo -> honey` devshell/operator lane
- Bazel remains the external Rocky control-plane concern in `rockies`

Do not turn `honey` into the Bazel control plane by default. Use `honey` for:

- live XR/runtime proof
- package install and host validation
- systemd, OpenXR, Monado, DRM, and connector debugging
- installed OpenXR smoke-client validation after a package is built

Use `rockies` Bazel surfaces for:

- Rocky graph validation
- manifest-to-artifact orchestration
- control-plane helpers around Rocky-compatible handoff artifacts

## Authority Surfaces

### `Jesssullivan/XoxdWM`

This repo is the personal-authoritative implementation surface for:

- shell and compositor code
- input, VR, eye-tracking, and BCI experimentation
- support-matrix and reality-check truth for this codebase

It does not vendor the external Rocky Bazel/remote-build configuration.

### `tinyland-inc/rockies`

The local sibling repo at `/Users/jess/git/rockies` is the Rocky-facing control
plane.

Relevant references there:

- [README.md](</Users/jess/git/rockies/README.md>)
- [bazel/README.md](</Users/jess/git/rockies/bazel/README.md>)
- [docs/build-strategy.md](</Users/jess/git/rockies/docs/build-strategy.md>)
- [docs/repo-topology.md](</Users/jess/git/rockies/docs/repo-topology.md>)

What those docs make explicit:

- `rockies` owns umbrella composition and Bazel orchestration
- Bazel there is internal orchestration and validation glue, not release truth
- live proof should prefer the shared-runner workflow lanes over local rebuilds
- the current Rocky 10 Budgie bootstrap profile explicitly marks `Jesssullivan/XoxdWM`
  as an external surface and out of scope for that non-VR bootstrap slice

### `tinyland-inc/linux-xr`

The case-sensitive checkout at `/Volumes/linux-xr-cs/linux-xr` is the
kernel-authoritative surface for XR kernel packaging and carry. The older
Darwin checkout at `/Users/jess/git/linux-xr` is not an ingestion source unless
it is intentionally rebuilt or moved onto a case-sensitive filesystem; the
Linux tree contains case-distinct paths that a default macOS checkout cannot
represent safely.

Relevant references:

- [README.md](</Volumes/linux-xr-cs/linux-xr/README.md>)
- [xr/patches/README.md](</Volumes/linux-xr-cs/linux-xr/xr/patches/README.md>)
- [site/docs/upstream-status.md](</Volumes/linux-xr-cs/linux-xr/site/docs/upstream-status.md>)
- [xr/scripts/build-rpm.sh](</Volumes/linux-xr-cs/linux-xr/xr/scripts/build-rpm.sh>)

Current truth from that repo:

- kernel builds run on GloriousFlywheel infrastructure
- kernel carry patches are owned in `linux-xr` under `xr/patches`
- linux-xr PR #23 reconciled the current carry hygiene at merge commit
  `35ccbe2`
- linux-xr PR #26 reconciled Dell host-validation wording at merge commit
  `f991999`
- linux-xr PR #28 refreshed DSC upstream status at merge commit `323597c`;
  treat the DSC carry as partial upstream overlap, not wholly absent upstream
- DisplayID DSC BPP and the Bigscreen Beyond non-desktop quirk remain the
  active upstreamable split; SMI, NUMA, tuned-profile, and rollback validation
  remain Dell-7810/XoxdWM operator surfaces rather than kernel-patch carry

### `Jesssullivan/Dell-7810`

The local sibling repo at `/Users/jess/git/Dell-7810` is the Dell Precision
7810 host-evidence authority for `honey`.

Relevant references there:

- [docs/platform/rt-research-contract.md](</Users/jess/git/Dell-7810/docs/platform/rt-research-contract.md>)
- [docs/tracking/rt-smi-numa-chapel-focus-2026-04-25.md](</Users/jess/git/Dell-7810/docs/tracking/rt-smi-numa-chapel-focus-2026-04-25.md>)
- [docs/platform/authority-map.md](</Users/jess/git/Dell-7810/docs/platform/authority-map.md>)
- [docs/platform/xoxdwm-symbiosis-touchpoints.md](</Users/jess/git/Dell-7810/docs/platform/xoxdwm-symbiosis-touchpoints.md>)

What those docs make explicit:

- Dell-7810 owns BIOS, SMI, RT/NUMA, reset, fan, enclosure, and Chapel host
  probe evidence for `honey`
- XoxdWM may consume that evidence as a named-host precondition, but XoxdWM
  should not restate Dell raw measurements as software proof
- XoxdWM owns downstream software-benefit claims, especially `C4` RT benefit
  claims
- generic-lane Chapel evidence exists in Dell-7810, while RT-lane Chapel
  evidence remains a Dell follow-up before any software-side claim should use it

### `tinyland-inc/GloriousFlywheel`

The local sibling repo at `/Users/jess/git/GloriousFlywheel` is the runner and
cache substrate.

Relevant references:

- [docs/current-state.md](</Users/jess/git/GloriousFlywheel/docs/current-state.md>)
- [tofu/stacks/arc-runners/honey.tfvars](</Users/jess/git/GloriousFlywheel/tofu/stacks/arc-runners/honey.tfvars>)

Current truth from that repo:

- XoxdWM self-hosted Nix workflows target the shared `tinyland-nix`
  capability lane instead of the stale `xoxdwm-nix` repo-shaped label
- a queued shared-runner job with no `runner_name` is an enrollment, owner
  boundary, or ARC reachability signal, not an XoxdWM product proof failure
- workflows only select `tinyland-nix` when both `USE_SELFHOSTED` and
  `GF_SHARED_RUNNERS_REACHABLE` are `true`; otherwise they use hosted Linux
  fallback or skip self-hosted-only lanes
- hosted-capable XoxdWM jobs must not directly reference private/external
  GloriousFlywheel actions, because GitHub resolves `uses:` actions before
  fallback step gating can protect them; `.github/actions/ensure-nix` carries
  the minimal local consumer shim
- `openxr-smoke-client.yml` is an XoxdWM-owned companion RPM lane for the
  Khronos `hello_xr` smoke client; it proves package buildability, and run
  `24938791255` has been installed on `honey` for status/preflight selection,
  while repeated runtime proof still belongs to named-host operator runs
- `tinyland-inc/GloriousFlywheel#413` is the current tracker for proving
  `Jesssullivan/XoxdWM` can reach the shared `tinyland-nix` lane without
  recreating repo-shaped runner labels
- `just runner-reachability-truth` is the repo-local read-only classifier for
  this boundary; it should report blocked until an opt-in `tinyland-nix` job
  has a non-empty `runner_name` on a compliant shared runner
- Bazel remote cache and Attic live here as acceleration layers, not as
  publication truth by themselves

## What This Means Operationally

From `neo`, the correct default sequence is:

1. Edit and review code here in `XoxdWM`.
2. Run cheap local sanity only.
3. Use the repo-owned remote proof lanes in [remote-proof-lanes.md](remote-proof-lanes.md)
   when you need live workflow evidence.
4. Use the explicit remote host lane for live `honey` work:
   - `just honey-devshell`
   - `just honey-run honey -- <command...>`
   - `just honey-proof-env`
5. Hand Bazel/control-plane build and package proof to the external Rocky /
   Linux lanes in `rockies`, `linux-xr`, and `GloriousFlywheel`.
6. Hand runtime proof to named hosts.
7. Reflect the remote result back into [status.md](status.md) and
   [support-matrix.md](support-matrix.md).

## What Is Still Missing

This repo now points at the real external authority surfaces, but it still does
not provide a repo-local wrapper around those external Bazel entrypoints.

That is intentional for now. The next step is not to invent a fake local Bazel
world in XoxdWM. The next step is to add explicit references or wrappers only
once the exact external entrypoints we want to bless are stable enough to point
at directly.
