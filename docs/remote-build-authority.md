# XoxdWM Remote Build Authority

Snapshot date: 2026-04-22

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

The local sibling repo at `/Users/jess/git/linux-xr` is the kernel-authoritative
surface for XR kernel packaging and carry.

Relevant references:

- [README.md](</Users/jess/git/linux-xr/README.md>)
- [xr/scripts/build-rpm.sh](</Users/jess/git/linux-xr/xr/scripts/build-rpm.sh>)

Current truth from that repo:

- kernel builds run on GloriousFlywheel infrastructure
- XoxdWM patch payloads are consumed from XoxdWM during kernel packaging

### `tinyland-inc/GloriousFlywheel`

The local sibling repo at `/Users/jess/git/GloriousFlywheel` is the runner and
cache substrate.

Relevant references:

- [docs/current-state.md](</Users/jess/git/GloriousFlywheel/docs/current-state.md>)
- [tofu/stacks/arc-runners/honey.tfvars](</Users/jess/git/GloriousFlywheel/tofu/stacks/arc-runners/honey.tfvars>)

Current truth from that repo:

- `Jesssullivan/XoxdWM` has a real repo-owned runner authority surface again
- the `xoxdwm-nix` runner exists as a repo-scoped self-hosted lane
- Bazel remote cache and Attic live here as acceleration layers, not as
  publication truth by themselves

## What This Means Operationally

From `neo`, the correct default sequence is:

1. Edit and review code here in `XoxdWM`.
2. Run cheap local sanity only.
3. Use the repo-owned remote proof lanes in [remote-proof-lanes.md](remote-proof-lanes.md)
   when you need live workflow evidence.
4. Hand build/package/runtime proof to the external Rocky / Linux remote lanes
   and named hosts.
5. Reflect the remote result back into [status.md](status.md) and
   [support-matrix.md](support-matrix.md).

## What Is Still Missing

This repo now points at the real external authority surfaces, but it still does
not provide a repo-local wrapper around those external Bazel entrypoints.

That is intentional for now. The next step is not to invent a fake local Bazel
world in XoxdWM. The next step is to add explicit references or wrappers only
once the exact external entrypoints we want to bless are stable enough to point
at directly.
