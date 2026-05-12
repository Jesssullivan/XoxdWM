# XoxdWM Reality-Driven Development Arc - Q2 2026

This document turns the repo-wide audit into an execution model.

It is not a feature wishlist. It is a constraint document for how XoxdWM should
be described, tested, and advanced from here.

Read this together with [reality-check-2026-04-22.md](reality-check-2026-04-22.md),
[status.md](status.md), and [support-matrix.md](support-matrix.md).

## Repo Snapshot

Snapshot basis: local checkout on 2026-04-22.

- IPC command surface in [compositor/src/ipc/dispatch.rs](../compositor/src/ipc/dispatch.rs): 184 commands
- Elisp modules under [lisp](../lisp): 78 files
- Rust source and integration-test files under [compositor](../compositor): 71 files
- Documentation files under [docs](../docs): 72 files
- Research docs under [docs/research](research): 47 files
- GitHub Actions workflows under [.github/workflows](../.github/workflows): 12 files
- ERT tests under [test](../test): 1891 tests
- Rust `#[test]` functions under [compositor/src](../compositor/src) and [compositor/tests](../compositor/tests): 460 tests

This is a substantial codebase. The current problem is not lack of implementation
surface. The problem is that validated reality is much narrower than the combined
implementation, documentation, and research surface suggests.

## Hard Findings

### 1. The repo has three different semantic layers

XoxdWM is currently all three of these at once:

- a real native compositor plus Emacs/eGreg app-layer client codebase
- a real packaging and integration repo for Rocky, Debian, Nix, systemd, and SELinux
- a large research and subsystem inventory for VR, eye tracking, hand tracking, and BCI

Those layers should not be described with the same confidence level.

### 2. The implementation surface is ahead of the named-host proof surface

The compositor state in [compositor/src/state.rs](../compositor/src/state.rs)
already wires real protocol state for:

- session lock
- idle notify and idle inhibit
- primary selection
- dmabuf
- cursor shape
- xdg activation
- foreign toplevel list
- screencopy
- output management
- pointer constraints

That means some older roadmap language about foundational protocol gaps is now stale.
The implementation is no longer the main bottleneck on every axis.

At the same time, [compositor/src/handlers/data_control.rs](../compositor/src/handlers/data_control.rs)
is still an explicit stub, so not every desktop-credibility gap is closed.

### 3. The documentation surface has been the biggest falsehood vector

The recent audit found several obvious drift markers:

- active reference docs were still carrying `Version 0.1.0` headers
- [docs/developer-guide.md](developer-guide.md) still references a missing `PLAN.md`
- [docs/feature-matrix.md](feature-matrix.md) says the dispatch table contains 131 commands, but the current dispatcher exposes 184

This is the clearest repo-wide falsehood class: inventory and reference docs can
silently tell the wrong story unless the repo has cheap checks for stale
headers, missing-file references, and inventory-count mismatches.

### 4. The local control-plane surface is usable, but Darwin is still the wrong authority

What is strong:

- `just test` passes locally on this checkout
- the repo has a large deterministic Elisp and Rust logic-test surface
- Rocky container CI exists
- Nix test surfaces exist

What is true after the current repair pass:

- `nix flake check --no-build` passes on this Apple host
- `nix develop -c just build` passes on this Apple host
- `cargo test --manifest-path compositor/Cargo.toml --no-default-features` passes on this Apple host

What is still true strategically:

- XoxdWM is not meant to run on macOS
- `neo` should be treated as orchestration, documentation, and control-plane surface
- Rocky / Linux remote lanes should remain the authoritative build and runtime surface
- this repo currently contains no Bazel or remote-build configuration of its own, so that authority boundary must be documented rather than assumed
- the external authority split should be made explicit through [remote-build-authority.md](remote-build-authority.md), not by pretending the control plane lives here
- the repo-owned remote proof lanes should also stay explicit through
  [remote-proof-lanes.md](remote-proof-lanes.md) so shared CI, Rocky container
  smoke, and honey hardware evidence do not get conflated

### 5. The test count is high, but the test pyramid is skewed

The current test suite is strongest in deterministic logic and weakest in named-host
acceptance.

Strong today:

- Elisp command and state tests
- Rust logic tests for VR, BCI, IPC, timing, and scene math
- headless integration tests such as [compositor/tests/headless_integration.rs](../compositor/tests/headless_integration.rs)
- synthetic full-stack logic tests such as [compositor/tests/full_stack_integration.rs](../compositor/tests/full_stack_integration.rs)

Weaker today:

- build-path truth tests
- doc/reference freshness checks
- named-host acceptance on `yoga`
- real VR smoke acceptance on `honey`

An example of the skew: [test/week20-integration-test.el](../test/week20-integration-test.el)
mostly verifies file presence, document existence, and legacy release markers,
not current deployment truth.

### 6. The product story is narrower than the subsystem story

The repo can honestly claim this near-term product shape:

- `yoga`: Rocky desktop/dev MVP
- `honey`: first real VR smoke path

It cannot honestly claim broad readiness for:

- polished daily-driver VR
- named-host eye tracking support
- named-host hand tracking support
- named-host BCI support
- cross-platform contributor smoothness

Those areas remain design or subsystem work until they are tied to named-host proof.

## Semantic Rules

From this point forward, use these labels consistently:

- `Product`: repeatably validated on a named host or stable automation and
  ready to present as a supported product surface
- `Smoke`: packaged or validated once, but not yet stable
- `Prototype`: active implementation exists, but still lacks named-host proof
- `Synthetic`: code/static/headless/simulated proof only
- `Design`: code, docs, or research exist without named-target proof

And use these narrative rules:

- If it only exists in research or subsystem docs, do not describe it as active support.
- If it has deterministic tests but no named-host proof, call it implemented logic, not validated product behavior.
- If a path is broken on the active contributor host, do not present it as the default workflow without qualification.
- If a claim depends on `yoga` or `honey`, the docs must say so explicitly.

## Development Arc

### Track 1: Truth Surface

Goal: make public repo claims mechanically harder to overstate.

Required outcomes:

- keep [README.md](../README.md), [status.md](status.md), [support-matrix.md](support-matrix.md),
  and [reality-check-2026-04-22.md](reality-check-2026-04-22.md) aligned
- reduce or rewrite stale `v0.1.0` reference docs
- stop presenting inventories as support promises
- keep named-host claims explicit

### Track 2: Developer Surface And Build Authority

Goal: keep `neo` useful as an orchestration host without pretending Darwin is the product surface.

Required outcomes:

- keep the documented local control-plane workflows real
- document that Rocky / Linux remote lanes are the authoritative build and runtime surface
- keep the repo-owned workflow map explicit so remote CI and named-host proof are not confused
- avoid investing in Darwin as if it were a runtime target
- make the cross-repo boundary to the actual Rocky remote build toolchain explicit

### Track 3: CI And Test Discipline

Goal: align automation with the repo's actual risk surface.

Required outcomes:

- keep lightweight deterministic checks mandatory on code changes
- keep hardware lanes opt-in and capability-gated
- add truth-surface linting so stale docs become visible before release
- separate "exists in tree" tests from "works on host" tests

### Track 4: `yoga` Desktop/Dev MVP

Goal: convert the current bounded package success into a usable local session.

Required outcomes:

- documented local login or launch path
- repeatable compositor startup outside an ssh-bounded smoke
- clear rollback and operator path
- package lane stays green while VR extras remain optional

### Track 5: `honey` XR Substrate Plus First XoxdWM Smoke

Goal: prove the host substrate that the goggles product depends on, then
produce the first honest named-host XoxdWM smoke result on top of it.

Required outcomes:

- stable enough host substrate facts for connector, runtime, and bridge path
- active OpenXR runtime selection
- working client-tool path such as `openxr-info`
- deployed XoxdWM build or package on host
- one recorded compositor launch or failure mode with evidence
- host instability issues recorded separately from compositor claims

## Test Model

The repo should move to a layered test model.

### Tier 0: Truth Lint

Cheap checks that keep the docs honest:

- no stale `Version 0.1.0` headers in active canonical docs
- no references to missing files like `PLAN.md`
- support-matrix/status/reality docs agree on named-host claims
- feature-matrix inventory numbers are regenerated from code, not hand-maintained guesses

### Tier 1: Deterministic Logic

Current strong area. Keep it mandatory.

- ERT logic tests
- Rust unit tests
- synthetic integration around IPC, gaze, gesture, BCI, scene graph, secure input

### Tier 2: Build And Packaging Truth

These should gate routine code changes on supported automation surfaces.

- Elisp byte-compile path
- Rust headless build and test path
- flake evaluation on supported systems
- Rocky container build and test path

### Tier 3: Named-Host Acceptance

These should be explicit smoke lanes, not implied by lower-tier success.

- `yoga` package install and local session acceptance
- `honey` userspace/runtime acceptance

### Tier 4: Hardware-In-The-Loop VR

This remains opt-in until the host is trustworthy.

- HMD enumeration
- connector and lease state
- OpenXR runtime smoke
- compositor launch against real display path

## Recommended Near-Term Backlog

1. Fix [justfile](../justfile) so the documented Elisp build path is real.
2. Rewrite or sharply reduce the stale `v0.1.0` reference docs.
3. Add a cheap truth-lint check for stale version headers, missing-doc references, and feature-matrix drift.
4. Document `neo` as orchestration-only and Rocky / Linux remote lanes as authoritative build surface.
5. Finish the `yoga` local session path.
6. Produce the first evidence-backed `honey` XoxdWM smoke path, even if it fails for host reasons.

## Decision Rule

When choosing what to work on next:

- prefer work that narrows the gap between repo claims and named-host truth
- prefer fixes that make contributor workflows real over adding new speculative subsystem surface
- do not treat research depth as product maturity
- do not treat test count as host validation
