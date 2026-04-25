# XoxdWM / Rockies Hygiene Mini-Sprint - 2026-04-25

This mini-sprint turns the current audit into an execution checklist for
stabilizing XoxdWM, `rockies`, and the shared GloriousFlywheel substrate before
the next public-facing workstream push.

It is a hygiene and parity sprint, not a feature sprint. The goal is to make
the active repo, PR, issue, runner, cache, and proof surfaces tell the same
story.

Read this with:

- [remote-build-authority.md](remote-build-authority.md)
- [remote-proof-lanes.md](remote-proof-lanes.md)
- [status.md](status.md)
- [support-matrix.md](support-matrix.md)
- `/Users/jess/git/rockies/docs/build-strategy.md`
- `/Users/jess/git/GloriousFlywheel/docs/current-state.md`

Linear coordination:

- `TIN-556`: XoxdWM, `rockies`, and GloriousFlywheel hygiene mini-sprint parity

## Execution Board

Use this as the sprint todo list. The order matters: stabilize truth first,
then reconcile trackers, then widen repeatability work.

- [ ] Let XoxdWM PR #34 checks settle on the latest pushed head, then classify
  every non-green result as required, waived, cancelled-noise, or real blocker.
  Current queue finding: stale `xoxdwm-nix` jobs have no assigned runner.
- [ ] Verify the `tinyland-nix` migration on the next PR #34 head; if those
  jobs still queue, track it as GloriousFlywheel/repo-enrollment infrastructure,
  not XoxdWM product failure.
- [ ] Reconcile XoxdWM GitHub issues `#10`, `#11`, `#12`, `#13`, `#20`, and
  `#22` against PR #34 evidence. Triage comments are posted; closures remain
  gated on PR #34 stabilization.
- [x] Decide and retire or close stale XoxdWM Greptile canary PR `#27`.
- [x] Update Linear `TIN-341`, `TIN-342`, `TIN-343`, `TIN-344`, and `TIN-345`
  so completed evidence no longer looks open.
- [x] Keep `TIN-346` open until `honey` VR proof is repeatable, not just
  one-shot.
- [x] Keep `TIN-398` as the cross-repo honey RT/kernel posture lane.
- [x] Treat `/private/tmp/xoxdwm-host-contract` as duplicate/equivalent work
  unless a later audit finds missing content.
- [x] For `rockies` PR #121, identify whether the blocker is only draft/review
  state or a hidden merge requirement.
- [ ] For `rockies` PR #125, rerun the KVM VM smoke once if allowed; if the
  signal-9 failure reproduces, patch diagnostics and a bounded retry policy in
  `rockies`, not XoxdWM.
- [x] Preserve or branch dirty local GloriousFlywheel work before using it as an
  ingestion source. Preserved on `tinyland-inc/GloriousFlywheel#408`.
- [x] Keep GloriousFlywheel as the implementation authority for ARC runners,
  Attic, Bazel remote cache, runner lifecycle, and dogfood proof.
- [x] Keep XoxdWM docs as consumer-facing references to remote-build authority,
  not copied GloriousFlywheel operator implementation.
- [ ] Keep Dell reset, PSU, management-display, and `rke2` safety constraints in
  Dell-7810 authority surfaces, with only software-facing implications mirrored
  here.
- [ ] Convert `honey` from one-shot Smoke toward repeatability only through
  stale IPC cleanup, durable packaged client tooling, and repeated operator
  runs.

## Parallel Work Packages

- XoxdWM parity: PR #34 check state, stale GitHub issues, and canary PR #27.
- `rockies` CI: PR #121 blocker classification and PR #125 signal-9 VM smoke
  root cause.
- GloriousFlywheel substrate: dirty-work preservation and ingestion boundary
  for runner/cache docs.
- PM surfaces: Linear and GitHub state changes needed after the technical facts
  are classified.

## Sprint Thesis

The current system has real evidence, but uneven administration:

- XoxdWM has a clean pushed branch and a strong draft PR, but its PR checks,
  public issues, and Linear state lag the actual proof record.
- `rockies` is the right cross-repo composition and Bazel control-plane
  surface, but its active PRs need cleaner blocked/green/failing status.
- GloriousFlywheel is the runner, Nix cache, Bazel remote-cache, and ARC
  substrate. XoxdWM and `rockies` should consume and reference that substrate;
  they should not recreate it or invent repo-shaped runner taxonomy.
- `honey` is valuable as the live XR/runtime proof host, but it must not become
  the default Bazel build farm or a place for unsafe reset experiments.

## Non-Goals

- Do not make Darwin a supported XoxdWM runtime.
- Do not move Bazel authority into XoxdWM.
- Do not duplicate GloriousFlywheel runner or cache implementation in
  XoxdWM or `rockies`.
- Do not convert one-shot `honey` VR proof into a product claim without
  repeated operator evidence.
- Do not stop `rke2` as part of reset experiments. Dell reset work already
  showed container shutdown pressure can remain after service pre-stop, and
  `rke2` is not an acceptable destructive-test lever.

## Desired End State

By the end of the sprint:

- XoxdWM PR #34 is either merge-ready or has a short, explicit blocker list.
- XoxdWM README, status, support matrix, GitHub issues, and Linear issues agree
  on what is `Smoke`, `Design`, and not claimed.
- The `codex/xoxdwm-host-contract` worktree is retired or documented as a
  duplicate/equivalent patch surface.
- `rockies` has a clear status for PR #121 and PR #125, especially the Budgie
  display-persistence VM failure.
- GloriousFlywheel remains the named shared substrate for ARC runners, Attic,
  Bazel remote cache, and source dogfood proof.
- Any remaining public-facing work is framed as bounded, evidence-backed next
  work rather than broad platform aspiration.

## Track A: XoxdWM Truth And PR Hygiene

Current facts:

- Main checkout: `codex/reality-authority-surface`
- Current pushed sprint head: use PR #34 as the live authority; avoid pinning a
  SHA in this checklist because every checklist update changes the head.
- Active PR: <https://github.com/Jesssullivan/XoxdWM/pull/34>
- Local truth lint: `just truth-lint` passes `18/18` after the sprint doc
  guardrail
- Local full test surface: `just test` passes `1936/1936`
- Dhall boot validation passes via `nix shell nixpkgs#dhall --command just
  boot-validate`
- PR #34 instability is currently check-state instability on the fresh pushed
  head, not a known local test failure. Earlier instability was from cancelled
  slow/self-hosted/multi-arch follow-on checks; the latest head still needs
  GitHub checks to settle or be explicitly classified.

Tasks:

1. Fix README drift around installed `monado-beyond`.
2. Rerun or explicitly waive the cancelled PR #34 checks.
3. Decide whether the cancelled self-hosted/Nix/multi-arch lanes are required
   for this PR or only follow-on confidence.
4. Preserve `yoga` and `honey` support language as `Smoke`, not `Proven`.
5. Keep `just truth-lint` green after every doc truth change.
6. Before merge, run the relevant local cheap checks from `neo`:
   `just truth-lint`; `just test` when code or tests changed.

Acceptance:

- PR #34 status is explainable in one paragraph.
- The README no longer contradicts [support-matrix.md](support-matrix.md) or
  [status.md](status.md) about the installed Monado companion lane.
- Public docs do not imply daily-driver VR, repeatable `honey` goggles
  product, or Darwin runtime support.

## Track B: Issue And Linear Parity

Current facts:

- XoxdWM GitHub issues #10, #12, #13, #20, and #22 still describe the older
  MVP decomposition.
- Linear `TIN-346` tracks the active `honey` VR smoke path well.
- Linear `TIN-345` still lags the stronger `yoga` SDDM/package evidence.
- Linear `TIN-341`, `TIN-342`, `TIN-343`, and `TIN-344` remain open even
  though much of their work landed in PR #34.
- Dell-owned `TIN-338`, `TIN-339`, and `TIN-340` correctly belong in
  `Dell-7810`.
- `TIN-398` is the correct cross-repo follow-up for honey RT/kernel posture.

Tasks:

1. Add issue comments or close/re-scope stale XoxdWM GitHub issues after PR
   #34 is stabilized.
2. Update Linear issue state to reflect landed evidence:
   `TIN-345`, `TIN-341`, `TIN-342`, `TIN-343`, and `TIN-344`.
3. Keep `TIN-346` open until `honey` is repeatable, not just one-shot.
4. Keep `TIN-398` as the active cross-repo kernel posture reconciliation lane.
5. Link issue updates to PR #34 comments and named proof docs rather than
   restating host evidence from memory.

Acceptance:

- GitHub issues and Linear status no longer make completed blockers look open.
- Open items describe the next real blocker: repeatability, durable client
  tooling, stale IPC handling, and remote substrate parity.

## Track C: Host-Contract Worktree Decision

Current facts:

- Side worktree: `/private/tmp/xoxdwm-host-contract`
- Unique commit: `7150bcf dhall: extract Dell host facts and timing posture`
- Diff surface: `justfile` plus `packaging/dhall/*`
- The patch content is already effectively present on the active branch:
  `git cherry codex/reality-authority-surface codex/xoxdwm-host-contract`
  marks `7150bcf` with `-`, and path history shows the equivalent active
  commit as `a7029e6 dhall: extract Dell host facts and timing posture`.
- The remaining work is worktree hygiene, not re-ingestion.

Tasks:

1. Validate the active Dhall consumer surface with `just boot-validate`.
2. Retire or annotate `/private/tmp/xoxdwm-host-contract` so future audits do
   not treat `7150bcf` as missing work.
3. Keep the Dell/XoxdWM authority split explicit: Dell-7810 owns host
   evidence; XoxdWM may carry a consumer copy for boot/config generation.

Acceptance:

- No untracked strategic work remains parked in `/private/tmp` without a
  decision.
- Dhall host facts are not duplicated in a way that confuses Dell-7810
  authority versus XoxdWM consumer configuration.

## Track D: `honey` Repeatability, Not New Claims

Current facts:

- `honey` has one-shot direct-mode proof from the installed XoxdWM package
  surface.
- `monado-beyond` is installed on-host and `/usr/bin/monado-service` can reach
  direct-mode proof without `MONADO_SERVICE_BIN`.
- The current client proof still depends on a local `/usr/local/bin/hello_xr`
  tool.
- The proof was captured once and torn down.

Tasks:

1. Make stale `monado_comp_ipc` cleanup explicit in launcher behavior and
   operator docs.
2. Replace local `/usr/local/bin/hello_xr` dependency with a packaged or
   durable Rocky-facing client-tool path.
3. Rerun the installed `honey` path enough times to classify it as repeatable.
4. Record failures as either host/substrate, packaging/deployment, or
   compositor/runtime integration blockers.
5. Keep Dell reset and power findings in Dell-7810, with only sanitized
   software-facing implications mirrored here.

Acceptance:

- XoxdWM can honestly say whether `honey` repeatability is blocked by tooling,
  host hygiene, or compositor/runtime behavior.
- Support docs remain at `Smoke` unless repeated operator evidence justifies a
  stronger label.

## Track E: Rockies And GloriousFlywheel Parity

Current facts:

- `rockies` owns umbrella composition, Bazel orchestration policy, and
  Rocky-facing handoff artifacts.
- GloriousFlywheel owns ARC runners, Attic, Bazel remote cache, runner
  lifecycle, and source dogfood proof.
- The current GloriousFlywheel operating contract prefers shared capability
  lanes such as `tinyland-nix`, `tinyland-nix-heavy`, `tinyland-nix-kvm`,
  `tinyland-nix-gpu`, and `tinyland-dind`.
- Repo-shaped runner taxonomy is debt, not the target model.
- The local GloriousFlywheel `main` checkout has broad dirty work as of the
  follow-up audit. Treat it as preservation/reconciliation work, not a safe
  source for direct ingestion into XoxdWM.
- `rockies` PR #121 has visible green checks but still shows blocked.
- `rockies` PR #125 is blocked by one Budgie display-persistence VM execution
  smoke failure.

Tasks:

1. In `rockies`, document or resolve why PR #121 is blocked despite visible
   green checks.
2. Debug PR #125's Budgie display-persistence VM execution smoke failure.
3. Ensure `rockies` docs keep Bazel as orchestration/control-plane validation,
   not release truth by itself.
4. In XoxdWM, keep remote-build docs pointed at GloriousFlywheel and
   `rockies`, without adding fake local Bazel wrappers.
5. Preserve or branch dirty GloriousFlywheel local work before using it as an
   ingestion source.
6. Record the runner/cache substrate map in one place:
   GloriousFlywheel for implementation, `rockies` for composition planning,
   XoxdWM for runtime proof consumption.

Acceptance:

- `rockies` PR statuses are actionable, not ambiguous.
- XoxdWM docs can explain which remote lane to use without importing the whole
  GloriousFlywheel operator surface.
- No public-facing doc suggests that Attic or Bazel remote cache is publication
  truth by itself.

## Track F: Publication Readiness Pass

Tasks:

1. Review public-facing XoxdWM docs for overclaims.
2. Review `rockies` public orientation docs for internal/private leakage.
3. Keep GloriousFlywheel internal docs out of public XoxdWM prose except as
   high-level authority references.
4. Add final PR comments that summarize evidence, tests, and remaining risks.
5. Prefer narrow commits with explicit validation notes.

Acceptance:

- A reader can tell what works now, what was proven once, and what remains
  future work.
- Public docs route implementation work to the correct repo without exposing
  unnecessary internal runner topology.

## Suggested Execution Order

1. Land XoxdWM doc drift fixes and truth-lint guardrails.
2. Decide the `7150bcf` host-contract worktree.
3. Stabilize or explicitly classify PR #34's cancelled checks.
4. Update XoxdWM GitHub and Linear issue parity.
5. Fix `rockies` PR #125's failing VM smoke or write the blocker precisely.
6. Resolve `rockies` PR #121 blocked status.
7. Convert `honey` repeatability into a narrow follow-on issue/PR instead of
   expanding PR #34 indefinitely.

## Evidence Checklist

Use this checklist before claiming sprint completion:

- `git status -sb` clean in XoxdWM
- `just truth-lint` passes in XoxdWM
- PR #34 has no unexplained failing/cancelled required checks
- XoxdWM README, status, support matrix, and milestone docs agree
- Linear `TIN-*` states reflect actual landed work
- XoxdWM GitHub issues are closed, re-scoped, or commented with the current
  evidence boundary
- `rockies` PR #121 and #125 have explicit next actions
- GloriousFlywheel remains a referenced shared substrate, not copied local
  implementation
