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

- [x] Let XoxdWM PR #34 checks settle on the latest pushed head, then classify
  every non-green result as required, waived, cancelled-noise, or real blocker.
  Final check finding: PR #34 merged at `28e8073` from head `72ba290c`; core
  Rocky, Nix, native-deps, Monado companion, multi-arch, VM integration, and
  Greptile lanes were green. Self-hosted fast jobs are intentionally skipped by
  the reachability gate. Secondary cross-target hosted Nix builds are treated
  as informational with bounded step timeouts instead of allowing job-level
  timeouts to leave the PR in `cancelled`.
- [x] Verify the `tinyland-nix` migration on the next PR #34 head; if those
  jobs still queue, track it as GloriousFlywheel/repo-enrollment infrastructure,
  not XoxdWM product failure. The follow-up owner-boundary issue is
  `tinyland-inc/GloriousFlywheel#413`.
- [x] Add a reachability proof gate so `USE_SELFHOSTED=true` no longer sends
  jobs to `tinyland-nix` unless `GF_SHARED_RUNNERS_REACHABLE=true` is also set.
  Until then, mixed lanes use hosted Linux fallback and self-hosted-only lanes
  skip.
- [x] Remove direct `tinyland-inc/GloriousFlywheel/.github/actions/setup-flywheel`
  dependencies from hosted-capable XoxdWM jobs. GitHub resolves `uses:` actions
  before step gating is enough to protect hosted fallback jobs, so XoxdWM now
  carries a minimal local `ensure-nix` consumer shim while GloriousFlywheel
  remains substrate authority.
- [ ] Reconcile XoxdWM GitHub issues `#11`, `#20`, and `#22` against merged
  PR #34/#35 evidence and the follow-on OpenXR wrapper work. Older MVP issues
  `#10`, `#12`, and `#13` have already been closed or retired; `#11`, `#20`,
  and `#22` remain open because they track live runner reachability, repeatable
  honey VR proof, and yoga local-session promotion.
- [x] Decide and retire or close stale XoxdWM Greptile canary PR `#27`.
- [x] Update Linear `TIN-341`, `TIN-342`, `TIN-343`, `TIN-344`, and `TIN-345`
  so completed evidence no longer looks open.
- [x] Keep `TIN-346` open until `honey` VR proof is repeatable, not just
  one-shot. As of `2026-04-25` EDT, three packaged-client smoke passes work in
  one active service session, and three clean stop/start cycles also pass;
  fresh-boot repeatability remains open.
- [x] Split the packaged OpenXR smoke-client blocker into `TIN-595`; PR #37
  added the package lane, and the follow-up install now replaces devshell or
  `/usr/local/bin/hello_xr` status evidence with an installed
  Rocky-compatible artifact.
- [x] Keep `TIN-398` as the cross-repo honey RT/kernel posture lane.
- [x] Treat `/private/tmp/xoxdwm-host-contract` as duplicate/equivalent work
  unless a later audit finds missing content.
- [x] For `rockies` PR #121, identify whether the blocker is only draft/review
  state or a hidden merge requirement. It is currently draft plus review
  required; visible checks are green.
- [x] For `rockies` PR #125, classify the KVM VM smoke state without moving the
  blocker into XoxdWM. Current refresh shows the Budgie display-persistence VM
  execution smoke is green on head `6b19abe`; PR #125 remains draft/blocked,
  not blocked by the earlier signal-9 VM evidence.
- [x] Preserve or branch dirty local GloriousFlywheel work before using it as an
  ingestion source. Preserved on `tinyland-inc/GloriousFlywheel#408`.
- [x] Keep GloriousFlywheel as the implementation authority for ARC runners,
  Attic, Bazel remote cache, runner lifecycle, and dogfood proof.
- [x] Keep XoxdWM docs as consumer-facing references to remote-build authority,
  not copied GloriousFlywheel operator implementation.
- [x] Treat the local `linux-xr` checkout as a no-ingest surface until its
  dirty worktree and 22-commit upstream lag are intentionally reconciled. The
  safe ingestion lane is now `/Volumes/linux-xr-cs/linux-xr`, a case-sensitive
  clone aligned to `xr/main`; linux-xr PR #23 merged the carry hygiene patch at
  `35ccbe2`, PR #26 merged the Dell host-authority wording at `f991999`, and
  upstreamable follow-ups remain in `tinyland-inc/linux-xr#24` and `#25`.
- [ ] Keep Dell reset, PSU, management-display, and `rke2` safety constraints in
  Dell-7810 authority surfaces, with only software-facing implications mirrored
  here.
- [ ] Convert `honey` from clean service-cycle Smoke toward fresh-boot
  repeatability only through stale IPC cleanup, durable packaged client
  tooling, and repeated operator runs.

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
- Do not convert clean service-cycle `honey` VR proof into a product claim
  without fresh-boot repeated operator evidence.
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
- PR #34 instability is currently check-state semantics on the fresh pushed
  head, not a known local or Rocky runtime failure. Hosted Rocky/package lanes
  are green, docs-only CI lanes are skipped by path filters, self-hosted-only
  jobs are intentionally skipped unless GloriousFlywheel reachability is proven,
  and aarch64/s390x cross-builds are explicitly informational while the
  hosted/shared cache path is too slow for those secondary targets.
- The earlier concrete reachability root cause was owner/scope mismatch:
  `Jesssullivan` is a GitHub user account, `Jesssullivan/XoxdWM` exposed zero
  accessible repo-level self-hosted runners, and GloriousFlywheel's current
  personal compatibility lane was anchored to `Jesssullivan/jesssullivan.github.io`,
  not XoxdWM. PR #34 now avoids queuing those jobs by default.
- A second failure mode was exposed after hosted fallback replaced queued
  shared-runner jobs: nested or direct references to the GloriousFlywheel
  `setup-flywheel` action are not resolvable from this personal repo before job
  step gating can protect them. XoxdWM's local `.github/actions/ensure-nix`
  action must stay self-contained for hosted fallback.

Tasks:

1. Keep README, status, and support-matrix language aligned around installed
   `monado-beyond`.
2. Keep any future PR #34 `tinyland-nix` queue explicitly classified as
   shared-lane reachability debt through GloriousFlywheel.
3. Require `GF_SHARED_RUNNERS_REACHABLE=true` before any future PR #34 head
   selects `tinyland-nix`; otherwise use hosted Linux fallback or skip
   self-hosted-only jobs.
4. Keep hosted-capable jobs free of private/external GloriousFlywheel action
   dependencies; consume the runner/cache contract through local workflow
   shims and repo variables until shared-runner reachability is proven.
5. Preserve `yoga` and `honey` support language as `Smoke`, not `Proven`.
6. Keep `just truth-lint` green after every doc truth change.
7. Before merge, run the relevant local cheap checks from `neo`:
   `just truth-lint`; `just test` when code or tests changed.

Acceptance:

- PR #34 status is explainable in one paragraph.
- The README no longer contradicts [support-matrix.md](support-matrix.md) or
  [status.md](status.md) about the installed Monado companion lane.
- Public docs do not imply daily-driver VR, fresh-boot repeatable `honey`
  goggles product, in-goggles first-frame proof, or Darwin runtime support.

## Track B: Issue And Linear Parity

Current facts:

- XoxdWM GitHub issues #10, #12, #13, #20, and #22 still describe the older
  MVP decomposition.
- Linear `TIN-346` tracks the active `honey` VR smoke path well and remains
  open because the proof is still limited to clean service cycles.
- Linear `TIN-345` is done for the stronger `yoga` SDDM/package evidence.
- Linear `TIN-341`, `TIN-342`, `TIN-343`, and `TIN-344` are in review rather
  than still appearing as untriaged blockers.
- Dell-owned `TIN-338`, `TIN-339`, and `TIN-340` correctly belong in
  `Dell-7810`.
- `TIN-398` is the correct cross-repo follow-up for honey RT/kernel posture.

Tasks:

1. Add issue comments or close/re-scope stale XoxdWM GitHub issues after PR
   #34 is stabilized.
2. Keep Linear issue state aligned with landed evidence:
   `TIN-345`, `TIN-341`, `TIN-342`, `TIN-343`, and `TIN-344`.
3. Keep `TIN-346` open until `honey` is repeatable across fresh boot cycles,
   not just clean user-service cycles.
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

- `honey` has direct-mode proof from the installed XoxdWM package surface.
- `monado-beyond` is installed on-host and `/usr/bin/monado-service` can reach
  direct-mode proof without `MONADO_SERVICE_BIN`.
- The repo now has `packaging/scripts/exwm-vr-openxr-smoke` plus
  `just honey-openxr-status` / `just honey-openxr-smoke`.
- The `exwm-vr-openxr-smoke-client` RPM from run `24938791255` is installed
  on-host, and status preflight resolves `/usr/libexec/exwm-vr/hello_xr`.
- Three bounded packaged-client smoke passes succeeded in one active
  user-service session on `2026-04-25` EDT.
- Three clean stop/start cycles also passed on `2026-04-25` EDT; the
  repo-owned command is `just honey-openxr-clean-cycle`.

Tasks:

1. Preserve stale `monado_comp_ipc` cleanup in launcher behavior and keep the
   OpenXR status wrapper as the safe preflight.
2. Keep the packaged `exwm-vr-openxr-smoke-client` path as the client-tool
   authority for future smoke runs; avoid returning to ad hoc
   `/usr/local/bin/hello_xr` evidence.
3. Rerun the installed `honey` path across fresh-boot cycles before
   classifying it as a stable operator lane.
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
- GloriousFlywheel PR #408 has merged at head `eceec84` after the public-alpha
  dogfood contract checks passed and the cross-org canary stayed intentionally
  skipped.
- `tinyland-inc/GloriousFlywheel#413` is now the live XoxdWM `tinyland-nix`
  reachability/enrollment proof blocker.
- The original local `linux-xr` checkout on the Darwin filesystem remains a
  no-ingest surface because Linux case-colliding paths can show false dirty
  kernel-tree edits. Use the case-sensitive `/Volumes/linux-xr-cs/linux-xr`
  clone for linux-xr work. That clone is clean on `xr/main` after PR #23
  (`35ccbe2`), PR #26 (`f991999`), and PR #28 (`323597c`), with upstreamable
  carry follow-ups tracked in linux-xr #24 and #25.
- `rockies` PR #121 has visible green checks but still shows blocked because it
  requires review.
- `rockies` PR #125 has visible green checks, including the Budgie
  display-persistence VM execution smoke, but remains draft and review-required.

Tasks:

1. In `rockies`, request/review PR #121 or leave it explicitly blocked on
   review state.
2. In `rockies`, move PR #125 out of draft only after the project owner is ready
   to promote the now-green Budgie display-persistence evidence.
3. Ensure `rockies` docs keep Bazel as orchestration/control-plane validation,
   not release truth by itself.
4. In XoxdWM, keep remote-build docs pointed at GloriousFlywheel and
   `rockies`, without adding fake local Bazel wrappers.
5. Keep GloriousFlywheel issue #413 as the active substrate tracking surface
   now that PR #408 has merged; do not copy runner/cache implementation here.
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
