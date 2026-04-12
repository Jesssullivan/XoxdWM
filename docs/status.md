# XoxdWM Status

Snapshot date: 2026-04-11

## Honest Assessment

- The repo has a real public release and substantial packaging work.
- The repo did not have a root README describing current support state.
- `honey` is not currently running a deployed XoxdWM stack.
- `yoga` is not currently running a deployed XoxdWM stack.
- Recent push CI failures were concentrated in three places:
  - Rocky test: optional `sccache` setup step
  - Nix cache workflow: attempting to install Nix on a pre-provisioned self-hosted runner
  - self-hosted fast CI: heavy checks on all pushes, including non-code work
- Current self-hosted runner scope does not match the current public repo:
  - historical self-hosted evidence on `honey` points at `tinyland-inc/XoxdWM`
  - live runners we found are registered to `tinyland-inc`, `Jesssullivan/cmux`, `Jesssullivan/outbot-ci`, and `Jesssullivan/chapel`
  - we did not find a runner registration that can accept jobs from `Jesssullivan/XoxdWM`

## What This Repo Is Good For Right Now

- tracking compositor and packaging work in public
- publishing experimental package artifacts
- carrying the userspace side of Rocky 10 / Wayland / VR integration work
- serving as the research and implementation repo for the `honey` and `yoga` MVP paths

## What This Repo Is Not Claiming Right Now

- daily-driver VR on `honey`
- a complete turnkey Rocky 10 VR deployment
- proven eye tracking, hand tracking, or BCI support on current named lab hosts

## Immediate Priorities

1. Restore a green critical CI path for code changes.
2. Keep the public docs honest and scoped to `Proven`, `Smoke`, and `Design`.
3. Produce one reproducible Rocky 10 desktop/dev install on `yoga`.
4. Produce one reproducible `honey` VR smoke path.
