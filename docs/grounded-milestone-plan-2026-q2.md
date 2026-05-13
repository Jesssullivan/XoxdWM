# XoxdWM Grounded Milestone Plan - Q2 2026

This document turns the current repo truth into a milestone and iteration plan.

It exists to prevent one specific planning error: treating a generic 2D or
"preview" surface as if it were already proof of the goggles product.

Read this together with [reality-check-2026-04-22.md](reality-check-2026-04-22.md),
[status.md](status.md), [support-matrix.md](support-matrix.md), and
[roadmap-2026-q2.md](roadmap-2026-q2.md).

## Sanity Check

The current repo and host evidence support three different claims, not one:

- `yoga` is the current named-host proof surface for a 2D Rocky compositor and
  session lane.
- `honey` is the current named-host proof surface for XR substrate work:
  kernel, GPU, connector, non-desktop, runtime, and lease-path reality.
- `honey` is not yet a proven XoxdWM goggles product surface.

Two practical rules follow from that:

- do not treat `yoga` as proof of the goggles product
- do not treat `honey` host recovery evidence as proof of a working XoxdWM VR session

One more boundary matters:

- the `preview` and `headless` display modes in
  [compositor/src/vr/drm_lease.rs](../compositor/src/vr/drm_lease.rs) and
  [lisp/vr/ewwm-vr-display.el](../lisp/vr/ewwm-vr-display.el) are useful
  implementation surfaces, but they are not currently named-host MVP acceptance
  targets

## Milestone Stack

### Milestone 1: `yoga` 2D Session Closure

Goal: finish the Rocky 10 local session lane that the repo already partially
proves.

This milestone is about turning the current bounded package smoke into a
repeatable local operator path. It is not about VR.

Acceptance:

- the public Rocky package or documented source path launches locally on `yoga`
- the display-manager or local-launch wrapper is documented and repeatable
- seat backend propagation into user services is stable
- rollback and operator notes are documented
- support docs can honestly upgrade `yoga` from bounded startup smoke toward a
  real session lane

Primary code and packaging surfaces:

- [packaging/desktop/exwm-vr-session](../packaging/desktop/exwm-vr-session)
- [packaging/systemd/exwm-vr-compositor.service](../packaging/systemd/exwm-vr-compositor.service)
- [packaging/systemd/exwm-vr.target](../packaging/systemd/exwm-vr.target)
- [packaging/rpm/exwm-vr.spec](../packaging/rpm/exwm-vr.spec)
- [docs/installation-quickstart.md](installation-quickstart.md)

Evidence required:

- named-host run notes from `yoga`
- updated operator docs
- no regression in repo truth checks or Rocky container smoke

Current state on `2026-04-22`:

- `yoga` now has both a staged named-host proof and a real installed-package
  proof for the branch-scoped `0.5.4-1.el10` session payload; see
  [yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md)
- that closes the ambient-bootstrap question and the "is the real package lane
  installable on the host" question
- the stop-path issue is understood, carried in the refreshed packaged unit,
  and no longer requires a host-only drop-in on `yoga`
- `yoga` now has a one-time SDDM greeter-path proof via `sddm-autologin` on
  `seat0`, which closes the basic local-session viability question
- the remaining gap is repeated operator evidence, not package or greeter-path
  viability; [yoga-local-session-runbook.md](yoga-local-session-runbook.md)
  defines the manual/fresh-login packet and rollback surface for that evidence

### Milestone 2: `honey` XR Substrate Proof

Goal: prove the real display/runtime substrate that the goggles product depends
on before claiming a XoxdWM VR MVP.

This milestone is intentionally product-agnostic. Its job is to answer whether
the host can sustain the bridge architecture at all.

Acceptance:

- the chosen kernel lane is explicit and repeatable on `honey`
- warm reboot versus hard-reset behavior is recorded in
  [`Dell-7810/docs/research/honey-reset-matrix-2026-04-22.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/research/honey-reset-matrix-2026-04-22.md)
- Dell management display and HMD display behavior are documented in the
  reset matrix and in
  [`Dell-7810/docs/research/honey-management-display-and-recovery-path-2026-04-22.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/research/honey-management-display-and-recovery-path-2026-04-22.md)
- `non_desktop`, EDID, and DRM lease preconditions are observed and recorded
- active OpenXR runtime selection is explicit
- OpenXR client tools exist on-host and are documented
- the actual bridge compositor is explicit:
  - either Sway/wlroots remains the current trusted bridge
  - or XoxdWM itself is promoted into that role by host evidence

Primary code and packaging surfaces:

- [packaging/scripts/exwm-vr-setup](../packaging/scripts/exwm-vr-setup)
- [packaging/systemd/exwm-vr-beyond-power.service](../packaging/systemd/exwm-vr-beyond-power.service)
- [packaging/sway/config](../packaging/sway/config)
- [packaging/rpm/monado-beyond.spec](../packaging/rpm/monado-beyond.spec)
- [packaging/rpm/sway-beyond.spec](../packaging/rpm/sway-beyond.spec)
- [packaging/rpm/wlroots-beyond.spec](../packaging/rpm/wlroots-beyond.spec)
- [nix/kernel/xr-kernel.nix](../nix/kernel/xr-kernel.nix)
- [nix/packages/wlroots-beyond.nix](../nix/packages/wlroots-beyond.nix)

Evidence required:

- named-host logs from `honey`
- explicit connector and runtime observations
- one documented client-tool check such as `openxr-info`, `hello_xr`, or equivalent
- platform blockers tracked separately from compositor claims

Current state on `2026-04-22`:

- `honey` now has a bounded named-host XoxdWM compositor startup with
  `exwm-vr.target`, `exwm-vr-compositor.service`, and `exwm-vr-emacs.service`
  all active
- the host evidence for this milestone is recorded in
  [honey-substrate-proof-2026-04-22.md](honey-substrate-proof-2026-04-22.md)
- `honey` now has an explicit active OpenXR runtime file on-host
- `honey` now has direct-mode proof from the installed package
  surface where `/usr/bin/ewwm-compositor` initializes `wp_drm_lease_v1`,
  reserves `DP-2` via explicit host-side override, and grants a real DRM lease
  to Monado
- `hello_xr -g Vulkan` now reaches Monado runtime selection, Bigscreen Beyond
  selection, session `READY`, and eye swapchain creation on `honey`
- the remaining gap is no longer missing lease support; it is converting this
  installed proof into a repeated operator lane with explicit stale-socket
  handling; the packaged `exwm-vr-openxr-smoke-client` path is now installed
  on `honey`, and three bounded smoke passes plus three clean stop/start cycles
  succeeded on `2026-04-25` EDT

### Milestone 3: `honey` First XoxdWM Smoke On True Substrate

Goal: run XoxdWM once on the real `honey` substrate and record the result with
enough evidence to classify it honestly.

This is the first milestone that should answer a product-shaped question.
Before this point, the work is still mostly platform and bridge validation.

Acceptance:

- a deployed XoxdWM binary, package, or source checkout is used on `honey`
- compositor startup is attempted on the proven substrate, not on an unverified host state
- the run records connector state, runtime state, and compositor logs together
- if the launch fails, the failure is categorized:
  - host/substrate blocker
  - packaging/deployment blocker
  - compositor/lease/runtime integration blocker
- support docs update only after evidence is captured

Current state on `2026-04-22`:

- this milestone now has named-host smoke proof from the installed package
  surface
- the installed `exwm-vr-compositor-0.5.4-1.el10` package on `honey` was
  reinstalled from packaging run `24776900393`, and the current proof no
  longer depends on a staged binary
- Monado received a real `DP-2` DRM lease and `hello_xr -g Vulkan` reached
  `READY` plus eye swapchain creation
- a later staged `monado-beyond` companion RPM proof from run `24804821792`
  also reached active direct-mode Monado plus eye swapchain creation on
  `honey`
- the remaining work is to make that same path repeatable from the installed
  package surface across fresh boot cycles with explicit stale-socket handling
  and the installed `exwm-vr-openxr-smoke-client` path instead of ad hoc
  host-local binaries

Primary code surfaces:

- [compositor/src/backend/drm.rs](../compositor/src/backend/drm.rs)
- [compositor/src/vr/openxr_state.rs](../compositor/src/vr/openxr_state.rs)
- [compositor/src/vr/drm_lease.rs](../compositor/src/vr/drm_lease.rs)
- [lisp/vr/ewwm-vr-display.el](../lisp/vr/ewwm-vr-display.el)
- [.github/workflows/vr-hardware.yml](../.github/workflows/vr-hardware.yml)

Evidence required:

- one real `honey` launch attempt with logs
- recorded outcome in repo docs
- no upgrade from `Design` to `Smoke` without named-host evidence

## Iteration Model

### Iteration 0: Planning And Truth

Purpose:

- reconcile milestone language with actual host and code reality
- keep `yoga` and `honey` from being used as proxies for each other
- make bridge architecture assumptions explicit

Exit criteria:

- this document exists
- roadmap and audit docs point to it
- Linear tracking reflects the split between `yoga`, `honey` substrate, and
  `honey` XoxdWM smoke

### Iteration 1: `yoga` Session Closure

Purpose:

- finish the local session/operator path

Scope:

- session wrapper
- user units
- package install behavior
- operator docs

Do not expand scope to:

- Monado packaging on `yoga`
- goggles-specific UX
- BCI, eye tracking, or hand tracking

### Iteration 2: `honey` Substrate Proof

Purpose:

- validate the real GPU/runtime/display bridge independent of XoxdWM marketing

Scope:

- kernel and reboot behavior
- power and connector reality
- Monado/OpenXR userspace
- bridge compositor choice and lease path

Do not expand scope to:

- XoxdWM interaction polish
- eye tracking, hand tracking, or BCI
- public support claims beyond substrate truth

### Iteration 3: `honey` XoxdWM Smoke

Purpose:

- attempt the first honest XoxdWM-on-honey launch on the proven substrate

Scope:

- deployment path
- compositor launch
- lease/runtime integration evidence
- failure categorization

Do not expand scope to:

- daily-driver ergonomics
- product-grade interaction and BCI features
- broad HMD compatibility claims

## Promotion Rules

- Remote CI, Rocky container CI, and local control-plane tests are not named-host proof.
- `preview` mode is not goggles proof.
- `Smoke` on `honey` requires host evidence, not only code or packaging presence.
- `Proven` requires repeatable named-host validation.
- If a blocker lives in power sequencing, kernel behavior, or display detection,
  track it as platform truth, not compositor success or failure.

## Stop Conditions

Pause the XoxdWM goggles milestone and return to substrate or platform work if:

- `honey` warm reboot versus hard-reset behavior is still materially unstable
- connector state is not trustworthy across boots
- OpenXR runtime selection or client-tool path is still missing
- the trusted bridge compositor is still ambiguous on the real host

Pause goggles-adjacent product planning and return to `yoga` session work if:

- the only current named-host success is still ssh-bounded compositor startup
- the local Rocky operator path is not yet repeatable enough to support routine work

## Current Mapping

- `TIN-345`: best fit for Milestone 1, `yoga` 2D session closure
- `TIN-346`: should no longer be treated as one monolithic "VR MVP"
  - use it for Milestone 3 only if substrate acceptance is already satisfied
  - otherwise split or track Milestone 2 substrate work separately

That is the main sanity check this plan enforces: substrate truth first,
product smoke second, broader VR and BCI claims later.
