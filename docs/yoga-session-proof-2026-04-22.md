# `yoga` Session Proof - 2026-04-22

This note records the exact named-host evidence behind the current `yoga`
session claim.

Read this together with [status.md](status.md),
[support-matrix.md](support-matrix.md), and
[grounded-milestone-plan-2026-q2.md](grounded-milestone-plan-2026-q2.md).

## Inputs

- Host: `yoga`
- Date: `2026-04-22`
- Branch under test: `codex/reality-authority-surface`
- Artifact source: GitHub Actions packaging run `24768509226`
- Branch-scoped RPM payload:
  - `exwm-vr-0.5.4-1.el10.x86_64.rpm`
  - `exwm-vr-compositor-0.5.4-1.el10.x86_64.rpm`
  - `exwm-vr-elisp-0.5.4-1.el10.noarch.rpm`
  - `exwm-vr-headless-0.5.4-1.el10.x86_64.rpm`
- Installed host RPM state before and after this note:
  - `exwm-vr-0.5.3-1.el10.x86_64`
  - `exwm-vr-compositor-0.5.3-1.el10.x86_64`
  - `exwm-vr-elisp-0.5.3-1.el10.noarch`

## Constraint

`jsullivan2@yoga` did not have a working non-interactive privilege path during
this pass. `sudo -n true` failed, so a real unattended RPM upgrade was not
possible from `neo`.

Because of that, this note is a staged named-host proof, not a claim that the
system-installed `exwm-vr-*` units were upgraded in place.

## Method

- copied the branch-scoped `0.5.4-1.el10` RPMs to
  `~/xoxdwm-stage-0.5.4/rpms/` on `yoga`
- extracted them under `~/xoxdwm-stage-0.5.4/root`
- launched temporary `systemd --user` stage units against the extracted payload
  instead of the installed system units
- used:
  - compositor: `ewwm-compositor --backend drm --wayland-socket wayland-0`
  - Emacs: `emacs --fg-daemon=exwm-vr-stage --quick --load .../exwm-vr-session-init.el`
- removed the temporary stage units after the run and kept the extracted payload
  on-host for reuse

## Observed Evidence

The staged named-host run on `yoga` produced the expected session markers:

- `ewwm-compositor v0.5.4 starting`
- `backend: drm`
- `IPC server listening socket_path="/run/user/1000/ewwm-ipc.sock"`
- `ewwm-ipc: connected to /run/user/1000/ewwm-ipc.sock`
- `ewwm: initialized`

The old ambient user-config contamination did not reappear:

- no `gregs-repo` path showed up in the Emacs logs
- no ambient `init.el` behavior replaced the dedicated packaged bootstrap

## What This Proves

- the branch-scoped `0.5.4-1.el10` Rocky session payload works on named host
  `yoga`
- the dedicated packaged Emacs bootstrap works as intended
- the packaged session can launch with explicit `drm` backend and canonical
  `wayland-0` / `ewwm-ipc.sock` paths on the host

## What This Does Not Yet Prove

- it was a staged user-unit run, not a privileged RPM upgrade
- the actual installed `exwm-vr-*` RPMs on `yoga` remained at `0.5.3-1.el10`
- the real installed `exwm-vr-compositor.service`, `exwm-vr-emacs.service`, and
  `exwm-vr.target` still need a rerun after a privileged upgrade
- a display-manager-driven local login flow is still not claimed as complete

## Next Gate

- gain a legitimate privilege path on `yoga`
- install the `0.5.4-1.el10` RPMs from run `24768509226`
- rerun the proof against the real installed `exwm-vr-*` units
- only then upgrade the `yoga` support claim beyond staged host proof
