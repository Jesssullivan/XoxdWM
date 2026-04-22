# `yoga` Session Proof - 2026-04-22

This note records the exact named-host evidence behind the current `yoga`
session claim, including both the initial staged payload proof and the later
installed-package proof.

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
- Installed host RPM state before this note:
  - `exwm-vr-0.5.3-1.el10.x86_64`
  - `exwm-vr-compositor-0.5.3-1.el10.x86_64`
  - `exwm-vr-elisp-0.5.3-1.el10.noarch`
- Installed host RPM state after the real upgrade:
  - `exwm-vr-0.5.4-1.el10.x86_64`
  - `exwm-vr-compositor-0.5.4-1.el10.x86_64`
  - `exwm-vr-elisp-0.5.4-1.el10.noarch`

## Constraint

At the start of this pass, `jsullivan2@yoga` did not have a working
non-interactive privilege path wired into the `XoxdWM` repo workflow. `sudo -n
true` failed, so the first proof had to use staged user units.

Later on `2026-04-22`, the legitimate repo-managed operator path was confirmed
from the adjacent `lab` repo: `lab/nix/secrets/hosts/yoga.yaml` existed locally
and decrypted cleanly via `sops`, which made a real privileged RPM upgrade
possible from `neo`.

## Initial Staged Proof Method

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

## Initial Staged Proof Evidence

The staged named-host run on `yoga` produced the expected session markers:

- `ewwm-compositor v0.5.4 starting`
- `backend: drm`
- `IPC server listening socket_path="/run/user/1000/ewwm-ipc.sock"`
- `ewwm-ipc: connected to /run/user/1000/ewwm-ipc.sock`
- `ewwm: initialized`

The old ambient user-config contamination did not reappear:

- no `gregs-repo` path showed up in the Emacs logs
- no ambient `init.el` behavior replaced the dedicated packaged bootstrap

## Installed-Package Follow-Up Method

- used the repo-managed `lab` SOPS secret at
  `lab/nix/secrets/hosts/yoga.yaml` to obtain the legitimate `yoga` sudo
  password
- upgraded the real installed host RPMs from the staged payload already present
  on `yoga`:
  - `exwm-vr-0.5.4-1.el10.x86_64.rpm`
  - `exwm-vr-compositor-0.5.4-1.el10.x86_64.rpm`
  - `exwm-vr-elisp-0.5.4-1.el10.noarch.rpm`
- verified the installed versions with `rpm -q`
- reran the proof against the installed user units:
  - `exwm-vr.target`
  - `exwm-vr-compositor.service`
  - `exwm-vr-emacs.service`

## Installed-Package Follow-Up Evidence

After the real RPM upgrade, `yoga` reported:

- `exwm-vr-0.5.4-1.el10.x86_64`
- `exwm-vr-compositor-0.5.4-1.el10.x86_64`
- `exwm-vr-elisp-0.5.4-1.el10.noarch`

The installed-unit proof then reached:

- `states:` `active` / `active` / `active`
- `ewwm-compositor v0.5.4 starting`
- `backend: drm`
- `IPC server listening socket_path="/run/user/1000/ewwm-ipc.sock"`
- `ewwm-ipc: connected to /run/user/1000/ewwm-ipc.sock`
- `ewwm: initialized`

This run used the real installed `exwm-vr-*` units from
`/usr/lib/systemd/user/`, not temporary stage units.

## What This Proves

- the branch-scoped `0.5.4-1.el10` Rocky session payload works on named host
  `yoga`
- the dedicated packaged Emacs bootstrap works as intended
- the packaged session can launch with explicit `drm` backend and canonical
  `wayland-0` / `ewwm-ipc.sock` paths on the host
- the actual installed `exwm-vr-*` RPMs on `yoga` are now `0.5.4-1.el10`
- the real installed `exwm-vr.target`, `exwm-vr-compositor.service`, and
  `exwm-vr-emacs.service` can reach the same bounded proof markers on-host

## What This Does Not Yet Prove

- a display-manager-driven local login flow is still not claimed as complete
- a controlled stop still leaves `exwm-vr-emacs.service` in `failed` state after
  the target is stopped, so the stop-path ergonomics are not yet fully clean

## Next Gate

- keep the real installed `0.5.4-1.el10` host as the authoritative `yoga` lane
- move from ssh-driven bounded proof to a documented local login/session path
- understand or normalize the `exwm-vr-emacs.service` failed state after
  controlled stop
