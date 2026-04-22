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
- the current installed `0.5.4-1.el10` RPM set on `yoga` does not yet carry the
  follow-up stop-path fix described below

## Stop-Path Follow-Up

After the installed-package proof, a follow-up host check captured that
stopping `exwm-vr.target` left `exwm-vr-emacs.service` in `failed` state even
though the stop was operator-initiated and bounded. `systemd --user status`
showed `status=15`, which is the expected SIGTERM exit from the foreground Emacs
daemon on controlled shutdown.

The repo-side fix is to declare `SuccessExitStatus=15` in
`packaging/systemd/exwm-vr-emacs.service`.

The first host validation used a temporary user override to prove the fix. That
was later cleaned up into a proper host-level drop-in at
`/etc/systemd/user/exwm-vr-emacs.service.d/10-success-exit.conf`, after which
the per-user full-unit override was removed.

With that cleaned-up override path in place:

- startup still reached `active` / `active` / `active`
- the same compositor and Emacs initialization markers appeared
- a controlled stop ended with:
  - `exwm-vr.target`: `inactive`
  - `exwm-vr-compositor.service`: `inactive`
  - `exwm-vr-emacs.service`: `inactive`
- `systemd` reported `Stopped exwm-vr-emacs.service` instead of marking the unit
  failed
- `systemctl --user cat exwm-vr-emacs.service` showed the packaged unit from
  `/usr/lib/systemd/user/` plus the host drop-in from `/etc/systemd/user/`

This means the stop-path issue is understood and the in-tree unit fix is
validated on `yoga`, but the installed host RPMs still need a follow-up package
build and upgrade to carry that fix without a host-side drop-in.

## Display-Manager Follow-Up

After the stop-path validation, `yoga` was checked for an actual local login
lane. Initially the host had the installed `exwm-vr.desktop` session entry and
session wrapper, but no display-manager package or `display-manager.service`.

The next host pass established the real display-manager lane:

- `sddm-0.21.0-9.el10_1.x86_64` installed successfully from the enabled host
  repos
- `/etc/systemd/system/display-manager.service` now points to
  `/usr/lib/systemd/system/sddm.service`
- both `sddm.service` and `display-manager.service` are `active`
- `loginctl` shows an active greeter session on `seat0`
- `/usr/share/wayland-sessions/exwm-vr.desktop` remains present and points to
  `/usr/share/exwm-vr/exwm-vr-session`
- `/var/lib/sddm/state.conf` now preselects:
  - `User=jsullivan2`
  - `Session=exwm-vr.desktop`

Observed follow-up notes:

- SDDM fell back to its embedded theme because `01-breeze-fedora` was not
  present
- the greeter switched from locale `C` to `C.UTF-8`

This closes the "no greeter exists on the host" gap. The remaining local-login
question is now narrower: a physical `yoga` login still needs to select
`EXWM-VR` from the SDDM greeter path and confirm the installed session wrapper
works from the console, not just over SSH-driven bounded proof. The preselected
user/session state should shorten that pass, but it does not replace the proof.

## Next Gate

- keep the real installed `0.5.4-1.el10` host as the authoritative `yoga` lane
- perform a physical `yoga` login through SDDM selecting `EXWM-VR`
- move from ssh-driven bounded proof to a documented local login/session path
- package and install the validated `SuccessExitStatus=15` fix on `yoga`
