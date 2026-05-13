# `yoga` Local Session Runbook

This runbook is the operator path for promoting `yoga` from bounded compositor
smoke toward a repeatable Rocky 10 local session.

It does not add new host evidence by itself. The current proof boundary remains
the one-time SDDM `sddm-autologin` session recorded in
[yoga-session-proof-2026-04-22.md](yoga-session-proof-2026-04-22.md). Use this
runbook to capture the next manual/fresh-login packet without drifting into VR
or Honey-specific claims.

## Supported Path

Use the Rocky 10 display-manager path:

1. Boot `yoga` into the normal Rocky desktop/default kernel lane.
2. Keep `seatd` active.
3. Use SDDM as `display-manager.service`.
4. Select the packaged XoxdWM session from the greeter.

The current installed `yoga` proof used the compatibility session entry
`/usr/share/wayland-sessions/exwm-vr.desktop`, which launches
`/usr/share/exwm-vr/exwm-vr-session`. Newer package surfaces may also expose
`/usr/share/wayland-sessions/xoxdwm.desktop`; prefer that primary name when it
is installed, and keep `exwm-vr.desktop` as the compatibility lane.

Autologin is not the acceptance path for #22. It was only the earlier proof
harness. The next evidence packet should come from a real fresh login or manual
session selection at the greeter.

## Required Host State

Packages expected on `yoga`:

```bash
rpm -q \
  exwm-vr \
  exwm-vr-compositor \
  exwm-vr-elisp \
  emacs \
  dbus-daemon \
  seatd \
  sddm \
  xorg-x11-server-Xwayland
```

Services expected before login:

```bash
systemctl is-active seatd
systemctl is-active sddm
systemctl is-active display-manager
systemctl status display-manager --no-pager
```

Session entries expected:

```bash
ls -l /usr/share/wayland-sessions/xoxdwm.desktop 2>/dev/null || true
ls -l /usr/share/wayland-sessions/exwm-vr.desktop
grep -nE '^(User|Session)=' /var/lib/sddm/state.conf 2>/dev/null || true
```

User/session prerequisites:

- the target local user is a member of `seat`
- the target user has logged out and back in after group changes
- no temporary SDDM autologin drop-in remains enabled
- no host-only user-unit override is required for the packaged Emacs stop path

## Fresh Login Procedure

1. From the local console, log out to SDDM.
2. Select `XoxdWM` if `xoxdwm.desktop` is present; otherwise select the
   compatibility `EXWM-VR` session.
3. Log in as the normal `yoga` desktop user.
4. Do not enable or rely on SDDM autologin for this proof.
5. After the session starts, capture the verification packet below from a local
   terminal or from SSH without disturbing the local seat.

## Verification Packet

Record the host identity:

```bash
hostname
date -Is
uname -r
rpm -q exwm-vr exwm-vr-compositor exwm-vr-elisp
```

Find the local Wayland session:

```bash
loginctl list-sessions
loginctl show-session SESSION_ID \
  -p Name -p Class -p Type -p Service -p Desktop -p Seat -p VTNr -p State -p Remote
```

Expected shape for the manual/fresh-login packet:

- `Class=user`
- `Type=wayland`
- `Seat=seat0`
- `State=active`
- `Remote=no`
- `Service=sddm` or the non-autologin SDDM service name
- `Desktop` identifies the packaged XoxdWM/EXWM-VR session

Check user units from inside the logged-in desktop session when possible:

```bash
systemctl --user is-active exwm-vr.target
systemctl --user is-active exwm-vr-compositor.service
systemctl --user is-active exwm-vr-emacs.service
systemctl --user status exwm-vr.target --no-pager
```

If the primary aliases are installed, also record:

```bash
systemctl --user is-active xoxdwm.target 2>/dev/null || true
systemctl --user is-active xoxdwm-compositor.service 2>/dev/null || true
systemctl --user is-active xoxdwm-emacs.service 2>/dev/null || true
```

Check runtime markers:

```bash
pgrep -a ewwm-compositor
pgrep -a emacs
ls -l "$XDG_RUNTIME_DIR/wayland-0" "$XDG_RUNTIME_DIR/ewwm-ipc.sock"
journalctl --user -u exwm-vr-compositor.service -b --no-pager | tail -n 80
journalctl --user -u exwm-vr-emacs.service -b --no-pager | tail -n 80
```

Expected journal/runtime markers:

- `ewwm-compositor` starts with explicit `drm` backend
- IPC socket appears at `$XDG_RUNTIME_DIR/ewwm-ipc.sock`
- Emacs uses the packaged session bootstrap
- `ewwm: initialized` appears
- no ambient `~/.emacs` or unrelated repo bootstrap replaces the packaged path

## Rollback

If the session fails but the machine remains reachable:

```bash
systemctl --user stop exwm-vr.target 2>/dev/null || true
systemctl --user stop xoxdwm.target 2>/dev/null || true
```

From SSH or a root console, restore the greeter:

```bash
sudo systemctl restart sddm
sudo systemctl status sddm --no-pager
```

If SDDM repeatedly relaunches the broken session, remove any temporary autologin
configuration and restart SDDM:

```bash
sudo grep -Rni autologin /etc/sddm.conf /etc/sddm.conf.d /var/lib/sddm 2>/dev/null || true
sudo rm -f /etc/sddm.conf.d/*autologin*.conf
sudo systemctl restart sddm
```

If the desktop remains unusable, select a different known-good session at the
greeter or switch to a TTY/SSH recovery shell. Do not promote #22 from a packet
that required permanent autologin, host-only unit overrides, or VR/OpenXR
services.

## Evidence Template

Attach a short packet to GitHub #22 when the manual/fresh-login pass is real:

```text
date:
host: yoga
kernel:
rpm_versions:
display_manager:
session_entry:
login_method: manual SDDM selection | fresh greeter login
loginctl_session:
user_units:
runtime_sockets:
journal_markers:
rollback_tested:
remaining_gaps:
```

Promotion criteria:

- local Wayland session starts from the greeter without autologin
- compositor and app-layer Emacs service reach active state
- IPC and `ewwm: initialized` markers appear
- recovery path is documented and works
- VR/OpenXR claims remain out of scope for this lane
