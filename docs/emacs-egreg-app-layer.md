# Emacs/eGreg App-Layer Contract

This document defines the supported Emacs/eGreg role under XoxdWM.

## Authority Boundary

The Rust compositor is the native WM/DE authority. It owns compositor startup
config, workspace count, active workspace, layout reflow, focus, configured key
actions, configured app-launch targets, autostart, lock/logout, idle
supervision, DPMS, and surface lifecycle.

Emacs/eGreg is an app-layer client. It can provide editing, shells, code
navigation, diagnostic views, calibration/control panels, and optional IPC
helpers, but it is not the WM authority.

## Supported App Profile

- Preferred runtime: Emacs pgtk as a Wayland native application on the
  compositor's `WAYLAND_DISPLAY`.
- Packaged app-layer service: `xoxdwm-emacs.service`, provided by the
  compatibility unit `exwm-vr-emacs.service`.
- Interactive frame path: `emacsclient -c` creates a normal managed Emacs
  frame under XoxdWM.
- Configuration path: app-layer settings belong in
  `~/.config/exwm-vr/config.el` on the packaged Rocky lane, or in regular
  Emacs config for non-packaged development sessions.
- Compatibility path: non-pgtk Emacs builds may run through XWayland only when
  the compositor was built with the explicit `xwayland` feature. The XWayland
  compatibility path is explicit and separate from the preferred pgtk profile.

The default `xoxdwm.target` does not require or start the Emacs app-layer
service. Start `xoxdwm-emacs.service` only when editor/control integration is
wanted.

## IPC Role

The IPC boundary is control/diagnostic/app integration only. Connected
Emacs/eGreg helpers may request native actions such as workspace switching,
layout cycling, surface focus, configured app launch, config reload, session
lock/logout, idle supervision, and DPMS changes. The compositor accepts,
rejects, and broadcasts the resulting state as the source of truth.

Disconnected fallback behavior in Lisp helpers is for debug/editor-only
sessions. It must not be used to reintroduce WM policy ownership into the
normal XoxdWM runtime path.

## Non-Goals

- Emacs/eGreg does not own workspace or layout policy in normal runtime.
- Emacs/eGreg does not define the compositor IPC schema.
- Emacs/eGreg does not start by default through `xoxdwm.target`.
- XWayland is not required for the preferred Emacs pgtk app profile.

## Proof Surfaces

- `just package-no-default-lisp-core-assert` proves the default package/session
  graph keeps `xoxdwm.target` native-only.
- `just native-authority-test` guards native policy ownership and app-layer IPC
  client behavior.
- `just truth-lint` keeps public docs from drifting back to Emacs-as-WM
  authority language.
