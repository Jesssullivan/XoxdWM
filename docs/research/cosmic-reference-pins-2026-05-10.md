# COSMIC Reference Pins - 2026-05-10

This note records the COSMIC modules worth pinning as reference material for
the native XoxdWM authority work. It is a comparison ledger, not a vendoring or
packaging plan.

## Scope

Use COSMIC to study how a serious Rust/Wayland desktop divides compositor,
session, shell, settings, portal, launcher, workspace, and toolkit authority.
Do not make COSMIC a runtime dependency for XoxdWM unless a later issue names a
specific protocol, crate, or implementation pattern and proves the integration
cost.

Primary upstream release reference:

- `pop-os/cosmic-epoch` release: `epoch-1.0.12`
- published: 2026-05-05
- release URL: https://github.com/pop-os/cosmic-epoch/releases/tag/epoch-1.0.12

COSMIC's own README says the `cosmic-epoch` submodule commits are kept building
and working together, while individual module repositories may move ahead. That
means XoxdWM should pin the release-coherent `cosmic-epoch` graph for research,
not arbitrary latest module HEADs.

## High-Value Pins

| Module | Epoch 1.0.12 commit | Why XoxdWM should study it |
| --- | --- | --- |
| `cosmic-comp` | `b5a1a6d3179810627fa0bffac7bd5d78c7df4fa0` | Closest reference for a production Rust/Smithay compositor with shell policy, input, backend, Wayland protocol, xwayland, config, and systemd integration. |
| `cosmic-session` | `17cf4485a917c5e7490c0e1a26cdf348f06bf486` | Reference for session ownership, process launch boundaries, logind/systemd integration, and autostart behavior without making an editor the session authority. |
| `cosmic-comp-config` | inside `cosmic-comp` | Reference for splitting compositor config parsing/state from compositor runtime. Useful for replacing the current XoxdWM flat JSON parser when policy shape stabilizes. |
| `cosmic-protocols` | `160b086` in `cosmic-comp` dependency graph | Reference for protocol extension boundaries. Study before inventing XoxdWM-only shell/input protocols. |
| `xdg-desktop-portal-cosmic` | `db8ec7cf496ed0f2028c67f4eec7ffdc2cbcf145` | Reference for portal-mediated desktop capabilities and safer input/screencast boundaries than X-era global control. |
| `cosmic-settings-daemon` | `716da6d6af0b252e2f78aba2ad72ee19ae0241e0` | Reference for DE settings daemon boundaries that should stay outside compositor hot paths. |
| `cosmic-settings` | `cb58af2be4e96dae60dfeb4fb7c11f64d0832de5` | Reference for a GUI control client over system/session settings. This maps to future XoxdWM calibration/config panels, not compositor authority. |
| `cosmic-panel` | `2358f0473bf68b79f54a0906994a218de211de34` | Reference for shell UI as a client/layer, not as the compositor core. |
| `cosmic-applets` | `c003924f0816efff1cf5bbfcdf1d7e1f1f36cae5` | Reference for small applet/plugin surfaces around the DE shell. Useful for later BCI/XR status surfaces. |
| `cosmic-launcher` | `296e5cb66c77159840d2039540ede315bcd51ab0` | Reference for launcher process/UI boundaries. XoxdWM now has native configured app launch; launcher UI should remain a client. |
| `cosmic-applibrary` | `f2024ce95574cc3222ff6668ef378a63bee8993c` | Reference for app discovery and grouping, separate from compositor launch authority. |
| `cosmic-workspaces-epoch` | `d56a59208e8443e0179bd612e0b27b561e887f51` | Reference for user-facing workspace UI, separate from compositor workspace state. |
| `cosmic-randr` | `6e8e795970fa06d434af22775e415b517f7552d3` | Reference for display/output configuration client behavior. Relevant to Honey/Dell/BS2E topology handling. |
| `cosmic-bg` | `b1ca4c180ab29dd185472b777ab0abdb1f96ccaf` | Reference for wallpaper/background as shell/client policy rather than compositor core. |
| `cosmic-osd` | `cbc1e9cbf3af02faf8caa471dddcb03b98cd5f55` | Reference for transient shell feedback surfaces. Useful later for gaze/BCI/input status feedback. |
| `cosmic-notifications` | `a899bfbc6715c36b1f02d7a0f4d3601a3ea0295f` | Reference for notification client/service split. Useful for operator feedback but not core WM authority. |
| `libcosmic` | release-coherent via module dependency graph | Reference toolkit for COSMIC app/client design. Study for app-layer panels, not compositor implementation. |

## Do Not Pin For XoxdWM Authority Yet

- `cosmic-edit`, `cosmic-term`, `cosmic-files`, `cosmic-store`,
  `cosmic-player`, and `cosmic-screenshot` are useful examples of COSMIC apps,
  but they are not high-leverage for XoxdWM's native WM authority boundary.
- `cosmic-greeter` is relevant only when XoxdWM decides to own a display-manager
  or greeter integration lane. Current package/session work should stay with the
  existing Rocky/SDDM proof lane.
- `cosmic-wallpapers`, `cosmic-icons`, and `cosmic-initial-setup` are not
  priority reference pins for the current XR/BCI/developer DE milestone.

## Lessons To Pull Into XoxdWM

1. Keep the compositor as policy/runtime authority, but split config, IPC, and
   session concerns clearly enough to test them without a live desktop.
2. Treat shell UI, launcher UI, workspaces UI, settings UI, notifications, OSD,
   and panels as clients or sidecars, not the WM brain.
3. Prefer portal/protocol-mediated capabilities for privileged desktop behavior.
   This supports the XoxdWM direction away from EXWM/XCB-era global control.
4. Keep XWayland as a compatibility feature while removing EXWM/XCB as an
   authority model.
5. For packaging research, pin the `cosmic-epoch` release graph. For
   implementation research, inspect `cosmic-comp` module boundaries and
   `cosmic-session` process/session boundaries first.

## XoxdWM Tracker Mapping

- `#45` Native WM authority boundary: `cosmic-comp`, `cosmic-session`,
  `cosmic-comp-config`.
- `#46` IPC and native config contract: `cosmic-comp-config`,
  `cosmic-protocols`, `xdg-desktop-portal-cosmic`.
- `#47` Legacy EXWM and X retirement: `cosmic-comp` XWayland feature split and
  COSMIC shell-client separation.
- `#48` Emacs/eGreg application-layer integration: `cosmic-settings`,
  `cosmic-panel`, `cosmic-launcher`, `cosmic-workspaces-epoch`.
- `#49` Honey P4 visual first frame: `cosmic-randr` and `cosmic-comp` output,
  DPMS, backend, and DRM handling as references only.
- `#50` Multimodal input reality ladder: `cosmic-osd`,
  `cosmic-notifications`, and portal surfaces for feedback/mediation patterns.

## Pin Refresh Rule

Refresh this ledger only at explicit COSMIC release tags or at a named
XoxdWM research issue. Do not chase moving `master` branches as part of normal
native-authority implementation.
