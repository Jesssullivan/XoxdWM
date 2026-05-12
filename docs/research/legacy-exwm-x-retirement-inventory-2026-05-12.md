# Legacy EXWM/X Retirement Inventory - 2026-05-12

## Scope

This inventory tracks where EXWM/X11/XCB still appears after native XoxdWM
became the WM/DE authority. It is a retirement map, not a removal plan for
every compatibility path.

## Runtime Rule

Default XoxdWM runtime authority is native Rust plus protocol-neutral IPC.
EXWM/XCB Lisp and XWayland are compatibility surfaces:

- EXWM/XCB Lisp may be packaged in the optional Emacs/eGreg subpackage for
  archive, debugging, and migration work.
- XWayland may be compiled as an explicit compatibility feature for legacy X11
  applications.
- Neither surface should be required for the default compositor package,
  session target, or native authority proof.

## Inventory

| Surface | Current owner | Classification | Retirement rule |
| --- | --- | --- | --- |
| `lisp/core/exwm-*.el` | Emacs/eGreg optional subpackage | Legacy EXWM/XCB archive | Do not add to default site-start load path or native proof path. |
| `lisp/vr/ewwm-*.el` | Emacs/eGreg app-layer client | Supported app/control client | Keep IPC client behavior protocol-neutral; native Rust owns WM policy. |
| `ewwm-surface-protocol` | Lisp app-layer metadata | Canonical surface protocol | Prefer `:protocol`; use X11 fields only as XWayland compatibility metadata. |
| `ewwm-x11-*` fields | Lisp app-layer metadata | XWayland compatibility | Keep for legacy manage rules and debugging, not as canonical app identity. |
| `compositor/src/handlers/xwayland.rs` | Rust compositor | Optional XWayland compatibility | Compile only with the `xwayland` Cargo feature. |
| DRM/Winit XWayland spawn | Rust compositor backends | Optional XWayland compatibility | Guard startup behind `#[cfg(feature = "xwayland")]`. |
| `compositor/Cargo.toml` `full-backend` | Rust compositor build feature | Native Linux backend support | Must not imply `xwayland`; use `full-backend,xwayland` when compatibility is needed. |
| `packaging/rpm/exwm-vr.spec` | Rocky package lane | Native package with compatibility names | Default build uses `full-backend`; `%bcond xwayland_compat` is opt-in and carries the Xwayland runtime dependency. |
| `xoxdwm.target` | Package/Nix session lane | Native compositor target | Start compositor only; do not want `exwm-vr-emacs.service`. |
| `exwm-vr.target` | Package/Nix session lane | Legacy EXWM-VR compatibility target | Keep as an explicit app-layer compatibility route that may want Emacs. |
| `packages.compositor-xwayland` | Nix package lane | Explicit compatibility build | Keep separate from the default `packages.compositor` output. |
| `test/exwm-*.el` | Static legacy tests | Inventory/compatibility guard | Keep as tests for legacy archive behavior, not product proof. |
| Research docs with EXWM examples | Historical docs | Research/archive | Leave unless they make current support or authority claims. |

## Explicitly Kept

XWayland is not deleted. It remains the bridge for legacy X11 applications when
a build or host profile intentionally enables it. This keeps compatibility
available without letting X11 define the default product shape.

## Guardrails

- `native-authority/package-default-does-not-load-lisp-core`
- `native-authority/default-session-target-does-not-pull-emacs`
- `native-authority/xwayland-is-optional-compatibility-feature`
- `ipc-contract/surface-metadata-is-protocol-neutral`
- `just build-compositor-xwayland`
- `just test-compositor-xwayland`
