# Native Authority PR Split Manifest - 2026-05-10

This manifest maps the current native XoxdWM authority worktree into the
review stack for #45. GitHub issues stay open until their implementing PR
merges; Linear TIN-1086 receives one breadcrumb per PR.

## 1. Truth + Tracker Foundation

Covers #52, #58, #66 and shared tracker/docs guardrails.

- `.github/labels.json`
- `docs/api-reference.md`
- `docs/developer-guide.md`
- `docs/feature-matrix.md`
- `docs/installation-quickstart.md`
- `docs/research/cosmic-reference-pins-2026-05-10.md`
- `docs/research/native-authority-pr-split-manifest-2026-05-10.md`
- `docs/status.md` native-authority/support truth hunks
- `docs/support-matrix.md` native-authority/support truth hunks
- `docs/user-guide.md` native-config authority hunks
- `test/truth-surface-test.el`
- `test/ipc-contract-test.el`
- `test/native-authority-test.el`
- `justfile` truth/static-test target hunks

Focused checks:

- `just truth-lint`
- `just ipc-contract-test`
- `just native-authority-test`

## 2. Native Config + IPC Authority Base

Covers #51, #54, #55, #59, #64, #65.

- `compositor/Cargo.toml`
- `compositor/src/config.rs`
- `compositor/src/input.rs`
- `compositor/src/ipc/dispatch.rs` app-launch, config-reload, workspace, layout, key-action, and command-list hunks
- `compositor/src/ipc/recorder.rs`
- `compositor/src/ipc/server.rs`
- `compositor/src/main.rs`
- `compositor/src/state.rs` config, workspace, focus, key-action, and launch hunks
- `compositor/src/backend/headless.rs`
- `compositor/src/backend/mod.rs`
- `docs/ipc-protocol.md` app-launch/config-reload/workspace/layout hunks
- `test/v041-wm-commands-test.el`

Focused checks:

- `cargo fmt --manifest-path compositor/Cargo.toml --check`
- `cargo check --manifest-path compositor/Cargo.toml --bin ewwm-compositor --no-default-features`
- `just ipc-contract-test`

## 3. Native Runtime Policy

Covers #60, #61, #67, #68, #69, #70.

- `compositor/src/autotype.rs`
- `compositor/src/backend/drm.rs`
- `compositor/src/backend/winit.rs`
- `compositor/src/handlers/compositor.rs`
- `compositor/src/handlers/dpms.rs`
- `compositor/src/handlers/layer_shell.rs`
- `compositor/src/handlers/mod.rs`
- `compositor/src/handlers/output_management.rs`
- `compositor/src/handlers/pointer_constraints.rs`
- `compositor/src/handlers/seat.rs`
- `compositor/src/handlers/xdg_activation.rs`
- `compositor/src/handlers/xdg_shell.rs`
- `compositor/src/handlers/xwayland.rs`
- `compositor/src/input_source.rs`
- `compositor/src/lib.rs`
- `compositor/src/secure_input.rs`
- `compositor/src/state.rs` layout reflow, workspace visibility, autostart, session, idle, and DPMS hunks
- `compositor/tests/full_stack_integration.rs`
- `compositor/tests/headless_integration.rs`
- `docs/ipc-protocol.md` autostart/session/idle hunks
- `test/v040-focus-cursor-test.el`

Focused checks:

- `cargo fmt --manifest-path compositor/Cargo.toml --check`
- `cargo check --manifest-path compositor/Cargo.toml --bin ewwm-compositor --no-default-features`
- `just native-authority-test`

## 4. Emacs/eGreg App-Layer Bridge

Covers #71, #72, #73.

- `lisp/vr/ewwm-floating.el`
- `lisp/vr/ewwm-input.el`
- `lisp/vr/ewwm-ipc.el`
- `lisp/vr/ewwm-launch.el`
- `lisp/vr/ewwm-layout.el`
- `lisp/vr/ewwm-manage.el`
- `lisp/vr/ewwm-session.el`
- `lisp/vr/ewwm.el`
- `docs/ipc-protocol.md` connected-client compatibility hunks

Focused checks:

- `just ipc-contract-test`
- `just test`

## 5. Runtime Proof + Packaging Boundary

Covers #53, #56, #62, #63.

Follow-on #47 runtime-boundary slice:

- keep the RPM meta package hard-dependent on the native compositor only
- make Emacs/eGreg Elisp a suggested app-layer package, not default WM
  authority runtime
- make `:protocol` canonical in surface IPC while keeping X11 class/instance
  as XWayland compatibility metadata
- keep XWayland optional behind the native Cargo feature gate

- `justfile` native-authority proof, boot-without-Emacs smoke, and package assertion hunks
- `packaging/rpm/exwm-vr.spec`
- `packaging/scripts/boot-without-emacs-smoke`
- `packaging/scripts/xoxdwm-native-authority-proof`
- `packaging/sway/config`
- `packaging/sway/status.sh`
- `docs/status.md` no-product-proof/native-proof boundary hunks
- `docs/support-matrix.md` native WM authority support row hunks
- `docs/user-guide.md` JSON-backed native-config reference hunks

Focused checks:

- `just package-no-default-lisp-core-assert`
- `just boot-without-emacs-smoke 1`
- `just native-authority-test`

## 6. Honey P4 / XR Evidence Lane

Covers #49, #57 and Honey-specific proof helpers.

- `README.md`
- `compositor/src/handlers/drm_lease.rs`
- `compositor/src/render.rs`
- `compositor/src/vr/attention.rs`
- `compositor/src/vr/bci_state.rs`
- `compositor/src/vr/beyond_hid.rs`
- `compositor/src/vr/blink_wink.rs`
- `compositor/src/vr/drm_lease.rs`
- `compositor/src/vr/eye_tracking.rs`
- `compositor/src/vr/fatigue.rs`
- `compositor/src/vr/fatigue_eeg.rs`
- `compositor/src/vr/follow_mode.rs`
- `compositor/src/vr/frame_timing.rs`
- `compositor/src/vr/gaze_focus.rs`
- `compositor/src/vr/gaze_scroll.rs`
- `compositor/src/vr/gaze_zone.rs`
- `compositor/src/vr/gesture.rs`
- `compositor/src/vr/gpu_power.rs`
- `compositor/src/vr/hand_tracking.rs`
- `compositor/src/vr/link_hints.rs`
- `compositor/src/vr/mod.rs`
- `compositor/src/vr/motor_imagery.rs`
- `compositor/src/vr/openxr_state.rs`
- `compositor/src/vr/overlay.rs`
- `compositor/src/vr/p300.rs`
- `compositor/src/vr/radial_menu.rs`
- `compositor/src/vr/scene.rs`
- `compositor/src/vr/ssvep.rs`
- `compositor/src/vr/stub.rs`
- `compositor/src/vr/texture.rs`
- `compositor/src/vr/transient_3d.rs`
- `compositor/src/vr/virtual_keyboard.rs`
- `compositor/src/vr/vr_interaction.rs`
- `compositor/src/vr/vr_renderer.rs`
- `docs/honey-fresh-boot-evidence-template.md`
- `docs/honey-fresh-boot-runbook-2026-04-26.md`
- `docs/research/fpga-dsc-verification.md`
- `docs/research/honey-kernel-dsc-truth-2026-05-10.md`
- `docs/status.md` Honey topology/P3/P4 hunks
- `docs/support-matrix.md` Honey proof-ladder/topology hunks
- `docs/user-guide.md` Honey HMD connector hunks
- `justfile` Honey smoke/status/proof hunks
- `packaging/scripts/beyond-power-on`
- `packaging/scripts/exwm-vr-hmd-connector`
- `packaging/scripts/exwm-vr-kernel-dsc-truth`
- `packaging/scripts/exwm-vr-monado-launch`
- `packaging/scripts/exwm-vr-openxr-smoke`
- `packaging/scripts/exwm-vr-setup`
- `patches/amdgpu-dsc-pps-debugfs.patch`
- `test/honey-display-regression-test.el`
- `test/honey-substrate-test.el`
- `test/v050-vr-renderer-test.el`

Focused checks:

- `just truth-lint`
- `just test`
- `cargo check --manifest-path compositor/Cargo.toml --bin ewwm-compositor --no-default-features`
