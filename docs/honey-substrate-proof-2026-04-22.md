# `honey` Substrate Proof - 2026-04-22

This note records the first bounded named-host `honey` proof after the repo
truth surface shifted from `yoga` closure back to the XR substrate, and the
follow-up direct-mode proof that turned "missing lease support" into a solved
substrate gate.

Read this together with [status.md](status.md),
[support-matrix.md](support-matrix.md), and
[grounded-milestone-plan-2026-q2.md](grounded-milestone-plan-2026-q2.md).

## Inputs

- Host: `honey`
- Date: `2026-04-22`
- Kernel: `6.19.5-7.xr.el10`
- Branch under test: `codex/reality-authority-surface`
- Installed package baseline: branch-scoped `exwm-vr-0.5.4-1.el10`
- Direct-mode proof artifact: GitHub Actions packaging run `24776900393`
- Direct-mode proof commit: `3cae58e`

## Host Prerequisites and Reset Context

The `honey` host platform is documented in the companion
[`Dell-7810`](https://github.com/Jesssullivan/Dell-7810) repository. The reset,
power, and display recovery that preceded this proof is recorded in
[`honey-reset-matrix-2026-04-22.md`](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/research/honey-reset-matrix-2026-04-22.md).
Hardware setup, BIOS, SMI, and kernel baseline validation are owned by that repo;
see the
[boundary audit](https://github.com/Jesssullivan/Dell-7810/blob/main/docs/platform/xoxdwm-boundary-audit.md).

## Host Normalization

The host started in a half-migrated state:

- legacy package `ewwm-compositor-0.5.0-1.x86_64` was installed
- `exwm-vr-*` RPMs were not installed
- `monado-service` and `hello_xr` were present under `/usr/local`
- `/etc/xdg/openxr/1/active_runtime.json` was absent

The normalization pass did the following:

- removed `ewwm-compositor-0.5.0-1.x86_64`
- installed:
  - `exwm-vr-0.5.4-1.el10.x86_64`
  - `exwm-vr-compositor-0.5.4-1.el10.x86_64`
  - `exwm-vr-elisp-0.5.4-1.el10.noarch`
- created:
  - `/etc/xdg/openxr/1/active_runtime.json -> /usr/local/share/openxr/1/openxr_monado.json`
- added a local drop-in for the host-provided Monado unit:
  - `/etc/systemd/user/monado.service.d/10-exwm-vr.conf`
  - this supplied:
    - `XR_RUNTIME_JSON=/etc/xdg/openxr/1/active_runtime.json`
    - `XRT_COMPOSITOR_FORCE_WAYLAND=1` (Wayland window fallback, not direct lease mode)
    - `XRT_COMPOSITOR_WAYLAND_CONNECTOR=DP-2`
    - `STEAMVR_LH_ENABLE=1`
    - `XRT_COMPOSITOR_COMPUTE=1`
    - `LH_OVERRIDE_IPD_MM=64`

## XoxdWM Compositor Proof

With the refreshed RPM surface installed, `systemctl --user start exwm-vr.target`
on `honey` reached:

- `exwm-vr.target`: `active`
- `exwm-vr-compositor.service`: `active`
- `exwm-vr-emacs.service`: `active`

Observed proof markers:

- `ewwm-compositor v0.5.4 starting`
- `backend: drm`
- `IPC server listening socket_path="/run/user/1000/ewwm-ipc.sock"`
- `libseat session initialized seat=seat0`
- `output configured name=DP-2 mode=5088x2544@75Hz`
- `output configured name=HDMI-A-2 mode=1920x1080@60Hz`
- `Wayland socket: /run/user/1000/wayland-0`
- `XWayland ready display_number=0`
- `ewwm: initialized`

This is the first bounded named-host XoxdWM compositor startup recorded on
`honey`.

## Monado And Client-Tool Proof

With explicit Monado/OpenXR environment on `honey`, `monado-cli probe` moved
past the earlier "unable to find HMD" path and selected the SteamVR builder for
the Beyond:

- `Got devices:`
  - `0: Bigscreen Beyond`
- `Result: XRT_SUCCESS`

The first bounded `hello_xr` pass needed one extra correction that matters for
remote operation:

- over SSH, `XDG_RUNTIME_DIR` was unset
- without that, `hello_xr` looked for Monado IPC under
  `~/.cache/monado_comp_ipc`
- setting `XDG_RUNTIME_DIR=/run/user/1000` aligned the client with the running
  user service at `%t/monado_comp_ipc`

After that correction, `hello_xr` reached:

- `xrCreateInstance`
- `xrGetSystem`
- runtime selection:
  - `Instance RuntimeName=Monado(XRT) by Collabora et al '5976596'`
- HMD selection:
  - `Head: 'Bigscreen Beyond'`
- Vulkan instance creation
- Vulkan device creation

This partial proof was important because it moved the host from "runtime exists"
to "runtime plus client tool sees the headset," but it was still the fallback
window path, not Monado's true Wayland direct mode.

## Direct-Mode Follow-Up

A later follow-up on the same day used the fresh packaging artifact from run
`24776900393` for commit `3cae58e` to stage only a newer compositor binary on
`honey` without disturbing the installed package baseline:

- staged binary:
  - `/home/jess/xoxdwm-stage-3cae58e/root/usr/bin/ewwm-compositor`
- staged compositor user-unit override:
  - `~/.config/systemd/user/exwm-vr-compositor.service.d/10-stage-binary.conf`
- direct-mode Monado user-unit override:
  - `~/.config/systemd/user/monado.service.d/30-direct-lease-probe.conf`
- explicit direct-mode environment:
  - `XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1`
  - `XRT_COMPOSITOR_FORCE_WAYLAND` unset
  - `WAYLAND_DISPLAY=wayland-0`
  - `EWWM_DRM_LEASE_CONNECTORS=DP-2`

With those overrides in place, `systemctl --user start exwm-vr.target` and
`systemctl --user start monado.service` produced the markers that had been
missing earlier:

- XoxdWM compositor:
  - `initialized wp_drm_lease_v1 global`
  - `using explicit lease connector override connector=DP-2`
  - `skipping desktop output mapping for lease connector name=DP-2`
  - `granting DRM lease request`
  - `new DRM lease became active`
- Monado:
  - `Available DRM lease device: /dev/dri/card0`
  - connector `DP-2`
  - `Lease granted`
  - direct mode selected at `5088x2544@75`

This was the first named-host proof that the running XoxdWM compositor on
`honey` could actually hand off `DP-2` to Monado via Wayland direct mode.

## Installed-Package Follow-Up

A later follow-up on the same day carried the direct-mode proof onto the real
installed package surface on `honey`.

The important host changes were:

- reinstalled `/usr/bin/ewwm-compositor` from the same `24776900393` RPM artifact
  with `rpm -Uvh --replacepkgs`
- removed the staged binary override:
  - `~/.config/systemd/user/exwm-vr-compositor.service.d/10-stage-binary.conf`
- removed the earlier literal IPC shim:
  - `/home/jess/~/.cache/monado_comp_ipc`

That clean run immediately exposed the next real gap:

- the installed compositor binary was now correct
- but without any host-side lease designation, `DP-2` returned as a normal
  desktop output
- the compositor log showed:
  - `HMD manager: 4 total connectors, 0 HMD connectors`
  - `output configured name=DP-2 mode=5088x2544@75Hz`

Adding only a minimal installed-service override restored the direct path
without bringing back the staged binary:

- `~/.config/systemd/user/exwm-vr-compositor.service.d/20-honey-dp2-lease.conf`
- contents:
  - `Environment=EWWM_DRM_LEASE_CONNECTORS=DP-2`

With that override in place, the installed `/usr/bin/ewwm-compositor` produced:

- `using explicit DRM lease connector overrides overrides={"DP-2"}`
- `treating connector as a DRM lease candidate via explicit override connector=DP-2`
- `lease-designated connector, skipping desktop output connector=DP-2`
- `granting DRM lease request`
- `new DRM lease became active`

This matters because it proves the binary/package transition is done. The
remaining dependency is host-side connector designation, not a staged binary.

## Current Blocker

The direct-mode substrate blocker is no longer "compositor missing drm-lease
support." That gap is now closed by named-host evidence. The remaining blockers
are productization and repeatability:

1. The direct-mode proof still depends on explicit user-unit overrides for host
   configuration:
   - `EWWM_DRM_LEASE_CONNECTORS=DP-2`
   - `XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1`
2. SSH-launched clients still need `XDG_RUNTIME_DIR=/run/user/1000` so the
   local `hello_xr` process targets the active Monado IPC socket.
3. The run was captured once and then intentionally torn down; it is not yet a
   repeated operator lane.

With `XDG_RUNTIME_DIR=/run/user/1000` exported in the SSH shell, the local
OpenXR client no longer needed the literal IPC shim and the proof crossed a
much stronger line than the earlier fallback run:

- `hello_xr -g Vulkan` reached:
  - `xrCreateInstance`
  - `xrGetSystem`
  - `Head: 'Bigscreen Beyond'`
  - session `READY`
  - two eye swapchains at `3561x3561`
- Monado logged:
  - `Client 1 connected`
  - application `HelloXR`
  - swapchain creation for both eye color and depth chains
  - clean client disconnect

That is a real direct-mode session bootstrap on `honey` from the installed
compositor package surface, even though it is not yet the final zero-override
operator path.

## What This Proves

- `honey` now has a named-host `exwm-vr` package install on the current branch
- `honey` now has a bounded named-host XoxdWM compositor startup
- `honey` now has an explicit active OpenXR runtime selection on-host
- `monado-cli probe` can now identify the Bigscreen Beyond on `honey`
- `ewwm-compositor` now initializes `wp_drm_lease_v1`, reserves `DP-2`, and
  grants a real DRM lease to Monado on `honey`
- `hello_xr -g Vulkan` can now reach `READY` and create eye swapchains on
  `honey` in the true Wayland direct path
- the installed `/usr/bin/ewwm-compositor` now matches the lease-capable proof
  binary from packaging run `24776900393`

## What This Does Not Yet Prove

- a repeated installed operator lane on `honey`
- a successful first-frame or long-running XR session path
- that the current proof works without host-side user-unit overrides
- that SSH-invoked client tooling always inherits the correct runtime env by default
- that XoxdWM itself is ready to be called a stable trusted XR bridge on `honey`

## Next Gate

- keep the `exwm-vr` package surface installed on `honey`
- fold `EWWM_DRM_LEASE_CONNECTORS=DP-2` into the supported host configuration
  surface for `honey`
- make `XDG_RUNTIME_DIR=/run/user/1000` explicit in remote client-tool
  automation and operator docs
- repeat the direct-mode proof without any user-unit overrides
- keep the older fallback `VK_ERROR_SURFACE_LOST_KHR` crash categorized as a
  separate Monado window-path problem, not the whole bridge story
