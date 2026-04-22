# `honey` Substrate Proof - 2026-04-22

This note records the first bounded named-host `honey` proof after the repo
truth surface shifted from `yoga` closure back to the XR substrate.

Read this together with [status.md](status.md),
[support-matrix.md](support-matrix.md), and
[grounded-milestone-plan-2026-q2.md](grounded-milestone-plan-2026-q2.md).

## Inputs

- Host: `honey`
- Date: `2026-04-22`
- Kernel: `6.19.5-7.xr.el10`
- Branch under test: `codex/reality-authority-surface`
- RPM payload source: GitHub Actions packaging run `24771056471`

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
    - `XRT_COMPOSITOR_FORCE_WAYLAND=1`
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

## Current Blocker

The client-tool path still does not complete. `hello_xr` fails during
`xrCreateSession`, and the running `monado.service` crashes:

- Monado log:
  - `vkGetPhysicalDeviceSurfaceFormatsKHR failed: VK_ERROR_SURFACE_LOST_KHR`
  - `vk_surface_info_fill_in: VK_ERROR_SURFACE_LOST_KHR`
  - `comp_target_acquire: VK_ERROR_INITIALIZATION_FAILED`
- `hello_xr`:
  - `XR_ERROR_RUNTIME_FAILURE in xrCreateSession`
- `systemd`:
  - `monado.service: Main process exited, code=dumped, status=11/SEGV`

The coredump stack on this pass lands in Monado's compositor path, not in the
XoxdWM compositor.

## What This Proves

- `honey` now has a named-host `exwm-vr` package install on the current branch
- `honey` now has a bounded named-host XoxdWM compositor startup
- `honey` now has an explicit active OpenXR runtime selection on-host
- `monado-cli probe` can now identify the Bigscreen Beyond on `honey`
- `hello_xr` can now reach real OpenXR runtime and HMD selection on `honey`

## What This Does Not Yet Prove

- a working VR session on `honey`
- a successful `hello_xr` frame submission path
- a stable Monado compositor path on the current AMD / kernel stack
- that XoxdWM itself is the final trusted XR bridge on `honey`

## Next Gate

- keep the `exwm-vr` package surface installed on `honey`
- treat the Monado `VK_ERROR_SURFACE_LOST_KHR` plus coredump as the active
  substrate blocker
- debug whether the failing path is:
  - Monado's Wayland surface handling on this host
  - AMD Vulkan / surface-loss behavior on the current kernel+Mesa stack
  - the current bridge topology choice for Beyond on `honey`
