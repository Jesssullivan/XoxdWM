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

This partial proof is important, but it is still the fallback window path, not
Monado's true Wayland direct mode.

## Direct-Mode Follow-Up

A follow-up probe forced Monado's direct Wayland target instead of the fallback
window path:

- `XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1`
- `XRT_COMPOSITOR_FORCE_WAYLAND` unset
- `XRT_COMPOSITOR_WAYLAND_CONNECTOR=DP-2`

That changed the failure mode in a useful way:

- the earlier `VK_ERROR_SURFACE_LOST_KHR` path did not appear first
- Monado instead logged:
  - `ERROR [comp_window_direct_wayland_init] Compositor is missing drm-lease support`
  - `ERROR [compositor_init_window_pre_vulkan] Failed to init Wayland Direct-Mode backend!`
- `hello_xr` then failed earlier with `XR_ERROR_INSTANCE_LOST` because Monado
  could not create a direct-mode system compositor

This is the first named-host evidence that the current `honey` smoke path is
not yet the real DRM-lease bridge. The current XoxdWM compositor startup is
still treating `DP-2` as a normal output, not handing it off to Monado via
Wayland direct mode.

## Current Blocker

The VR client path still does not complete, but there are now two distinct
blockers rather than one generic "Monado crash":

1. In the current fallback window path, `hello_xr` fails during
   `xrCreateSession`, and the running `monado.service` crashes:

- Monado log:
  - `vkGetPhysicalDeviceSurfaceFormatsKHR failed: VK_ERROR_SURFACE_LOST_KHR`
  - `vk_surface_info_fill_in: VK_ERROR_SURFACE_LOST_KHR`
  - `comp_target_acquire: VK_ERROR_INITIALIZATION_FAILED`
- `hello_xr`:
  - `XR_ERROR_RUNTIME_FAILURE in xrCreateSession`
- `systemd`:
  - `monado.service: Main process exited, code=dumped, status=11/SEGV`

2. In the true Wayland direct path, Monado fails earlier because the running
   compositor does not expose DRM lease support:

- Monado log:
  - `ERROR [comp_window_direct_wayland_init] Compositor is missing drm-lease support`
  - `ERROR [compositor_init_window_pre_vulkan] Failed to init Wayland Direct-Mode backend!`
- `hello_xr`:
  - `XR_ERROR_INSTANCE_LOST in xrGetSystem`

The coredump stack on the fallback pass lands in Monado's compositor path, not
in the XoxdWM compositor. The direct-mode follow-up shows the deeper substrate
gap more clearly: the true lease-backed path is not available yet.

## What This Proves

- `honey` now has a named-host `exwm-vr` package install on the current branch
- `honey` now has a bounded named-host XoxdWM compositor startup
- `honey` now has an explicit active OpenXR runtime selection on-host
- `monado-cli probe` can now identify the Bigscreen Beyond on `honey`
- `hello_xr` can now reach real OpenXR runtime and HMD selection on `honey`
- the current `honey` smoke path is still a fallback Wayland-window path, not
  a true Wayland-direct DRM-lease handoff

## What This Does Not Yet Prove

- a working VR session on `honey`
- a successful `hello_xr` frame submission path
- a stable Monado compositor path on the current AMD / kernel stack
- that `ewwm-compositor` exposes usable DRM lease support to Monado on `honey`
- that XoxdWM itself is the final trusted XR bridge on `honey`

## Next Gate

- keep the `exwm-vr` package surface installed on `honey`
- treat compositor-side DRM lease support plus HMD reservation as the next
  substrate gate
- keep the fallback `VK_ERROR_SURFACE_LOST_KHR` crash categorized as a separate
  Monado window-path problem, not the whole bridge story
- decide whether the next direct-mode proof should come from XoxdWM itself or
  from an explicit Sway/wlroots bridge on `honey`
