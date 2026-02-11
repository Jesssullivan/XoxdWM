# GPU Driver Compatibility Matrix

## Overview

The EXWM-VR compositor is built on Smithay 0.7 using `GlesRenderer`, which
requires a working EGL + GBM stack. The compositor obtains a DRM device via
libseat, creates a GBM device on top of it, and initializes EGL for rendering.
VR features additionally require DRM lease support and a Vulkan-capable driver
for the OpenXR runtime (Monado).

Key constraints:

- **GBM is mandatory.** EGLStream-only drivers (NVIDIA < 535) are not supported.
- **EGL_KHR_platform_gbm** must be advertised by the driver.
- **DMA-BUF import** (via `zwp_linux_dmabuf_v1`) is needed for zero-copy client
  buffer sharing.
- **DRM lease** (`wp_drm_lease_v1`) is needed for handing off HMD displays to
  the OpenXR runtime.

## Compatibility Matrix

| GPU | Driver | OpenGL | Vulkan | DMA-BUF | DRM Lease | VR | Compositor | Notes |
|-----|--------|--------|--------|---------|-----------|-----|------------|-------|
| AMD Radeon (GCN+) | radeonsi / radv | 4.6 | 1.3 | Yes | Yes | Full | Full | Primary target; best tested path |
| Intel HD/Iris (Gen8+) | iris / anv | 4.6 | 1.3 | Yes | Yes | Full | Full | Secondary target; laptop/NUC recommended |
| NVIDIA (nouveau) | nouveau | 4.5 (NV110+) | No | Partial | No | No | Limited | SW cursor fallback; no re-clock on most GPUs |
| NVIDIA (proprietary >=535) | nvidia + GBM | 4.6 | 1.3 | Yes | Yes | Possible | Works | Needs `WLR_RENDERER=gles2`; GBM support since 535.43 |
| NVIDIA (proprietary <535) | nvidia (EGLStream) | 4.6 | 1.3 | No | No | No | NOT supported | EGLStream only; no GBM; use >=535 or nouveau |
| ARM Mali (Bifrost+) | Panfrost | 3.1 / ES 3.1 | No | Yes | Partial | No | 2D only | aarch64; RK3588, MT8183; no Vulkan yet |
| ARM Mali (400/450) | Lima | 2.1 / ES 2.0 | No | Yes | No | No | Limited | aarch64; older SoCs; basic GLES2 |
| Qualcomm Adreno (6xx+) | Freedreno | 3.1 / ES 3.1 | 1.1 (turnip) | Yes | Partial | No | 2D only | aarch64; Snapdragon dev kits; turnip for Vulkan |
| Broadcom VideoCore (V3D) | V3D | ES 3.1 | 1.2 (v3dv) | Yes | No | No | Works | Raspberry Pi 5; GLES2 renderer path |
| Software (llvmpipe) | llvmpipe | 4.5 | 1.0 (lavapipe) | No | No | No | Functional | s390x / headless; 2-5 fps for desktop use |
| VirtIO GPU (virgl) | virgl | 4.3 | No | Partial | No | No | Functional | VM guests; depends on host GPU capabilities |

### Legend

- **Full**: Feature works with no known limitations.
- **Works**: Core feature works, minor caveats may apply.
- **Functional**: Technically works but performance or features are degraded.
- **Limited**: Significant limitations; not recommended for daily use.
- **Partial**: Some sub-features work, others do not.
- **No**: Feature is not available with this driver.
- **NOT supported**: Configuration is explicitly unsupported.

## Tested Configurations

These configurations are validated in CI or developer hardware:

| Config | Hardware | OS | Driver | Status |
|--------|----------|----|--------|--------|
| AMD primary | RX 7800 XT | NixOS | radeonsi/radv (Mesa 24.x) | Primary dev machine |
| Intel laptop | i7-1365U (Iris Xe) | NixOS | iris/anv (Mesa 24.x) | Tested weekly |
| NVIDIA prop | RTX 4070 | Rocky 9 | nvidia 550.x | GBM path verified |
| Pi 5 | BCM2712 | Raspberry Pi OS | V3D (Mesa 24.x) | 2D compositor only |
| llvmpipe | s390x LPAR | Rocky 9 | llvmpipe (Mesa 23.x) | Headless CI |
| VirtIO | QEMU/KVM | NixOS | virgl | CI validation |

## Known Issues

### NVIDIA EGLStream (proprietary < 535)

The pre-535 NVIDIA proprietary driver uses EGLStream instead of GBM for buffer
allocation. Smithay's `GlesRenderer` requires GBM, so these drivers are
fundamentally incompatible. There is no workaround.

**Resolution**: Upgrade to driver >= 535.43 which adds GBM support, or use the
nouveau open-source driver (with significant performance limitations).

### NVIDIA GBM (proprietary >= 535)

The GBM implementation in the NVIDIA proprietary driver has quirks:

- Set `WLR_RENDERER=gles2` to avoid the default Vulkan renderer path (which
  has known issues with NVIDIA GBM).
- Set `GBM_BACKEND=nvidia-drm` if auto-detection fails.
- Hardware cursors may flicker; `WLR_NO_HARDWARE_CURSORS=1` as fallback.
- DRM lease for VR is possible but less tested than AMD/Intel.

### VirtIO GPU Limitations

- No DMA-BUF import (buffers go through shared memory fallback).
- Performance depends entirely on the host GPU and virgl translation layer.
- No VR support (no DRM lease, no direct GPU access).
- Useful for development and CI, not production VR workloads.

### nouveau Re-clocking

nouveau cannot re-clock most NVIDIA GPUs to their full performance state. This
means the GPU runs at its lowest power state, resulting in very poor rendering
performance (often worse than llvmpipe for complex scenes).

### Panfrost / Freedreno DRM Lease

ARM GPU drivers have partial DRM lease support in the kernel but it is not
exercised by VR runtimes. Monado does not target ARM Mali or Adreno GPUs for
VR rendering.

## VR Requirements

VR operation imposes stricter requirements than 2D desktop compositing:

| Requirement | Minimum | Recommended |
|-------------|---------|-------------|
| GPU | OpenGL 4.5 + Vulkan 1.1 | Vulkan 1.3 |
| Driver | DRM lease support | AMD radv or Intel anv |
| Frame rate | 72 fps sustained | 90 fps sustained |
| Frame time | < 14 ms | < 5 ms |
| DMA-BUF | Required | Required |
| VRAM | 4 GB | 8 GB |

**Recommended VR GPUs** (in order of preference):

1. AMD Radeon RX 6000/7000 series (radv) -- best open-source Vulkan driver
2. Intel Arc A-series (anv) -- good Vulkan, lower power
3. NVIDIA RTX 30xx/40xx (proprietary >= 550) -- works but requires workarounds
4. AMD Radeon RX 5000 series (radv) -- minimum for 90fps VR

GPUs that **cannot** do VR:

- Any GPU without Vulkan support (nouveau, Lima, older Panfrost)
- Software renderers (llvmpipe, lavapipe)
- VirtIO GPU (no direct hardware access)
- NVIDIA proprietary < 535 (no GBM)

## Troubleshooting

### Environment Variables

```bash
# Force GLES2 renderer (needed for NVIDIA GBM)
export WLR_RENDERER=gles2

# Select specific DRM device (multi-GPU systems)
export WLR_DRM_DEVICES=/dev/dri/card0

# Disable hardware cursors (NVIDIA flicker workaround)
export WLR_NO_HARDWARE_CURSORS=1

# NVIDIA GBM backend hint
export GBM_BACKEND=nvidia-drm

# Mesa debug output
export MESA_DEBUG=1
export EGL_LOG_LEVEL=debug

# Vulkan driver selection (for Monado/OpenXR)
export VK_ICD_FILENAMES=/usr/share/vulkan/icd.d/radeon_icd.x86_64.json

# DRM debug (kernel log)
echo 0x1f | sudo tee /sys/module/drm/parameters/debug
```

### Diagnostic Commands

```bash
# Check EGL/GBM support
eglinfo | grep -i gbm

# List DRM devices and their drivers
ls -la /dev/dri/
for card in /dev/dri/card*; do
    echo "$card: $(udevadm info -q property "$card" | grep DRIVER)"
done

# Check for non-desktop connectors (HMD displays)
for card in /dev/dri/card*; do
    echo "=== $card ==="
    drm_info "$card" 2>/dev/null | grep -A2 "non-desktop"
done

# Vulkan device enumeration
vulkaninfo --summary 2>/dev/null

# Test DMA-BUF support
weston-simple-dmabuf-drm  # from weston-clients package

# Mesa driver in use
glxinfo | grep "OpenGL renderer"
```

### Common Failures

| Symptom | Likely Cause | Fix |
|---------|-------------|-----|
| "No DRM devices found" | Missing seat access | Check `loginctl seat-status`; ensure libseat configured |
| "Failed to create GBM device" | NVIDIA < 535 or missing mesa | Upgrade driver or install `mesa-libgbm` |
| "EGL_KHR_platform_gbm not found" | Wrong EGL implementation | Set `__EGL_VENDOR_LIBRARY_FILENAMES` to Mesa's ICD |
| "DRM lease failed" | GPU does not support leasing | Check `drm_info` for lease capability |
| Black screen after launch | Renderer initialization failed | Try `WLR_RENDERER=gles2` and check journal |
| < 30 fps on desktop | llvmpipe / nouveau un-reclocked | Verify hardware acceleration with `glxinfo` |

## References

- [Smithay renderer documentation](https://smithay.github.io/smithay/)
- [Mesa driver matrix](https://mesamatrix.net/)
- [Monado supported GPUs](https://monado.freedesktop.org/)
- [wlroots DRM lease protocol](https://wayland.app/protocols/wp-drm-lease-v1)
- [NVIDIA GBM support announcement](https://download.nvidia.com/XFree86/Linux-x86_64/535.43.02/README/gbm.html)
