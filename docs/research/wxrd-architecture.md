# R3.2: wxrd (Collabora VR Compositor) Architecture

## Overview

wxrd is a standalone Wayland compositor for VR/XR, developed by Collabora as part
of the xrdesktop project (sponsored by Valve). It is built on wlroots and uses a
custom Vulkan renderer backed by the gulkan library. wxrd takes a fundamentally
different approach from KWin VR -- it runs as a **standalone compositor** where
applications are launched directly into the VR environment, rather than mirroring
an existing 2D desktop.

Source: https://www.collabora.com/news-and-blog/news-and-events/wxrd-a-standalone-wayland-compositor-for-xrdesktop.html

## Architecture Overview

```
                    OpenXR Runtime (Monado / SteamVR)
                         |
                         v  (VkInstance, VkDevice)
                    +---------+
                    |   gxr   |  (XR runtime abstraction)
                    +---------+
                         |
                    +-----------+
                    | xrdesktop |  (window management in 3D)
                    +-----------+
                         |
     +-------------------+-------------------+
     |                                       |
+---------+                           +-------------+
| gulkan  |  (Vulkan abstraction)     | wxrd_renderer|
+---------+                           +-------------+
     |                                       |
     v                                       v
VkImage textures <---- DMA-BUF import ---- wlroots
     |                                       |
     v                                       v
OpenXR Swapchain                     Wayland Clients
```

## Component Stack

### gulkan -- Vulkan Abstraction Library

Gulkan is a GLib-based library providing high-level abstractions over Vulkan:

- **VkInstance / VkDevice management** -- wraps Vulkan device initialization
- **Texture creation** -- from CPU memory, GdkPixbuf, Cairo surfaces, and DMA-BUFs
- **Shader management** -- SPIR-V shader loading and pipeline setup
- **DMA-BUF import** -- converts DMA-BUF file descriptors into VkImage objects

Critical capability: gulkan can import DMA-BUF memory into Vulkan textures, which
is the bridge between Wayland client buffers and VR rendering.

### gxr -- XR Runtime Abstraction

Gxr abstracts over different VR runtimes:

- Currently supports OpenVR (SteamVR)
- OpenXR support planned/in development (enabling FOSS runtimes like Monado)
- Provides unified API for session management, controller tracking, and rendering

### xrdesktop -- VR Desktop Window Management

Manages the 3D spatial layout of 2D windows:

- **Window placement** -- initializes windows in 3D space with configurable
  Pixels-per-Meter (PPM) setting
- **Window manipulation** -- grab, move, resize, scale in 3D
- **Input routing** -- maps VR controller interactions to window input events
- **Scene graph** -- maintains spatial relationships between windows

### wxrd -- The Compositor

Ties everything together as a wlroots-based Wayland compositor with a custom
Vulkan renderer.

## Vulkan Rendering Pipeline

### The VkInstance/VkDevice Problem

A key architectural challenge: wlroots' built-in Vulkan renderer creates its own
`VkInstance` and `VkDevice`, but OpenXR requires the application to use the
`VkInstance` and `VkDevice` **provided by the runtime**. These cannot be mixed.

wxrd solves this by implementing a **custom wlroots renderer** (`wxrd_renderer`)
that:

1. Receives VkInstance/VkDevice from the OpenXR runtime (via gxr)
2. Uses these for all Vulkan operations (via gulkan)
3. Implements the wlroots renderer interface for compositor integration

### DMA-BUF Import into Vulkan

The import pipeline for Wayland client buffers:

```
Wayland Client
    |
    v  (wl_buffer backed by DMA-BUF)
wlroots buffer handling
    |
    v  (DMA-BUF fd, format, modifier, stride, offset)
gulkan DMA-BUF import
    |
    v  (VkImage via VK_EXT_external_memory_fd + VK_EXT_image_drm_format_modifier)
VkImageView
    |
    v  (sampled in fragment shader)
3D quad rendering in VR scene
```

### The Tiled Format Challenge

Importing DMA-BUF memory with **linear** layout is straightforward. However, modern
GPUs store textures in **tiled** and **compressed** formats for bandwidth efficiency.
When sharing buffers between Wayland clients and the VR renderer, both sides must
agree on the memory layout.

This requires `VK_EXT_image_drm_format_modifier`, which:

- Allows Vulkan to understand DRM format modifiers (uint64 values encoding
  vendor-specific tiling/compression schemes)
- Enables correct import of tiled DMA-BUF memory
- Reports supported modifier+format combinations to Wayland clients via
  `get_dmabuf_texture_formats()`

**Multi-plane formats** add further complexity:
- Some modifiers store pixel data in plane 0 and compression metadata in plane 1
- Both planes may share the same underlying DMA-BUF fd with different offsets
- The Vulkan driver must bind all planes correctly to reconstruct the image

### Required Vulkan Extensions

| Extension | Mesa Version | Purpose |
|---|---|---|
| `VK_EXT_image_drm_format_modifier` | >= 21.1 | Import tiled/compressed DMA-BUFs |
| `VK_EXT_physical_device_drm` | >= 21.2 | Match GPU device to DRM render node |
| `VK_KHR_external_memory` | (core 1.1) | Share memory between processes |
| `VK_KHR_external_memory_fd` | (core 1.1) | Import memory via file descriptor |

## 2D Wayland Surfaces to 3D Quads

### Surface Acquisition

1. Application connects to wxrd's Wayland socket (`WAYLAND_DISPLAY=wayland-0`)
2. Application renders frames normally (it doesn't know it's in VR)
3. wlroots receives the committed `wl_buffer` (SHM or DMA-BUF)
4. wxrd_renderer imports the buffer as a VkImage texture

### 3D Projection

xrdesktop manages window placement in 3D:

- Each window gets a **3D pose** (position + orientation in world space)
- Window size is determined by pixel dimensions and the global **PPM** setting
  (Pixels Per Meter), allowing font readability tuning for VR
- Windows are rendered as textured quads: two triangles with the client's
  buffer as the texture
- Window decorations are rendered as additional geometry around the content quad

### Pixel Density

Unlike monitor-based compositors where resolution is fixed, wxrd adjusts pixel
density independently:

- The PPM setting controls how large windows appear in 3D space
- Higher PPM = smaller windows but sharper text
- Lower PPM = larger windows but blurrier text
- This is analogous to DPI scaling but in 3D metric space

## Input Ray-Casting

### VR Controller Input

wxrd processes VR controller input through xrdesktop:

1. **Pose tracking** -- controller position and orientation in 3D space
2. **Ray casting** -- a ray from the controller tip into the scene
3. **Hit testing** -- determine which window quad (if any) the ray intersects
4. **UV mapping** -- convert the 3D intersection point to 2D pixel coordinates
   on the window surface
5. **Wayland event synthesis** -- generate `wl_pointer` motion/button events at
   the computed pixel coordinates

### Keyboard Input

wxrd implements a novel approach to keyboard input:

- Physical keyboard events from the wlroots window are forwarded to the focused
  VR window
- For programmatic text input, wxrd **dynamically generates custom keymaps**:
  it creates a temporary XKB keymap containing exactly the characters needed,
  sets it as active, sends the corresponding key codes, then restores the
  original keymap
- This enables full Unicode and emoji support without relying on the (incomplete)
  `zwp_virtual_keyboard_v1` protocol

### Focus Model

- Window focus is driven by the VR controller cursor position
- The window whose quad the controller ray intersects receives focus
- This replaces the traditional mouse-driven focus model

## Comparison Against Our Smithay Approach

### wxrd Advantages

| Aspect | wxrd | Our Smithay Approach |
|---|---|---|
| **Maturity** | Prototype but functional | Not yet started |
| **Vulkan native** | Yes (gulkan) | No (Smithay uses GLES primarily) |
| **OpenXR integration** | Via gxr | Must build from scratch |
| **wlroots ecosystem** | Inherits wlroots protocol support | Smithay has own protocol stack |
| **Language** | C (GLib) | Rust (safer, but more work) |

### wxrd Limitations

| Aspect | wxrd | Our Smithay Approach |
|---|---|---|
| **Standalone only** | Cannot mirror existing desktop | Can potentially do both |
| **wlroots version** | Pinned to 0.15 (stale) | Smithay actively maintained |
| **Build complexity** | Custom branches of 4+ libraries | Single Smithay dependency |
| **Renderer flexibility** | Vulkan only | GLES + potential Vulkan |
| **Rust safety** | C with manual memory mgmt | Rust ownership model |
| **Protocol coverage** | wlroots 0.15 era | Smithay 0.7 (modern protocols) |

### Key Lessons for EXWM-VR

1. **Custom renderer is mandatory** -- wxrd's biggest contribution is proving that
   a custom renderer (replacing the default wlroots one) is necessary to share
   VkInstance/VkDevice with OpenXR. We will face the same challenge with Smithay's
   GlesRenderer.

2. **DMA-BUF format modifier support is critical** -- without
   `VK_EXT_image_drm_format_modifier`, only linear buffers can be imported, causing
   severe performance issues or fallback to CPU copies. Our renderer must support
   modifiers.

3. **PPM-based scaling is the right model** -- traditional DPI scaling doesn't
   translate to VR. We should adopt a similar Pixels-Per-Meter approach.

4. **Ray-cast input works** -- wxrd validates that ray-based pointer input is
   viable for interacting with 2D windows in 3D space. We should implement the
   same ray-to-UV-to-wl_pointer pipeline.

5. **Dynamic keymap generation is clever but fragile** -- for keyboard input,
   we should consider whether Smithay's input handling provides better
   alternatives via `wl_keyboard` directly.

### Architectural Recommendations

For our Smithay-based compositor:

1. **Use Smithay's GLES renderer initially** with `ImportDma` for zero-copy buffer
   sharing. The GLES path is more mature in Smithay.

2. **Render to offscreen FBO, then blit to OpenXR swapchain** -- this avoids the
   VkInstance sharing problem entirely for the initial prototype. Performance cost
   is one extra blit per eye per frame.

3. **Long-term: investigate Smithay Vulkan renderer** or implement a custom one that
   can accept an externally-provided VkDevice from the OpenXR runtime, similar to
   wxrd's approach.

4. **Adopt wxrd's window-in-3D-space model** -- each Wayland surface becomes a
   textured quad with a 3D pose, using PPM for size scaling.

## Current Status (2026)

wxrd development appears to have slowed since its initial 2021 announcement. The
project remains at prototype quality. Key repositories:

- wxrd: https://github.com/patchedsoul/wxrd (also on freedesktop GitLab)
- gulkan: https://gitlab.freedesktop.org/xrdesktop/gulkan
- gxr: https://gitlab.freedesktop.org/xrdesktop/gxr
- xrdesktop: https://gitlab.freedesktop.org/xrdesktop/xrdesktop

The wlroots 0.15 dependency is significantly out of date (current wlroots is 0.18+),
making wxrd difficult to build and use on modern systems.

## References

- wxrd announcement: https://www.collabora.com/news-and-blog/news-and-events/wxrd-a-standalone-wayland-compositor-for-xrdesktop.html
- xrdesktop overview: https://www.collabora.com/news-and-blog/news-and-events/moving-the-linux-desktop-to-another-reality.html
- wxrd source: https://github.com/patchedsoul/wxrd
- VK_EXT_image_drm_format_modifier spec: https://registry.khronos.org/vulkan/specs/latest/man/html/VK_EXT_image_drm_format_modifier.html
- Implementing DRM format modifiers in NVK: https://www.collabora.com/news-and-blog/news-and-events/implementing-drm-format-modifiers-in-nvk.html
- OpenXR Tutorial (Vulkan): https://openxr-tutorial.com/linux/vulkan/3-graphics.html
