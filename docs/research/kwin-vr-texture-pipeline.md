# R3.1: KWin VR DMA-BUF Texture Import Pipeline

## Overview

KDE KWin's VR Mode (MR !8671) adds native VR desktop support directly within the
KWin compositor. Rather than building an external mirroring tool, the implementation
embeds VR rendering inside KWin itself, using Qt Quick 3D XR as the rendering
framework and OpenXR as the VR runtime abstraction. The key design decision is
**direct DMA-BUF import** of Wayland client buffers into the VR rendering pipeline,
avoiding expensive GPU-to-CPU-to-GPU copies.

Source: https://invent.kde.org/plasma/kwin/-/merge_requests/8671

## Architecture

```
Wayland Client
    |
    v  (wl_buffer / DMA-BUF)
KWin Compositor Core
    |
    v  (DMA-BUF fd + metadata)
EGLImage Creation (EGL_EXT_image_dma_buf_import)
    |
    v  (EGLImageKHR)
GL Texture Binding (glEGLImageTargetTexture2DOES)
    |
    v  (GL_TEXTURE_2D / GL_TEXTURE_EXTERNAL_OES)
Qt Quick 3D XR Scene Graph
    |
    v  (rendered stereo frames)
OpenXR Swapchain Submission
    |
    v  (xrEndFrame)
VR Runtime (Monado / WiVRn / SteamVR)
    |
    v
HMD Display
```

## DMA-BUF to EGLImage Pipeline

### Step 1: DMA-BUF Acquisition

Wayland clients submit rendered frames as `wl_buffer` objects. When the buffer is
backed by DMA-BUF (via the `zwp_linux_dmabuf_v1` protocol), the compositor receives:

- **File descriptor(s)** (`fd`) -- one per plane (most formats use 1 plane; YUV may use 2-3)
- **DRM fourcc format** (e.g., `DRM_FORMAT_ARGB8888`, `DRM_FORMAT_XRGB8888`)
- **Stride** (bytes per row) per plane
- **Offset** per plane
- **DRM format modifier** (uint64, describes tiling/compression layout)

### Step 2: EGLImage Creation

The EGLImage is created via `eglCreateImageKHR` with the `EGL_LINUX_DMA_BUF_EXT`
target. The call requires the following EGL attributes:

```c
EGLint attribs[] = {
    EGL_LINUX_DRM_FOURCC_EXT,           format,
    EGL_WIDTH,                           width,
    EGL_HEIGHT,                          height,
    EGL_DMA_BUF_PLANE0_FD_EXT,          fd,
    EGL_DMA_BUF_PLANE0_OFFSET_EXT,      offset,
    EGL_DMA_BUF_PLANE0_PITCH_EXT,       stride,
    // With EGL_EXT_image_dma_buf_import_modifiers:
    EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT, modifier_lo,
    EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT, modifier_hi,
    EGL_NONE
};

EGLImageKHR image = eglCreateImageKHR(
    egl_display,
    EGL_NO_CONTEXT,       // no GL context needed
    EGL_LINUX_DMA_BUF_EXT,
    NULL,                 // no client buffer
    attribs
);
```

**Required EGL extensions:**
- `EGL_EXT_image_dma_buf_import` -- basic DMA-BUF import
- `EGL_EXT_image_dma_buf_import_modifiers` -- format modifier support (critical for
  modern GPUs that use tiled/compressed layouts)

### Step 3: GL Texture Binding

The EGLImage is bound to an OpenGL texture for use in the rendering pipeline:

```c
GLuint texture;
glGenTextures(1, &texture);
glBindTexture(GL_TEXTURE_2D, texture);
glEGLImageTargetTexture2DOES(GL_TEXTURE_2D, image);
```

For some buffer formats (especially YUV), `GL_TEXTURE_EXTERNAL_OES` is used instead
of `GL_TEXTURE_2D`, requiring `OES_EGL_image_external` and a different sampler in
shaders (`samplerExternalOES`).

### Step 4: Qt Quick 3D XR Scene Rendering

KWin's VR plugin renders each Wayland window as a textured quad in 3D space using
Qt Quick 3D XR. The GL texture from Step 3 becomes a material texture on a 3D model
node. The scene includes:

- Window content as textured rectangles
- Window decorations and shadows (rendered by KWin's decoration engine)
- 3D spatial positioning (user-configurable per window)

Qt Quick 3D XR handles stereo rendering automatically -- it renders the scene twice
(once per eye) with appropriate view/projection matrices from the OpenXR runtime.

### Step 5: OpenXR Frame Submission

The rendered stereo frames are submitted to the VR runtime via the standard OpenXR
frame loop:

```
Per frame:
1. xrWaitFrame()           -- get predicted display time, throttle to HMD refresh
2. xrBeginFrame()          -- signal start of GPU work
3. For each view (eye):
   a. xrAcquireSwapchainImage() -- get VkImage/GLuint index
   b. xrWaitSwapchainImage()    -- block until compositor releases image
   c. [Render scene to swapchain image]
   d. xrReleaseSwapchainImage() -- hand image back to compositor
4. xrEndFrame()            -- submit XrCompositionLayerProjection with all views
```

The `XrCompositionLayerProjection` contains per-view data:
- Swapchain reference and sub-image rectangle
- View pose (position + orientation from head tracking)
- Field of view (per-eye frustum from the runtime)

## Why Direct Integration in KWin

The MR author explicitly chose to embed VR inside KWin rather than creating a
separate process because:

1. **Zero-copy buffer path** -- DMA-BUF fds are imported directly; no intermediate
   copies or offscreen renders needed
2. **Latency** -- extra IPC or buffer copies severely impact VR comfort at high
   resolutions (typically 2x 2064x2208 for Quest 3)
3. **Decoration integration** -- window shadows, borders, and effects are available
   natively within the compositor

## GPU Driver Requirements

### Mesa Requirements
- **Mesa >= 21.1** -- required for `VK_EXT_image_drm_format_modifier` (Vulkan side)
- **Mesa >= 21.2** -- required for `VK_EXT_physical_device_drm` (GPU device matching)
- Both `EGL_EXT_image_dma_buf_import` and `EGL_EXT_image_dma_buf_import_modifiers`
  must be supported by the driver

### GPU Vendor Support
- **AMD (radeonsi/radv)** -- full support in Mesa; tiled formats work with modifiers
- **Intel (iris/ANV)** -- full support in Mesa
- **NVIDIA** -- requires proprietary driver with GBM support (515+); historically
  used EGLStreams which is incompatible with this pipeline. NVK (open-source) is
  gaining modifier support.

### OpenXR Runtime Requirements
- KWin reads `/etc/xdg/openxr/1/active_runtime.json` (does NOT read `XR_RUNTIME_JSON`)
- Tested runtimes: Monado (Rokid Max, HP G2), WiVRn (Quest 3)

## Mapping to Smithay APIs

### DMA-BUF Import

Smithay provides equivalent functionality through its renderer traits:

| KWin/EGL Concept | Smithay Equivalent |
|---|---|
| `eglCreateImageKHR(EGL_LINUX_DMA_BUF_EXT)` | `GlesRenderer::import_dmabuf()` / `ImportDma` trait |
| `glEGLImageTargetTexture2DOES` | Internal to `GlesRenderer` (automatic) |
| DMA-BUF format negotiation | `ImportDma::dmabuf_formats()` |
| `Bind<Dmabuf>` for render targets | `GlesRenderer: Bind<Dmabuf>` |
| EGLDisplay dmabuf capabilities | `EGLDisplay` methods |

### Smithay DMA-BUF Import Flow

```rust
// 1. Client submits dmabuf via zwp_linux_dmabuf_v1
// 2. DmabufHandler::dmabuf_imported() is called
fn dmabuf_imported(&mut self, dmabuf: &Dmabuf, notifier: ImportNotifier) {
    // 3. Import into renderer (creates EGLImage + GL texture internally)
    if self.renderer.import_dmabuf(&dmabuf, None).is_ok() {
        notifier.successful::<State>();
    } else {
        notifier.failed();
    }
}

// 4. During rendering, the imported dmabuf is available as a GlesTexture
//    which can be used in render_elements
```

### Key Differences for VR

Smithay's current pipeline targets 2D output via DRM/KMS. For VR, we would need to:

1. **Replace the output backend** -- instead of submitting to a DRM CRTC, render to
   OpenXR swapchain images
2. **Add stereo rendering** -- render the scene twice per frame with different
   view/projection matrices
3. **Integrate OpenXR frame loop** -- Smithay's event loop needs to accommodate
   `xrWaitFrame` timing
4. **Export textures to OpenXR** -- either render directly into OpenXR swapchain
   images (via `Bind<>` on the swapchain VkImage/GLuint), or blit from Smithay's
   render target to the swapchain

### Recommended Approach for EXWM-VR

```
Smithay GlesRenderer
    |
    v  (ImportDma::import_dmabuf -- creates EGLImage internally)
GlesTexture (per Wayland window)
    |
    v  (render_elements into offscreen FBO or directly into swapchain)
OpenXR Swapchain Image (GL texture from xrEnumerateSwapchainImages)
    |
    v  (xrReleaseSwapchainImage + xrEndFrame)
VR Runtime
```

The critical integration point is binding Smithay's `GlesRenderer` to render into
OpenXR swapchain images. This could be done by:

1. Getting the GL texture ID from `XrSwapchainImageOpenGLKHR`
2. Creating a `GlesTexture` wrapper around it
3. Using `Bind<GlesTexture>` to set it as the render target
4. Calling `Space::render_elements_for_output()` to render the scene

## Current Status and Caveats

- KWin VR MR is still in **draft** status (as of early 2026)
- Requires patched Qt Quick 3D (6.10.2 patches, most upstreamed)
- Requires patched XWayland for proper surface handling
- Missing features: headgaze control, surface thickness settings
- The approach is tightly coupled to Qt's rendering -- our Smithay approach will
  need to implement equivalent 3D scene management ourselves

## References

- KWin VR MR: https://invent.kde.org/plasma/kwin/-/merge_requests/8671
- EGL_EXT_image_dma_buf_import spec: https://registry.khronos.org/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import.txt
- OpenXR Tutorial (Vulkan graphics): https://openxr-tutorial.com/linux/vulkan/3-graphics.html
- Smithay GlesRenderer docs: https://smithay.github.io/smithay/smithay/backend/renderer/gles/struct.GlesRenderer.html
- Smithay DmabufHandler docs: https://smithay.github.io/smithay/smithay/wayland/dmabuf/trait.DmabufHandler.html
