# R8.2 / R8.3: Foveated Rendering & DMA-BUF Format Compatibility

Research date: 2026-02-09

---

## Part 1: DMA-BUF Format Compatibility for EGL Import (R8.3)

### Overview

The EXWM-VR compositor imports Wayland client buffers as DMA-BUFs and composites them
into OpenXR swapchain images via Smithay's `GlesRenderer`. The EGL import path
(`eglCreateImageKHR` with `EGL_LINUX_DMA_BUF_EXT`) requires the GPU driver to understand
both the DRM fourcc format and the format modifier (tiling/compression layout) of the
buffer. Format/modifier mismatches cause import failure, and some combinations require
expensive conversion. This section catalogs which combinations work across AMD, Intel,
and NVIDIA drivers.

### DRM Format Reference

| Format | Fourcc | Bits/Pixel | Planes | Alpha | Typical Use |
|--------|--------|-----------|--------|-------|-------------|
| `DRM_FORMAT_ARGB8888` | `AR24` | 32 | 1 | Yes | Desktop compositing, alpha-blended windows |
| `DRM_FORMAT_XRGB8888` | `XR24` | 32 | 1 | No | Opaque windows, KMS primary plane |
| `DRM_FORMAT_ABGR8888` | `AB24` | 32 | 1 | Yes | OpenGL/Vulkan default (reversed channel order) |
| `DRM_FORMAT_XBGR8888` | `XB24` | 32 | 1 | No | Vulkan default opaque |
| `DRM_FORMAT_NV12` | `NV12` | 12 | 2 | No | Video decode, camera capture (YUV 4:2:0) |

**Channel order note**: DRM formats are defined in memory byte order. `ARGB8888` stores
bytes as B, G, R, A (little-endian) -- the "ARGB" refers to the high-to-low bit layout
in a 32-bit word. OpenGL/Vulkan natively produce `ABGR8888` (GL_RGBA). Wayland clients
using EGL typically produce `ARGB8888` or `XRGB8888` because Mesa's EGL platform code
handles the swizzle. Video decoders produce `NV12`.

### DRM Format Modifiers Reference

| Modifier | Value | Meaning |
|----------|-------|---------|
| `DRM_FORMAT_MOD_LINEAR` | `0x0` | Row-major, left-to-right, top-to-bottom. Universal baseline. Every GPU and display controller supports this. |
| `DRM_FORMAT_MOD_INVALID` | `0x00ffffffffffffff` | "Implementation-defined." The producer did not specify a modifier. The consumer must infer the layout, typically by assuming linear or by querying the allocator. Legacy behavior before modifiers were standardized. |
| `I915_FORMAT_MOD_X_TILED` | vendor-specific | Intel X-tiling: 512-byte rows within tiles. Supported since early i915 hardware. Good for scanout. |
| `I915_FORMAT_MOD_Y_TILED` | vendor-specific | Intel Y-tiling: 128-byte-wide, 32-row tiles. Better cache locality for 3D rendering than X-tiling. Supported Gen9+ (Skylake). |
| `I915_FORMAT_MOD_Y_TILED_CCS` | vendor-specific | Y-tiled with Color Control Surface (lossless compression). Gen9-Gen11. Auxiliary surface stores compression metadata. |
| `I915_FORMAT_MOD_4_TILED` | vendor-specific | Gen12.5+ (Meteor Lake, Lunar Lake). Replaces Y-tiled. New tile layout optimized for modern memory controllers. |
| AMD GFX9+ tiled | vendor-specific | Macro-tiled layouts with configurable pipe/bank swizzle. Encoded as `AMD_FMT_MOD(TILE, TILE_VERSION, PIPE_XOR_BITS, BANK_XOR_BITS, ...)`. |
| AMD DCC | vendor-specific | Delta Color Compression. Lossless, block-based bandwidth compression. Adds auxiliary metadata surface. GCN 1.2+ (Tonga, Fiji, Polaris, Vega, RDNA). |
| NVIDIA (NVK) | vendor-specific | Block-linear tiling with PTE-kind encoding. Tiling parameters baked into GPU page table entries. NVK (open-source Mesa) gained modifier support in Mesa 24.2. |

### Format/Modifier Compatibility by Driver

#### AMD radeonsi (Mesa)

| Format | `MOD_LINEAR` | `MOD_INVALID` | AMD Tiled | AMD DCC | Notes |
|--------|-------------|---------------|-----------|---------|-------|
| `ARGB8888` | OK | OK (implicit tiled) | OK | OK | Primary desktop format. DCC provides ~20-30% bandwidth savings. |
| `XRGB8888` | OK | OK | OK | OK | Preferred for scanout (no alpha). |
| `ABGR8888` | OK | OK | OK | OK | Vulkan/GL native order. |
| `NV12` | OK | OK | OK | DCC on RDNA4+ (Mesa 25.1) | Multi-plane DCC landed for GFX12 (RDNA4). Older GPUs: tiled but no DCC for multi-plane. |

**DCC details**: Delta Color Compression processes blocks of pixels, storing one value at
full precision and the rest as deltas. If colors within a block are similar, the delta
values require fewer bits, reducing memory bandwidth. DCC is lossless and transparent to
the shader. The DCC metadata is stored in an auxiliary surface, and the modifier encodes
the DCC parameters (pipe/bank XOR bits, max compressed block size). DCC is enabled by
default on GCN 1.2+ for render targets and textures. For DMA-BUF import, the importing
driver must understand the DCC modifier to decompress on read; otherwise, import fails
or produces garbage.

**EGL import path**: Mesa's radeonsi + EGL supports both
`EGL_EXT_image_dma_buf_import` and `EGL_EXT_image_dma_buf_import_modifiers`. When the
modifier extension is present, clients and compositors negotiate format+modifier pairs
via `zwp_linux_dmabuf_v1`. The compositor advertises supported pairs from
`ImportDma::dmabuf_formats()`. Clients pick from the intersection.

#### Intel iris (Mesa)

| Format | `MOD_LINEAR` | `MOD_INVALID` | X-tiled | Y-tiled | Y-tiled CCS | Tile4 |
|--------|-------------|---------------|---------|---------|-------------|-------|
| `ARGB8888` | OK | OK | OK | OK (Gen9+) | OK (Gen9-11) | OK (Gen12.5+) |
| `XRGB8888` | OK | OK | OK | OK | OK | OK |
| `ABGR8888` | OK | OK | OK | OK | OK | OK |
| `NV12` | OK | OK | X-tiled OK | Y-tiled OK | No CCS for NV12 | Limited |

**Tiling evolution**: Intel has gone through several tiling generations:
- Gen1-8 (pre-Skylake): X-tiling only for scanout, linear for everything else.
- Gen9-11 (Skylake through Ice Lake): Y-tiling for 3D, X-tiling for scanout. CCS
  compression available with Y-tiled.
- Gen12-12.4 (Tiger Lake, Alder Lake): Y-tiled with Gen12 CCS (RC_CCS, RC_CCS_CC).
  There was a Mesa issue (mesa#6459) where `I915_FORMAT_MOD_Y_TILED_GEN12_RC_CCS` was
  unsupported in the ANV Vulkan driver for import.
- Gen12.5+ (Meteor Lake, Lunar Lake): Tile4 replaces Y-tiled entirely. New compression
  formats.

**CCS import caveat**: When a client allocates a Y-tiled CCS buffer and the compositor
imports it, the compositor's EGL must support the same CCS modifier. If the compositor
runs on a different GPU generation or a different driver version, CCS import can fail.
In practice, same-GPU import works reliably. Cross-GPU scenarios (e.g., Intel iGPU
client, discrete AMD compositor) fall back to `MOD_LINEAR`.

#### NVIDIA

**Proprietary driver (535+)**:

| Format | `MOD_LINEAR` | `MOD_INVALID` | Block-linear | Notes |
|--------|-------------|---------------|--------------|-------|
| `ARGB8888` | OK | OK | Limited | Proprietary driver gained GBM support in 495+. DMA-BUF v4 protocol support in 535. |
| `XRGB8888` | OK | OK | Limited | Modifier advertisement is minimal; most imports use linear. |
| `ABGR8888` | OK | OK | Limited | |
| `NV12` | Problematic | Problematic | No | KWin bug 502844: NV12 not advertised via `zwp_linux_dmabuf_v1` with NVIDIA. |

The NVIDIA proprietary driver historically used EGLStreams instead of GBM/DMA-BUF. GBM
support was added starting with driver 495. The driver supports
`EGL_EXT_image_dma_buf_import` and `EGL_EXT_image_dma_buf_import_modifiers`, but modifier
advertisement is conservative -- most buffers are allocated linear or with
`DRM_FORMAT_MOD_INVALID`. Block-linear tiling is used internally but not reliably exposed
through the DMA-BUF modifier protocol. The result is functional but suboptimal: linear
layouts waste memory bandwidth compared to tiled layouts.

**NVK (open-source Mesa Vulkan driver)**:

NVK gained DRM format modifier support in Mesa 24.2 (mid-2024). The implementation was
complex because NVIDIA GPUs encode tiling information in page table entries (PTE kind),
not just in the buffer layout. NVK assigns unique virtual address ranges to each image
via `VM_BIND` to handle different PTE kinds. This support enables proper tiled buffer
sharing between NVK and compositors (e.g., Gamescope tested successfully). However, NVK
is Vulkan-only; there is no NVK-based EGL/OpenGL driver. The Nouveau GL driver has
known bugs around modifier handling (ignores imported modifiers, lacks PTE kind override).

**Practical recommendation for NVIDIA**: Prefer `DRM_FORMAT_MOD_LINEAR` for cross-process
sharing. Accept `DRM_FORMAT_MOD_INVALID` and treat as linear. Do not expect tiled modifiers
to work reliably in EGL import on NVIDIA.

### Formats Requiring Conversion (Performance Penalty)

| Scenario | Conversion | Cost | Notes |
|----------|-----------|------|-------|
| `NV12` (YUV) -> RGB sampling | YUV-to-RGB color space conversion in shader | Moderate (~10-15% per texture sample) | GPU inserts conversion instructions into fragment shader via `samplerExternalOES`. Not a CPU copy, but adds ALU work per pixel. |
| `NV12` multi-plane -> single-plane | Plane merging | Low-moderate | Some EGL implementations require separate EGLImages per plane, then shader combines them. |
| Linear -> tiled (compositor-side) | GPU blit | Significant for large buffers | If client produces linear and compositor needs tiled for rendering: `glBlitFramebuffer` or `glCopyTexImage`. At 4K RGBA, one copy = ~33MB. At 90Hz = ~3 GB/s. |
| Tiled -> linear (readback) | GPU blit | Significant | Only needed for screen capture or CPU-side processing. Not in the VR hot path. |
| `ARGB8888` <-> `ABGR8888` | Channel swizzle | Negligible | GL texture swizzle parameters or shader swizzle. No data copy. |
| DCC compressed -> uncompressed | GPU decompression pass | Low | Transparent to the application; handled by the GPU's texture unit. Only an issue if the importing driver does not understand DCC. |

**Key takeaway**: The only conversion with meaningful performance impact in the VR path is
linear-to-tiled blit for large buffers. This is avoided by proper format/modifier
negotiation: the compositor advertises its preferred tiled modifiers, and clients allocate
in a compatible tiled format. The `zwp_linux_dmabuf_v1` v4 feedback mechanism
(`DmabufFeedback`) further optimizes this by telling clients which device and tranches are
preferred for scanout vs. rendering.

### Smithay DMA-BUF Import Architecture

#### DmabufHandler Trait

The `DmabufHandler` trait is the Wayland protocol interface for DMA-BUF import validation:

```rust
trait DmabufHandler {
    fn dmabuf_state(&self) -> &mut DmabufState;

    fn dmabuf_imported(
        &mut self,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
        notifier: ImportNotifier,
    );

    fn new_surface_feedback(
        &mut self,
        surface: &WlSurface,
    ) -> Option<DmabufFeedback> {
        None
    }
}
```

When a client submits a DMA-BUF, `dmabuf_imported()` is called with the `Dmabuf` object
(containing fd, format, modifier, stride, offset per plane) and an `ImportNotifier`. The
compositor attempts to import into its renderer and calls `notifier.successful()` or
`notifier.failed()`.

#### ImportDma Trait

The `ImportDma` trait is implemented by renderers (e.g., `GlesRenderer`) to perform the
actual EGL import:

```rust
trait ImportDma {
    fn import_dmabuf(
        &mut self,
        dmabuf: &Dmabuf,
        damage: Option<&[Rectangle<i32, BufferCoords>]>,
    ) -> Result<GlesTexture, ...>;

    fn dmabuf_formats(&self) -> FormatSet;

    fn has_dmabuf_format(&self, format: Format) -> bool;
}
```

- `dmabuf_formats()` returns the set of `(Fourcc, Modifier)` pairs the renderer can
  import. This is queried from EGL via `eglQueryDmaBufFormatsEXT()` and
  `eglQueryDmaBufModifiersEXT()`.
- `import_dmabuf()` creates an EGLImage from the DMA-BUF attributes, then binds it to
  a GL texture via `glEGLImageTargetTexture2DOES`. The resulting `GlesTexture` wraps the
  GL texture ID.

#### DmabufState and Format Advertisement

```rust
// Create DmabufState with format feedback (v4 protocol)
let formats: Vec<Format> = renderer.dmabuf_formats().collect();
let default_feedback = DmabufFeedbackBuilder::new(render_node.dev_id(), formats.clone())
    .build()
    .unwrap();
let dmabuf_state = DmabufState::new();
let dmabuf_global = dmabuf_state.create_global_with_default_feedback::<State>(
    &display_handle,
    &default_feedback,
);

// Per-surface feedback for scanout optimization
impl DmabufHandler for State {
    fn new_surface_feedback(&mut self, surface: &WlSurface) -> Option<DmabufFeedback> {
        // If this surface can be scanned out directly, add a scanout tranche
        // with the KMS plane's supported formats
        let scanout_formats = self.drm_surface.supported_formats();
        Some(DmabufFeedbackBuilder::new(self.render_node.dev_id(), self.render_formats.clone())
            .add_preference_tranche(self.drm_node.dev_id(), Some(TrancheFlags::Scanout), scanout_formats)
            .build()
            .unwrap())
    }
}
```

#### Import Flow for VR Rendering

In the EXWM-VR compositor, the DMA-BUF import flow is:

```
1. Client allocates DMA-BUF (via GBM/Mesa EGL)
   - Format: ARGB8888 or XRGB8888 (most Wayland clients)
   - Modifier: driver-preferred (tiled on AMD/Intel, linear on NVIDIA)

2. Client submits buffer via zwp_linux_dmabuf_v1
   - dmabuf_imported() called with Dmabuf object

3. Compositor validates: renderer.import_dmabuf(&dmabuf)
   - EGLImage created from DMA-BUF fd + metadata
   - GL texture bound to EGLImage (zero-copy)
   - GlesTexture returned

4. During VR frame:
   - Smithay GlesRenderer renders window textures into FBO
   - FBO content copied/blitted to OpenXR swapchain image
   - xrReleaseSwapchainImage() + xrEndFrame()
```

**VR-specific consideration**: The OpenXR swapchain format is typically `GL_SRGB8_ALPHA8`
(for correct gamma in the VR headset). Client window textures are typically linear-light
`ARGB8888`. The compositor must handle the sRGB conversion during compositing, either via
GL sRGB framebuffer blending or an explicit shader conversion.

### Practical Recommendations for EXWM-VR

1. **Advertise all formats from `dmabuf_formats()`**: Let the EGL driver determine what it
   can import. Do not filter the list.

2. **Prefer tiled modifiers on AMD/Intel**: The default behavior is correct -- Mesa
   allocates tiled buffers when tiled modifiers are advertised. No special handling needed.

3. **Handle `DRM_FORMAT_MOD_INVALID` gracefully**: Older clients or NVIDIA clients may
   submit buffers without explicit modifiers. The EGL driver handles this transparently
   in most cases.

4. **NV12 requires `GL_TEXTURE_EXTERNAL_OES`**: Video surfaces (mpv, browser video)
   produce NV12 buffers. These must be sampled via `samplerExternalOES` in GLSL, not
   `sampler2D`. Smithay's `GlesRenderer` handles this internally, but custom VR shaders
   must account for it.

5. **Use `DmabufFeedback` (v4 protocol)** when available: This tells clients the preferred
   device and format tranches, reducing format mismatch and fallback-to-linear scenarios.

6. **Test matrix**: Validate with at least AMD radeonsi + `ARGB8888` (tiled + DCC),
   Intel iris + `XRGB8888` (Y-tiled), and NVIDIA proprietary + `ARGB8888` (linear).

---

## Part 2: Foveated Rendering for VR (R8.2)

### Overview

Foveated rendering reduces GPU workload by rendering the center of the visual field at
full resolution and progressively reducing resolution toward the periphery. In VR, this
exploits the fact that HMD optics and human visual acuity both degrade away from the
optical/foveal center. Two primary variants exist: fixed foveated rendering (FFR), where
the high-resolution region is static at screen center, and eye-tracked foveated rendering
(ETFR), where the high-resolution region follows the user's gaze via eye tracking.

For a VR window manager, foveated rendering is particularly valuable: text-heavy content
(code, documents) requires high resolution in the reading area but not in peripheral
windows. A 30-50% fragment shader reduction directly translates to either higher frame
rates or the ability to composite more windows.

### Techniques

#### 1. Variable Rate Shading (VRS)

**How it works**: VRS is a hardware feature (NVIDIA Turing+, AMD RDNA2+, Intel Gen11+)
that allows the GPU to execute a single fragment shader invocation for a block of pixels
(2x2, 4x4, etc.) instead of per-pixel. The shading rate is controlled per-tile (typically
16x16 pixel tiles) via a shading rate image or per-draw-call API.

**Shading rate options** (NVIDIA specification):
- 1x1: Full resolution (1 shader per pixel)
- 1x2 / 2x1: Half rate (1 shader per 2 pixels)
- 2x2: Quarter rate (1 shader per 4 pixels)
- 2x4 / 4x2: Eighth rate (1 shader per 8 pixels)
- 4x4: Sixteenth rate (1 shader per 16 pixels)

**VR foveated pattern**: Concentric elliptical rings centered on the gaze point:
- Inner ring (foveal): 1x1 shading rate. Covers ~10-15 degrees of visual angle.
- Middle ring (parafoveal): 2x2 shading rate. Covers ~15-30 degrees.
- Outer ring (peripheral): 4x4 shading rate. Covers the remainder.

**Performance savings**: Highly dependent on shader complexity. The more ALU-intensive the
fragment shader, the greater the savings. For typical VR content:
- Simple shaders (textured quads, UI): 15-25% GPU time reduction
- Complex shaders (PBR lighting, shadows): 30-50% GPU time reduction
- Extremely complex shaders (ray marching, volumetrics): up to 60%

For a window manager compositor, shaders are relatively simple (texture sampling +
compositing), so expect savings toward the lower end (15-30%). However, the savings
compound: fewer fragments also means less bandwidth for framebuffer writes.

**API support**:
- Direct3D 12 Ultimate: Native VRS support (Tier 1 and Tier 2)
- Direct3D 11: Via NVIDIA NvAPI extension
- Vulkan: `VK_KHR_fragment_shading_rate`
- OpenGL: `NV_shading_rate_image` (NVIDIA only), `GL_QCOM_shading_rate` (Qualcomm)
- OpenGL ES: `QCOM_shading_rate` on mobile

**Limitation for our compositor**: VRS requires either Vulkan (`VK_KHR_fragment_shading_rate`)
or vendor-specific OpenGL extensions. Since Smithay's `GlesRenderer` uses OpenGL ES, and
the portable VRS path is Vulkan-only, VRS integration requires either:
1. A future Vulkan renderer in Smithay, or
2. Using the NVIDIA-specific `NV_shading_rate_image` extension (limits portability)

This makes VRS a medium-term optimization (Week 12+) rather than an initial feature.

#### 2. Radial Density Masking

**How it works**: A technique patented and demoed by Valve (Alex Vlachos, GDC 2016). The
renderer draws the full scene at full resolution, then a post-processing pass masks out
pixels in a radial pattern from the center. Masked pixels form a checkerboard pattern
where every other 2x2 quad in the middle ring and 3/4 of quads in the outer ring are
discarded. A reconstruction pass fills in the holes.

**Ring configuration** (from `openvr_foveated` implementation):
- Ring 1 (center): Full resolution. Configurable radius.
- Ring 2: Half resolution (2x2 quad masking). 1/2 pixel density.
- Ring 3: Quarter resolution (4x4 masking). 1/4 pixel density.
- Ring 4 (periphery): 1/16 resolution.

**Reconstruction stages**:
1. **Hole filling**: Copy neighboring pixel values into masked positions.
2. **Cross-cell blending**: Blend across cell boundaries to reduce block artifacts.
3. **Gaussian blur**: Low-pass filter to smooth the reconstruction.

**Performance**: Radial density masking achieves savings through reduced fragment shader
execution: since GPUs operate on 2x2 pixel quads, masking entire quads prevents their
shader execution. Savings range from 20-40% depending on the inner ring size.

**Advantage**: Works on any GPU -- no hardware VRS support required. This is the universal
fallback technique.

**Disadvantage**: The reconstruction pass adds latency and can produce visible artifacts
(shimmer at ring boundaries, temporal instability). For text rendering in a window
manager, reconstruction artifacts could degrade readability. The technique is also
"externally injected" -- it works best when integrated into the rendering pipeline from
the start.

**Relevance for EXWM-VR**: Radial density masking could be implemented as a post-processing
pass on the composited VR frame before submission to OpenXR. Since our compositor controls
the entire rendering pipeline, integration is feasible. However, text readability concerns
make this less suitable than VRS for a productivity-focused VR WM.

#### 3. Multi-Resolution Rendering

**How it works**: Render different viewport regions at different resolutions into separate
render targets, then composite them into the final frame. The center region uses a
high-resolution target, the periphery uses lower-resolution targets that are upscaled.

**Varjo's implementation** (via `XR_VARJO_quad_views`): Varjo headsets physically have a
dual-display architecture -- a high-DPI micro-OLED inset (~70 PPD) surrounded by a
lower-DPI peripheral display (~30 PPD). The OpenXR runtime exposes 4 views (2 per eye:
focus + context) instead of the standard 2. The application renders 4 images per frame at
different resolutions. Eye tracking moves the focus region.

**Performance savings** (from Varjo documentation):
- Very low dynamic projection: 34% pixel reduction vs quad rendering
- Low: 39% reduction
- Medium: 43% reduction
- High: 46% reduction
- Highest: 51% reduction

These savings come directly from rendering fewer pixels, which reduces both fragment shader
and bandwidth costs proportionally.

**Non-Varjo approach**: Without hardware quad-views, a compositor can implement
multi-resolution rendering by:
1. Rendering the foveal region at full swapchain resolution into a center viewport
2. Rendering the peripheral region at 50% resolution into a smaller texture
3. Upscaling the peripheral texture and compositing it around the foveal region

This requires custom shader work and careful seam blending, but avoids hardware VRS
dependencies.

### OpenXR Foveation Extensions

#### XR_FB_foveation

**Vendor**: Meta (Facebook Reality Labs)
**Runtime support**: Meta Quest (1/2/3/3S/Pro), Bytedance PICO, HTC Vive Focus 3
**openxrs crate**: `InstanceExtensions::fb_foveation`

**Architecture**: The `XR_FB_foveation` extension provides runtime-managed fixed foveated
rendering. The application creates a foveation profile and attaches it to swapchain
images. The runtime applies foveation at the hardware level (typically via tile-based
rendering on mobile GPUs like Qualcomm Adreno).

**Key API**:
```
xrCreateFoveationProfileFB(session, create_info) -> FoveationProfileFB
```

**Foveation levels** (via `XR_FB_foveation_configuration`):
- `XR_FOVEATION_LEVEL_NONE_FB`: No foveation (full resolution everywhere)
- `XR_FOVEATION_LEVEL_LOW_FB`: Mild peripheral reduction
- `XR_FOVEATION_LEVEL_MEDIUM_FB`: Moderate reduction
- `XR_FOVEATION_LEVEL_HIGH_FB`: Aggressive reduction

**Dynamic modes** (via `XR_FB_foveation_configuration`):
- Static: Fixed center point
- Dynamic: Runtime adjusts based on eye tracking (Quest Pro)

**Integration**: The compositor creates a foveation profile, attaches it to the OpenXR
swapchain via `XrSwapchainCreateInfoFoveationFB`, and the runtime handles the rest. The
application renders at full resolution into the swapchain, but the runtime's tile-based
renderer applies variable quality. This is zero-effort integration but limited to
Meta/PICO/HTC hardware.

**Performance**: Meta reports 20-30% GPU savings on Quest with `MEDIUM` foveation for
typical VR content. Fragment shader reduction varies by level:
- Low: ~15% reduction
- Medium: ~25% reduction
- High: ~35% reduction

#### XR_VARJO_foveated_rendering

**Vendor**: Varjo
**Runtime support**: Varjo (Aero, XR-4, VR-3/XR-3)
**openxrs crate**: `InstanceExtensions::varjo_foveated_rendering`

**Architecture**: Augments `XR_VARJO_quad_views` to enable eye-tracked foveation. The
runtime returns 4 views per frame (focus + context per eye) with different recommended
resolutions. The focus view resolution and position change each frame based on gaze.

**Key behavior**:
- `XrFoveatedViewConfigurationViewVARJO` contains `foveatedRenderingActive` boolean
- When active, view configuration views return dynamically adjusted recommended sizes
- The application queries `varjo_GetRenderingGaze()` for the current gaze point
- Projection matrices are computed per-frame from `varjo_GetFoveatedFovTangents()`

**Dynamic projection savings** (Varjo measurements on VR-3/XR-3):
| Setting | Pixel Reduction vs Quad |
|---------|------------------------|
| Very Low | 34% |
| Low | 39% |
| Medium | 43% |
| High | 46% |
| Highest | 51% |

**Combination with VRS**: Varjo recommends combining dynamic projection with VRS for
maximum savings. VRS handles per-pixel shading rate within each view, while dynamic
projection handles the coarse resolution of each view. Combined savings can reach 60%+.

**Gaze tracking specs**: Varjo eye trackers operate at 100-200 Hz with ~20-30ms latency.
The gaze data is available through both the Varjo native API and the OpenXR
`XR_EXT_eye_gaze_interaction` extension.

#### XR_EXT_eye_gaze_interaction (for foveation integration)

**Vendor**: Khronos (multi-vendor)
**Runtime support**: Varjo, Meta Quest Pro, HTC Vive Cosmos/Focus 3, Monado, PICO
**openxrs crate**: `InstanceExtensions::ext_eye_gaze_interaction`

This extension provides the gaze ray needed to position the foveal region in eye-tracked
foveated rendering. The gaze pose is exposed as an action path:
`/user/eyes_ext/input/gaze_ext/pose`

For EXWM-VR, the gaze ray from this extension serves double duty:
1. **Focus targeting**: Which window receives keyboard input (Week 12)
2. **Foveation center**: Where to place the high-resolution rendering region

The gaze ray origin is approximately at the midpoint between the user's eyes, and the
orientation points in the gaze direction. To determine the foveation center in screen
space, project the gaze ray onto the swapchain's image plane using the current view
projection matrix.

### Performance Savings Estimates

| Technique | Fragment Savings | Bandwidth Savings | Hardware Required | Complexity |
|-----------|-----------------|-------------------|-------------------|------------|
| VRS (fixed) | 25-40% | 15-25% | Turing+/RDNA2+/Gen11+ | Medium |
| VRS (eye-tracked) | 35-50% | 20-35% | VRS GPU + eye tracker | High |
| Radial density masking | 20-35% | 10-20% | Any | Medium |
| Multi-resolution (quad views) | 35-50% | 30-50% | Varjo HMD | Low (runtime managed) |
| XR_FB_foveation (fixed) | 15-35% | 15-30% | Quest/PICO/HTC | Very Low |
| XR_FB_foveation (eye-tracked) | 25-40% | 20-35% | Quest Pro | Low |

**Note on savings estimates**: These percentages represent fragment shader workload
reduction, not overall frame time reduction. Overall frame time improvement depends on
whether the application is fragment-shader-bound (where foveation helps most),
vertex-bound (no benefit), or CPU-bound (no benefit). For a VR compositor doing textured
quad compositing, the workload is predominantly fragment-bound (texture sampling +
blending), so fragment savings translate fairly directly to frame time savings.

### Integration with Eye Tracking (Week 11)

The foveated rendering implementation connects to eye tracking in Week 11-12:

**Week 11 (Eye Tracker Integration)**:
1. Enable `XR_EXT_eye_gaze_interaction` if available
2. Create gaze action: `XR_ACTION_TYPE_POSE_INPUT` bound to `/user/eyes_ext/input/gaze_ext/pose`
3. Each frame: `session.locate_views()` for gaze pose
4. Fall back to head-gaze (center of view) when eye tracking unavailable

**Week 12 (Foveated Rendering Integration)**:
1. Project gaze ray to screen-space coordinates per eye
2. If `XR_FB_foveation` available: Set dynamic foveation profile with gaze center
3. If VRS available (Vulkan path): Generate shading rate image with foveal center at gaze
4. If neither: Apply radial density masking post-process centered on gaze
5. Fall back to fixed center foveation when eye tracking is unavailable

**Gaze-to-foveation latency**: Eye tracker latency (~20-30ms) plus one frame of rendering
latency means the foveal region lags behind saccades by ~30-40ms. During saccades (rapid
eye movements), human visual acuity is suppressed (saccadic suppression), so this latency
is perceptually masked. During smooth pursuit (tracking a moving target), the lag is more
noticeable but typically acceptable at <50ms.

### Recommended Implementation Strategy for EXWM-VR

**Phase 1 (Week 8-9): Fixed foveation via runtime**
- If `XR_FB_foveation` is available, enable it at `MEDIUM` level
- Zero code complexity; runtime handles everything
- Provides 20-30% savings on Quest/PICO hardware

**Phase 2 (Week 12: Eye-tracked foveation via runtime)**
- If `XR_FB_foveation` + eye tracking available, switch to dynamic mode
- Gaze position from `XR_EXT_eye_gaze_interaction` drives foveation center
- Provides 30-40% savings on Quest Pro

**Phase 3 (Week 12+: Custom foveation for non-FB runtimes)**
- For Monado/SteamVR (no `XR_FB_foveation`): implement radial density masking
- Post-process pass on composited frame before swapchain submission
- Use concentric elliptical rings: 1x center, 1/4x middle, 1/16x periphery
- Reconstruction: bilinear upscale + edge-aware blur

**Phase 4 (Future: VRS path)**
- When/if Smithay gains a Vulkan renderer, integrate `VK_KHR_fragment_shading_rate`
- Generate shading rate image per-frame from gaze data
- Most efficient approach: no reconstruction artifacts, hardware-native

**Varjo special case**:
- If `XR_VARJO_quad_views` + `XR_VARJO_foveated_rendering` available, render 4 views
- Use dynamic projection for resolution savings
- Optionally combine with VRS for additional gains
- This is the highest-quality path but Varjo-specific

---

## References

### DMA-BUF and Format Modifiers
- [EGL_EXT_image_dma_buf_import_modifiers spec](https://registry.khronos.org/EGL/extensions/EXT/EGL_EXT_image_dma_buf_import_modifiers.txt)
- [Collabora: Notes on DRM Format Modifiers](https://www.collabora.com/news-and-blog/blog/2017/02/09/notes-on-drm-format-modifiers/)
- [Collabora: Implementing DRM Format Modifiers in NVK](https://www.collabora.com/news-and-blog/news-and-events/implementing-drm-format-modifiers-in-nvk.html)
- [AMD GPUOpen: DCC Overview](https://gpuopen.com/learn/dcc-overview/)
- [Linux Kernel: Exchanging Pixel Buffers](https://docs.kernel.org/userspace-api/dma-buf-alloc-exchange.html)
- [Phoronix: NVK Lands DRM Format Modifiers](https://www.phoronix.com/news/NVK-DRM-Modifiers-Land)
- [Phoronix: NVIDIA R535 DMA-BUF v4 Support](https://www.phoronix.com/news/NVIDIA-535.43.02-Linux-Driver)
- [Phoronix: AMD DCC for Multi-Plane Formats with RDNA4](https://www.phoronix.com/news/Mesa-25.1-Multi-Plane-DCC-RDNA4)
- [NVIDIA open-gpu-kernel-modules: True DMABUF Discussion](https://github.com/NVIDIA/open-gpu-kernel-modules/discussions/243)
- [Mesa issue #6459: Intel Gen12 RC_CCS import failure](https://gitlab.freedesktop.org/mesa/mesa/-/issues/6459)

### Smithay
- [Smithay dmabuf module](https://smithay.github.io/smithay/smithay/wayland/dmabuf/index.html)
- [Smithay DmabufHandler trait](https://smithay.github.io/smithay/smithay/wayland/dmabuf/trait.DmabufHandler.html)
- [Smithay GlesRenderer](https://smithay.github.io/smithay/smithay/backend/renderer/gles/struct.GlesRenderer.html)
- [Smithay Anvil udev.rs](https://github.com/Smithay/smithay/blob/master/anvil/src/udev.rs)
- [Wayland Book: DMA-BUF](https://wayland-book.com/surfaces/dmabuf.html)

### Foveated Rendering
- [OpenXR Toolkit: Foveated Rendering](https://mbucchia.github.io/OpenXR-Toolkit/fr.html)
- [NVIDIA: Turing VRS in VRWorks](https://developer.nvidia.com/blog/turing-variable-rate-shading-vrworks/)
- [NVIDIA: VRWorks Variable Rate Shading](https://developer.nvidia.com/vrworks/graphics/variablerateshading)
- [Varjo: Dynamic Foveated VRS with OpenXR Toolkit](https://varjo.com/blog/dynamic-foveated-variable-shader-rate-with-openxr-toolkit-and-aero-a-k-a-foveated-rendering/)
- [Varjo: Foveated Rendering API](https://developer.varjo.com/docs/native/foveated-rendering-api)
- [Varjo: Achieving Performance with High Resolution Rendering](https://developer.varjo.com/docs/get-started/achieving-performace-with-high-resolution-rendering)
- [Meta: Save GPU with Eye Tracked Foveated Rendering](https://developers.meta.com/horizon/blog/save-gpu-with-eye-tracked-foveated-rendering/)
- [Valve: Radial Density Masking Patent (US20170221184A1)](https://patents.google.com/patent/US20170221184A1/en)
- [Valve: Advanced VR Rendering Performance (GDC 2016)](http://media.steampowered.com/apps/valve/2016/Alex_Vlachos_Advanced_VR_Rendering_Performance_GDC2016.pdf)
- [openvr_foveated: Fixed Foveated Rendering for SteamVR](https://github.com/fholger/openvr_foveated)
- [Tobii: Dynamic Foveated Rendering](https://www.tobii.com/blog/realistic-virtual-vision-with-dynamic-foveated-rendering)
- [Wikipedia: Foveated Rendering](https://en.wikipedia.org/wiki/Foveated_rendering)

### OpenXR Extensions
- [XR_FB_foveation spec](https://registry.khronos.org/OpenXR/specs/1.0/man/html/XR_FB_foveation.html)
- [XR_VARJO_foveated_rendering spec](https://registry.khronos.org/OpenXR/specs/1.1/html/xrspec.html#XR_VARJO_foveated_rendering)
- [XR_EXT_eye_gaze_interaction spec](https://registry.khronos.org/OpenXR/specs/1.1/html/xrspec.html#XR_EXT_eye_gaze_interaction)
- [Quad-Views-Foveated OpenXR API layer](https://github.com/mbucchia/Quad-Views-Foveated)
- [OpenXR Runtime Extension Support](https://github.khronos.org/OpenXR-Inventory/runtime_extension_support.html)
