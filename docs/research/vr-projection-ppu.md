# VR Virtual Monitor Projection Types & PPU/Text Readability

## Overview

This document evaluates projection surface geometries for rendering 2D Wayland
windows as virtual monitors in VR, and analyzes pixels-per-degree (PPD)
requirements for text-heavy workloads (coding, terminal use). The goal is to
inform EXWM-VR's choice of projection surface and default virtual monitor
placement parameters.

---

## Part 1: Projection Surface Comparison

Four projection geometries are candidates for mapping a 2D texture (the Wayland
surface) onto a 3D mesh displayed inside the VR scene.

### 1.1 Flat Plane

**Geometry**: A single quad (two triangles) or lightly tessellated rectangle.
The texture maps 1:1 onto the plane with trivial UV coordinates.

**Visual characteristics**:
- Center of the screen is sharp and undistorted.
- Edges and especially corners are geometrically farther from the viewer's eye
  than the center. On a 120-degree-wide flat plane at 1.5 m, the corners are
  roughly 2x the center distance, causing apparent minification: text at the
  edges appears smaller in angular size and less sharp.
- For narrow virtual monitors (< 60 degrees of arc), the distortion is
  negligible. Once the monitor subtends > 80 degrees, edge distortion becomes
  noticeable for text.

**Comfort for coding**:
- Excellent for single-monitor setups that subtend < 60 degrees.
- Problematic for ultrawide or multi-monitor wrapping layouts where the user
  expects uniform text density.
- No curvature-induced eye refocusing issues.

**Implementation complexity**: Trivial. One quad, four vertices, simple
orthographic UV mapping. No tessellation required.

**Industry adoption**:
- Default in most VR desktop apps when curvature is set to 0.
- SteamVR Desktop Theater default mode.
- Simula VR (Haskell/Godot Wayland compositor) uses flat planes.

### 1.2 Cylindrical Projection

**Geometry**: A horizontal arc segment -- vertices placed along a circular arc
in the XZ plane, with constant Y (height). The arc subtends a configurable
angle (typically 90-180 degrees). Each column of the texture maps to a vertical
strip at a constant angular offset from center.

**Mesh generation**: For an arc of `theta_total` radians, radius `r`, and `N`
horizontal segments:

```
for i in 0..=N:
    angle = -theta_total/2 + i * (theta_total / N)
    x = r * sin(angle)
    z = -r * cos(angle)   // negative Z = forward in right-hand coords
    u = i / N             // linear UV mapping
    // top vertex:  (x, +height/2, z), uv = (u, 0)
    // bottom vertex: (x, -height/2, z), uv = (u, 1)
```

N = 32-64 segments is sufficient for smooth curvature at typical VR resolutions.
Triangle strip or indexed triangles connect adjacent columns.

**UV mapping**: Linear `u` coordinate along the arc means each horizontal pixel
of the source texture maps to an equal angular width. This is the key property:
every character in a line of code subtends the same angular size regardless of
horizontal position.

**Visual characteristics**:
- Uniform horizontal angular density -- text at the left/right edges is the same
  angular size as text at the center. This is the single most important property
  for code readability across wide monitors.
- Vertical axis remains flat (no vertical curvature), so line spacing is
  consistent and vertical scrolling feels natural.
- Mild perspective foreshortening at extreme horizontal angles (> 70 degrees
  from center) but this is inherent to the viewing geometry, not the projection.

**Comfort for coding**:
- The best geometry for wide virtual monitors used in coding. Maintains uniform
  character size across the full width.
- No vertical disorientation since lines remain straight and horizontal.
- Physical curved monitors (Samsung Odyssey, LG UltraGear) use exactly this
  geometry (1000R-1800R curvature), and developers report equivalent or better
  readability vs flat panels for wide displays.

**Implementation complexity**: Moderate. Requires generating a tessellated arc
mesh (32-64 segments typical). UV mapping is straightforward -- linear `u` along
the arc. The main complexity is exposing a curvature parameter (arc radius or
subtended angle) and regenerating the mesh when it changes.

**Industry adoption**:
- **Immersed**: Cylindrical curvature with user-adjustable slider. Default and
  recommended mode for productivity. Supports up to 5 curved virtual monitors.
- **Virtual Desktop**: Cylindrical curvature with adjustable slider. The most
  popular VR desktop streaming app.
- **Meta Horizon Workrooms**: Curved virtual monitors for the personal office
  mode (up to 3 displays). Uses cylindrical curvature.
- **Apple Vision Pro**: Mac Virtual Display uses cylindrical curvature, described
  as equivalent to "two 5K monitors side by side in one massive, curved display."
- **BigScreen**: Cylindrical projection for virtual theater and desktop modes.

Cylindrical is the **industry standard** for VR productivity applications.

### 1.3 Spherical Projection

**Geometry**: Vertices placed on a sphere segment -- both horizontal and vertical
axes are curved. The texture maps onto a patch of a sphere centered on the
viewer.

**Mesh generation**: For a sphere of radius `r` with horizontal arc
`theta_total` and vertical arc `phi_total`, using `N` horizontal and `M`
vertical segments:

```
for j in 0..=M:
    phi = -phi_total/2 + j * (phi_total / M)
    for i in 0..=N:
        theta = -theta_total/2 + i * (theta_total / N)
        x = r * sin(theta) * cos(phi)
        y = r * sin(phi)
        z = -r * cos(theta) * cos(phi)
        u = i / N
        v = j / M
```

**Visual characteristics**:
- Equal angular density in all directions -- every pixel subtends the same solid
  angle regardless of position.
- Vertical curvature means horizontal lines of text are no longer straight --
  they curve upward at the edges. This is disorienting for reading code where
  line alignment is a critical visual cue.
- For small vertical extents (< 30 degrees), the vertical curvature is minimal
  and approaches cylindrical behavior.

**Comfort for coding**:
- Worse than cylindrical for text-heavy use. The vertical curvature disrupts
  line-scanning eye movements and makes code indentation harder to judge.
- Acceptable for small virtual monitors where the vertical arc is < 20 degrees.
- Research on VR reading surfaces found that "the horizontal cylinder surface
  was more comfortable to read on than the sphere surface" (Openreview, 2024).

**Implementation complexity**: Moderate-to-high. Similar mesh generation to
cylindrical but with an additional vertical curvature loop. UV mapping is
straightforward (linear in both axes on the sphere patch). The main added
complexity is handling aspect ratio correctly when both axes curve.

**Industry adoption**:
- Not widely used for virtual monitors in productivity apps.
- Used in 180-degree VR video players for immersive content.
- Some VR environments use spherical patches for very large "dome" displays,
  but not for text-oriented work.

### 1.4 Equirectangular Projection

**Geometry**: A full or partial sphere where texture coordinates map linearly to
latitude/longitude. This is the standard projection for 360-degree panoramic
content.

**Visual characteristics**:
- Designed for full-sphere or hemisphere wrapping. For a single virtual monitor,
  it introduces the same issues as spherical projection plus additional
  distortion near the poles (area magnification at top/bottom edges).
- Text near the top and bottom edges of the monitor would appear stretched
  horizontally, degrading readability.
- The pole-distortion problem means a rectangular UI texture does not map cleanly
  -- characters at different vertical positions would have different angular
  widths.

**Comfort for coding**: Poor. The non-uniform angular density (latitude-dependent
horizontal scaling) makes text inconsistent across the monitor surface. Lines of
code would appear wider at the top and bottom than in the middle.

**Implementation complexity**: Low for mesh generation (standard UV sphere), but
the non-linear relationship between texture pixels and angular size means
additional shader work to correct for uniform text rendering, which partially
defeats the purpose.

**Industry adoption**:
- Standard for 360-degree video and photo viewers.
- Not used for virtual monitor surfaces in any major productivity application.
- Overkill and counterproductive for single-surface virtual monitors.

### 1.5 Projection Comparison Summary

| Property                    | Flat Plane | Cylindrical | Spherical | Equirectangular |
|-----------------------------|-----------|-------------|-----------|-----------------|
| Horizontal angular uniformity | Poor (>60 deg) | Excellent | Excellent | Varies by latitude |
| Vertical angular uniformity   | Excellent | Excellent | Poor (curved lines) | Poor (pole stretch) |
| Text readability at edges     | Degrades  | Uniform   | Uniform (but curved) | Distorted |
| Coding comfort (wide monitor) | Fair      | Excellent | Fair      | Poor |
| Implementation complexity     | Trivial   | Low-Moderate | Moderate | Low (but needs correction) |
| Industry adoption (productivity) | Secondary | **Primary** | Rare | None |
| Mesh vertices (typical)      | 4         | 66-130    | 1000-2000 | 1000-2000 |
| UV mapping complexity         | Trivial   | Linear    | Linear    | Requires correction |

**Recommendation for EXWM-VR**: Cylindrical projection as the default, with flat
plane as a fallback/option for narrow monitors. Spherical and equirectangular
should not be implemented for the initial version.

---

## Part 2: Pixels Per Degree (PPD) and Text Readability

### 2.1 Angular Resolution Fundamentals

**Pixels per degree (PPD)** is the primary metric for VR display sharpness. It
measures how many display pixels fall within one degree of the viewer's visual
field.

**Formula**:

```
PPD = horizontal_pixels_per_eye / horizontal_FOV_degrees
```

**Human vision baseline**: The fovea resolves approximately 60 PPD (1 arc-minute
per pixel). This is the 20/20 vision threshold -- at 60 PPD, a person with normal
vision cannot distinguish individual pixels. Recent research suggests the true
achromatic foveal limit may be as high as 94 PPD.

**Current headset PPD values** (calculated from specs):

| Headset              | Resolution/Eye  | H-FOV   | PPD (approx) | Year |
|----------------------|----------------|---------|-------------|------|
| Valve Index          | 1440 x 1600    | 108 deg | ~13 PPD     | 2019 |
| Meta Quest 2         | 1832 x 1920    | 97 deg  | ~19 PPD     | 2020 |
| Meta Quest 3         | 2064 x 2208    | 104 deg | ~20 PPD (center), ~25 PPD (effective w/ pancake lenses) | 2023 |
| Meta Quest 3S        | 1832 x 1920    | 96 deg  | ~19 PPD     | 2024 |
| Apple Vision Pro     | 3660 x 3200    | 100 deg | ~34 PPD     | 2024 |
| Varjo XR-4           | 3840 x 3744    | 120 deg | ~32 PPD (peripheral), ~51 PPD (foveal) | 2024 |
| Pimax Crystal Super  | 3840 x 3840    | 120 deg | ~32 PPD     | 2025 |
| Human fovea (target) | --             | --      | 60 PPD      | --   |

Note: Effective PPD can differ from calculated PPD due to lens distortion,
foveated rendering, and subpixel layout. The values above are nominal.

### 2.2 Angular Size of Text

Text readability depends on the **angular size** of characters, not their pixel
size in the source texture. The angular size is determined by:

```
angular_height_deg = (character_height_meters / distance_meters) * (180 / pi)
```

Or equivalently using distance-independent millimeters (dmm), where 1 dmm = 1 mm
at 1 m distance:

```
angular_height_dmm = character_height_mm * (1000 / distance_mm)
```

**Research-based minimum sizes**:

| Context                | Minimum Angular Size | Source |
|------------------------|---------------------|--------|
| Absolute minimum legible | 1.33 deg (~23 dmm) | Kurbatov, 2018 |
| Comfortable body text    | 24 dmm (1.38 deg)  | Google Daydream guidelines |
| Preferred reading size   | 41 +/- 14 dmm      | Dingler et al., 2018 (CHI) |
| Short labels/headers     | 20-40 dmm          | General industry consensus |

For **coding specifically**, monospace fonts require higher angular resolution
than proportional text because:
1. Character differentiation (l vs 1 vs I, 0 vs O) relies on fine detail.
2. Indentation alignment requires consistent character width perception.
3. Syntax highlighting colors need sufficient pixel density to be distinguishable.
4. Programmers scan long lines horizontally, demanding edge-to-center uniformity.

### 2.3 PPD Requirements for Coding at Various Configurations

The critical question: how many source texture pixels per character are needed for
a given headset PPD to produce readable monospace text?

**Setup**: Virtual monitor at distance `d`, character height `h` (in the virtual
world), headset with `P` PPD.

```
pixels_on_retina = (h / d) * (180 / pi) * P
```

For a character to be legible, it needs approximately 12-16 vertical pixels on
the retina (similar to 12-16px fonts on a physical monitor). For comfortable
extended reading, 20+ retinal pixels per character height is preferred.

**Worked examples for a virtual monitor at 1.5 m distance**:

Assume a virtual monitor 1.2 m wide x 0.675 m tall (16:9, roughly 50-inch
equivalent), with source texture resolution of 2560x1440.

Character height for a 14pt monospace font at this resolution: approximately
18 px in texture = 18/1440 * 0.675 m = 0.00844 m per character.

Angular size of one character:

```
angular_height = atan(0.00844 / 1.5) * (180/pi) = 0.322 degrees
```

Retinal pixels per character at various PPD:

| Headset (PPD)         | Retinal px/char | Readable? |
|-----------------------|-----------------|-----------|
| Valve Index (13)      | 4.2 px          | No -- illegible at 14pt |
| Quest 2 (19)          | 6.1 px          | Barely -- strain, squinting |
| Quest 3 (25)          | 8.1 px          | Marginal -- usable with effort |
| Vision Pro (34)       | 10.9 px         | Acceptable -- mild strain |
| Varjo foveal (51)     | 16.4 px         | Good -- comfortable for sessions |
| Human retina (60)     | 19.3 px         | Excellent -- equivalent to monitor |

**Implication**: At 14pt font on a 2560x1440 virtual monitor at 1.5 m, even the
Quest 3 is marginal for extended coding. Users must either:
1. Increase font size (18pt+ gives ~10+ retinal px on Quest 3).
2. Move the virtual monitor closer (1.0 m instead of 1.5 m, gaining ~50% more
   retinal pixels, but the monitor subtends a larger FOV).
3. Reduce virtual monitor resolution (fewer source pixels = larger angular size
   per character, but less content on screen).

### 2.4 Recommended Configurations by Headset

**Valve Index (13 PPD)** -- limited by low angular resolution:
- Virtual monitor distance: 0.8-1.0 m (closer compensates for low PPD).
- Recommended font size: 18-20pt minimum for coding.
- Effective source resolution: 1920x1080 maximum (higher resolutions waste pixels
  that the headset cannot resolve).
- Expected comfort: Adequate for short sessions. Not recommended for all-day
  coding due to eye strain from low PPD.
- Curvature: Cylindrical helpful since the monitor should be closer and wider,
  making edge distance variance more significant.

**Meta Quest 3 (20-25 PPD)** -- the current mainstream target:
- Virtual monitor distance: 1.0-1.5 m.
- Recommended font size: 16-18pt for coding, 14pt possible with high-contrast
  theme.
- Effective source resolution: 2560x1440 is reasonable; 3840x2160 yields
  diminishing returns (characters become sub-pixel at the headset's resolution).
- Expected comfort: Usable for 2-4 hour sessions with breaks. Text "shimmering"
  from subpixel misalignment is the main complaint.
- Curvature: Cylindrical strongly recommended for monitors > 60 degrees of arc.

**Apple Vision Pro (34 PPD)** -- highest current mainstream PPD:
- Virtual monitor distance: 1.0-2.0 m.
- Recommended font size: 14-16pt for coding.
- Effective source resolution: 3840x2160 is well-matched; 5120x2880 (5K) is
  the sweet spot for the ultrawide curved mode.
- Expected comfort: Viable for extended work sessions (4-8 hours reported by
  users). Text scintillation still present but reduced compared to Quest.
- Curvature: Apple uses cylindrical by default in Mac Virtual Display.

**Varjo XR-4 (32-51 PPD with foveation)** -- professional tier:
- Virtual monitor distance: 1.5-2.0 m (high PPD allows farther placement).
- Recommended font size: 12-14pt -- approaching physical monitor parity.
- Effective source resolution: 5120x2880 or dual 4K.
- Expected comfort: Best-in-class for text work. The foveated display
  concentrates resolution where the user is looking.

### 2.5 How Commercial Apps Handle PPD Limitations

**Immersed**:
- Allows per-monitor resolution, size, and distance adjustment.
- Recommends 1440p per virtual monitor on Quest 3.
- Provides curvature slider (cylindrical).
- Uses sharpening filters to compensate for subpixel rendering artifacts.
- Supports up to 2560x1440 per monitor with Pro subscription.

**Virtual Desktop**:
- Streams the full desktop at native resolution, then maps onto a curved or flat
  virtual screen.
- Uses SteamVR's compositor for Valve Index, native Quest compositor for Meta
  headsets.
- Exposes curvature and distance sliders.
- Supports SSW (Synchronous Spacewarp) to maintain frame rate during text
  rendering, which reduces shimmering.

**Apple Vision Pro (Mac Virtual Display)**:
- Renders at the Mac's native resolution, composited by visionOS.
- The ultrawide mode is explicitly cylindrical.
- Uses the headset's high PPD (34) to render 14pt text comfortably.
- Foveated rendering helps allocate resolution to the area the user is looking at.

**Meta Horizon Workrooms** (discontinued Feb 2026):
- Supported up to 3 virtual monitors with cylindrical curvature.
- Required physical monitor mirroring on Windows; virtual monitors on Mac.
- Limited to 1080p per virtual monitor in practice, offset by close placement.

### 2.6 The Font Rendering Problem

Beyond PPD, VR virtual monitors face a text quality issue that physical monitors
do not: **subpixel instability**. On a physical monitor, text is rendered once
and remains static. In VR, every frame re-renders the virtual monitor texture
onto the headset display at a slightly different subpixel alignment (because the
user's head is never perfectly still). This causes:

1. **Text scintillation/shimmering**: Characters appear to "wriggle" as subpixel
   coverage shifts between frames.
2. **Loss of subpixel rendering benefit**: ClearType/FreeType subpixel hinting
   assumes a fixed pixel grid, which VR breaks.
3. **Moire patterns**: Regular grids (code indentation, table borders) can
   produce interference patterns with the headset's pixel grid.

Mitigations:
- **Supersampling**: Render the source texture at 1.5-2x the nominal resolution,
  then filter down. This is the most effective fix and is used by Immersed and
  Virtual Desktop.
- **Temporal filtering**: Average subpixel positions across frames to stabilize
  text appearance. Used by Apple Vision Pro's compositor.
- **Font choice**: Sans-serif fonts with uniform stroke width (e.g., JetBrains
  Mono, Fira Code, Cascadia Mono) render more stably than fonts with thin
  strokes or serifs.
- **High-contrast themes**: Light text on dark background reduces the perceptual
  impact of shimmering (dark backgrounds have fewer edge transitions to shimmer).

---

## Part 3: Implications for EXWM-VR

### 3.1 Default Projection: Cylindrical

The cylindrical projection should be the default and primary implementation:

1. It is the industry standard across Immersed, Virtual Desktop, and Apple
   Vision Pro.
2. It provides uniform horizontal angular density, which is the most important
   property for code readability.
3. It preserves straight horizontal lines, critical for code indentation.
4. Implementation complexity is modest: 32-64 segment arc mesh with linear UV.

**Parameters to expose**:
- `curvature_radius`: Distance from viewer to the cylinder surface (default:
  1.5 m). Larger radius = flatter; smaller = more curved.
- `arc_angle`: Total horizontal angle subtended (default: 60 deg for single
  monitor, up to 180 deg for ultrawide).
- `height`: Vertical extent of the virtual monitor.
- `resolution`: Source texture resolution.

A `curvature_radius` of infinity (or a very large value) degenerates to a flat
plane, so flat projection is a special case and does not need separate geometry.

### 3.2 PPD-Aware Defaults

EXWM-VR should detect or allow configuration of the headset's PPD and set
sensible defaults:

| Headset PPD | Default Distance | Default Resolution | Min Font Guidance |
|-------------|------------------|--------------------|-------------------|
| < 15        | 0.8 m           | 1920x1080          | 20pt              |
| 15-24       | 1.2 m           | 2560x1440          | 16pt              |
| 25-39       | 1.5 m           | 3840x2160          | 14pt              |
| 40+         | 1.5-2.0 m       | 5120x2880          | 12pt              |

These can be computed from the OpenXR view configuration:

```
ppd = view.recommendedImageRectWidth / (fov.angleRight - fov.angleLeft) * (pi/180)
```

### 3.3 Supersampling Strategy

The source texture (Wayland surface) should be rendered at 1.0x native
resolution. The VR compositor should apply **bilinear or trilinear filtering**
when mapping the texture onto the curved mesh, and the OpenXR swapchain should
use a resolution of at least 1.5x the headset's native per-eye resolution for
the layer containing virtual monitors. This provides implicit supersampling
without requiring the Wayland clients to render at higher resolution.

### 3.4 Future Considerations

- **Foveated texture mapping**: When eye tracking is available (Quest Pro, Vision
  Pro, Varjo), the mesh tessellation and texture sampling rate can be increased
  in the foveal region and reduced in the periphery. This is a significant
  optimization for high-resolution virtual monitors.
- **Per-monitor curvature**: Each virtual monitor could have independent
  curvature, allowing flat monitors for small windows (terminals, chat) and
  curved monitors for wide editor windows.
- **Dynamic PPD detection**: OpenXR provides view configuration hints that could
  be used to automatically adjust font size recommendations displayed to the
  user at startup.

---

## Sources

- Road to VR -- Understanding Pixel Density & Retinal Resolution:
  https://www.roadtovr.com/understanding-pixel-density-retinal-resolution-and-why-its-important-for-vr-and-ar-headsets/
- Pimax -- What is PPD:
  https://pimax.com/blogs/blogs/what-is-ppd-pixels-per-degree-and-how-high-ppd-improves-vr-image-quality
- Meta -- Stacking the Optical Deck (PPD discussion):
  https://www.meta.com/blog/vr-display-optics-pancake-lenses-ppd/
- KGuttag -- Apple Vision Pro Monitor Replacement Analysis:
  https://kguttag.com/2023/08/05/apple-vision-pro-part-5a-why-monitor-replacement-is-ridiculous/
- Kurbatov -- 10 Rules of Using Fonts in VR:
  https://medium.com/inborn-experience/10-rules-of-using-fonts-in-virtual-reality-da7b229cb5a1
- Google Daydream -- DMM Sizing:
  https://www.ryanhinojosa.com/2018/01/08/device-independent/
- Dingler et al. -- VR Reading UIs (CHI 2018):
  https://kaikunze.de/papers/pdf/dingler2018vr.pdf
- Openreview -- Display Type Impact on VR Reading:
  https://openreview.net/pdf?id=0k1GnhWmTN
- Valve -- Index FOV Deep Dive:
  https://www.valvesoftware.com/en/index/deep-dive/fov
- VRcompare -- Headset Specifications:
  https://vr-compare.com/headset/valveindex
- Immersed -- Virtual Desktop Productivity:
  https://immersed.com/
- Apple -- Vision Pro Mac Virtual Display:
  https://support.apple.com/en-us/118521
- Collabora -- WXRD Wayland XR Compositor:
  https://www.collabora.com/news-and-blog/news-and-events/wxrd-a-standalone-wayland-compositor-for-xrdesktop.html
- SimulaVR -- Linux VR Desktop:
  https://github.com/SimulaVR/Simula
