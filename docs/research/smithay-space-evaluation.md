# R3.3: Smithay desktop::Space Evaluation

## Overview

Smithay's `desktop` module provides high-level abstractions for building desktop-style
Wayland compositors. The central type is `Space<E>`, a 2D coordinate plane onto which
windows, outputs, and custom elements are mapped. This evaluation assesses whether
Space provides sufficient layout flexibility for tiling, floating, and workspace
management, compares it against niri's custom layout approach, and recommends a
strategy for our Smithay 0.7 compositor.

Smithay docs: https://smithay.github.io/smithay/smithay/desktop/index.html

## Space API (Smithay 0.7)

### Core Type

```rust
pub struct Space<E: SpaceElement + PartialEq> { /* ... */ }
```

Space is generic over any type implementing `SpaceElement`. The simplest case is
`Space<Window>`, but custom element types are supported.

### Element Management

| Method | Description |
|---|---|
| `map_element(elem, location, activate)` | Place element at (x,y); moves to top of z-stack |
| `raise_element(elem)` | Move element to top of z-stack |
| `unmap_elem(elem)` | Remove element from space |
| `elements()` | Iterate elements in z-order (back to front) |
| `elements_for_output(output)` | Elements visible on a specific output |
| `element_location(elem)` | Get element's (x,y) position |
| `element_bbox(elem)` | Bounding box including decorations |
| `element_geometry(elem)` | Content geometry excluding decorations |

### Spatial Queries

| Method | Description |
|---|---|
| `element_under(point)` | Topmost element at point (with input region testing) |
| `outputs_for_element(elem)` | All outputs an element overlaps |
| `output_under(point)` | Output at a given point |

### Output Management

| Method | Description |
|---|---|
| `map_output(output, location)` | Place output in space; updatable |
| `unmap_output(output)` | Remove output |
| `outputs()` | Iterate all mapped outputs |
| `output_geometry(output)` | Output geometry with position |

### Rendering

| Method | Description |
|---|---|
| `render_elements_for_region(region)` | Render elements for arbitrary region (no layers) |
| `render_elements_for_output(output)` | Render elements for output (includes layer surfaces) |
| `refresh()` | Update internal state; handle output enter/leave events |

### Supporting Types

- **`Window`** -- abstracts over xdg-shell toplevels and Xwayland surfaces. Provides
  size calculation, user_data attachment, and rendering helpers. Has no inherent
  position (must be placed in a Space).
- **`LayerMap`** / **`LayerSurface`** -- per-output layer shell surfaces. Retrieved
  via `layer_map_for_output()`. Automatically included in `render_elements_for_output()`.
- **`PopupManager`** -- tracks popup parent-child relationships. Popups render
  automatically alongside their parent toplevel.

### SpaceElement Trait

Custom elements can be placed in a Space by implementing `SpaceElement`:

```rust
pub trait SpaceElement: /* bounds */ {
    fn geometry(&self) -> Rectangle<i32, Logical>;
    fn bbox(&self) -> Rectangle<i32, Logical>;
    fn is_in_input_region(&self, point: &Point<f64, Logical>) -> bool;
    fn z_index(&self) -> u8;
    // ... rendering methods
}
```

Multiple element types can be aggregated using the `space_elements!` macro:

```rust
space_elements! {
    MyElements;
    Window=Window,
    Custom=MyCustomElement,
}
// Creates: enum MyElements { Window(Window), Custom(MyCustomElement) }
// with SpaceElement impl
```

## Layout Flexibility Assessment

### What Space Provides

1. **Absolute positioning** -- elements are placed at explicit (x, y) coordinates.
   The compositor is fully responsible for computing these positions.
2. **Z-ordering** -- a simple stack model (most recently mapped/raised = on top).
   Custom z-index via `SpaceElement::z_index()`.
3. **Output viewports** -- outputs are windows into the infinite 2D plane. Multiple
   outputs can view different regions.
4. **Hit testing** -- `element_under()` performs input region testing against the
   z-ordered element stack.
5. **Damage tracking** -- integrated damage tracking via RenderElements eliminates
   the need to manually track dirty regions.
6. **Layer shell integration** -- automatic per-output layer surface management.

### What Space Does NOT Provide

1. **No layout algorithms** -- Space has no concept of tiling, floating, stacking,
   tabbing, or any layout policy. It is purely a spatial container.
2. **No workspace abstraction** -- there is no built-in workspace switching, virtual
   desktop management, or workspace-per-output model.
3. **No constraints** -- no snap-to-edge, no minimum/maximum size enforcement, no
   gap management, no split ratios.
4. **No animation** -- no built-in transition animations for window movement/resize.
5. **No window grouping** -- no tabs, stacks, or container hierarchy.
6. **No focus management** -- Space tracks z-order but not keyboard/pointer focus
   policies (that's handled by Smithay's `Seat`).

### Assessment for Tiling

Space can **support** tiling layouts, but the compositor must implement all tiling
logic externally:

```rust
// Pseudocode: simple horizontal tiling
fn tile_windows(space: &mut Space<Window>, output: &Output) {
    let geo = space.output_geometry(output).unwrap();
    let windows: Vec<_> = space.elements().cloned().collect();
    let width = geo.size.w / windows.len() as i32;
    for (i, window) in windows.iter().enumerate() {
        let loc = Point::from((geo.loc.x + (i as i32 * width), geo.loc.y));
        window.toplevel().with_pending_state(|state| {
            state.size = Some(Size::from((width, geo.size.h)));
        });
        space.map_element(window.clone(), loc, false);
    }
}
```

This works, but the compositor owns 100% of the layout logic. Space just stores
the resulting positions.

### Assessment for Floating

Floating windows are Space's natural mode -- position elements freely, raise on
focus, use `element_under()` for click-to-focus.

### Assessment for Workspaces

Workspaces require **multiple Space instances** (one per workspace) and compositor
logic to switch between them:

```rust
struct WorkspaceState {
    spaces: Vec<Space<Window>>,
    active: usize,
}

fn switch_workspace(&mut self, idx: usize) {
    self.active = idx;
    // Only render the active space's elements
}
```

This is straightforward but means the compositor manages workspace semantics entirely.

## Comparison: niri's Custom Layout Approach

### niri's Architecture

niri (by YaLTeR) is a scrollable-tiling Wayland compositor built on Smithay. It
implements a **completely custom layout system** rather than using `desktop::Space`.

**Layout hierarchy:**

```
Niri
  -> Layout
       -> MonitorSet
            -> Monitor
                 -> Workspace
                      -> Column
                           -> Tile
                                -> Window (Smithay Window)
```

### Why niri Bypasses Space

1. **Per-monitor independence** -- niri maintains completely separate workspace
   strips per monitor with no shared coordinate space. Space's single 2D plane
   model doesn't naturally support this.

2. **Scrollable strip model** -- windows exist on an infinite horizontal strip.
   The "viewport" scrolls smoothly across the strip. This requires:
   - Fractional scroll offsets (`view_offset: f64`)
   - Smooth animation of scroll position
   - Column-based layout with variable widths
   None of these are provided by Space.

3. **Custom rendering** -- niri generates its own `RenderElement` instances through
   `Layout::render_elements()`, computing per-element transforms, opacity, and
   clipping for scroll animations.

4. **Dynamic workspaces** -- workspaces are created/destroyed automatically as
   windows are added/removed. Each monitor always has one empty workspace at the
   bottom.

### What niri Still Uses from Smithay

niri uses Smithay extensively -- just not `desktop::Space`:

- **Protocol handling** -- xdg-shell, layer-shell, DMA-BUF, etc.
- **Renderer** -- `GlesRenderer` / `GlowRenderer` via the `Renderer` trait
- **Backend** -- DRM/KMS, libinput, session management
- **Input** -- `Seat`, `KeyboardHandle`, `PointerHandle`
- **Window** -- `Window` type for xdg-shell surface management

This demonstrates that Smithay is **modular** -- you can skip `desktop::Space` while
still benefiting from the rest of the framework.

## Comparison: cosmic-comp's Approach

COSMIC (System76/Pop!_OS) also builds on Smithay and implements custom layout logic:

- Supports both tiling and floating modes per workspace
- BSP-style tiling with adjustable split ratios
- Uses Smithay's protocol handling and rendering
- Implements its own workspace and layout management layer

This further validates the pattern: use Smithay for protocol/rendering, build custom
layout on top.

## Other Smithay-Based Compositors

| Compositor | Layout Approach | Uses Space? |
|---|---|---|
| **anvil** (Smithay demo) | Simple floating | Yes |
| **niri** | Scrollable-tiling | No (custom Layout) |
| **cosmic-comp** | Tiling + floating | Custom (on Smithay) |
| **Pinnacle** | Configurable (Lua/Rust) | Custom (AwesomeWM-style) |

Pattern: **only the simplest compositors use Space directly**. Anything with
non-trivial layout requirements implements custom logic.

## Recommendation for EXWM-VR

### Phase 1: Use Space Initially

**Recommendation: YES, start with Space.**

Rationale:

1. **Fast prototyping** -- Space handles damage tracking, z-ordering, output
   management, layer shells, and popup tracking out of the box. This is significant
   boilerplate we don't need to write.

2. **Floating mode is natural** -- our VR compositor will initially position windows
   as floating quads in 3D space. In the 2D projection (for non-VR fallback), Space's
   floating model works perfectly.

3. **Tiling is implementable** -- basic tiling (even i3/bspwm-style) only requires
   computing window positions externally and calling `map_element()` with the results.
   Space stores the layout; we compute it.

4. **Workspaces via multiple Spaces** -- create one `Space<Window>` per workspace.
   Switch by rendering from the active one. This is a proven pattern (anvil does it).

5. **VR 3D layout is separate anyway** -- in VR mode, the 3D spatial positioning of
   window quads is inherently custom (OpenXR poses, ray-cast hit testing). Space
   won't help with 3D but can manage the 2D "flat desktop" fallback.

### Phase 2: Evaluate Custom Layout Later

As the compositor matures, we may outgrow Space if we need:

- **Smooth animations** -- Space doesn't interpolate positions. If we want animated
  workspace transitions, window fly-in effects, etc., we'll need to drive animations
  externally and call `map_element()` on each frame, or switch to custom
  RenderElement generation.

- **Complex tiling semantics** -- if we implement EXWM-style per-workspace tiling
  with master/stack, tabs, containers, etc., the external layout calculator may
  become complex enough that Space is just overhead.

- **3D-native layout** -- if we eventually want the VR layout to be the primary
  mode (not just a 2D fallback projected into 3D), Space's 2D coordinate model
  becomes irrelevant. We'd compute 3D poses directly and generate RenderElements
  manually.

### Concrete Plan

```
Phase 1 (MVP):
  - One Space<Window> per workspace
  - Floating layout (Space's natural mode)
  - Basic tiling via external position calculator
  - Space handles damage tracking, popups, layer shells
  - VR mode: project Space element positions into 3D poses

Phase 2 (Maturity):
  - Evaluate whether Space overhead justifies its benefits
  - If tiling/animation needs exceed Space's model, migrate to:
    a. Custom layout struct (like niri's Layout)
    b. Direct RenderElement generation
    c. Keep Space for layer shell / popup management only
  - VR-native layout with 3D-first positioning
```

### API Usage Sketch (Phase 1)

```rust
use smithay::desktop::{Space, Window, space_elements, layer_map_for_output};

// Workspace management
struct Compositor {
    workspaces: Vec<Space<Window>>,
    active_workspace: usize,
    // VR-specific state
    vr_poses: HashMap<Window, Pose3D>,
}

impl Compositor {
    fn map_window(&mut self, window: Window, location: Point<i32, Logical>) {
        let space = &mut self.workspaces[self.active_workspace];
        space.map_element(window.clone(), location, true);
        // In VR mode, also compute initial 3D pose
        self.vr_poses.insert(window, Pose3D::from_2d(location));
    }

    fn render_2d(&mut self, output: &Output, renderer: &mut GlesRenderer) {
        let space = &self.workspaces[self.active_workspace];
        let elements = space.render_elements_for_output(output, renderer);
        // ... submit to output
    }

    fn render_vr(&mut self, renderer: &mut GlesRenderer) {
        // For VR, iterate elements and render each as a textured 3D quad
        let space = &self.workspaces[self.active_workspace];
        for elem in space.elements() {
            let texture = /* get texture from element */;
            let pose = self.vr_poses.get(elem);
            // Render textured quad at 3D pose into OpenXR swapchain
        }
    }

    fn retile(&mut self, output: &Output) {
        let space = &mut self.workspaces[self.active_workspace];
        let geo = space.output_geometry(output).unwrap();
        let windows: Vec<_> = space.elements().cloned().collect();
        // Compute tiling layout
        let layout = compute_bsp_layout(&windows, geo);
        for (window, rect) in layout {
            window.toplevel().with_pending_state(|state| {
                state.size = Some(rect.size);
            });
            space.map_element(window, rect.loc, false);
        }
    }
}
```

## Summary Table

| Criterion | Space | Custom (niri-style) |
|---|---|---|
| **Time to MVP** | Fast | Slow |
| **Damage tracking** | Free | Must implement |
| **Popup management** | Free | Must implement |
| **Layer shell** | Free | Must implement |
| **Floating layout** | Natural | Must implement |
| **Tiling layout** | External calc | External calc |
| **Workspace switching** | Multiple Spaces | Custom structs |
| **Smooth animations** | Awkward (manual) | Natural (custom render) |
| **Complex containers** | Overhead | Clean |
| **VR 3D layout** | Irrelevant (2D only) | Irrelevant (custom either way) |
| **Rendering control** | Opaque | Full |

**Bottom line: Use Space for Phase 1. The free damage tracking, popup management,
and layer shell integration save significant development time. The layout computation
lives outside Space regardless, so migration to a custom approach later is
straightforward -- we'd only need to replace the spatial container and rendering,
not the layout algorithms themselves.**

## References

- Smithay desktop module: https://smithay.github.io/smithay/smithay/desktop/index.html
- Smithay Space struct: https://smithay.github.io/smithay/smithay/desktop/space/struct.Space.html
- Smithay space_elements macro: https://smithay.github.io/smithay/smithay/desktop/space/macro.space_elements.html
- Smithay releases: https://github.com/smithay/smithay/releases
- niri source: https://github.com/YaLTeR/niri
- niri layout architecture: https://deepwiki.com/YaLTeR/niri/2.1-window-and-layout-management
- niri workspace model: https://deepwiki.com/YaLTeR/niri/2.2-workspace-and-monitor-model
- cosmic-comp: https://github.com/pop-os/cosmic-comp
- Smithay Window Management discussion: https://github.com/Smithay/smithay/discussions/363
- Pinnacle compositor: https://github.com/pinnacle-comp/pinnacle
