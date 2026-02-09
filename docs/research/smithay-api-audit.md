# Smithay API Surface Audit

Research date: 2026-02-08
Smithay version: 0.7.0 (latest stable), unreleased master has breaking changes

## Overview

Smithay is a Rust library providing building blocks for Wayland compositor development. It uses a callback-oriented architecture built on calloop (event loop library). The primary pattern is: store module-specific `*State` in your compositor state struct, implement the corresponding `*Handler` trait, then invoke the matching `delegate_*!` macro to wire up Wayland protocol dispatch.

All handler callbacks receive `&mut self` (your compositor state), enabling centralized mutable state access without `Rc`/`Arc` synchronization overhead. This is possible because calloop callback invocation is always sequential (single-threaded).

---

## Core Handler Traits

### 1. CompositorHandler

**Module:** `smithay::wayland::compositor`
**Delegate macro:** `delegate_compositor!`

Handles wl_compositor and wl_subcompositor protocol objects. This is the most fundamental handler -- every compositor must implement it.

**Required methods:**

```rust
trait CompositorHandler {
    /// Return mutable reference to CompositorState stored in your state
    fn compositor_state(&mut self) -> &mut CompositorState;

    /// Retrieve per-client compositor state from a Wayland client
    fn client_compositor_state<'a>(
        &self,
        client: &'a wayland_server::Client,
    ) -> &'a CompositorClientState;

    /// Called on every surface buffer commit (the final step of a 5-stage commit sequence)
    fn commit(&mut self, surface: &WlSurface);
}
```

**Implementation pattern (from anvil/niri):**

```rust
impl CompositorHandler for State {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        client.get_data::<ClientState>().unwrap().compositor_state()
    }

    fn commit(&mut self, surface: &WlSurface) {
        // Handle buffer commits: update window textures, check for
        // configure acks, process subsurface tree changes
        // Use with_surface_tree_upward/downward for tree traversal
    }
}
delegate_compositor!(State);
```

**API stability:** Core trait, stable since 0.3. The unreleased master changes surface state accessors: "current state" fields removed in favor of `with_committed_state()` and `last_acked` field. Migration required when upgrading past 0.7.

---

### 2. ShmHandler

**Module:** `smithay::wayland::shm`
**Delegate macro:** `delegate_shm!`

Manages shared-memory buffer protocol (wl_shm). Used for CPU-rendered client buffers.

**Required methods:**

```rust
trait ShmHandler {
    /// Return reference to ShmState
    fn shm_state(&self) -> &ShmState;
}
```

**Related trait -- BufferHandler:**

```rust
trait BufferHandler {
    /// Called when a wl_buffer is destroyed by a client
    fn buffer_destroyed(&mut self, buffer: &WlBuffer);
}
```

**Buffer access API:**

```rust
// Read buffer contents via closure
with_buffer_contents(&buffer, |ptr: *const u8, len: usize, data: BufferData| {
    // Access raw pixel data
    // BufferData contains format, stride, dimensions
}) -> Result<T, BufferAccessError>
// Errors: NotManaged, BadMap, NotReadable
```

**Safety:** Smithay installs a SIGBUS handler to protect against clients providing incorrect memory pool sizes.

**Implementation pattern:**

```rust
impl ShmHandler for State {
    fn shm_state(&self) -> &ShmState {
        &self.shm_state
    }
}
delegate_shm!(State);
```

**API stability:** Very stable, minimal changes across versions.

---

### 3. SeatHandler

**Module:** `smithay::input`
**Delegate macro:** `delegate_seat!`

Central input management trait. The Seat concept represents a group of input devices (keyboard, pointer, touch) -- typically one seat `"seat0"` for most compositors.

**Required associated types:**

```rust
trait SeatHandler {
    type KeyboardFocus: KeyboardTarget;   // What can receive keyboard input
    type PointerFocus: PointerTarget;     // What can receive pointer input
    type TouchFocus: TouchTarget;         // What can receive touch input
}
```

**Required methods:**

```rust
trait SeatHandler {
    /// Return mutable reference to SeatState
    fn seat_state(&mut self) -> &mut SeatState<Self>;

    /// Called when focus changes on any input device
    fn focus_changed(
        &mut self,
        seat: &Seat<Self>,
        focused: Option<&Self::KeyboardFocus>,
    );

    /// Called when cursor image should change
    fn cursor_image(
        &mut self,
        seat: &Seat<Self>,
        image: CursorImageStatus,
    );
}
```

**Adding input capabilities:**

```rust
let keyboard = seat.add_keyboard(xkb_config, repeat_delay, repeat_rate)?;
let pointer = seat.add_pointer();
let touch = seat.add_touch();
```

**Implementation pattern (niri):**

```rust
impl SeatHandler for Niri {
    type KeyboardFocus = WlSurface;  // or a custom FocusTarget enum
    type PointerFocus = WlSurface;
    type TouchFocus = WlSurface;

    fn seat_state(&mut self) -> &mut SeatState<Self> {
        &mut self.seat_state
    }

    fn focus_changed(&mut self, _seat: &Seat<Self>, focused: Option<&WlSurface>) {
        // Update data device focus to follow keyboard focus
        set_data_device_focus(&self.display_handle, seat, focused.cloned());
    }

    fn cursor_image(&mut self, _seat: &Seat<Self>, image: CursorImageStatus) {
        self.cursor_image = image;
    }
}
delegate_seat!(Niri);
```

**API stability:** Stable. The associated type system has been consistent since 0.3. `KeyboardHandle::set_modifier_state()` added in 0.7.

---

### 4. KeyboardHandler (Server-side Grab Interface)

**Module:** `smithay::input::keyboard`

Smithay's server-side keyboard handling does not use a single "KeyboardHandler" trait. Instead, it uses the **grab pattern** via `KeyboardGrab`:

**KeyboardGrab trait (required methods):**

```rust
trait KeyboardGrab {
    /// Process a keyboard event (key press/release)
    fn input(
        &mut self,
        data: &mut D,
        handle: &mut KeyboardInnerHandle<'_, D>,
        keycode: u32,
        state: KeyState,
        modifiers: Option<ModifiersState>,
        serial: Serial,
        time: u32,
    );

    /// Handle focus change request during grab
    fn set_focus(
        &mut self,
        data: &mut D,
        handle: &mut KeyboardInnerHandle<'_, D>,
        focus: Option<&<D as SeatHandler>::KeyboardFocus>,
        serial: Serial,
    );

    /// Return the start data for this grab
    fn start_data(&self) -> &GrabStartData<D>;

    /// Called when the grab is removed
    fn unset(&mut self, data: &mut D);
}
```

**Known implementations:** `PopupKeyboardGrab`, `InputMethodKeyboardGrab`, `XWaylandKeyboardGrab`.

**Note for EXWM-VR:** For processing keyboard input from backend events (libinput), compositors typically pattern-match on `InputEvent::Keyboard` variants and then call `keyboard.input(...)` on the `KeyboardHandle`. The grab mechanism is for intercepting events already routed to the seat.

**API stability:** Stable. The grab pattern has been consistent.

---

### 5. PointerHandler (Server-side Grab Interface)

**Module:** `smithay::input::pointer`

Similar to keyboard, pointer handling uses the grab pattern:

**PointerGrab trait:**

Defines the interface for intercepting pointer events (motion, button, axis). Compositors implement this for interactive operations like window move/resize.

**PointerTarget trait:**

```rust
trait PointerTarget {
    // Objects that can receive pointer interactions
    // Must be implemented by your focus target type
}
```

**Core handle types:**

- `PointerHandle` -- main pointer manager, initiates grabs, sets cursor position
- `PointerInnerHandle` -- accessed inside grab logic, directly sends events to clients

**Implementation pattern:** Most compositors use the default pointer behavior and only implement custom grabs for window management operations (move, resize). The `PointerHandle` is obtained from the seat and used to set focus, send motion/button events, and manage grabs.

**API stability:** Stable since 0.3.

---

### 6. DataDeviceHandler

**Module:** `smithay::wayland::selection::data_device`
**Delegate macro:** `delegate_data_device!`

Handles clipboard (selections) and drag-and-drop.

**Required methods:**

```rust
trait DataDeviceHandler {
    /// Return mutable reference to DataDeviceState
    fn data_device_state(&mut self) -> &mut DataDeviceState;
}
```

**Related trait -- DnD handling (BREAKING CHANGE in unreleased master):**

In 0.7 and earlier:
```rust
trait ServerDndGrabHandler {
    fn dnd_requested(&mut self, ...);
}
```

In unreleased master, this was split:
```rust
trait DnDGrabHandler { ... }       // Generic DnD interface
trait WaylandDndGrabHandler { ... } // Wayland-specific DnD
// WaylandDndGrabHandler::started() must now be explicitly implemented
```

**Key APIs:**

```rust
// Set clipboard content from compositor
set_data_device_selection(&display_handle, &seat, mime_types, source);

// Request current selection from client
request_data_device_client_selection(&seat);

// Focus follows keyboard focus
set_data_device_focus(&display_handle, &seat, surface);
```

**API stability:** Breaking changes in unreleased master. The DnD interface has been significantly reworked for X11-Wayland interop support.

---

### 7. XdgShellHandler

**Module:** `smithay::wayland::shell::xdg`
**Delegate macro:** `delegate_xdg_shell!`

Manages xdg_shell protocol -- the standard window management protocol for Wayland.

**Required methods:**

```rust
trait XdgShellHandler {
    /// Return mutable reference to XdgShellState
    fn xdg_shell_state(&mut self) -> &mut XdgShellState;

    /// Called when a client creates a new toplevel window
    fn new_toplevel(&mut self, surface: ToplevelSurface);

    /// Called when a client creates a popup/tooltip
    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState);

    /// Called when a popup requests input grab
    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial);

    /// Called when a popup requests repositioning (optional with default)
    fn reposition_request(
        &mut self,
        surface: PopupSurface,
        positioner: PositionerState,
        token: u32,
    );
}
```

**Surface handle types:**

- `ShellClient` -- per-client metadata
- `ToplevelSurface` -- main application windows (has `send_configure()`, `send_close()`, etc.)
- `PopupSurface` -- context menus, tooltips, dropdowns

**Implementation pattern (niri):**

```rust
impl XdgShellHandler for Niri {
    fn xdg_shell_state(&mut self) -> &mut XdgShellState {
        &mut self.xdg_shell_state
    }

    fn new_toplevel(&mut self, surface: ToplevelSurface) {
        // Add window to layout, send initial configure
        self.layout.add_window(surface.clone());
        surface.send_configure();
    }

    fn new_popup(&mut self, surface: PopupSurface, positioner: PositionerState) {
        // Position popup relative to parent using positioner
    }

    fn grab(&mut self, surface: PopupSurface, seat: WlSeat, serial: Serial) {
        // Set up popup grab for menus
    }
}
delegate_xdg_shell!(Niri);
```

**API stability:** Stable core, but unreleased master adds client acking enforcement for configure events and cached state structures tracking `last_acked` configurations. `reset_initial_configure_sent()` methods are being removed.

---

### 8. DmabufHandler

**Module:** `smithay::wayland::dmabuf`
**Delegate macro:** `delegate_dmabuf!`

Handles GPU buffer import via linux-dmabuf protocol. Critical for zero-copy rendering.

**Required methods:**

```rust
trait DmabufHandler {
    /// Return mutable reference to DmabufState
    fn dmabuf_state(&self) -> &mut DmabufState;

    /// Called when a client submits a dmabuf for import
    fn dmabuf_imported(
        &mut self,
        global: &DmabufGlobal,
        dmabuf: Dmabuf,
        notifier: ImportNotifier,
    );

    /// Optional: per-surface feedback for format negotiation
    fn new_surface_feedback(
        &mut self,
        surface: &WlSurface,
    ) -> Option<DmabufFeedback> {
        None // default
    }
}
```

**Import validation flow:**

```rust
impl DmabufHandler for State {
    fn dmabuf_imported(&mut self, _global: &DmabufGlobal, dmabuf: Dmabuf, notifier: ImportNotifier) {
        // Test if the renderer can import this buffer
        if self.renderer.import_dmabuf(&dmabuf).is_ok() {
            notifier.successful::<State>();
        } else {
            notifier.failed();
        }
    }
}
```

**API stability:** `GbmFramebufferExporter::new()` changed in both 0.7 and unreleased master (added `NodeFilter` parameter for dmabuf origin filtering).

---

### 9. DRM Backend (DrmDevice, DrmSurface, DrmCompositor)

**Module:** `smithay::backend::drm`

The DRM backend is not a single handler trait but a set of structures for direct hardware display management. Note: smithay 0.7 introduced `DrmSyncobjHandler` for explicit sync.

**Core types:**

- `DrmDevice` -- wraps a DRM file descriptor, manages hardware resources
- `DrmSurface` -- combines one CRTC + connectors + mode for output
- `DrmCompositor` -- optional high-level helper for hardware plane composition
- `DrmNode` -- represents a DRM device node (render vs primary)

**DrmSyncobjHandler trait (added 0.7):**

```rust
trait DrmSyncobjHandler {
    fn drm_syncobj_state(&mut self) -> Option<&mut DrmSyncobjState>;
    // Changed from returning &DrmSyncobjState to Option in 0.7
}
```

**Device initialization pattern:**

```rust
// 1. Open DRM device
let (device, notifier) = DrmDevice::new(fd, logger)?;

// 2. Insert into calloop event loop
let token = event_loop.handle().insert_source(notifier, |event, _, state| {
    // Handle DRM events (vblank, page flip)
})?;

// 3. Create surface: CRTC + connector + mode
// Encoder is auto-selected for the connector
let surface = device.create_surface(crtc, mode, &[connector])?;

// 4. Surface state is double-buffered
// Changes to connectors/mode are stored and applied on next commit
surface.commit(framebuffer)?;
```

**CRTC/connector management:**

- One CRTC per surface (cannot share CRTCs between surfaces)
- Connectors represent physical display ports
- Encoders are auto-selected during surface creation
- Number of available CRTCs determines max independent outputs
- Overlapping connector sets across surfaces produce undefined behavior

**Key types added in 0.7:**

- `WeakDrmDeviceFd` with `downgrade()`/`upgrade()` methods
- `DrmSyncobjState::into_global()` and `update_device()` methods

**API stability:** Active area of development. Breaking changes in most releases. The `GbmFramebufferExporter::new()` signature changed in both 0.7 and unreleased master.

---

### 10. Input Backend (InputBackend trait)

**Module:** `smithay::backend::input`

Generic trait for input event sources. Implemented by `LibinputInputBackend` and `WinitInputBackend`.

**InputBackend trait:**

The trait describes objects providing a source of input events. All input backends guarantee the same base precision for events.

**InputEvent enum variants dispatch to specialized event traits:**

| Category | Event Traits |
|----------|-------------|
| **Keyboard** | `KeyboardKeyEvent` (keycode, `KeyState::Pressed/Released`) |
| **Pointer** | `PointerMotionEvent` (relative), `PointerMotionAbsoluteEvent` (absolute), `PointerButtonEvent` (`MouseButton`, `ButtonState`), `PointerAxisEvent` (`Axis`, `AxisSource`, `AxisRelativeDirection`) |
| **Touch** | `TouchDownEvent`, `TouchUpEvent`, `TouchMotionEvent`, `TouchFrameEvent`, `TouchCancelEvent` |
| **Tablet** | `TabletToolEvent`, `TabletToolAxisEvent`, `TabletToolButtonEvent`, `TabletToolTipEvent`, `TabletToolProximityEvent` |
| **Gesture** | Swipe, Pinch, Hold -- each with begin/update/end phases |
| **Switch** | `SwitchToggleEvent` (lid close, tablet mode) |

**LibinputInputBackend integration pattern:**

```rust
// Insert libinput backend as calloop source
let backend = LibinputInputBackend::new(libinput_context);
event_loop.handle().insert_source(backend, |event, _, state| {
    // Pattern match on InputEvent variants
    match event {
        InputEvent::Keyboard { event } => {
            let keycode = event.key_code();
            let key_state = event.state();
            state.seat.get_keyboard().unwrap().input(
                state, keycode, key_state, serial, time,
                |state, modifiers, keysym| {
                    // Filter or handle compositor keybindings
                    FilterResult::Forward // or Intercept
                },
            );
        }
        InputEvent::PointerMotion { event } => { ... }
        InputEvent::PointerButton { event } => { ... }
        _ => {}
    }
});
```

**API stability:** Event traits are stable. The `InputBackend` trait itself is stable. Backend-specific details may change.

---

## Additional Handler Traits (Brief)

| Trait | Delegate Macro | Purpose |
|-------|---------------|---------|
| `LayerShellHandler` | `delegate_layer_shell!` | wlr-layer-shell for panels, notifications, lockscreens |
| `OutputHandler` | `delegate_output!` | wl_output advertisement to clients |
| `XdgActivationHandler` | `delegate_xdg_activation!` | Window activation tokens (focus stealing prevention) |
| `KeyboardShortcutsInhibitHandler` | `delegate_keyboard_shortcuts_inhibit!` | Allow apps to capture compositor shortcuts |
| `VirtualKeyboardManagerHandler` | `delegate_virtual_keyboard_manager!` | Virtual keyboard protocol |
| `TabletSeatHandler` | `delegate_tablet_manager!` | Tablet input protocol |
| `PrimarySelectionHandler` | `delegate_primary_selection!` | Primary selection (middle-click paste) |
| `XdgDecorationHandler` | `delegate_xdg_decoration!` | Server-side vs client-side decoration negotiation |

---

## API Stability Assessment Summary

| Trait/Module | Stability | Notes |
|-------------|-----------|-------|
| CompositorHandler | **High** | Core trait, stable since 0.3. Surface state accessor changes pending in master. |
| ShmHandler | **Very High** | Minimal changes across all versions. |
| SeatHandler | **High** | Associated type system consistent since 0.3. |
| KeyboardGrab | **High** | Grab pattern consistent. |
| PointerGrab | **High** | Grab pattern consistent. |
| DataDeviceHandler | **Medium** | DnD interface significantly reworked in unreleased master. |
| XdgShellHandler | **High** | Core methods stable; configure acking changes in master. |
| DmabufHandler | **Medium** | GbmFramebufferExporter signature changes in recent versions. |
| DRM Backend | **Low-Medium** | Active development area, breaking changes in most releases. |
| InputBackend | **High** | Event traits stable. |

---

## CHANGELOG: Notable Breaking Changes (0.3 -> 0.7 -> unreleased)

### Unreleased (master)

- `ServerDndGrabHandler` removed; split into `DnDGrabHandler` + `WaylandDndGrabHandler`
- Surface state: "current state" fields removed; use `with_committed_state()` and `last_acked`
- `ToplevelSurface::reset_initial_configure_sent()` removed
- `GbmFramebufferExporter::new` now accepts `NodeFilter` parameter
- `X11WM::start_wm` requires new trait implementations (`DndGrabHandler`, `DndFocus`)
- New: `ExtBackgroundEffect` protocol, generic DnD interface for X11-Wayland interop

### Version 0.7.0

- `DrmSyncobjHandler::drm_syncobj_state()` returns `Option` instead of reference
- `DrmTimeline::new()` accepts `OwnedFd` instead of `BorrowedFd`
- `GbmFramebufferExporter::new()` signature changed (added `import_node`)
- New: `WeakDrmDeviceFd`, `KeyboardHandle::set_modifier_state()`, `XdgToplevelTag`

### Version 0.6.0

- `RenderContext::draw()` callback now receives mutable reference
- `Framebuffer` type must implement `Texture` trait
- Damage API renamed: `damage()` -> `raw()` on DamageBag/DamageSnapshot

---

## Recommendations for EXWM-VR

1. **Pin to smithay 0.7.0** for initial development; the unreleased master has significant breaking changes. Upgrade when 0.8 stabilizes.

2. **Start with the minimal handler set:** CompositorHandler, ShmHandler, SeatHandler, XdgShellHandler, OutputHandler. Add DmabufHandler and DRM backend when GPU rendering is needed.

3. **Follow the anvil/niri pattern:** Centralized state struct with all `*State` fields, implement traits on it, use `delegate_*!` macros.

4. **For VR rendering:** The DRM backend will need careful attention since we are rendering to a headset rather than physical monitors. The `DrmCompositor` may not be directly applicable; we may need to compose to textures submitted via OpenXR instead.

5. **Input handling for VR controllers:** Smithay's input traits cover standard input devices. VR controller input will need a custom `InputBackend` implementation or a separate event source registered with calloop.

---

## References

- [Smithay GitHub Repository](https://github.com/Smithay/smithay)
- [Smithay API Documentation (master)](https://smithay.github.io/smithay/)
- [Smithay API Documentation (docs.rs)](https://docs.rs/smithay)
- [Smithay Compositor Module](https://smithay.github.io/smithay/smithay/wayland/compositor/index.html)
- [Smithay XDG Shell Module](https://smithay.github.io/smithay/smithay/wayland/shell/xdg/index.html)
- [Smithay Input Backend](https://smithay.github.io/smithay/smithay/backend/input/index.html)
- [Smithay DRM Backend](https://smithay.github.io/smithay/smithay/backend/drm/index.html)
- [Smithay CHANGELOG](https://github.com/Smithay/smithay/blob/master/CHANGELOG.md)
- [Anvil Reference Compositor](https://github.com/Smithay/smithay/tree/master/anvil)
- [Smithay Project Website](https://smithay.github.io/index.html)
