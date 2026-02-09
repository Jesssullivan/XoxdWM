# niri Architecture Study

Research date: 2026-02-08
niri version: ~0.1.10+ (actively developed, scrollable-tiling Wayland compositor)
Repository: https://github.com/YaLTeR/niri

## Overview

niri is the most successful Smithay-based Wayland compositor in production use. It implements a scrollable-tiling window management paradigm where windows are arranged in an infinite horizontal strip per monitor. It is a single-threaded, event-driven application built on calloop + Smithay.

---

## Crate Structure

niri is organized as a Cargo workspace with four crates:

| Crate | Purpose |
|-------|---------|
| `niri` | Main compositor binary (~6300 lines in `src/niri.rs` alone) |
| `niri-config` | KDL-based configuration parser using the `knuffel` crate |
| `niri-ipc` | JSON-based IPC protocol definitions (minimal dependencies) |
| `niri-visual-tests` | GTK4-based visual testing application |

This separation is deliberate: `niri-ipc` has minimal dependencies so external tools can link against it without pulling in all of Smithay. `niri-config` is separated so config validation can be performed independently of the compositor.

---

## Event Loop Structure (calloop sources)

niri uses **calloop 0.14.3** as its event loop, with features: `executor`, `futures-io`, and Unix signals.

### Registered Event Sources

The calloop event loop multiplexes these sources:

1. **Wayland client connections** -- Smithay's `Display` is inserted as a calloop source. When a client sends a protocol request, calloop dispatches it to the appropriate handler trait method on the `Niri` struct.

2. **Input device events** -- `LibinputInputBackend` is registered as a calloop source. Events are dispatched to `Niri::handle_input()` in `src/input.rs`.

3. **DRM device events** -- Page flip completions and vblank events from DRM devices trigger rendering of the next frame.

4. **Animation timers** -- calloop timers drive animation updates. When animations are active, the compositor schedules redraws at the display refresh rate.

5. **IPC socket connections** -- A Unix domain socket listener is registered. New connections are accepted and their requests are processed within the event loop.

6. **D-Bus messages** -- Asynchronous D-Bus integration (behind a feature flag) for screencasting, portal integration, and system notifications.

7. **Unix signals** -- SIGHUP triggers configuration reload. SIGTERM/SIGINT trigger graceful shutdown.

8. **udev device monitor** -- Watches for GPU hotplug, monitor connect/disconnect events.

### Event Loop Flow

```
calloop::EventLoop::dispatch()
    |
    +-- Wayland client request arrives
    |     -> Smithay dispatches to handler trait method
    |     -> Handler updates Niri state (layout, focus, etc.)
    |     -> Sends configure events back to clients
    |
    +-- libinput event arrives
    |     -> Niri::handle_input() pattern matches event type
    |     -> Keybindings checked against Config::binds
    |     -> If bound: Niri::do_action(Action) dispatches to Layout
    |     -> If unbound: forwarded to focused client via seat handles
    |
    +-- DRM page flip complete
    |     -> Render next frame
    |     -> Layout::render_elements() -> Workspace -> Column -> Tile
    |     -> Submit to DRM surface
    |
    +-- Timer fires
    |     -> Update animation state
    |     -> Schedule redraw if needed
    |
    +-- IPC request arrives
    |     -> Parse JSON, execute command, return JSON response
    |
    +-- Signal received
          -> SIGHUP: reload config
          -> SIGTERM: graceful shutdown
```

---

## State Management Pattern

### Central State Struct

All compositor state is centralized in the `Niri` struct (`src/niri.rs`). This follows the Smithay-recommended pattern of avoiding `Rc`/`Arc` by using calloop's shared data mechanism.

Key fields (reconstructed from architecture analysis):

```rust
struct Niri {
    // Smithay core
    display_handle: DisplayHandle,
    compositor_state: CompositorState,
    xdg_shell_state: XdgShellState,
    shm_state: ShmState,
    seat_state: SeatState<Niri>,
    data_device_state: DataDeviceState,
    dmabuf_state: DmabufState,

    // Layout hierarchy (the window management core)
    layout: Layout,

    // Configuration
    config: Config,

    // Backend (winit or tty)
    backend: Backend,

    // Input state
    seat: Seat<Niri>,
    cursor_image: CursorImageStatus,
    gesture_state: Option<GestureState>,
    tablet_state: Option<TabletState>,

    // Outputs
    outputs: Vec<Output>,

    // IPC
    ipc_server: Option<IpcServer>,

    // Event loop handle for registering new sources
    event_loop_handle: LoopHandle<'static, Niri>,
}
```

### Layout Hierarchy

The layout system is the core differentiator. It enforces per-monitor independence:

```
Niri
  -> Layout
       -> MonitorSet
            -> Vec<Monitor>    (one per connected display)
            |    -> Output     (physical display reference)
            |    -> Vec<Workspace>
            |         -> Vec<Column>
            |         |    -> Vec<Tile>
            |         |         -> Window (wraps ToplevelSurface)
            |         -> view_offset: f64  (horizontal scroll position)
            |         -> active_column_idx: usize
            -> active_monitor_idx: usize
            -> primary_idx: Option<usize>
```

- **MonitorSet** manages multi-display state, tracks which monitor is active
- **Monitor** owns an independent workspace stack per physical output
- **Workspace** represents the infinite horizontal strip; `view_offset` tracks scroll position
- **Column** groups vertically-stacked windows
- **Tile** wraps a window with tile-specific metadata

### Dynamic Workspace Management

Workspaces are created/destroyed automatically:
- Each monitor always has one empty workspace at the bottom
- Opening a window on the empty workspace creates a new empty one
- Closing all windows on a workspace removes it (except the last empty one)
- On monitor disconnect, workspaces store `original_output` connector name
- On reconnect, workspaces migrate back to the matching output

---

## Smithay Trait Implementation

niri implements all required Smithay handler traits on the `Niri` struct and uses delegate macros:

```rust
// Protocol handlers
delegate_compositor!(Niri);
delegate_xdg_shell!(Niri);
delegate_layer_shell!(Niri);    // wlr-layer-shell for panels/notifications
delegate_seat!(Niri);
delegate_shm!(Niri);
delegate_dmabuf!(Niri);
delegate_data_device!(Niri);
delegate_output!(Niri);
delegate_xdg_activation!(Niri);
// ... and many more
```

When a client sends a protocol request, Smithay's event loop dispatches it to the appropriate trait method on `Niri`, which updates internal state and sends configure events back.

### Key trait implementations:

- **CompositorHandler::commit()** -- Processes buffer commits, triggers rendering
- **XdgShellHandler::new_toplevel()** -- Adds window to Layout, applies window rules
- **SeatHandler::focus_changed()** -- Updates data device focus, triggers border redraws
- **SeatHandler::cursor_image()** -- Updates cursor texture for rendering

---

## Input Processing

Located in `src/input.rs`, the input system has a clear dispatch chain:

```
libinput events
  -> Niri::handle_input()
       -> Pattern match on InputEvent variant
            -> KeyboardKeyEvent: check keybindings -> forward or intercept
            -> PointerMotionEvent: update cursor, check focus target
            -> PointerButtonEvent: handle clicks, window interactions
            -> GestureSwipeEvent: accumulate in GestureState
            -> TabletToolEvent: map to output, update TabletState
```

### Gesture System

Built on libinput's `GestureSwipe` events:
1. Accumulate deltas in `GestureState` struct
2. Recognize gesture type and direction
3. Apply visual feedback through layout updates (workspace switch animation)
4. Finalize or cancel based on threshold values

### Action Dispatch

```rust
// Config binds map key combinations to Action variants
enum Action {
    FocusLeft, FocusRight, FocusDown, FocusUp,
    MoveLeft, MoveRight,
    Spawn(Vec<String>),
    CloseWindow,
    SwitchWorkspace(i32),
    // ... many more
}

// Niri::do_action() routes to Layout methods
fn do_action(&mut self, action: Action) {
    match action {
        Action::FocusLeft => self.layout.focus_left(),
        Action::Spawn(cmd) => self.spawn(cmd),
        // ...
    }
}
```

---

## IPC Mechanism (niri-ipc crate)

### Protocol Design

- **Transport:** Unix domain socket at `$NIRI_SOCKET` (typically `/run/user/UID/niri.sock`)
- **Format:** JSON, one message per line (newline-delimited)
- **Pattern:** Request/Response with optional event streaming

### Message Types

```rust
// In niri-ipc crate
enum Request {
    FocusedWindow,
    Workspaces,
    Outputs,
    Action(Action),
    EventStream,          // Subscribe to continuous events
    ValidateConfig,
    ReloadConfig,
    // ... many more
}

enum Reply {
    Ok(Response),         // Wraps specific response data
    Err(String),          // Error message
}

enum Event {
    WorkspacesChanged { ... },
    WindowFocusChanged { ... },
    OutputChanged { ... },
    // Sent continuously after EventStream request
}
```

### Event Streaming

When a client sends `EventStream` request:
1. niri sends complete current state up-front
2. Then continuously streams incremental updates
3. Connection stays open until client disconnects
4. No polling required -- ideal for status bars (waybar, etc.)

### Stability Guarantees

- JSON field names and enum variants are backward-compatible (new fields may be added)
- The Rust API (niri-ipc crate) follows niri's version, NOT semver -- new struct fields and enum variants are added freely
- Human-readable output from `niri msg` has no stability guarantees

### CLI Tool

```bash
niri msg [--json] <command>
# Examples:
niri msg focused-window
niri msg workspaces
niri msg action focus-workspace 2
niri msg validate-config
niri msg reload-config
```

---

## Configuration System

### Format and Location

niri uses **KDL (KDL Document Language)** for configuration, parsed by the `niri-config` crate via the `knuffel` library.

**File search order:**
1. `$NIRI_CONFIG` environment variable
2. `$XDG_CONFIG_HOME/niri/config.kdl` (typically `~/.config/niri/config.kdl`)
3. `~/.config/niri/config.kdl` (fallback)

### Configuration Architecture

The `niri-config` crate:
- Parses KDL via the `knuffel` library into strongly-typed Rust structs
- Validates at three levels: syntax (KDL parser), type (Rust type system), semantic (custom validators for colors, animation parameters, etc.)
- Supports `serde` serialization for external tooling
- Is independent of the compositor -- can be used for validation without running niri

### KDL Syntax Example

```kdl
input {
    keyboard {
        xkb {
            layout "us"
        }
        repeat-delay 300
        repeat-rate 50
    }
    touchpad {
        tap
        natural-scroll
    }
}

layout {
    gaps 16
    center-focused-column "never"
    border {
        width 2
        active-color "#7fc8ff"
        inactive-color "#505050"
    }
}

animations {
    window-open {
        spring damping-ratio=1.0 stiffness=800.0 epsilon=0.0001
    }
}

binds {
    Mod+T { spawn "alacritty"; }
    Mod+Q { close-window; }
    Mod+Left { focus-column-left; }
    Mod+Right { focus-column-right; }
}
```

### Live Reloading

Configuration reloading is triggered by:
- **SIGHUP signal** (calloop Unix signal source)
- **`niri msg reload-config`** (IPC command)
- **File watcher** detecting changes to config.kdl

Reload process:
1. Parse new config file
2. Validate syntax, types, and semantics
3. If valid: apply changes immediately (borders, gaps, keybindings, animations)
4. If invalid: show error notification, retain previous config, log to journal

---

## DRM Backend Initialization Sequence

niri supports two backends: **winit** (development) and **tty** (production). The backend module provides a unified abstraction.

### TTY/DRM Backend Startup

```
1. Session Acquisition
   -> libseat opens a session (logind/seatd)
   -> Grants access to DRM and input devices

2. Device Discovery
   -> udev enumerates GPU devices at /dev/dri/card*
   -> DrmNode identifies render vs primary nodes

3. DRM Device Initialization
   -> DrmDevice::new(fd) wraps the device file descriptor
   -> Register DrmDevice as calloop source for page flip events

4. Connector Enumeration
   -> Read EDID data from connected monitors via libdisplay-info
   -> Parse: supported modes, preferred mode, manufacturer, model
   -> Match against output configuration in niri config

5. CRTC Assignment
   -> Assign one CRTC per connector (output)
   -> Auto-select encoder for each connector
   -> Cannot share CRTCs between surfaces

6. Surface Creation
   -> DrmSurface = CRTC + connector(s) + mode
   -> GBM allocates graphics buffers
   -> EGL context created for GPU rendering

7. Renderer Setup
   -> GlesRenderer for hardware-accelerated rendering (OpenGL ES 2.0+)
   -> PixmanRenderer as CPU software fallback
   -> MultiRenderer for automatic backend selection

8. Initial Commit
   -> DRM atomic test validates configuration
   -> If test passes, apply settings atomically
   -> Begin render loop
```

### Winit Backend Startup (Development)

```
1. Create winit window
2. Initialize EGL context on the window
3. Create GlesRenderer
4. Register winit event source with calloop
5. Render into the winit window (nested compositor)
```

### Monitor Hotplug

- udev notifies of connector state changes
- niri re-enumerates connectors
- New monitors get CRTCs assigned and surfaces created
- Disconnected monitors have their workspaces preserved (stored with `original_output`)
- Reconnected monitors restore their original workspaces

---

## Rendering Pipeline

```
Layout::render_elements()
  -> MonitorSet selects active monitor
       -> Monitor::render_elements()
            -> Workspace::render_elements()
                 -> For each visible Column:
                      -> Column::render_elements()
                           -> For each Tile:
                                -> Tile::render_elements()
                                     -> Window surface + decorations
  -> Collect all RenderElement instances
  -> Smithay Renderer::render() composites to framebuffer
  -> Submit framebuffer to DRM backend (page flip)
```

### Rendering Backends

| Backend | Type | Use Case |
|---------|------|----------|
| `GlesRenderer` | GPU (OpenGL ES 2.0+) | Primary rendering path |
| `PixmanRenderer` | CPU (software) | Fallback when GPU unavailable |
| `MultiRenderer` | Automatic selection | Chooses best available backend |

### Animation System (`src/animation.rs`)

Animations use configurable easing/spring functions:

- **Easing functions:** `ease-out-cubic`, `ease-out-expo`, variants
- **Spring physics:** tunable `damping-ratio` and `stiffness`
- **Custom GLSL shaders:** user-defined vertex/fragment shaders for effects

Each animation implements an `Animation` trait with `value() -> f64` returning interpolation factor (0.0 to 1.0). Configuration is defined in the `animations` block of config.kdl.

Applied to: workspace switches, window open/close, focus changes, layout transitions.

---

## Nix Packaging Approach (niri-flake)

### niri's Own flake.nix

niri includes a `flake.nix` in its repository for Nix-based builds.

### niri-flake (Community Package)

Repository: https://github.com/sodiboo/niri-flake

A comprehensive Nix flake providing NixOS and Home Manager integration:

#### Module Architecture

**NixOS Module (`nixosModules.niri`):**
- System-wide installation
- Display manager configuration
- Security services: PolicyKit, GNOME Keyring
- XDG portal setup
- Options: `programs.niri.enable`, `programs.niri.package`, `niri-flake.cache.enable`
- Automatically imports Home Manager modules when detected

**Home Manager Modules:**
- `homeModules.niri` -- Package installation and service configuration
- `homeModules.config` -- Core configuration generation and validation
- `homeModules.stylix` -- Automatic theme integration with Stylix

#### KDL Generation from Nix

The pipeline transforms Nix expressions into validated KDL:

```
1. Nix attribute set (programs.niri.settings)
   -> Type checking via settings.nix schema
2. kdl.nix library serializes to KDL format
3. programs.niri.finalConfig produces complete KDL string
4. niri validate verifies generated KDL
5. Written to ~/.config/niri/config.kdl
```

Build fails immediately if validation fails, preventing broken configs from deployment.

#### Settings Type System

`settings.nix` provides:
- Full type safety and IDE support for all config options
- Automatic action filtering based on selected niri package version
- Build-time error prevention (not runtime)
- Categories: input devices, outputs, layout, keybindings, animations, window rules

#### Versioning Strategy

Two version functions in niri-flake:

- **`version-string`**: Human-readable, e.g., "stable 25.08 (commit abc1234)" or "unstable 2025-01-17 (commit def5678)"
- **`package-version`**: Derivation version, e.g., "25.08" or "unstable-2025-01-17-d7184a0"

`refs.nix` maintains commit-to-tag mapping, auto-regenerated via post-commit hooks.

#### Binary Cache

`niri.cachix.org` provides pre-built packages:
- 4 variants: stable/unstable x niri/xwayland-satellite
- 2 nixpkgs channels
- Auto-enabled via `niri-flake.cache.enable` (default: true)

---

## Lessons for EXWM-VR

### What to Adopt from niri

1. **Centralized state struct pattern** -- Single `State` struct with all Smithay `*State` fields, implements all handler traits. This is the proven Smithay pattern.

2. **calloop as event loop** -- Required by Smithay. Our VR event sources (Monado/OpenXR frame timing, controller input) should be registered as calloop sources alongside Wayland and input.

3. **Separate IPC crate** -- Keep `exwm-vr-ipc` as a lightweight crate with minimal dependencies. JSON over Unix socket is a proven pattern. Event streaming is essential for external tooling.

4. **Separate config crate** -- Allows config validation without running the compositor. KDL is a good format choice (human-readable, supports nesting).

5. **Backend abstraction** -- Winit backend for development, VR headset backend for production. The niri pattern of a `Backend` enum with winit/tty variants maps directly to our winit/openxr variants.

6. **niri-flake pattern for Nix packaging** -- If we want Nix support, the niri-flake approach (NixOS module + Home Manager module + settings type system + build-time config validation) is the gold standard.

### What Differs for VR

1. **No DRM backend** -- We render to an OpenXR swapchain, not to physical monitors via DRM. Our "output" is the VR headset, composed via Monado.

2. **Layout hierarchy** -- niri's horizontal scrolling paradigm won't apply. We need a 3D spatial layout system. But the hierarchical ownership pattern (MonitorSet -> Monitor -> Workspace -> Window) is still useful.

3. **Input sources** -- VR controllers and hand tracking are not covered by libinput. We need a custom input backend that translates OpenXR input actions to Smithay-compatible events, or handle them separately and inject into the seat.

4. **Rendering pipeline** -- Instead of compositing to a DRM framebuffer, we composite Wayland surfaces onto textures that are submitted as OpenXR layers. The `render_to_texture()` pattern in niri is directly relevant.

5. **Frame timing** -- VR requires precise frame timing synchronized with the headset's refresh rate and prediction. The calloop timer approach needs to be replaced with OpenXR's frame waiting/beginning/ending cycle.

---

## References

- [niri GitHub Repository](https://github.com/YaLTeR/niri)
- [niri DeepWiki Overview](https://deepwiki.com/YaLTeR/niri)
- [niri IPC Wiki](https://github.com/YaLTeR/niri/wiki/IPC)
- [niri-ipc on crates.io](https://crates.io/crates/niri-ipc)
- [niri-ipc API Documentation](https://yalter.github.io/niri/niri_ipc/)
- [niri Configuration Basics (DeepWiki)](https://deepwiki.com/YaLTeR/niri/3.1-configuration-basics)
- [niri Output Configuration (DeepWiki)](https://deepwiki.com/YaLTeR/niri/3.8-output-configuration)
- [niri Input Handling (DeepWiki)](https://deepwiki.com/YaLTeR/niri/2.3-rendering-and-animation)
- [niri-flake GitHub Repository](https://github.com/sodiboo/niri-flake)
- [niri-flake Overview (DeepWiki)](https://deepwiki.com/sodiboo/niri-flake/1-overview)
- [niri on NixOS Wiki](https://wiki.nixos.org/wiki/Niri)
- [niri on ArchWiki](https://wiki.archlinux.org/title/Niri)
- [calloop Issue #163: Event Loop Clarifications](https://github.com/Smithay/calloop/issues/163)
- [LWN.net: Tour of niri](https://lwn.net/Articles/1025866/)
