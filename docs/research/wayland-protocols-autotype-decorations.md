# Wayland Protocols, KeePassXC Auto-Type, and Decoration Strategy Research

Research date: 2026-02-09
Applicable plan sections: R6.1 (Wayland Protocol Extension Survey), R6.2 (KeePassXC Auto-Type Deep-Dive), R6.3 (SSD Strategy)

---

## R6.1: Wayland Protocol Extension Survey

### Overview

Our Smithay-based compositor must advertise a set of Wayland protocol extensions to be
a useful desktop environment. Smithay already implements 60+ protocols. Below is an
assessment of the five critical protocol extensions for our use case, with Smithay
integration status and implementation priority.

---

### 1. xdg-decoration-unstable-v1 (Server-Side Decorations)

**Protocol:** `zxdg_decoration_manager_v1` / `zxdg_toplevel_decoration_v1`
**Stability:** Unstable (still has `z` prefix)
**Smithay status:** Implemented (`smithay::wayland::shell::xdg::decoration`)

**What it does:**
Allows a compositor and client to negotiate who draws window decorations (title bar,
close/minimize/maximize buttons, resize borders). Two modes exist:

| Mode | Value | Meaning |
|------|-------|---------|
| `client_side` | 1 | Client draws its own decorations (CSD) |
| `server_side` | 2 | Compositor draws decorations (SSD) |

**Negotiation flow:**
1. Client creates `zxdg_toplevel_decoration_v1` via `get_toplevel_decoration(toplevel)`
2. Client calls `set_mode(mode)` to express preference
3. Compositor responds with `configure(mode)` event -- the compositor has final say
4. Client acknowledges via `xdg_surface.commit`
5. If no negotiation occurs, clients self-decorate as they see fit

**Smithay integration:**
```rust
// Create XdgDecorationState, store in compositor state
// Implement XdgDecorationHandler trait:
//   - new_decoration(): advertise preferred mode
//   - request_mode(): handle client mode request, respond with configure
// Use delegate_xdg_decoration!() macro
```

**Our action:** MUST implement. See R6.3 for strategy. Critical for Emacs pgtk
(which uses libdecor/GDK4 to negotiate decorations) and for GTK/Qt applications.

**Known pitfall:** SDL2 versions before ~2.28 crash if the compositor advertises
xdg-decoration but then responds with `client_side`. niri works around this by
hiding the xdg-decoration global entirely when `prefer-no-csd` is not set.

---

### 2. viewporter (Surface Scaling)

**Protocol:** `wp_viewporter` / `wp_viewport`
**Stability:** Stable (v1)
**Smithay status:** Implemented (version 2)

**What it does:**
Allows clients to crop and scale surface contents independently of the buffer size.
This disconnects the relationship between buffer pixel dimensions and the rendered
surface size.

**Key operations:**
- `set_source(x, y, width, height)` -- define source rectangle (crop region)
- `set_destination(width, height)` -- define rendered surface size (scale target)

**Coordinate transformation pipeline:**
1. `buffer_transform` (rotation/reflection via `wl_surface.set_buffer_transform`)
2. `buffer_scale` (integer HiDPI scaling via `wl_surface.set_buffer_scale`)
3. Crop and scale (viewporter `set_source` / `set_destination`)

Source coordinates are post-transform, in surface-local coordinates. State is
double-buffered via `wl_surface.commit`.

**Error conditions:**
- `bad_value` -- negative or zero width/height
- `bad_size` -- non-integer source dimensions when destination is unset
- `out_of_buffer` -- source rectangle exceeds buffer bounds
- `viewport_exists` -- surface already has a viewport

**Our action:** MUST implement. Essential for:
- Fractional scaling (used with `wp_fractional_scale_v1`)
- Video playback surfaces
- VR texture rendering where source textures differ from surface dimensions
- Emacs pgtk uses this for HiDPI scaling

---

### 3. linux-dmabuf-v1 (GPU Buffer Import)

**Protocol:** `zwp_linux_dmabuf_v1` / `zwp_linux_buffer_params_v1` / `zwp_linux_dmabuf_feedback_v1`
**Stability:** Stable (v1, currently at version 5)
**Smithay status:** Implemented (version 5)

**What it does:**
Allows clients to create `wl_buffer` objects backed by DMA-BUF file descriptors,
enabling zero-copy GPU buffer sharing between client and compositor. This is the
primary mechanism for hardware-accelerated rendering on Wayland.

**Buffer creation flow:**
1. Client calls `create_params` to get a `zwp_linux_buffer_params_v1`
2. Client calls `add(fd, plane_idx, offset, stride, modifier_hi, modifier_lo)` for
   each buffer plane (typically 1 for simple formats, 2-3 for planar YUV)
3. Client calls `create(width, height, format, flags)` (async) or
   `create_immed(width, height, format, flags)` (sync)
4. Compositor responds with `created(buffer)` on success or `failed` on failure

**Format/modifier advertisement (v4+):**
Modern clients use the feedback mechanism:
- `get_default_feedback` -- global format/modifier preferences
- `get_surface_feedback(surface)` -- per-surface preferences (enables scan-out hints)
- Feedback delivers:
  - Main device (DRM node for preferred rendering)
  - Tranches of format+modifier pairs ordered by preference
  - Tranche flags (e.g., scan-out capable)
  - Format table as memory-mapped file descriptor

**Our action:** MUST implement. Critical for:
- Hardware-accelerated client rendering (all GPU-accelerated clients use this)
- VR texture import via DMA-BUF (OpenXR/Monado integration in our plan)
- Zero-copy compositing pipeline
- Emacs pgtk (via Mesa/GDK4) uses DMA-BUF for GPU-rendered content

---

### 4. text-input-unstable-v3 (Input Method Support)

**Protocol:** `zwp_text_input_manager_v3` / `zwp_text_input_v3`
**Stability:** Unstable v3
**Smithay status:** Implemented

**What it does:**
Enables compositors to act as input method brokers. Clients describe their text
input fields; the compositor relays this to an input method (like fcitx5 or ibus)
and sends back composed text.

**Key requests (client -> compositor):**
- `enable` / `disable` -- activate/deactivate text input on focused surface
- `set_surrounding_text(text, cursor, anchor)` -- provide editing context (max 4000 bytes UTF-8)
- `set_text_change_cause(cause)` -- whether change came from IM or user
- `set_content_type(hint, purpose)` -- content hints (completion, spellcheck, etc.)
  and purpose (normal, password, email, URL, phone, etc.)
- `set_cursor_rectangle(x, y, width, height)` -- position for IM popup placement
- `commit` -- atomically apply pending state (double-buffered)

**Key events (compositor -> client):**
- `enter` / `leave` -- text-input focus tracking
- `preedit_string(text, cursor_begin, cursor_end)` -- composing text preview
- `commit_string(text)` -- finalized text insertion
- `delete_surrounding_text(before_length, after_length)` -- delete around cursor
- `done(serial)` -- batch apply pending events, with serial for synchronization

**State management:**
Double-buffered within text-input context. Application must process events in order:
replace preedit -> delete surrounding -> insert committed -> calculate new surrounding
-> insert new preedit -> position cursor.

**Our action:** MUST implement. Critical for:
- CJK input method support (fcitx5, ibus)
- Emacs pgtk input method integration
- Accessibility (on-screen keyboards)
- Replaces X11's XIM protocol (which `exwm-xim.el` currently handles)

**Companion protocol:** `zwp_input_method_v2` (compositor-to-IM communication).
Also implemented in Smithay. Required for the compositor to relay between
`text_input` clients and the actual input method engine.

---

### 5. idle-inhibit-unstable-v1 (Idle/Screensaver Inhibition)

**Protocol:** `zwp_idle_inhibit_manager_v1` / `zwp_idle_inhibitor_v1`
**Stability:** Unstable v1
**Smithay status:** Implemented

**What it does:**
Allows clients to prevent the display from blanking, dimming, locking, or
entering power-save mode while a surface is visible.

**How it works:**
1. Client binds `zwp_idle_inhibit_manager_v1` global
2. Client calls `create_inhibitor(surface)` to create inhibitor tied to a surface
3. Compositor suppresses idle behavior while the inhibitor's surface is visible
4. Client calls `destroy` on the inhibitor to release

**When the inhibitor is NOT honored:**
- Surface is destroyed or unmapped
- Surface becomes occluded or invisible
- Surface otherwise becomes visually irrelevant to the user
- If system was already idle when inhibitor was created, it does not retroactively
  wake the display; it takes effect on the next active->idle transition

**Compositor support:** Broad -- Mutter, KWin, Sway, Hyprland, niri, and 12+ others.

**Our action:** MUST implement. Critical for:
- Video playback (mpv, Firefox video)
- VR sessions (must not blank during active VR use)
- Presentations
- KeePassXC auto-type sessions (should not lock during typing sequence)

---

### Protocol Priority Summary

| Protocol | Priority | Smithay Status | Our Week |
|----------|----------|---------------|----------|
| xdg-decoration-unstable-v1 | P0 | Implemented | Week 6 |
| viewporter | P0 | Implemented | Week 6 |
| linux-dmabuf-v1 | P0 | Implemented | Week 6 |
| text-input-unstable-v3 | P1 | Implemented | Week 7 |
| idle-inhibit-unstable-v1 | P1 | Implemented | Week 7 |
| virtual-keyboard-v1 | P1 | Implemented | Week 14 (KeePassXC) |
| fractional-scale-v1 | P1 | Implemented | Week 7 |
| wlr-layer-shell-v1 | P0 | Implemented | Week 6 |
| ext-idle-notify-v1 | P2 | Implemented | Week 8 |
| keyboard-shortcuts-inhibit-v1 | P2 | Implemented | Week 14 |

All required protocols are already implemented in Smithay. Our task is integration
into our compositor's handler traits and state management, not protocol implementation
from scratch.

---

## R6.2: KeePassXC Wayland Auto-Type Deep-Dive

### Current State of KeePassXC Auto-Type on Wayland

**Status:** Auto-type is disabled by default under Wayland. Users must use
`QT_QPA_PLATFORM=xcb` or `-platform xcb` to force X11/XWayland mode, which
only works for XWayland windows.

**Root cause:** Wayland's security model prevents applications from:
1. Enumerating other windows (no equivalent to X11's `XQueryTree`)
2. Injecting keystrokes into other applications (no equivalent to `XTEST`)
3. Reading keyboard state globally (no equivalent to `XGrabKeyboard`)

KeePassXC's X11 auto-type relies on all three capabilities.

### Approaches for Keystroke Injection on Wayland

There are four distinct mechanisms for injecting keyboard events on Wayland:

#### Approach 1: wlr-virtual-keyboard-unstable-v1 Protocol

**How it works:** A privileged Wayland client connects to the compositor and creates
a virtual keyboard device via the `zwp_virtual_keyboard_manager_v1` global. The
virtual keyboard can send keymaps, key press/release events, and modifier state
changes. Events are delivered to whatever surface currently has keyboard focus.

**Protocol interfaces:**
- `zwp_virtual_keyboard_manager_v1.create_virtual_keyboard(seat)` -- create virtual KB
- `zwp_virtual_keyboard_v1.keymap(format, fd, size)` -- set XKB keymap
- `zwp_virtual_keyboard_v1.key(time, key, state)` -- send key event
- `zwp_virtual_keyboard_v1.modifiers(depressed, latched, locked, group)` -- modifier state
- `zwp_virtual_keyboard_v1.destroy()` -- clean up

**Security model:** Trust-based. The compositor decides which clients may create
virtual keyboards. It SHOULD present an `unauthorized` error for untrusted clients.
Compositors typically allow all directly-connected clients (not sandboxed via
`security-context`).

**Tool using this:** `wtype` (xdotool replacement for Wayland)

**Compositor support:**
| Compositor | Supported |
|-----------|-----------|
| Sway | Yes |
| Hyprland | Yes |
| niri | Yes |
| labwc | Yes |
| COSMIC | Yes |
| GNOME/Mutter | No |
| KDE/KWin | No |

**Limitation:** Not supported by GNOME or KDE, so not a universal solution.
However, since we control the compositor, this is a viable path for us.

#### Approach 2: ydotool / dotool (uinput-based)

**How it works:** Creates a virtual input device via the Linux kernel's `/dev/uinput`
interface. Events appear as if from a physical keyboard, completely bypassing the
Wayland protocol layer. The kernel delivers events to the compositor via libinput,
which then routes them to the focused surface.

**Architecture:**
```
ydotool client --> ydotoold daemon --> /dev/uinput --> kernel evdev
    --> libinput --> compositor --> focused surface
```

**Requirements:**
- Access to `/dev/uinput` (typically requires `input` group membership or root)
- `ydotoold` daemon must be running (ydotool) or no daemon needed (dotool)
- No Wayland protocol support needed -- works with ANY compositor

**Comparison of uinput tools:**

| Feature | ydotool | dotool |
|---------|---------|--------|
| Daemon required | Yes (ydotoold) | No |
| Root required | Historically yes, now configurable | No (with uinput group) |
| Keyboard layout aware | No | Yes |
| Mouse support | Yes | Yes |
| Speed | Slow (known issue) | Fast |
| Nested compositors | No | No |
| License | MIT | ISC |

**Limitation:** Requires elevated permissions for `/dev/uinput`. Does not work
in nested compositor sessions. Cannot target specific windows.

#### Approach 3: xdg-desktop-portal RemoteDesktop (D-Bus)

**How it works:** Uses the `org.freedesktop.portal.RemoteDesktop` D-Bus interface
to request keyboard input injection rights. This is the approach KeePassXC PR #10905
is implementing.

**Flow:**
1. Client calls `CreateSession` on the RemoteDesktop portal
2. Client calls `SelectDevices` with `KEYBOARD` device type
3. Client calls `Start` -- user sees authorization dialog
4. Client calls `NotifyKeyboardKeycode(session, options, keycode, state)` to inject keys

**KeePassXC PR #10905 details:**
- Implements `AutoTypePlatformWayland` class
- Exposes D-Bus method `org.keepassxc.KeePassXC.MainWindow.requestGlobalAutoType`
  for external shortcut binding
- Window detection NOT implemented (no cross-platform way on Wayland)
- Global shortcuts not yet working (crashes in settings UI)
- Keyboard layout issues resolved in KDE Plasma 6.5+ (keysym instead of keycode)
- PR still in draft status

**Portal support:**

| Portal Backend | Supported |
|---------------|-----------|
| xdg-desktop-portal-kde | Yes |
| xdg-desktop-portal-gnome | Yes |
| xdg-desktop-portal-luminous | Yes |
| xdg-desktop-portal-hyprland | Partial (issue #252) |
| xdg-desktop-portal-wlr | No RemoteDesktop support |

**Limitation:** Requires portal infrastructure. Not ideal for wlroots-based or
Smithay-based compositors without a portal backend. Authorization dialog on every
session is intrusive.

#### Approach 4: libei / EIS (Emulated Input Server)

**How it works:** Modern replacement for XTEST. `libei` provides a client library;
`libeis` provides the compositor-side server library. Communication happens over a
Unix socket, with negotiation via D-Bus portal or direct socket.

**Architecture:**
```
libei client --> ei protocol --> libeis (in compositor) --> input routing
```

**Key properties:**
- Events are distinguishable from real input (compositor can apply access control)
- Supports multiple negotiation backends (portal, D-Bus, direct socket)
- Being adopted by GNOME (Mutter has libeis support)
- xdg-desktop-portal RemoteDesktop is migrating to EIS for actual event delivery
  (the `NotifyKeyboardKeycode` API is being superseded by EIS connections)

**Status:** libei 1.0 released. Supported by Mutter. Not yet in Smithay.

**Our action:** Monitor for Smithay support. Not needed initially.

---

### Recommended Strategy for Our Compositor

Since we control the compositor, we have a unique advantage. The recommended
approach for KeePassXC auto-type support is:

**Primary: Implement `zwp_virtual_keyboard_manager_v1` with trust policy**

1. Smithay already provides `VirtualKeyboardManagerState` and `VirtualKeyboardHandler`
2. We implement the handler trait in our compositor:
   ```rust
   impl VirtualKeyboardHandler for State {
       fn on_keyboard_event(&mut self, keyboard: &VirtualKeyboard, keycode: u32, state: KeyState) {
           // Route key event to currently focused surface via seat keyboard
       }
       fn on_keyboard_modifiers(&mut self, keyboard: &VirtualKeyboard, mods: ModifiersState) {
           // Update modifier state on seat keyboard
       }
   }
   ```
3. Create a client filter that allows trusted clients (KeePassXC, wtype) to
   create virtual keyboards. Use Wayland `security-context` protocol to
   distinguish sandboxed from trusted clients.
4. KeePassXC uses `wtype` or a custom Wayland client to send auto-type sequences

**Secondary: Compositor-level auto-type via IPC**

Since our compositor has a Unix socket IPC channel to Emacs:

1. Emacs receives KeePassXC credentials via browser integration or D-Bus
2. Emacs sends auto-type command to compositor via IPC:
   `(ewwm-autotype-sequence "username{TAB}password{ENTER}")`
3. Compositor parses sequence and directly injects keyboard events into the
   focused surface via the seat's keyboard:
   ```rust
   fn handle_autotype(&mut self, sequence: &str) {
       let keyboard = self.seat.get_keyboard().unwrap();
       for event in parse_autotype_sequence(sequence) {
           keyboard.input::<_, _>(
               self, event.keycode, event.state,
               SERIAL_COUNTER.next_serial(), event.time,
               |_, _| FilterResult::Forward,
           );
       }
   }
   ```
4. This approach requires NO external tools, NO portal infrastructure, and NO
   uinput access. The compositor is the keyboard event source.

**This is the most elegant solution.** A compositor can always inject keyboard
events to its focused surfaces -- it is the entity that normally delivers keyboard
events. We simply add a code path where the event source is IPC rather than
libinput.

---

### Security Considerations

- Auto-type sequences contain plaintext passwords -- IPC messages must not be logged
- The IPC socket should use Unix socket permissions (mode 0700) to prevent
  unauthorized access
- Consider adding a confirmation mechanism (Emacs prompt) before injecting
  passwords to prevent IPC-based attacks
- Virtual keyboard clients should be authenticated via Wayland security-context
  or PID verification

---

## R6.3: Server-Side Decoration (SSD) Strategy

### How Other Compositors Handle Decorations

#### niri

**Approach:** CSD by default, with opt-in SSD via `prefer-no-csd` config flag.

**Details:**
- When `prefer-no-csd` is **unset** (default):
  - niri hides the `xdg-decoration` global entirely from clients
  - Clients self-decorate (CSD)
  - Focus ring and border are drawn as a solid background rectangle behind windows
  - This avoids the SDL2 bug where windows won't open if xdg-decoration is
    advertised but CSD is requested

- When `prefer-no-csd` is **set**:
  - niri advertises `xdg-decoration` global
  - Negotiates `server_side` mode with clients that support it
  - Sets `tiled` state to true, which makes GTK4 clients square their rounded corners
  - Focus ring and border are drawn around the window outline (no solid background)
  - Clients that explicitly request CSD still get CSD

**Visual treatment for SSD windows:**
- Focus ring (configurable color, width)
- Border (configurable color, width, separate from focus ring)
- No title bar, no window buttons -- niri is keyboard-driven

**SDL2 workaround:** niri conditionally exposes the xdg-decoration global. When
`prefer-no-csd` is off, the global is hidden entirely to avoid the SDL2 crash bug
(fixed in SDL2 ~2.28+ but many games bundle older versions).

#### Sway

**Approach:** SSD by default for tiled windows, CSD for floating windows.

**Details:**
- Implements `xdg-decoration` protocol via wlroots
- Default decoration mode is `server_side` in `sway/xdg_decoration.c`
- SSD includes: title bar with window title, border with configurable width/color
- Floating windows can use CSD if client requests it
- Per-window configuration via `default_border` and `default_floating_border`
- Supports `none`, `normal` (title bar + border), `pixel` (border only) decoration modes
- Colors configurable: `client.focused`, `client.unfocused`, `client.urgent`, etc.

**Implementation in wlroots:**
```c
struct sway_server_decoration {
    struct wlr_server_decoration *wlr_server_decoration;
    struct wl_listener destroy;
    struct wl_listener mode;
};
```

#### Hyprland

**Approach:** CSD with compositor-drawn groupbar for tabbed window groups.

**Details:**
- Supports `xdg-decoration` protocol
- Primarily relies on CSD
- `CHyprGroupBarDecoration` provides compositor-drawn tab indicators for grouped windows
- Groupbar is customizable (colors, font, padding, rounding)
- No traditional SSD title bar; uses CSD + compositor borders

#### KWin (KDE)

**Approach:** Full SSD with Breeze window decorations.

**Details:**
- Draws complete title bars with buttons (close, minimize, maximize, keep-above, etc.)
- Supports both `xdg-decoration` and legacy `org_kde_kwin_server_decoration`
- Decoration themes are pluggable (Breeze, Oxygen, custom)
- Most feature-rich SSD implementation on Wayland

#### GNOME/Mutter

**Approach:** CSD only. Does NOT implement `xdg-decoration`.

**Details:**
- GNOME's position: clients should always draw their own decorations
- GTK4 applications have built-in header bars
- Non-GTK applications that don't draw decorations get no decorations at all
- This is a deliberate design decision, not a missing feature

---

### Recommendation for Our Compositor

**Decision: Implement SSD with configurable mode, following niri's approach
with Sway-style visual treatment.**

**Rationale:**

1. **Emacs is our primary client.** Emacs pgtk uses libdecor/GDK4 for decoration
   negotiation. If we advertise SSD via xdg-decoration, Emacs will let us draw
   decorations. This is desirable because:
   - We can draw decorations that match our tiling/VR theme
   - We avoid CSD inconsistency between GTK, Qt, and other toolkits
   - We control the visual language of the entire desktop

2. **Keyboard-driven workflow.** Like niri and Sway, our WM is keyboard-driven.
   We do NOT need full title bars with buttons. A minimal SSD approach is best:
   - Focus ring (color indicates focus state)
   - Border (configurable width, doubles as resize handle)
   - Optional title text in border (for window identification)
   - No close/minimize/maximize buttons (use keybindings)

3. **VR considerations.** In VR mode, decorations should be minimal or absent
   to maximize viewport. The compositor should be able to suppress decorations
   entirely in VR mode.

4. **SDL2 compatibility.** Follow niri's approach: conditionally expose
   xdg-decoration global. Only advertise it when SSD mode is enabled in config.

**Implementation plan:**

```rust
// In compositor config
struct DecorationConfig {
    mode: DecorationMode,           // SSD, CSD, or Negotiate
    border_width: u32,              // Default: 2px
    focus_ring_width: u32,          // Default: 2px
    focused_color: Color,           // Default: accent color
    unfocused_color: Color,         // Default: muted gray
    urgent_color: Color,            // Default: red
    show_title: bool,               // Default: false (keyboard-driven)
    vr_mode_decorations: bool,      // Default: false (no decorations in VR)
}

enum DecorationMode {
    ServerSide,                     // Always request SSD
    ClientSide,                     // Never advertise xdg-decoration
    Negotiate,                      // Advertise but respect client preference
}
```

**Smithay integration:**
```rust
impl XdgDecorationHandler for State {
    fn new_decoration(&mut self, toplevel: ToplevelSurface) {
        // Set initial mode based on config
        match self.config.decoration.mode {
            DecorationMode::ServerSide => {
                toplevel.with_pending_state(|state| {
                    state.decoration_mode = Some(Mode::ServerSide);
                });
            }
            DecorationMode::Negotiate => {
                // Accept client preference
            }
            DecorationMode::ClientSide => unreachable!(), // global not advertised
        }
    }

    fn request_mode(&mut self, toplevel: ToplevelSurface, mode: Mode) {
        // Client is requesting a specific mode
        // In ServerSide mode, we override to SSD
        // In Negotiate mode, we respect the client's request
    }
}
```

**Rendering SSD:**
The compositor draws decorations as part of its render pass, AFTER compositing
the client surface. For each managed surface with SSD:
1. Draw border rectangle around surface bounds (inset or outset configurable)
2. Draw focus ring (if focused) with accent color
3. Optionally draw title text above surface
4. Use Smithay's `damage_tracker` to only redraw decorations when state changes

---

## Cross-Cutting Concerns

### Protocol Dependencies for KeePassXC Integration

The full KeePassXC integration (Week 14) requires these protocols working together:

1. **virtual-keyboard-v1** -- for `wtype`-based auto-type from external clients
2. **keyboard-shortcuts-inhibit-v1** -- to ensure auto-type sequences are not
   intercepted by compositor keybindings
3. **idle-inhibit-v1** -- to prevent screen lock during auto-type sequences
4. **xdg-activation-v1** -- for KeePassXC to request focus transfer after
   auto-type (return focus to previous window)

### Protocol Dependencies for VR Mode

VR mode (Weeks 9-12) requires:
1. **linux-dmabuf-v1** -- zero-copy texture import from OpenXR/Monado
2. **viewporter** -- surface scaling for VR viewport rendering
3. **drm-lease-v1** -- leasing GPU output to VR headset
4. **xdg-decoration** -- suppress decorations in VR mode

### Emacs pgtk Protocol Requirements

Emacs 30.x pgtk (via GDK4) uses:
1. **xdg-shell** (primary shell protocol)
2. **xdg-decoration** (negotiate CSD vs SSD)
3. **text-input-v3** (input method support)
4. **viewporter** (HiDPI scaling)
5. **linux-dmabuf-v1** (GPU-accelerated rendering)
6. **wl_data_device** (clipboard, part of core protocol)
7. **idle-inhibit** (via application request)

---

## Appendix: Tool Comparison for Wayland Input Injection

| Tool | Mechanism | Root Required | Compositor Support | Window Targeting | Speed |
|------|-----------|--------------|-------------------|-----------------|-------|
| wtype | virtual-keyboard protocol | No | wlroots-based only | No (focused only) | Fast |
| ydotool | /dev/uinput (kernel) | Group membership | Any compositor | No | Slow |
| dotool | /dev/uinput (kernel) | Group membership | Any compositor | No | Fast |
| xdotool | XTEST (X11) | No | XWayland only | Yes | Fast |
| libei | EIS protocol | No | Mutter only (so far) | No | Fast |
| Portal | RemoteDesktop D-Bus | No | Portal-dependent | No | Medium |
| Compositor IPC | Direct seat injection | No | Our compositor | Yes (via IPC) | Fastest |

**Winner for our use case: Compositor IPC.** We bypass all the complexity of
external tools by having the compositor directly inject events into the seat
keyboard. This is the simplest, fastest, most secure approach, and it works
because we control the compositor.

---

## References

- [Virtual keyboard protocol specification](https://wayland.app/protocols/virtual-keyboard-unstable-v1)
- [XDG decoration protocol specification](https://wayland.app/protocols/xdg-decoration-unstable-v1)
- [Viewporter protocol specification](https://wayland.app/protocols/viewporter)
- [Linux DMA-BUF protocol specification](https://wayland.app/protocols/linux-dmabuf-v1)
- [Text input v3 protocol specification](https://wayland.app/protocols/text-input-unstable-v3)
- [Idle inhibit protocol specification](https://wayland.app/protocols/idle-inhibit-unstable-v1)
- [KeePassXC Wayland auto-type issue #2281](https://github.com/keepassxreboot/keepassxc/issues/2281)
- [KeePassXC Wayland auto-type PR #10905](https://github.com/keepassxreboot/keepassxc/pull/10905)
- [wtype -- xdotool type for Wayland](https://github.com/atx/wtype)
- [ydotool -- generic CLI automation tool](https://github.com/ReimuNotMoe/ydotool)
- [dotool -- simulate input anywhere](https://sr.ht/~geb/dotool/)
- [libei -- emulated input for Wayland](https://libinput.pages.freedesktop.org/libei/index.html)
- [niri decoration configuration](https://github.com/YaLTeR/niri/wiki/Configuration:-Miscellaneous)
- [niri FAQ on decorations](https://github.com/YaLTeR/niri/wiki/FAQ)
- [Sway xdg_decoration.c](https://github.com/swaywm/sway/blob/master/sway/xdg_decoration.c)
- [Smithay xdg decoration module](https://smithay.github.io/smithay/smithay/wayland/shell/xdg/decoration/index.html)
- [Smithay virtual keyboard module](https://smithay.github.io/smithay/smithay/wayland/virtual_keyboard/index.html)
- [Smithay protocol implementation tracker](https://github.com/Smithay/smithay/issues/781)
- [Privileged Wayland clients security discussion](https://blog.ce9e.org/posts/2025-10-03-wayland-security/)
- [Wayland keyboard input (The Wayland Book)](https://wayland-book.com/seat/keyboard.html)
- [RemoteDesktop portal specification](https://flatpak.github.io/xdg-desktop-portal/docs/doc-org.freedesktop.portal.RemoteDesktop.html)
