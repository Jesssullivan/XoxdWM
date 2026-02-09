# EWX and Emacs-Wayland Ecosystem Evaluation

Research for EXWM-VR: Smithay + Emacs pgtk separate-process compositor.

Last updated: 2025-02-08

---

## 1. EWX (Emacs Wayland eXperiment)

**Author:** Michael Bauer
**Source:** http://perma-curious.eu/repo-ewx/ (git clone: http://perma-curious.eu/ewx.git)
**Presented:** EmacsConf 2022 -- "Why and how Emacs should become a Wayland compositor"
**Status:** Proof-of-concept / demo, not production-ready

### 1.1 Architecture Overview

EWX splits into a C-based Wayland server and Elisp-based client/manager:

| Component | Language | LOC   | Purpose                                      |
|-----------|----------|-------|----------------------------------------------|
| ews.c     | C        | ~1000 | Minimal Wayland compositor (wlroots/tinywl)  |
| ewc.el    | Elisp    | ~300  | Wayland client -- speaks Wayland from Emacs  |
| ewp.xml   | XML      | --    | Custom Wayland protocol definition           |
| ewb.el    | Elisp    | ~500  | Buffer display in Emacs windows              |
| ewl.el    | Elisp    | --    | Layout management (windowed + floating)      |

Total: roughly 1800 lines across C and Elisp.

### 1.2 IPC Mechanism

**Socket type:** Wayland protocol over Unix domain socket.

EWX does NOT use a custom IPC protocol on top of the Wayland wire format. Instead, it
defines a **custom Wayland protocol extension** (ewp.xml) that extends the standard
Wayland protocol. Emacs connects to the compositor as a privileged Wayland client
using standard `wl_display` connections.

**Protocol format:** Wayland wire protocol (binary, message-based, fd-passing capable).
The custom protocol in ewp.xml defines additional interfaces for compositor-to-Emacs
communication (surface metadata, window management commands).

**Key insight:** By making Emacs a Wayland client rather than embedding the compositor
in-process, EWX sidesteps the event loop merging problem entirely. The compositor
runs its own `wl_display` event loop; Emacs polls the Wayland fd from its own event
loop.

### 1.3 Surface-to-Buffer Mapping

EWX maps Wayland surfaces to Emacs buffers, analogous to how EXWM maps X windows to
Emacs buffers:

1. Compositor receives a client surface (wl_surface + committed buffer)
2. Surface pixel data is transferred to Emacs via shared memory (wl_shm)
3. Emacs renders the surface content as an image inside an Emacs buffer
4. Buffer display is managed by ewb.el (~500 lines)

**Limitations identified:**
- Buffer mirroring does not handle different widths/heights correctly
- Aspect ratio enforcement is broken in some window configurations
- No GPU-accelerated path -- all rendering goes through shared memory pixel copies

### 1.4 Input Handling

**Status: Incomplete / missing.**

Input handling was explicitly called out as unfinished in the EmacsConf presentation
and in community discussions. This is significant because:

- Input is "a very big part of what makes EXWM so useful" (HN discussion)
- Wayland requires the compositor to manage all input routing
- EWX's C server would need to forward input events to Emacs and/or directly to
  client surfaces, and the protocol for this was not implemented

### 1.5 What Worked

- **Custom Wayland protocol extension** as IPC -- avoids inventing a new protocol
- **Separate process architecture** -- compositor in C, management in Elisp
- **Proof that Emacs can be a Wayland client** and manage windows
- **wlroots/tinywl base** -- minimal starting point (~1000 lines of C)
- **Two use cases demonstrated:**
  - EXWM-style: Wayland surfaces displayed inside Emacs windows
  - XWidget-style: embed video/web/3D content as Wayland surfaces

### 1.6 What Was Abandoned/Incomplete

- **Input handling** -- never implemented
- **GPU-accelerated rendering** -- all software rendering via shared memory
- **Multi-monitor support** -- not addressed
- **Robust surface lifecycle** -- no proper handling of surface destroy/resize
- **Project appears dormant** -- no significant updates since 2022-2023
- **Hosting fragile** -- perma-curious.eu server intermittently down

### 1.7 Key Differences from Our Approach

| Aspect                | EWX                                  | EXWM-VR                              |
|-----------------------|--------------------------------------|---------------------------------------|
| Compositor language   | C (wlroots)                          | Rust (Smithay)                        |
| Compositor framework  | wlroots + tinywl                     | Smithay + calloop                     |
| IPC mechanism         | Wayland protocol extension           | TBD (see findings below)              |
| Emacs integration     | Emacs as Wayland client              | Emacs pgtk as client + embedded view  |
| Rendering             | Shared memory pixel copy             | dmabuf / GPU texture sharing possible |
| Input handling        | Not implemented                      | Must be first-class                   |
| Target                | 2D desktop                           | VR spatial computing                  |
| Process model         | Separate (compositor + Emacs)        | Separate (compositor + Emacs)         |

**Both projects share the separate-process model**, which is the correct architecture.
EWX validates this approach. Our advantage: Smithay's Rust safety, GPU rendering
via dmabuf, and a more structured IPC design.

---

## 2. Eway (Scriptable Wayland Compositor)

**Source:** https://github.com/TCCQ/eway
**Status:** Experimental, 17 commits, minimal community adoption

Eway is a scriptable Wayland compositor written in C (89.9%) with an Emacs Lisp
layer (9.2%). It includes:

- `comp/` -- C compositor code
- `eway.el` -- Emacs Lisp control interface

The project demonstrates another take on Emacs-controlled Wayland compositing but
has no documentation, no community, and appears to be a personal experiment.
Not directly useful, but confirms interest in Elisp-driven compositor control.

---

## 3. Way-Cooler (Rust + Lua Wayland Compositor)

**Source:** https://github.com/way-cooler/way-cooler
**Post-mortem:** http://way-cooler.org/blog/2020/01/09/way-cooler-post-mortem.html
**Status:** Discontinued (2020)

### 3.1 Architecture History

Way-Cooler went through four major rewrites:

1. i3/AwesomeWM hybrid on wlc
2. Full AwesomeWM compatibility attempt
3. Split into separate compositor + Lua client processes
4. Attempted AwesomeWM fork for Wayland

### 3.2 Critical Lessons for EXWM-VR

**Lesson 1: Do not recreate X11 on Wayland.**
AwesomeWM is deeply tied to X11. Attempting to clone its model for Wayland meant
"basically recreating X11, but the point of Wayland is to move away from that model."
EXWM-VR must design natively for Wayland, not port EXWM's X11 assumptions.

**Lesson 2: Unsafe Rust requires extreme discipline.**
Way-Cooler's tree structure used `Rc<RefCell<Node>>` (unwieldy) then raw pointers
(segfaults on vector reallocation). They eventually adopted petgraph. For EXWM-VR,
prefer safe abstractions from Smithay and avoid custom unsafe tree structures.

**Lesson 3: Framework wrapping is expensive.**
wlroots-rs (safe Rust bindings for wlroots) consumed 18 months and was abandoned.
The author concluded: "rewriting everything in your pet language is not working with
the existing ecosystem." **Smithay avoids this problem** -- it IS the Rust ecosystem,
not a wrapper around C.

**Lesson 4: Binary separation creates X11-like architecture.**
When Way-Cooler split compositor and AwesomeWM across processes, it recreated the
very client-server split that Wayland was designed to eliminate. However, our use
case is different: we deliberately want separation between compositor (rendering)
and Emacs (window management/UI), with well-defined IPC. The key is making the IPC
boundary intentional rather than accidental.

**Lesson 5: Dogfood ruthlessly.**
Daily use of the compositor drove productivity. Design for actual VR workflows
first, not theoretical completeness.

### 3.3 Legacy Impact

Way-Cooler's discussions directly led to the creation of wlroots, which became the
dominant Wayland compositor library. The project's failure was productive for the
ecosystem even though the project itself died.

---

## 4. Pinnacle (Smithay + gRPC, Lua/Rust Config)

**Source:** https://github.com/pinnacle-comp/pinnacle
**Status:** Active development (2024+)

Pinnacle is the most architecturally relevant project for EXWM-VR because it uses
Smithay + a separate-process IPC model.

### 4.1 IPC Architecture

**Protocol:** gRPC over Unix domain sockets
**Schema:** Protocol Buffers (protobuf)
**Socket path:** `$PINNACLE_GRPC_SOCKET` environment variable

**Service definitions:**

| Service        | Purpose                              |
|----------------|--------------------------------------|
| WindowService  | Window positioning, focus, state     |
| TagService     | Tag/workspace organization           |
| ProcessService | Spawn processes, track exit status   |
| OutputService  | Display configuration                |
| InputService   | Keybindings, input device config     |
| SignalService  | Event subscription/notifications     |
| LayoutService  | Window layout algorithm control      |

**Communication patterns:**
- Unary request/response -- most API calls
- Server streaming -- event subscriptions, process monitoring
- Bidirectional streaming -- signal handling, interactive operations

**Async/sync bridge:** `StateFnSender` bridges async gRPC handlers with the
synchronous compositor state (calloop event loop). Uses oneshot channels for
unary responses, unbounded channels for streaming.

### 4.2 Relevance to EXWM-VR

Pinnacle proves that Smithay + gRPC + external config process works well.
The gRPC approach gives us:
- Language-agnostic API (Emacs could use an Elisp gRPC client or shim)
- Type-safe protocol definitions via protobuf
- Streaming for real-time event notification
- Well-defined service boundaries

**Concern:** gRPC may be heavyweight for Emacs integration. An Elisp gRPC client
does not exist. We would need either:
1. A thin bridge process (Elisp -> Unix socket -> gRPC)
2. A simpler JSON-over-Unix-socket protocol (see Niri below)
3. Emacs dynamic module wrapping a gRPC client library

---

## 5. Niri (Smithay, JSON IPC)

**Source:** https://github.com/YaLTeR/niri
**Status:** Active, well-maintained scrollable-tiling compositor

### 5.1 IPC Design

**Protocol:** JSON over Unix domain socket at `$XDG_RUNTIME_DIR/niri.sock`
**Crate:** `niri-ipc` (minimal deps: serde, schemars, clap)

**Communication model:**
- Synchronous request/response for queries and commands
- Event stream mode: after `Request::EventStream`, compositor continuously writes
  events; two sockets needed for bidirectional communication
- JSON schema exported via schemars for external tool generation

**Advantages over gRPC:**
- Trivially parseable from any language (including Elisp)
- No code generation step required
- Minimal dependencies
- Human-readable wire format for debugging

**Disadvantages:**
- No type safety at the wire level (schema is advisory)
- No built-in streaming multiplexing
- JSON parsing overhead (marginal for our use case)

### 5.2 Relevance to EXWM-VR

Niri's JSON-over-Unix-socket approach is the simplest viable IPC for Emacs
integration. Emacs has excellent built-in support for:
- Unix domain sockets (`make-network-process`)
- JSON parsing (`json-parse-string`, native since Emacs 27)
- Process filters for async event handling

This is likely the best IPC model for EXWM-VR's Emacs integration.

---

## 6. Casilda (GTK4 Wayland Compositor Widget)

**Source:** GNOME GitLab (blogs.gnome.org/xjuan)
**Status:** 1.0 released (2025)

### 6.1 Architecture

Casilda is a GTK4 widget that embeds a Wayland compositor. It uses wlroots
internally and exposes client surfaces as GdkTextures in the GTK scene graph.

**Rendering evolution:**
1. Initial: wlroots renders scene -> pixel buffer -> cairo surface -> GTK (slow)
2. Current: client dmabuf -> GdkTexture -> GTK scene graph (zero-copy GPU path)

**Client connection methods:**
- Named Unix socket (`WAYLAND_DISPLAY`)
- Spawned subprocess with inherited socket
- Raw socket fd for custom launching

**Key API:**
```c
compositor = casilda_compositor_new("/tmp/casilda-example.sock");
gtk_window_set_child(GTK_WINDOW(window), GTK_WIDGET(compositor));
```

### 6.2 Relevance to EXWM-VR

Casilda demonstrates the exact pattern we need: embedding Wayland client surfaces
inside a GTK application. Since Emacs pgtk IS a GTK4 application, Casilda's
approach to dmabuf texture sharing is directly applicable.

**Critical pattern:** Client surface -> dmabuf -> GdkTexture -> GTK snapshot.
This eliminates the shared-memory bottleneck that plagued EWX.

However, Casilda uses wlroots (C), not Smithay (Rust). We need the Smithay
equivalent of this rendering pipeline.

---

## 7. EXWM Issue #559 -- Elisp Compositor Limitations

**Source:** https://github.com/ch11ng/exwm/issues/559

### Key Technical Blockers for Pure Elisp

1. **File descriptor passing** -- Wayland requires fd passing via `SCM_RIGHTS`
   socket control messages. Elisp cannot do this. This is the fundamental blocker
   for a pure-Elisp compositor.

2. **Graphics manipulation** -- Wayland compositors need low-level GPU access
   that Elisp cannot provide.

3. **Dynamic modules** -- Suggested as workaround (load wlroots via Emacs module
   system), but module support is disabled in many Emacs distributions.

**Conclusion:** A separate compositor process is the only viable architecture.
This is exactly what EXWM-VR proposes.

---

## 8. COSMIC Desktop (Smithay + iced)

**Source:** https://github.com/pop-os/cosmic-comp
**Status:** Active (System76)

COSMIC's compositor (cosmic-comp) uses Smithay as its foundation, proving that
Smithay can support a full production desktop environment. Key takeaways:

- Smithay handles DRM/KMS, GBM, EGL, libinput, and all core Wayland protocols
- COSMIC adds its own UI layer via the iced GUI toolkit
- XWayland support is included
- The compositor is a standalone process; desktop shell components communicate
  via Wayland protocols and D-Bus

---

## 9. Actionable Findings for EXWM-VR Architecture

### Finding 1: Use JSON-over-Unix-socket IPC (Niri model), not gRPC

**Rationale:** Emacs has native support for Unix sockets and JSON parsing. gRPC
would require a bridge process or dynamic module. Niri proves JSON IPC is
sufficient for a full compositor. Define a schema with serde + schemars in Rust,
consume it with `json-parse-string` in Elisp.

**Implementation:**
- Compositor opens a Unix socket at a known path
- Emacs connects via `make-network-process`
- Request/response: JSON objects, one per line (newline-delimited JSON)
- Event stream: separate socket connection in streaming mode
- Schema generated from Rust types, used to generate Elisp message constructors

### Finding 2: Use dmabuf texture sharing for surface display, not shared memory

**Rationale:** EWX's shared-memory approach (wl_shm pixel copy) is a performance
bottleneck. Casilda proved that dmabuf -> GdkTexture -> GTK scene graph provides
zero-copy GPU-accelerated rendering. Since Emacs pgtk runs on GTK4, this path is
available to us.

**Implementation:**
- Compositor renders client surfaces to dmabufs via Smithay
- Export dmabuf fds to Emacs process (via the IPC socket with fd passing, or via
  a Wayland subsurface approach)
- Emacs pgtk wraps dmabuf as GdkTexture via a small C/Rust dynamic module
- GTK composites the texture into the Emacs window

**Alternative:** Use Wayland subsurfaces. The compositor creates subsurfaces
parented to the Emacs pgtk toplevel, positioned to overlay Emacs windows. This
avoids cross-process buffer sharing entirely -- the host Wayland compositor
handles the final compositing.

### Finding 3: Design for Wayland natively -- do not port EXWM's X11 model

**Rationale:** Way-Cooler's post-mortem is unambiguous: attempting to recreate
X11 semantics on Wayland is a doomed architecture. EXWM's model (X windows as
Emacs buffers, XCB protocol, reparenting) cannot be ported 1:1.

**What to keep from EXWM:**
- The concept of windows-as-buffers (surface metadata in Emacs buffers)
- Elisp-driven window management and keybinding
- Emacs as the "brain" that makes policy decisions

**What to discard:**
- Reparenting (Wayland has no reparenting)
- Direct X property manipulation (use Wayland protocol instead)
- Synchronous window management (Wayland is async by design)
- Assumption that WM and compositor are separate (in Wayland, they are unified)

### Finding 4: Compositor must own input; Emacs receives events via IPC

**Rationale:** EWX failed to implement input handling. In Wayland, the compositor
is the sole authority on input routing. There is no equivalent of X11's passive
grab mechanism.

**Implementation:**
- Smithay compositor handles all libinput events via calloop
- Compositor implements keyboard focus tracking and pointer routing
- Key events are forwarded to Emacs via IPC when Emacs-managed surfaces are focused
- Emacs processes keybindings and sends commands back via IPC
- Compositor forwards unhandled keys to the focused client surface
- For VR: 6DOF controller events must flow through the same pipeline

**Latency concern:** IPC round-trip for every keystroke adds latency. Mitigate by:
- Having the compositor handle common keybindings directly (configurable hotkeys)
- Only forwarding to Emacs for Emacs-specific bindings
- Using the event stream pattern (compositor pushes events, Emacs reacts async)

### Finding 5: Use Smithay directly -- do not wrap wlroots

**Rationale:** Way-Cooler spent 18 months on wlroots-rs bindings and abandoned
them. The author's conclusion: "rewriting everything in your pet language is not
working with the existing ecosystem." Smithay IS the Rust Wayland ecosystem:

- Native Rust, no FFI wrapping needed
- calloop event loop (same as Niri uses)
- Full protocol support (core, xdg-shell, layer-shell, dmabuf, etc.)
- Production-proven by COSMIC and Niri
- Active maintenance and community

Pinnacle, Niri, and COSMIC all validate Smithay as the correct foundation.

### Finding 6: Separate the rendering loop from the management loop

**Rationale:** EWX's core insight is correct: "Emacs and Wayland both have their
own event loop, and you can't merge them." The solution is two event loops in
two processes:

- **Compositor process** (Rust/Smithay): calloop drives rendering, input, and
  Wayland protocol handling at display refresh rate
- **Emacs process** (pgtk): Emacs event loop handles user interaction, buffer
  management, and window policy at human interaction rate

IPC connects them asynchronously. The compositor never blocks on Emacs; Emacs
never blocks on the compositor. This is the opposite of EXWM, where X event
processing is synchronous with Emacs's command loop.

### Finding 7: Consider Casilda's subsurface pattern for Emacs integration

**Rationale:** Rather than copying pixels or sharing buffers between processes,
use the Wayland subsurface mechanism. The EXWM-VR compositor creates subsurfaces
that are children of the Emacs pgtk toplevel surface. The host Wayland compositor
(or VR runtime) handles the final compositing.

**Advantages:**
- Zero buffer copying between compositor and Emacs
- GPU-accelerated path is automatic
- Standard Wayland mechanism, no custom rendering pipeline
- Works with any host compositor (nested Wayland, VR runtime)

**Risks:**
- Requires the VR runtime to support Wayland subsurfaces
- Positioning subsurfaces to align with Emacs windows requires coordination
- May not work for 3D spatial arrangement in VR (subsurfaces are 2D)

For VR, we may need a hybrid: subsurfaces for flat 2D panels, custom 3D rendering
for spatial arrangement.

---

## 10. Summary of Surveyed Projects

| Project      | Lang     | Framework | IPC               | Status        | Key Lesson                          |
|--------------|----------|-----------|-------------------|---------------|-------------------------------------|
| EWX          | C+Elisp  | wlroots   | Wayland protocol   | Dormant       | Separate process works, input hard  |
| Eway         | C+Elisp  | Unknown   | Unknown            | Experimental  | Interest in Elisp compositor control|
| Way-Cooler   | Rust+Lua | wlc/wlroots| D-Bus (abandoned) | Dead (2020)   | Don't recreate X11; use native Rust |
| Pinnacle     | Rust     | Smithay   | gRPC/protobuf      | Active        | Smithay+IPC works; gRPC is rich     |
| Niri         | Rust     | Smithay   | JSON/Unix socket   | Active        | JSON IPC is simple and sufficient   |
| COSMIC       | Rust     | Smithay   | Wayland+D-Bus      | Active        | Smithay scales to production DE     |
| Casilda      | C        | wlroots   | Wayland subsurface  | 1.0 (2025)   | GTK4 dmabuf embedding works         |
| EXWM #559    | Elisp    | N/A       | N/A                | Discussion    | Elisp cannot pass fds; need native  |

---

## Sources

- [EWX project page](http://perma-curious.eu/repo-ewx/)
- [EmacsConf 2022 Wayland talk](https://emacsconf.org/2022/talks/wayland/)
- [Way-Cooler post-mortem](http://way-cooler.org/blog/2020/01/09/way-cooler-post-mortem.html)
- [Way-Cooler GitHub](https://github.com/way-cooler/way-cooler)
- [EXWM issue #559](https://github.com/ch11ng/exwm/issues/559)
- [Pinnacle GitHub](https://github.com/pinnacle-comp/pinnacle)
- [Pinnacle gRPC architecture](https://deepwiki.com/pinnacle-comp/pinnacle/3.1-grpc-architecture)
- [Niri GitHub](https://github.com/YaLTeR/niri)
- [Niri IPC crate](https://crates.io/crates/niri-ipc)
- [Smithay GitHub](https://github.com/Smithay/smithay)
- [Casilda 1.0 release](https://blogs.gnome.org/gtk/2025/09/15/casilda-1-0-released/)
- [COSMIC compositor](https://github.com/pop-os/cosmic-comp)
- [Eway GitHub](https://github.com/TCCQ/eway)
- [HN: Emacs as Wayland compositor](https://news.ycombinator.com/item?id=46194043)
- [HN: Emacs should become a Wayland compositor](https://news.ycombinator.com/item?id=33849556)
- [GTK Graphics Offload](https://blog.gtk.org/2023/11/15/introducing-graphics-offload/)
