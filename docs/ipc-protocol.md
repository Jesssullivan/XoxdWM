# EWWM IPC Protocol Specification v1

## Overview

The EWWM IPC protocol enables bidirectional communication between the Rust
compositor (`ewwm-compositor`) and Emacs (`ewwm-ipc.el`) over a Unix domain
socket. The compositor is the native window-management authority. Emacs/eGreg,
debug tools, and calibration panels are IPC clients that can observe state and
request policy changes without owning the desktop runtime.

## Transport

- **Socket type:** Unix domain stream socket (`AF_UNIX`, `SOCK_STREAM`)
- **Socket path:** `$XDG_RUNTIME_DIR/ewwm-ipc.sock`
- **Permissions:** `0700` (owner-only, same as Wayland socket)
- **Stale socket:** Compositor removes existing socket file on startup
- **Multiple clients:** Supported (future: debug tools, multiple Emacs instances)

## Wire Format

Messages are **length-prefixed UTF-8 s-expressions**:

```
+-------------------+-----------------------------+
| Length (4 bytes)   | Payload (UTF-8 s-expression) |
| big-endian u32    |                              |
+-------------------+-----------------------------+
```

- **Length prefix:** 4-byte big-endian unsigned 32-bit integer, encoding the
  byte length of the payload (NOT including the 4-byte prefix itself).
- **Payload:** UTF-8 encoded s-expression. No trailing newline or null byte.
- **Maximum message size:** 1 MiB (1,048,576 bytes). Messages exceeding this
  are rejected with an error response.

### Why Length-Prefixed

S-expressions can contain nested parentheses, making delimiter-based framing
ambiguous without a full parser. Length-prefixed framing allows the receiver
to read exactly the right number of bytes before parsing, avoiding
incremental parsing complexity.

### Why S-Expressions

- **Native to Emacs:** `read` and `prin1-to-string` are built-in, zero
  dependency, ~1us per message.
- **Human-readable:** Easy to debug with `socat` or trace buffers.
- **Adequate performance:** `lexpr` crate in Rust benchmarks at ~5us per
  typical message.
- **Future:** Binary mode (msgpack) can be negotiated via hello handshake
  if s-expression overhead becomes a bottleneck for high-frequency data.

## Message Structure

### Request (Emacs -> Compositor)

```elisp
(:type MESSAGE-TYPE :id REQUEST-ID &rest PAYLOAD)
```

- `:type` — keyword symbol identifying the message type
- `:id` — monotonically increasing integer for request/response correlation
- Remaining fields are message-type-specific

### Response (Compositor -> Emacs)

```elisp
(:type :response :id REQUEST-ID :status :ok|:error &rest PAYLOAD)
```

- `:id` — matches the request that triggered this response
- `:status` — `:ok` for success, `:error` for failure
- On error: `:reason "human-readable error message"`

### Event (Compositor -> Emacs, unsolicited)

```elisp
(:type :event :event EVENT-TYPE &rest PAYLOAD)
```

- No `:id` field (events are not responses to requests)
- `:event` — keyword symbol identifying the event type

## Session Lifecycle

```
Client                              Server
  |                                    |
  |--- connect (Unix socket) --------->|
  |                                    |
  |--- :hello (version, client) ------>|
  |<-- :hello (version, server, features) ---|
  |                                    |
  |--- :key-grab (s-r) --------------->|
  |<-- :response :ok ------------------|
  |                                    |
  |--- :surface-list ----------------->|
  |<-- :response (surface data) -------|
  |                                    |
  |<-- :event :surface-created --------|  (unsolicited)
  |<-- :event :key-pressed ------------|  (unsolicited)
  |                                    |
  |--- :surface-focus :id 3 ---------->|
  |<-- :response :ok ------------------|
  |                                    |
  |--- disconnect -------------------->|
```

## Message Types

### Handshake

#### `:hello` (request)

First message from client. Required before any other message.

```elisp
(:type :hello :id 1 :version 1 :client "ewwm.el")
```

#### `:hello` (response)

```elisp
(:type :hello :id 1 :version 1 :server "ewwm-compositor"
 :features (:xwayland t :vr nil))
```

### Surface Management

#### `:surface-list`

Query all managed surfaces.

```elisp
;; Request
(:type :surface-list :id 2)

;; Response
(:type :response :id 2 :status :ok
 :surfaces ((:id 1 :app-id "foot" :title "foot" :geometry (:x 0 :y 0 :w 800 :h 600)
             :workspace 0 :focused t)
            (:id 2 :app-id "firefox" :title "Mozilla Firefox" :geometry (:x 800 :y 0 :w 800 :h 600)
             :workspace 0 :focused nil)))
```

#### `:surface-focus`

Focus a surface by ID (sets keyboard focus).

```elisp
(:type :surface-focus :id 3 :surface-id 2)
```

`focus-surface` is accepted as a temporary Emacs app-layer compatibility alias.

#### `:surface-close`

Request graceful close of a surface.

```elisp
(:type :surface-close :id 4 :surface-id 2)
```

#### `:surface-move`

Move a surface to absolute position.

```elisp
(:type :surface-move :id 5 :surface-id 1 :x 100 :y 200)
```

#### `:surface-resize`

Resize a surface. The compositor accepts both the legacy top-level
`:w`/`:h` fields and the geometry plist emitted by the Emacs app-layer layout
client.

```elisp
(:type :surface-resize :id 6 :surface-id 1 :w 1024 :h 768)
(:type :surface-resize :id 7 :surface-id 1
 :geometry (:x 0 :y 0 :w 1024 :h 768))
```

`surface-move-interactive` and `surface-resize-interactive` are recognized
compatibility commands, but return explicit errors until native pointer-driven
interactive move/resize is implemented.

#### `:surface-fullscreen`

Toggle fullscreen state.

```elisp
(:type :surface-fullscreen :id 7 :surface-id 1)
```

#### `:surface-float`

Toggle floating state.

```elisp
(:type :surface-float :id 8 :surface-id 1)
```

### Workspace Management

#### `:workspace-switch`

Switch to workspace by index.

```elisp
(:type :workspace-switch :id 9 :workspace 2)
```

#### `:workspace-list`

Query workspace state.

```elisp
;; Request
(:type :workspace-list :id 10)

;; Response
(:type :response :id 10 :status :ok
 :workspaces ((:index 0 :name "1" :surfaces (1 2) :active t)
              (:index 1 :name "2" :surfaces (3) :active nil)
              (:index 2 :name "3" :surfaces () :active nil)
              (:index 3 :name "4" :surfaces () :active nil)))
```

#### `:workspace-move-surface`

Move surface to a different workspace.

```elisp
(:type :workspace-move-surface :id 11 :surface-id 1 :workspace 2)
```

### Layout

Layout mode is native compositor state. Emacs/eGreg may request layout changes
as an application/control client, but the compositor owns the current layout
value and reports it over IPC.

#### `:layout-get`

Query current layout policy.

```elisp
(:type :layout-get :id 12)

;; Response
(:type :response :id 12 :status :ok :layout :tiling :master-ratio 0.55)
```

#### `:layout-set`

Set layout algorithm for current workspace.

```elisp
(:type :layout-set :id 13 :layout :tiling)  ; :tiling, :monocle, :grid, :floating

;; Response
(:type :response :id 13 :status :ok :layout :tiling :master-ratio 0.55)
```

#### `:layout-cycle`

Cycle to next layout algorithm.

```elisp
(:type :layout-cycle :id 14)

;; Response
(:type :response :id 14 :status :ok :layout :monocle :master-ratio 0.55)
```

### App Launch

#### `:app-launch-list`

Return the configured native app launch target names. The response lists names
from native `app_launch_commands`, not arbitrary shell commands.

```elisp
;; Request
(:type :app-launch-list :id 14)

;; Response
(:type :response :id 14 :status :ok :targets ("browser" "launcher" "terminal"))
```

#### `:app-launch`

Launch a configured native app target by name. The target must exist in native
`app_launch_commands`; the compositor does not accept arbitrary shell commands
through this IPC request.

```elisp
;; Request
(:type :app-launch :id 15 :name "terminal")

;; Response
(:type :response :id 15 :status :ok
 :name "terminal" :detail "launch:terminal")
```

### Native Config

#### `:config-reload`

Reload native compositor config from `~/.config/exwm-vr/compositor.json`.
The compositor owns the resulting workspace, layout, launch, and key-action
state; Emacs/eGreg clients may request reload but do not apply policy locally.
`:reload-config` is accepted as a compatibility alias. Missing config reloads
the built-in defaults; invalid config returns `:status :error` instead of
silently falling back.

```elisp
;; Request
(:type :config-reload :id 16)

;; Response
(:type :response :id 16 :status :ok :detail "config-reloaded"
 :source "loaded:/home/me/.config/exwm-vr/compositor.json"
 :workspace-count 4 :active-workspace 0 :layout :tiling)
```

### Native Autostart

#### `:autostart-list`

Return the native configured autostart target names and the targets already
launched in this compositor session.

```elisp
;; Request
(:type :autostart-list :id 17)

;; Response
(:type :response :id 17 :status :ok :enabled t
 :targets ("terminal" "browser") :launched ("terminal"))
```

#### `:autostart-run`

Run the configured `autostart_targets` through the native `app_launch_commands`
table. Startup autostart is controlled by `autostart_enabled`; this IPC command
is an explicit app-layer request and can be used to test or re-run the policy.
Already launched targets are skipped unless `:force t` is supplied.

```elisp
;; Request
(:type :autostart-run :id 18)

;; Response
(:type :response :id 18 :status :ok :force nil
 :results ((:target "terminal" :status :skipped :detail "already-launched")
           (:target "browser" :status :launched :detail "launch:browser")))
```

### Native Session Control

#### `:session-status`

Return compositor-owned session state that does not require Emacs/eGreg.

```elisp
;; Request
(:type :session-status :id 19)

;; Response
(:type :response :id 19 :status :ok :locked nil :lock-command-configured t)
```

#### `:session-lock`

Launch the configured native `session_lock_command`. The compositor's
`session_locked` state changes only when a Wayland session-lock client
successfully takes the lock.

```elisp
;; Request
(:type :session-lock :id 20)

;; Response
(:type :response :id 20 :status :ok :detail "session-lock")
```

#### `:session-logout`

Compatibility alias for native compositor shutdown. The compositor sets its
runtime shutdown flag and exits the backend loop.

```elisp
(:type :session-logout :id 21)
```

Power actions such as shutdown, reboot, suspend, and hibernate are not native
product surfaces yet. Keep them in app-layer/operator policy until a mediated
power-management contract is designed and tested.

#### `:session-idle-status`

Return the native idle daemon supervisor state.

```elisp
;; Request
(:type :session-idle-status :id 22)

;; Response
(:type :response :id 22 :status :ok :idle :running :pid 12345 :detail "running")
```

#### `:session-idle-start`

Start the configured native `session_idle_command`. The compositor rejects the
request when no idle command is configured.

```elisp
(:type :session-idle-start :id 23)
```

#### `:session-idle-stop`

Stop the native idle daemon process supervised by the compositor. If no daemon
is running, the command succeeds with `:idle :stopped`.

```elisp
(:type :session-idle-stop :id 24)
```

The real-session DRM and winit backends also start the configured idle daemon
at compositor startup when `session_idle_enabled` is true. The default is
disabled to keep smoke/headless behavior quiet.

### Compatibility Commands

These names keep the Emacs/eGreg application layer connected while native
XoxdWM owns the runtime policy:

| Compatibility command | Native command/state |
|-----------------------|----------------------|
| `:focus-surface` | `:surface-focus` |
| `:follow-status` | `:vr-follow-status` |
| `:follow-set-policy` | `:vr-follow-set-policy` |
| `:follow-recenter` | `:vr-follow-recenter` |
| `:follow-configure` | native follow-mode config state |
| `:focus-routing-status` | native gaze-focus state |
| `:focus-routing-set-mode` | native gaze-focus policy |
| `:focus-routing-set-dwell` | native gaze-focus dwell threshold |
| `:focus-routing-configure` | native gaze-focus policy/dwell threshold |
| `:gaze-zone-set-layout` | native gaze-zone layout map; supports `default`, `vim-like`, `spacemacs`, and custom `:zones` |
| `:hand-tracking-configure` | `:hand-tracking-config`; accepts legacy `:enable` |
| `:hand-tracking-toggle` | native hand-tracking config enable flag |
| `:command` | limited compatibility wrapper for native autotype commands only |
| `:input-latency-probe` | IPC round-trip benchmark response with client/server timestamps |
| `:session-logout` / `:compositor-exit` | native compositor shutdown flag |
| `:overlay-create` | `:vr-overlay-create`; legacy `hud`, `notification`, and `status-bar` map to head-locked overlays |
| `:overlay-remove` | `:vr-overlay-remove`; accepts legacy `:id` |
| `:overlay-list` / `:overlay-status` | `:vr-overlay-list` |
| `:overlay-set-alpha` / `:overlay-set-visible` | `:vr-overlay-configure`; accepts legacy `:id` |
| `:overlay-link-surface` | `:vr-overlay-configure`; accepts legacy `:id` and `:surface-id` |
| `:passthrough-enable` / `:passthrough-disable` | native VR scene background state |
| `:passthrough-status` | native VR scene background plus compositor passthrough config |
| `:passthrough-set-blend-mode` / `:passthrough-set-opacity` | native compositor passthrough config |
| `:transient-list` | `:vr-transient-list` |
| `:transient-status` | native transient chain state and config |
| `:transient-configure` / `:transient-set-offset` / `:transient-set-placement` | native transient chain config |
| `:anchor-create` / `:anchor-restore` | native compositor-local surface pose anchors; not XR_EXT spatial anchors |
| `:anchor-remove` / `:anchor-list` / `:anchor-status` | native anchor manager state |
| `:anchor-goto` | apply stored surface transform and focus the anchored surface when present |
| `:bci-hardware-check` | native BCI status and signal-quality report with `:acquisition :unproven` |
| `:bci-attention-calibrate` / `:bci-mi-calibrate` | native calibration-start handlers |
| `:bci-attention-toggle` / `:bci-mi-toggle` | native config enable flags |
| `:bci-ssvep-configure` | native SSVEP config; accepts legacy `:window` and frequency alist |
| `:bci-p300-cancel` | `:bci-p300-stop` |
| `:bci-dnd-enable` / `:bci-dnd-disable` | acknowledged with `:native nil`; notification/DND policy is not native yet |

The compositor also recognizes the current app-layer-only request names
`:bci-nfb-start`, `:bci-nfb-stop`, `:multimodal-enable`,
`:multimodal-disable`, `:multimodal-set-dwell`,
`:multimodal-three-factor-start`, and `:passkey-response`. These return
explicit `:status :error` responses instead of falling through as unknown
commands. That is intentional claim-gating: neurofeedback streaming,
multimodal fusion, and browser passkey response plumbing remain application
or design surfaces until native product authority exists.

### Input

#### `:key-grab`

Register a global key grab. Compositor intercepts matching key events
before forwarding to focused surface.

```elisp
(:type :key-grab :id 14 :key "s-r")
```

Key format uses Emacs key description syntax:
- `s-r` = Super+r
- `C-M-x` = Ctrl+Alt+x
- `s-S-2` = Super+Shift+2
- `s-RET` = Super+Return

#### `:key-ungrab`

Release a previously registered key grab.

```elisp
(:type :key-ungrab :id 15 :key "s-r")
```

#### `:autotype` (stub)

Inject keystrokes into a surface.

```elisp
(:type :autotype :id 16 :surface-id 1 :text "password123")
```

### VR (stubs)

#### `:vr-status`

Query VR session state.

```elisp
(:type :vr-status :id 17)
;; Response: (:type :response :id 17 :status :ok :session :idle :runtime "monado")
```

#### `:vr-surface-position`

Set 3D position of a surface in VR space.

```elisp
(:type :vr-surface-position :id 18 :surface-id 1
 :position (:x 0.0 :y 1.5 :z -2.0)
 :rotation (:yaw 0.0 :pitch 0.0 :roll 0.0))
```

#### `:gaze-data` (stub)

Query current gaze coordinates.

```elisp
(:type :gaze-data :id 19)
```

### Utility

#### `:ping`

Latency measurement. Compositor responds immediately.

```elisp
(:type :ping :id 20 :timestamp 1705312345123)
;; Response: (:type :response :id 20 :status :ok :client-timestamp 1705312345123 :server-timestamp 1705312345124)
```

## Event Types (Compositor -> Emacs)

Events are pushed to all connected clients. No acknowledgment required.

#### `:surface-created`

```elisp
(:type :event :event :surface-created :id 1 :app-id "foot" :title "foot")
```

#### `:surface-destroyed`

```elisp
(:type :event :event :surface-destroyed :id 1)
```

#### `:surface-title-changed`

```elisp
(:type :event :event :surface-title-changed :id 1 :title "foot — ~/src")
```

#### `:surface-updated`

Emitted when the compositor learns app metadata after initial map. Native
manage policy may emit follow-up workspace or floating events if app rules
match the new metadata.

```elisp
(:type :event :event :surface-updated :id 1 :app-id "foot" :title "foot")
```

#### `:surface-focused`

```elisp
(:type :event :event :surface-focused :id 2 :previous-id 1)
```

`:surface-focused` is the canonical focus event.  Legacy runtimes may also
have emitted `:focus-changed` with `:old` and `:new`; native XoxdWM no longer
emits that alias.

#### `:surface-geometry-changed`

```elisp
(:type :event :event :surface-geometry-changed :id 1
 :geometry (:x 0 :y 0 :w 1920 :h 1080))
```

#### `:workspace-changed`

```elisp
(:type :event :event :workspace-changed :workspace 2 :previous 1)
```

The native compositor updates workspace visibility before emitting the event:
inactive workspace surfaces are removed from the compositor space, active
workspace surfaces are remapped, and tiled surfaces are reflowed.

#### `:layout-changed`

```elisp
(:type :event :event :layout-changed :layout :grid :previous :tiling)
```

This event is emitted after the native compositor layout mode changes. The
compositor applies layout reflow to non-floating surfaces on the active
workspace before returning the layout response; Emacs/eGreg clients observe or
request the change but are not required for basic placement.

#### `:gaze-focus-requested`

Canonical gaze dwell focus request from the compositor. Emacs clients also
accept legacy `:gaze-focus-request` during the transition.

```elisp
(:type :event :event :gaze-focus-requested
 :surface-id 3 :dwell-ms 250 :x 960 :y 540)
```

#### `:gaze-zone-*`

Canonical gaze-zone events use the `:gaze-zone-` prefix so compositor output
matches the Emacs app-layer subscriptions. The shorter `:zone-*` names are not
native contract events.

```elisp
(:type :event :event :gaze-zone-entered :zone :top-left :surface-id 3)
(:type :event :event :gaze-zone-activated :zone :top-left :surface-id 3 :modifier "C-x")
(:type :event :event :gaze-zone-deactivated :zone :top-left :surface-id 3)
(:type :event :event :gaze-zone-dwell-progress :zone :top-left :elapsed-ms 100 :threshold-ms 200)
```

#### `:autotype-aborted`

Emitted when native compositor autotype stops before completion. The Emacs
secrets compositor client treats this as an app-layer error event and clears its
typing state.

```elisp
(:type :event :event :autotype-aborted :surface-id 3 :chars-typed 12)
```

#### `:key-pressed`

Delivered when a grabbed key is pressed.

```elisp
(:type :event :event :key-pressed :key "s-r"
 :modifiers (:super t :ctrl nil :alt nil :shift nil)
 :timestamp 1705312345123)
```

#### `:native-key-action`

Delivered after the compositor handles a configured native key action. Native
key actions run before IPC key grabs so workspace, focus, layout, launch,
reload, and exit behavior can work without Emacs/eGreg in the WM path.

```elisp
(:type :event :event :native-key-action :key "s-RET"
 :action "launch:terminal" :status :ok :detail "launch:terminal"
 :timestamp 1705312345123)
```

## Error Handling

### Malformed Message

```elisp
(:type :response :id 0 :status :error :reason "malformed s-expression")
```

### Unknown Message Type

```elisp
(:type :response :id 42 :status :error :reason "unknown message type: :bogus")
```

### Version Mismatch

```elisp
(:type :response :id 1 :status :error :reason "unsupported protocol version: 99")
```

### Not Authenticated

If a message other than `:hello` is sent before handshake:

```elisp
(:type :response :id 0 :status :error :reason "hello handshake required")
```

## Backpressure

If a client's write buffer exceeds 64 KiB, the compositor drops the oldest
**events** (not command responses) from that client's buffer. This prevents
a slow client from causing compositor memory growth.

## Security

- **Authentication:** None. Socket permissions (`0700`) restrict access to
  the same user. This is the same security model as `$WAYLAND_DISPLAY`.
- **Encryption:** None. Local-only transport; encryption adds latency with
  no benefit.
- **Authorization:** All connected clients have full access. Future:
  capability-based access control.

## Performance Targets

- Round-trip latency (ping/pong): p99 < 1ms
- S-expression parse time (Rust): < 50us per message
- S-expression encode time (Emacs): < 10us per message
- Maximum sustained throughput: > 1000 msg/s
- Event emission overhead: < 100us per event per client

## Future Extensions

- **Binary mode:** Negotiate msgpack encoding via hello handshake for
  high-frequency data (gaze at 200Hz, EEG at 250Hz)
- **Multi-socket:** Separate socket for high-frequency biometric data
  to avoid head-of-line blocking
- **Protocol versioning:** Version field in hello allows backward-compatible
  additions; breaking changes increment version
- **Event filtering:** Client subscribes to specific event types to reduce
  unnecessary traffic
