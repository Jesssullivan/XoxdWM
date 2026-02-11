# Changelog

All notable changes to the EXWM-VR (XoxdWM) project.

## [0.1.0] - 2026-02-11

### Added

#### Core Window Manager (Weeks 1-6)
- Smithay 0.7 Rust compositor with calloop event loop
- Emacs pgtk client with full window management (ewwm-core)
- S-expression IPC over Unix domain socket
- Workspace management with configurable count
- Tiling, monocle, and floating layout modes
- Focus-follows-mouse with configurable policies
- XWayland compatibility layer
- Layer-shell panel support
- Surface buffer management (ewwm--surface-buffer-alist)
- Key grab/ungrab pipeline (ewwm-input)
- Application launch helpers (ewwm-launch)
- Window manage rules with class matching (ewwm-manage)
- Extension framework (lisp/ext/)

#### VR Subsystem (Weeks 7-10)
- OpenXR runtime integration via Monado
- Full session lifecycle with recovery
- Frame timing infrastructure (p50/p95/p99 tracking)
- 3D scene graph (Vec3/Quat/Mat4, SceneNode, VrScene)
- DMA-BUF texture import pipeline
- Stereo rendering (per-eye, IPD, FOV)
- DRM lease lifecycle (connector detection, non-desktop filter)
- HMD manager with auto-select and hotplug
- VR window interaction (grab, move, release, depth)
- Head-gaze ray intersection

#### Eye Tracking (Weeks 11-13)
- Pupil Labs ZMQ integration + OpenXR native support
- Gaze-based window focus with dwell detection
- Saccade filtering and reading mode detection
- Hysteresis tracker for stable focus
- Blink detection and wink classification
- Wink calibration wizard
- 9-region gaze zones with layout presets
- Zone-to-modifier injection
- Fatigue monitoring (multi-level alerts, CSV logging)
- Gaze scrolling (edge-zone)
- Link hint overlay with dwell confirmation

#### Secrets & Authentication (Week 14)
- D-Bus Secret Service backend with auth-source integration
- KeePassXC NaCl X25519-XSalsa20-Poly1305 browser protocol
- Compositor wl_keyboard auto-type injection
- ydotool auto-type fallback (stdin pipe, never CLI args)
- Secure input mode (pauses all biometric streams)
- Gaze-away safety detection
- TOTP integration with mode-line countdown
- Experimental WebAuthn/FIDO2 passkey bridge

#### Packaging & Platform (Weeks 15-16)
- NixOS module (services.exwm-vr) with full options
- Monado NixOS module with headset selection
- Home-manager module with Nix-to-Elisp config generation
- RPM spec with 7 subpackages for Rocky Linux 9/10
- SELinux policy (3 confined domains, 13 interfaces)
- systemd user services and session target
- Desktop entry and session wrapper
- udev rules for VR hardware
- Headless compositor mode (multi-output, signal handling)
- Cross-compilation (aarch64, s390x)
- OCI container images via nix2container
- CI: multi-arch build (5 jobs) + ERT tests

#### Qutebrowser Integration (Week 17)
- Surface tracking and URL commands
- FIFO/socket IPC with JSON protocol
- Tab-as-buffer with permanent-local vars
- Emacs-to-Qutebrowser theme synchronization
- Consult sources (bookmarks, history, quickmarks)
- Download tracking with mode-line indicator
- Reader mode CSS injection
- Ad-block filter list config generation
- Userscript management bridge
- Gaze scrolling and link following for Qutebrowser

#### Hand Tracking (Week 18)
- 26-joint hand skeleton tracking (EMA smoothing)
- Gesture recognition (pinch, grab, point, palm, thumbsup, swipe)
- Gesture debounce and binding system
- Virtual keyboard (QWERTY/Dvorak/Colemak, hit testing)
- Emacs-style gesture binding alist

#### BCI / Brain-Computer Interface (Week 19)
- OpenBCI Cyton/Daisy board abstraction via BrainFlow
- BCI state management with session lifecycle
- Attention monitoring (sigmoid engagement index, 5 states)
- SSVEP frequency classification (Goertzel algorithm)
- P300 event-related potential detector (oddball paradigm)
- Motor imagery classification (ERD at C3/C4/Cz)
- EEG fatigue monitoring (5-indicator composite index)
- Neurofeedback training (alpha/beta/MI protocols, CSV export)
- Multi-modal fusion (adaptive dwell, two-factor, three-factor)

#### Release & Documentation (Week 20)
- End-to-end integration test suites (flat, VR, eye, BCI, full stack)
- Performance benchmark suite (IPC latency, memory, frame timing)
- Security audit (biometric data paths, socket perms, credential handling)
- User guide, VR guide, eye tracking guide, BCI guide
- Developer guide with IPC protocol reference
- API reference (functions, defcustoms, hooks, IPC messages)
- Competitive analysis, UX evaluation methodology, post-v0.1.0 roadmap

### Stats
- **46 Elisp modules** in lisp/vr/
- **25 Rust modules** in compositor/src/vr/
- **1400+ ERT tests** across 60+ test files
- **100+ Rust unit tests** across compositor modules
- **100+ IPC commands** in dispatch.rs
- **6 documentation guides** + API reference
- **Platforms**: NixOS x86_64, Rocky Linux x86_64, aarch64 (2D), s390x (headless)

[0.1.0]: https://github.com/Jesssullivan/XoxdWM/releases/tag/v0.1.0
