# EXWM-VR Security Audit v0.1.0

**Audit date:** 2026-02-11
**Scope:** EXWM-VR compositor, Emacs client, biometric subsystems, secrets management
**Auditor:** Internal review
**Classification:** Public

---

## 1. Executive Summary

This audit covers the full EXWM-VR stack: the Smithay-based Wayland compositor,
the Emacs pgtk client (ewwm-* modules), IPC transport, biometric input streams
(gaze, wink, hand tracking, EEG/BCI), credential handling, and the SELinux
confinement policy.  The system processes multiple categories of sensitive data:
biometric signals, authentication credentials, and user input events.

Overall assessment: **Moderate risk** with clear mitigations in place.  The
architecture follows a principle of minimal privilege and ephemeral data
handling.  Two medium-severity findings require attention before production
deployment.

---

## 2. Data Flow Analysis

### 2.1 Gaze Data Path

| Property | Value |
|----------|-------|
| Source | Pupil Labs eye tracker via ZeroMQ (tcp://127.0.0.1:50020) |
| Transport | Loopback TCP, unencrypted |
| Processing | GazeFocusManager, SaccadeDetector, ReadingDetector in compositor |
| Persistence | Ephemeral in-memory only |
| Secure input | Gaze stream paused when `ewwm-vr-secure-input-pause-gaze` is t |
| CSV export | Explicit user action only (`ewwm-vr-fatigue-log-to-csv`) |
| Disk writes | None during normal operation |

**Assessment:** The gaze data path is well-isolated.  Data flows from the eye
tracker through the compositor's in-memory processing pipeline and is discarded
each frame.  The only persistence path is explicit CSV export initiated by the
user for fatigue monitoring.  During secure input mode, all gaze processing is
paused, preventing gaze position from being correlated with credential entry.

**Risk:** Low.  Gaze data on the loopback interface is not routable.  The
ephemeral processing model prevents accumulation of gaze history.

### 2.2 EEG/BCI Data Path

| Property | Value |
|----------|-------|
| Source | OpenBCI via BrainFlow SDK (serial USB or Bluetooth) |
| Transport | Unix domain socket to BCI daemon |
| Processing | ewwm-bci-core.el, attention/SSVEP/P300/MI/NFB modules |
| Persistence | `data_retention_days` enforced by BCI daemon |
| Network | BCI daemon has NO TCP listeners, Unix socket only |
| Artifact rejection | Strips PII-adjacent data (EMG, EOG artifacts) |

**Assessment:** The BCI daemon correctly avoids exposing any network listeners.
Communication is restricted to a Unix domain socket with filesystem-level
access control.  The artifact rejection pipeline strips electromyographic and
electrooculographic signals that could be used for biometric identification.
The `data_retention_days` configuration is enforced by the daemon, automatically
purging old EEG recordings.

**Risk:** Low.  Physical access to the USB/Bluetooth interface would be required
to intercept raw EEG data.  The artifact rejection adds defense-in-depth
against re-identification from stored data.

### 2.3 Hand Tracking Data Path

| Property | Value |
|----------|-------|
| Source | OpenXR hand tracking extension (XR_EXT_hand_tracking) |
| Data | 26-joint skeleton per hand (position + orientation) |
| Transport | In-process (compositor), IPC for gesture events only |
| Persistence | Ephemeral; cleared on session end |
| Gesture history | NOT stored; only discrete gesture events emitted |
| IPC exposure | Gesture type + timestamp only (no raw joint data) |

**Assessment:** Raw hand skeleton data stays within the compositor process.
Only high-level gesture classifications (pinch, grab, swipe) are transmitted
over IPC to the Emacs client.  The 26-joint skeleton data is zeroed when the
OpenXR session ends.  No gesture history buffer persists across sessions.

**Risk:** Low.  Hand geometry data could theoretically be used for biometric
identification, but it never leaves the compositor process or touches disk.

### 2.4 IPC Socket Security

| Property | Value |
|----------|-------|
| Transport | Unix domain socket |
| Location | `$XDG_RUNTIME_DIR/ewwm-ipc.sock` |
| Permissions | 0700 (inherited from XDG_RUNTIME_DIR) |
| Protocol | Length-prefixed s-expressions (plaintext) |
| Authentication | Filesystem permissions only |
| Network listeners | NONE |
| Max message size | 1 MB (enforced in filter) |

**Assessment:** The IPC socket is protected by standard Unix filesystem
permissions.  `$XDG_RUNTIME_DIR` is typically mode 0700 and owned by the user,
preventing other users from connecting.  There are no TCP/UDP listeners
anywhere in the stack.  The 1 MB message size limit in `ewwm-ipc--filter`
prevents memory exhaustion from malformed messages.

**Risk:** Low.  A process running as the same user can connect to the socket.
This is the standard threat model for Wayland compositors (e.g., wl_display is
similarly protected).

### 2.5 KeePassXC Integration

| Property | Value |
|----------|-------|
| Protocol | KeePassXC Browser Protocol over Unix socket |
| Encryption | NaCl X25519-XSalsa20-Poly1305 (AEAD) |
| Key exchange | X25519 ECDH |
| Association key | Stored in D-Bus Secret Service |
| Channel | Local only, no network component |

**Assessment:** The KeePassXC browser protocol provides authenticated
encryption for credential retrieval.  The association key (identity keypair) is
stored in the D-Bus Secret Service, which is itself protected by the session
keyring.  All communication is local.

**Risk:** Low.  The NaCl primitives are not FIPS-approved (see
`docs/fips-compliance.md`), but provide strong security for local IPC.  In
FIPS-mandated environments, the D-Bus Secret Service backend should be used
instead.

**FIPS note:** See Section 11 for FIPS compliance details.

### 2.6 Credential Handling

| Property | Value |
|----------|-------|
| Buffers | `clear-string` called on all credential buffers after use |
| Secure input mode | Pauses ALL biometric streams (gaze, wink, EEG) |
| Auto-type | Compositor `wl_keyboard` injection preferred |
| Fallback | ydotool via stdin pipe (never CLI arguments) |
| Timeout | 30s auto-exit safety net on secure input mode |
| Visual indicator | Red border on active surface during secure input |

**Assessment:** Credential handling follows defense-in-depth.  The
`clear-string` function zeroes credential memory after use.  Secure input mode
(`ewwm-vr-secure-input.el`) pauses all biometric subsystems to prevent
side-channel leakage (e.g., gaze position revealing password field location,
EEG patterns correlated with typing).  The auto-exit timer prevents subsystems
from staying disabled if the exit signal is missed.

**Risk:** Medium.  Emacs Lisp strings are immutable and may be copied by the
garbage collector before `clear-string` is called.  This is a known limitation
of the Emacs runtime.  See Finding F-002.

### 2.7 Wayland Surface Isolation

| Property | Value |
|----------|-------|
| Model | Per-surface access control in Smithay compositor |
| Buffer access | No cross-client buffer access |
| Clipboard | Explicit data-device protocol, user-initiated only |
| Screencopy | wlr-screencopy requires explicit grant |
| Input | Compositor controls keyboard/pointer focus exclusively |

**Assessment:** The Smithay compositor enforces Wayland's client isolation
model.  Clients cannot read other clients' buffers, intercept input events, or
enumerate other surfaces.  The compositor is the sole arbiter of focus and
input routing.  The wlr-screencopy protocol (if enabled) would require explicit
user authorization.

**Risk:** Low.  This matches the standard Wayland security model, which is
a significant improvement over X11's lack of client isolation.

### 2.8 Compositor Privileges

| Property | Value |
|----------|-------|
| User | Regular user (no root, no setuid) |
| DRM access | Via systemd-logind session |
| Input access | Via systemd-logind session |
| Capabilities | None (no CAP_SYS_ADMIN, etc.) |
| Sandboxing | SELinux confined domain (see Section 9) |

**Assessment:** The compositor runs as a regular user with no elevated
privileges.  DRM and input device access is mediated by systemd-logind, which
grants file descriptors to the session's active VT.  No setuid binaries or
Linux capabilities are required.

**Risk:** Low.  Standard Wayland compositor privilege model.

---

## 3. SELinux Policy Review

The SELinux policy (`packaging/selinux/`) defines three confined domains:

### 3.1 `exwm_compositor_t`

- **Purpose:** Confines the Smithay compositor process
- **Allowed:** DRM device access, input device access, Unix socket creation,
  IPC socket in `$XDG_RUNTIME_DIR`, signal handling
- **Denied:** Network access, arbitrary file write, execution of other binaries
- **Transitions:** Can spawn `exwm_bci_t` for BCI daemon

### 3.2 `exwm_bci_t`

- **Purpose:** Confines the BCI/EEG daemon process
- **Allowed:** Serial USB device access (`/dev/ttyUSB*`), Unix socket creation,
  read/write to BCI data directory
- **Denied:** Network access, DRM access, input device access
- **Isolation:** Cannot communicate with compositor except via Unix socket

### 3.3 `exwm_session_t`

- **Purpose:** Confines the Emacs session process
- **Allowed:** Connect to compositor IPC socket, D-Bus access, home directory
  read/write, network access (for package management, browsing)
- **Denied:** Direct DRM access, direct input device access

### Policy Assessment

The three-domain model provides good separation of concerns.  The BCI daemon
is particularly well-confined: it can only access serial devices and its data
directory.  The compositor domain correctly denies network access, preventing
remote exploitation even if a compositor vulnerability is discovered.

**Risk:** Low.  The SELinux policy adds meaningful confinement beyond standard
Unix permissions.

---

## 4. Auto-Type Security

### 4.1 Compositor Injection (Preferred)

The compositor `wl_keyboard` injection method (`ewwm-secrets-compositor.el`) is
the preferred auto-type backend.  The compositor synthesizes keyboard events
directly through the Wayland input protocol, which:

- Does not require any external tools
- Cannot be intercepted by other Wayland clients
- Does not expose credentials in process arguments
- Is confined by the compositor's SELinux domain

### 4.2 ydotool Fallback

The ydotool backend (`ewwm-secrets-ydotool.el`) is the fallback when compositor
injection is unavailable.  Security measures:

- Credentials are passed via **stdin pipe**, never as CLI arguments
- Process arguments visible in `/proc` show only the ydotool binary path
- The ydotool process lifetime is minimized
- The `_proc` arg prefix convention prevents unused-variable warnings

### 4.3 Auto-Type Dispatcher

`ewwm-secrets-autotype.el` selects the backend:
1. Compositor injection if IPC is connected
2. ydotool if compositor is unavailable
3. Error if neither is available

**Risk:** Low for compositor injection.  Medium for ydotool: requires
`/dev/uinput` access which implies trust in the ydotool binary.

---

## 5. FIPS Compliance

The FIPS compliance posture is documented in detail in `docs/fips-compliance.md`.
Summary:

| Component | FIPS Status | Action |
|-----------|-------------|--------|
| KeePassXC NaCl | NOT approved | Disable; use D-Bus backend |
| WiVRn DTLS | Compatible | Enable OpenSSL FIPS provider |
| IPC socket | N/A | No crypto |
| Eye tracking | N/A | No crypto |
| BCI/EEG | N/A | No crypto |
| Passkeys | Approved | P-256/SHA-256 |
| TOTP | Approved | HMAC-SHA-1 |

The D-Bus Secret Service path is confirmed FIPS-clean: it uses no
cryptographic operations, relying entirely on Unix domain socket security
and the session keyring.

---

## 6. Biometric Side-Channel Analysis

### 6.1 Gaze-Credential Correlation

**Threat:** An attacker with access to gaze data could correlate eye movements
with password field locations and keyboard key positions.

**Mitigation:** Secure input mode (`ewwm-vr-secure-input.el`) pauses gaze
tracking during credential entry.  The `read-passwd` advice wrapper ensures
this happens automatically.

### 6.2 EEG-Keystroke Correlation

**Threat:** Motor imagery EEG patterns may correlate with specific keystrokes
during typing.

**Mitigation:** Secure input mode pauses EEG input.  The BCI artifact
rejection pipeline strips motor-related signals (EMG) that are most likely to
correlate with keystrokes.

### 6.3 Wink-Timing Correlation

**Threat:** Wink detection events during credential entry could leak timing
information.

**Mitigation:** Wink detection is paused during secure input mode.

### 6.4 Hand Gesture Replay

**Threat:** If gesture history were stored, an attacker could replay gestures
to authenticate.

**Mitigation:** No gesture history is stored.  Raw joint data stays in the
compositor process.  Only discrete gesture events (without positional data)
cross the IPC boundary.

---

## 7. Gaze-Away Safety

The `ewwm-secrets-gaze-away.el` module provides an additional safety layer
during auto-type:

1. If the user's gaze leaves the target surface, auto-type is **paused**
2. If gaze returns within the timeout, auto-type **resumes**
3. If gaze stays away beyond the timeout, the operation is **aborted**

This prevents credentials from being typed into the wrong surface if the user
looks away or if focus changes unexpectedly during auto-type.

**Risk:** Low.  This is a defense-in-depth measure.

---

## 8. Network Exposure

| Component | Listeners | Protocol |
|-----------|-----------|----------|
| Compositor | None | N/A |
| Emacs client | None (client-only) | N/A |
| BCI daemon | None | N/A |
| IPC | Unix socket only | s-expression |
| Eye tracker | Loopback only | ZeroMQ |
| KeePassXC | Unix socket only | NaCl |
| D-Bus | Session bus | D-Bus |
| WiVRn (optional) | UDP (configurable) | DTLS |

The only component with potential network exposure is WiVRn for wireless VR
streaming, which uses DTLS encryption.  All other components communicate
exclusively over local transports (Unix sockets, loopback TCP, serial USB).

**Risk:** Low.  Minimal attack surface from the network perspective.

---

## 9. Dependency Security

### 9.1 Rust Dependencies (compositor)

| Crate | Version | Role | Notes |
|-------|---------|------|-------|
| smithay | 0.7 | Compositor framework | Well-maintained, active security fixes |
| openxrs | 0.19 | OpenXR bindings | Thin wrapper, low risk |
| calloop | 0.14 | Event loop | Matches Smithay's version |
| wayland-server | (Smithay dep) | Protocol | Core Wayland |

Recommendation: Run `cargo audit` in CI to catch known vulnerabilities.

### 9.2 Emacs Dependencies

The Emacs client has minimal external dependencies:
- `cl-lib` (built-in)
- `json` (built-in)
- `auth-source` (built-in)
- `dbus` (built-in, optional)

No third-party Emacs packages are required.

---

## 10. Findings Table

| ID | Severity | Component | Description | Status | Mitigation |
|----|----------|-----------|-------------|--------|------------|
| F-001 | Medium | IPC | IPC socket has no application-level authentication; any process running as the same user can connect and send commands | Open | Filesystem permissions on `$XDG_RUNTIME_DIR` (0700). Consider adding a shared secret handshake or SO_PEERCRED verification in v0.2.0 |
| F-002 | Medium | Secrets | Emacs Lisp `clear-string` may not erase all copies due to GC copying; credential material may remain in memory | Open | Use `clear-string` immediately after use; minimize credential string lifetime. Consider a C module for secure memory in v0.2.0 |
| F-003 | Low | Eye Tracking | Pupil Labs ZeroMQ transport on loopback TCP (port 50020) is unencrypted; a local process could sniff gaze data | Accepted | Loopback-only traffic; same-user access required. ZeroMQ CurveZMQ encryption available but adds latency |
| F-004 | Low | BCI | BCI data retention period is configured but not cryptographically enforced; files could be recovered after deletion | Accepted | Standard file deletion; use encrypted filesystem for defense-in-depth |
| F-005 | Low | Auto-type | ydotool requires `/dev/uinput` access which could be abused by a compromised ydotool binary | Accepted | Compositor injection is preferred; ydotool is fallback only. SELinux confines the compositor domain |
| F-006 | Info | FIPS | KeePassXC browser protocol uses non-FIPS algorithms | Documented | Disable in FIPS environments; use D-Bus Secret Service instead |
| F-007 | Info | Compositor | Compositor runs as user but has DRM/input access via logind | Accepted | Standard Wayland compositor model; SELinux provides additional confinement |
| F-008 | Low | Hand Tracking | 26-joint skeleton data within compositor process could theoretically be used for biometric identification | Accepted | Data is ephemeral, never crosses IPC boundary, zeroed on session end |
| F-009 | Low | IPC | 1 MB message size limit could still allow memory pressure from rapid small messages | Open | Consider per-client rate limiting in v0.2.0 |

---

## 11. Recommendations for v0.2.0

1. **IPC authentication**: Add `SO_PEERCRED` verification to the IPC socket
   handler to validate the connecting process's UID/PID, and optionally
   require a shared secret exchanged during session setup.

2. **Secure memory module**: Implement a C module for Emacs that provides
   `mlock()`-backed secure memory allocation for credential strings, preventing
   GC copying and swap-out.

3. **Rate limiting**: Add per-client message rate limiting to the IPC
   dispatcher to prevent denial-of-service from a runaway client.

4. **ZeroMQ encryption**: Evaluate CurveZMQ for the Pupil Labs eye tracking
   connection.  Measure latency impact and document the tradeoff.

5. **Encrypted BCI storage**: Document the recommendation to use LUKS or
   `fscrypt` for the BCI data directory to ensure data-at-rest protection.

6. **Cargo audit CI**: Add `cargo audit` to the CI pipeline
   (`.github/workflows/multi-arch.yml`) to catch known Rust crate
   vulnerabilities automatically.

7. **Dependency pinning**: Pin Rust dependencies with `Cargo.lock` in version
   control (already done) and periodically review with `cargo outdated`.

8. **Compositor screencopy**: If wlr-screencopy is ever enabled, implement
   a user consent dialog before granting access to prevent silent screen
   capture.

9. **Wink calibration data**: Ensure wink calibration data
   (`WinkCalibration`) is not persisted across sessions unless explicitly
   enabled by the user, as it could serve as a biometric template.

10. **Security testing**: Add property-based tests for the IPC message
    parser to verify robustness against malformed input (fuzzing).

---

## 12. Compliance Checklist

| Requirement | Status | Notes |
|-------------|--------|-------|
| No network listeners (compositor) | PASS | Unix sockets only |
| No network listeners (BCI daemon) | PASS | Unix sockets only |
| Credential `clear-string` usage | PASS | Used in all credential paths |
| Secure input pauses biometrics | PASS | Gaze, wink, EEG paused |
| Auto-type stdin (not CLI args) | PASS | ydotool uses stdin pipe |
| SELinux 3-domain confinement | PASS | compositor, bci, session |
| No setuid binaries | PASS | logind mediates device access |
| DRM lease non-desktop filter | PASS | Only non-desktop connectors leased |
| Gaze-away abort on auto-type | PASS | Configurable timeout |
| FIPS-clean D-Bus path | PASS | No cryptographic operations |
| IPC message size limit | PASS | 1 MB enforced |
| Hand tracking data ephemeral | PASS | Zeroed on session end |
| Gesture events (no raw joints) over IPC | PASS | Classification only |

---

## 13. Threat Model Summary

### Assets

- User authentication credentials (passwords, TOTP seeds, passkeys)
- Biometric data (gaze position, EEG signals, hand geometry, blink patterns)
- User input events (keystrokes, pointer movements)
- Window contents (rendered surface buffers)

### Threat Actors

1. **Local unprivileged user**: Cannot access IPC socket (0700 on XDG_RUNTIME_DIR)
2. **Compromised local process (same user)**: Can connect to IPC socket; mitigated by secure input mode pausing biometrics during credential entry
3. **Remote attacker**: Minimal attack surface; no network listeners except optional WiVRn (DTLS-encrypted)
4. **Physical attacker**: Standard OS-level protections apply (disk encryption, screen lock)

### Trust Boundaries

```
+------------------+     Unix Socket      +------------------+
|   Emacs Client   | <=================> |   Compositor     |
|  (ewwm-*.el)     |   s-expr IPC        |  (Smithay/Rust)  |
+------------------+                      +--------+---------+
        |                                          |
        | D-Bus                          DRM/Input (logind)
        v                                          |
+------------------+                      +--------+---------+
| Secret Service   |                      |  Kernel (DRM,    |
| (gnome-keyring)  |                      |  evdev, uinput)  |
+------------------+                      +------------------+
        |
        | Unix Socket (NaCl encrypted)
        v
+------------------+     Serial/BT        +------------------+
|   KeePassXC      |                      |  BCI Daemon      |
+------------------+                      +--------+---------+
                                                   |
                                          +--------+---------+
                                          |  OpenBCI HW      |
                                          +------------------+
```

---

## Appendix A: File Inventory (Security-Relevant)

| File | Purpose |
|------|---------|
| `lisp/vr/ewwm-vr-secure-input.el` | Secure input mode (pauses biometrics) |
| `lisp/vr/ewwm-secrets.el` | D-Bus Secret Service backend |
| `lisp/vr/ewwm-keepassxc-browser.el` | NaCl-encrypted KeePassXC protocol |
| `lisp/vr/ewwm-secrets-ydotool.el` | ydotool auto-type (stdin pipe) |
| `lisp/vr/ewwm-secrets-compositor.el` | Compositor wl_keyboard injection |
| `lisp/vr/ewwm-secrets-gaze-away.el` | Gaze-away safety for auto-type |
| `lisp/vr/ewwm-secrets-autotype.el` | Auto-type backend dispatcher |
| `lisp/vr/ewwm-secrets-totp.el` | TOTP integration |
| `lisp/vr/ewwm-secrets-passkey.el` | WebAuthn/FIDO2 bridge |
| `lisp/vr/ewwm-bci-core.el` | BCI daemon communication |
| `compositor/src/vr/hand_tracking.rs` | Hand skeleton processing |
| `compositor/src/vr/gesture.rs` | Gesture classification |
| `compositor/src/ipc/dispatch.rs` | IPC message dispatch |
| `packaging/selinux/exwm-vr.te` | SELinux type enforcement |
| `packaging/selinux/exwm-vr.if` | SELinux interface definitions |
| `docs/fips-compliance.md` | FIPS 140-3 assessment |

---

## Appendix B: Revision History

| Version | Date | Changes |
|---------|------|---------|
| 0.1.0 | 2026-02-11 | Initial security audit |
