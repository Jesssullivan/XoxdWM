# FIPS 140-3 Compliance Assessment

## Overview

This document assesses the cryptographic operations present in the EXWM-VR
stack and identifies FIPS 140-3 compatible code paths for deployment in
regulated environments (US federal agencies, FISMA-compliant systems).

## Cryptographic Operations Inventory

### 1. KeePassXC Browser Protocol (ewwm-keepassxc-browser.el)

- **Algorithms**: X25519 (key exchange) + XSalsa20-Poly1305 (AEAD)
- **Library**: NaCl/libsodium (via Emacs Lisp implementation)
- **FIPS Status**: NOT FIPS-approved
  - X25519 is not in NIST SP 800-56A (which specifies ECDH with NIST curves)
  - XSalsa20 is not in NIST SP 800-38 series (which covers AES modes)
  - Poly1305 is not a NIST-approved MAC (HMAC-SHA256 would be)
- **Risk**: Medium — used only for local IPC with KeePassXC on the same machine
- **Mitigation**: Disable browser protocol in FIPS mode; use D-Bus Secret
  Service API instead (local Unix socket IPC, no cryptographic operations)

### 2. WiVRn VR Streaming (external dependency)

- **Algorithms**: DTLS 1.2/1.3 for stream encryption
- **Library**: OpenSSL (linked by WiVRn)
- **FIPS Status**: CONDITIONALLY FIPS-compatible
  - DTLS with AES-GCM cipher suites is FIPS-approved
  - OpenSSL has a FIPS provider module (openssl-fips-provider)
  - Requires `OPENSSL_MODULES` pointing to FIPS-validated provider
- **Risk**: Low — standard TLS, FIPS provider available
- **Mitigation**: Ensure WiVRn links against system OpenSSL with FIPS provider
  enabled (`OPENSSL_FIPS=1` or OpenSSL 3.x FIPS provider configuration)

### 3. IPC (Compositor <-> Emacs)

- **Transport**: Unix domain socket
- **Algorithms**: None — plaintext s-expression protocol
- **FIPS Status**: NOT APPLICABLE
  - Local IPC over Unix sockets has no cryptographic component
  - Protected by filesystem permissions (socket is mode 0700 in XDG_RUNTIME_DIR)
- **Risk**: None

### 4. Eye Tracking Data (Pupil Labs ZMQ)

- **Transport**: TCP localhost (127.0.0.1:50020) via ZeroMQ
- **Algorithms**: None — unencrypted local transport
- **FIPS Status**: NOT APPLICABLE
  - Local-only data, loopback interface
- **Risk**: None (loopback traffic not routable)

### 5. BCI/EEG Data (BrainFlow)

- **Transport**: Serial USB (/dev/ttyUSB0) or Bluetooth
- **Algorithms**: None at application layer
- **FIPS Status**: NOT APPLICABLE
  - Direct hardware interface, no network transport
- **Risk**: None

### 6. Passkey/WebAuthn Bridge (ewwm-secrets-passkey.el)

- **Algorithms**: ECDSA P-256, SHA-256, HMAC-SHA-256
- **Library**: System FIDO2 library (libfido2)
- **FIPS Status**: FIPS-COMPATIBLE
  - P-256 is NIST-approved (FIPS 186-5)
  - SHA-256 is NIST-approved (FIPS 180-4)
  - HMAC-SHA-256 is NIST-approved (FIPS 198-1)
- **Risk**: None
- **Note**: Hardware authenticator must also be FIPS-validated for full compliance

### 7. TOTP (ewwm-secrets-totp.el)

- **Algorithms**: HMAC-SHA-1 (RFC 6238)
- **Library**: Emacs Lisp implementation
- **FIPS Status**: PARTIALLY FIPS-compatible
  - HMAC is FIPS-approved (FIPS 198-1)
  - SHA-1 is deprecated but still allowed for HMAC in FIPS mode
  - TOTP itself is not a NIST standard but uses FIPS primitives
- **Risk**: Low

## FIPS-Compatible Configuration

For FIPS-mandated environments, use this configuration:

```elisp
;; Disable non-FIPS KeePassXC browser protocol
(setq ewwm-keepassxc-browser-enable nil)

;; Use D-Bus Secret Service instead (no crypto, local IPC)
(setq ewwm-secrets-backend 'dbus)

;; Passkeys are FIPS-compatible, can remain enabled
(setq ewwm-secrets-passkey-enable t)

;; TOTP uses FIPS primitives, can remain enabled
(setq ewwm-secrets-totp-enable t)
```

NixOS module configuration:

```nix
services.exwm-vr = {
  enable = true;
  # KeePassXC browser protocol disabled in FIPS mode
  # Use D-Bus Secret Service backend instead
};
```

For WiVRn, ensure OpenSSL FIPS provider:

```nix
# In NixOS configuration
security.fips.enable = true;  # NixOS FIPS mode (if available)
# Or manually:
environment.variables.OPENSSL_FIPS = "1";
environment.variables.OPENSSL_MODULES = "${pkgs.openssl}/lib/ossl-modules";
```

## Summary Table

| Component | Algorithms | FIPS Status | Action Required |
|-----------|-----------|-------------|-----------------|
| KeePassXC Browser | X25519+XSalsa20 | NOT approved | Disable; use D-Bus |
| WiVRn | DTLS/AES-GCM | Compatible | Enable FIPS provider |
| IPC | None | N/A | None |
| Eye Tracking | None | N/A | None |
| BCI/EEG | None | N/A | None |
| Passkeys | P-256/SHA-256 | Approved | None |
| TOTP | HMAC-SHA-1 | Approved | None |

## References

- NIST SP 800-140 series (FIPS 140-3 Implementation Guidance)
- NIST SP 800-56A Rev. 3 (Key Establishment)
- NIST SP 800-38D (AES-GCM)
- FIPS 186-5 (Digital Signature Standard)
- FIPS 180-4 (Secure Hash Standard)
- FIPS 198-1 (HMAC)
- OpenSSL FIPS Provider documentation
