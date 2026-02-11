# Per-Architecture Build Notes and Optimization

## Overview

EXWM-VR targets three primary architectures: x86_64, aarch64, and s390x.
RISC-V (riscv64) is tracked as an experimental/aspirational target. Each
architecture has different capabilities for GPU rendering, SIMD-accelerated
signal processing, hardware cryptography, and VR support.

## x86_64 (Primary Target)

Full stack support: compositor, VR, eye tracking, BCI, secrets.

### Optimization Flags

```bash
# Maximum performance (native CPU features, LTO)
export RUSTFLAGS="-C target-cpu=native -C lto=fat"

# Portable x86_64 (no AVX, compatible with older CPUs)
export RUSTFLAGS="-C target-cpu=x86-64-v2"

# AVX2 baseline (recommended for BrainFlow signal processing)
export RUSTFLAGS="-C target-cpu=x86-64-v3"
```

### Capabilities

| Feature | Status | Notes |
|---------|--------|-------|
| Compositor (Smithay) | Full | GlesRenderer + Vulkan for VR |
| VR (OpenXR/Monado) | Full | DRM lease, 90fps+ |
| Eye tracking | Full | USB + ZMQ pipeline |
| BCI (BrainFlow) | Full | AVX2 accelerates FFT/filtering |
| Secrets (NaCl) | Full | AES-NI for FIPS cipher suites |
| IPC | Full | < 1ms round-trip expected |

### Performance Targets

- **Compositor frame time**: < 5 ms (@ 1080p desktop)
- **VR frame time**: < 11 ms (@ 90 Hz HMD)
- **IPC round-trip**: < 1 ms (s-expression encode + decode)
- **BCI sampling**: 250 Hz (OpenBCI Cyton), < 4 ms processing per sample

### Nix Build

```bash
nix build .#packages.x86_64-linux.compositor
nix build .#packages.x86_64-linux.default  # full stack
```

## aarch64 (Secondary Target)

Desktop compositing on ARM64 boards and laptops. No VR (no ARM VR runtimes).

### Optimization Flags

```bash
# Native ARM features (NEON, crypto extensions)
export RUSTFLAGS="-C target-cpu=native"

# Portable aarch64 (Cortex-A53 baseline)
export RUSTFLAGS="-C target-cpu=cortex-a53"

# High-performance aarch64 (Cortex-A76+, Apple M-series)
export RUSTFLAGS="-C target-cpu=cortex-a76"
```

### Capabilities

| Feature | Status | Notes |
|---------|--------|-------|
| Compositor (Smithay) | Full | GLES2 renderer recommended |
| VR (OpenXR/Monado) | No | No ARM VR runtime support |
| Eye tracking | Partial | USB works, no ZMQ on some boards |
| BCI (BrainFlow) | Full | NEON SIMD for signal processing |
| Secrets (NaCl) | Full | ARM crypto extensions for AES-GCM |
| IPC | Full | < 2 ms round-trip expected |

### Platform-Specific Notes

**Raspberry Pi 5 (BCM2712 / V3D)**:
- GPU: VideoCore VII, V3D kernel driver, Mesa v3d/v3dv
- Renderer: GLES2 only (`WLR_RENDERER=gles2`)
- Expected performance: 30-60 fps for 2D desktop compositing
- Memory: 4-8 GB shared with GPU; set `gpu_mem=256` in config.txt
- No Vulkan desktop renderer (v3dv is compute-focused)

**NVIDIA Jetson (Orin / Xavier)**:
- GPU: Tegra integrated, NVIDIA proprietary driver
- Renderer: GLES2 via GBM (Jetson Linux >= 36.x supports GBM)
- Expected performance: 60 fps+ for 2D desktop
- Set `WLR_RENDERER=gles2` and `GBM_BACKEND=nvidia-drm`

**Rockchip RK3588 (Panfrost)**:
- GPU: Mali G610, Panfrost driver
- Renderer: GLES 3.1, DMA-BUF import works
- Expected performance: 60 fps 2D desktop
- DRM lease: partial kernel support, untested with VR

**Apple Silicon (Asahi Linux)**:
- GPU: Apple M-series, Asahi driver
- Renderer: OpenGL 4.x (via Asahi Mesa)
- Expected performance: excellent for 2D desktop
- DRM lease: not available (Apple GPU driver limitation)

### Cross-Compilation

```bash
# From x86_64 host via Nix
nix build .#packages.aarch64-linux.compositor

# Manual cross-compile
export CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER=aarch64-linux-gnu-gcc
cargo build --target aarch64-unknown-linux-gnu --release
```

## s390x (IBM Z / Headless)

Headless operation only. No GPU, no display, no VR. The compositor runs in IPC
mode for workspace management, or is omitted entirely in favor of terminal
Emacs.

### Capabilities

| Feature | Status | Notes |
|---------|--------|-------|
| Compositor (Smithay) | Headless only | llvmpipe if rendering needed at all |
| VR (OpenXR/Monado) | No | No GPU, no USB peripherals |
| Eye tracking | No | No USB, no camera |
| BCI (BrainFlow) | Theoretical | Serial-over-IP for OpenBCI (untested) |
| Secrets (NaCl) | Full | s390x CPACF for AES acceleration |
| IPC | Full | Workspace management over socket |

### Expected Use Cases

1. **Terminal Emacs** (`emacs -nw`) with IPC-only workspace management
2. **Emacs pgtk over VNC/SPICE** for remote graphical sessions
3. **CI/CD runner** for Elisp tests (ERT) and Rust compilation
4. **Headless compositor** for automated testing of IPC protocol

### Build Notes

- Skip VR features: `cargo build --release --no-default-features --features headless`
- Skip OpenXR, eye tracking, BCI build requirements entirely
- Use `emacs-nox` (no X11/pgtk needed for terminal mode)
- llvmpipe available for Mesa software rendering if needed

### Nix Build

```bash
nix build .#packages.s390x-linux.compositor-headless
```

## RISC-V / riscv64 (Experimental)

Aspirational target. Not currently tested, expected to require work.

### Capabilities

| Feature | Status | Notes |
|---------|--------|-------|
| Compositor (Smithay) | Unknown | Deps may lack riscv64 support |
| VR | No | No VR runtime for RISC-V |
| Eye tracking | No | No USB stack on most boards |
| BCI | No | No serial/USB peripherals |
| Secrets | Partial | No hardware crypto acceleration |
| IPC | Likely works | Pure Rust, no arch-specific code |

### Known Issues

- **No SIMD**: RISC-V V (vector) extension is not widespread; BrainFlow FFT
  will be scalar-only and significantly slower.
- **Smithay deps**: `wayland-client`, `libinput`, `libdrm` should have
  riscv64 support in mainline, but are untested in this project.
- **Mesa**: llvmpipe works on riscv64; no native GPU drivers exist for any
  RISC-V SoC GPU (as of 2026).
- **Emacs**: mainline support since Emacs 29 (riscv64-linux-gnu target).
- **Headless mode**: most likely to work, as it avoids GPU/driver dependencies.

### Build (Untested)

```bash
# Cross-compile from x86_64
export CARGO_TARGET_RISCV64GC_UNKNOWN_LINUX_GNU_LINKER=riscv64-linux-gnu-gcc
cargo build --target riscv64gc-unknown-linux-gnu --release \
    --no-default-features --features headless
```

## Optimization Flags Summary

| Architecture | SIMD | Crypto | Recommended RUSTFLAGS |
|-------------|------|--------|----------------------|
| x86_64 | AVX2, SSE4.2 | AES-NI, SHA-NI | `-C target-cpu=x86-64-v3` |
| aarch64 | NEON | ARMv8 Crypto | `-C target-cpu=native` |
| s390x | z/vector | CPACF | `-C target-cpu=z14` |
| riscv64 | None (V rare) | None | `-C target-cpu=generic-rv64` |

## Binary Size Comparison (Estimated)

| Architecture | Compositor (release) | Compositor (headless) | Elisp (.elc) |
|-------------|---------------------|-----------------------|---------------|
| x86_64 | ~15 MB | ~8 MB | ~2 MB (noarch) |
| aarch64 | ~14 MB | ~7 MB | ~2 MB (noarch) |
| s390x | N/A | ~7 MB | ~2 MB (noarch) |
| riscv64 | ~16 MB (est.) | ~8 MB (est.) | ~2 MB (noarch) |

## Memory Usage Targets

| Component | x86_64 | aarch64 | s390x |
|-----------|--------|---------|-------|
| Compositor (idle) | < 80 MB | < 60 MB | < 40 MB |
| Compositor (10 windows) | < 200 MB | < 150 MB | N/A |
| Compositor (VR active) | < 500 MB | N/A | N/A |
| Emacs (pgtk, WM loaded) | < 150 MB | < 150 MB | N/A |
| Emacs (nox, WM loaded) | < 80 MB | < 80 MB | < 80 MB |
| BCI venv (active session) | < 300 MB | < 300 MB | N/A |
