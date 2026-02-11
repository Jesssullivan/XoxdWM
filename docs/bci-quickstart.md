# BCI Quick-Start Guide for EXWM-VR

## Overview

This guide walks you through setting up brain-computer interface (BCI)
hardware and software for use with EXWM-VR. By the end, you will have
a working EEG stream feeding attention state into your VR window manager.

**Time required**: ~1 hour for first setup, ~5 minutes for daily use.

## Shopping List

### Recommended: 8-Channel (Cyton)

| Item                          | Price   | Notes                          |
|-------------------------------|---------|--------------------------------|
| OpenBCI Cyton Board           | $500    | 8 channels, 250 Hz, 24-bit    |
| OpenBCI USB Dongle            | Included| RFDuino 2.4 GHz wireless      |
| Ultracortex Mark IV           | $350    | 3D-printed, dry electrodes     |
| Ten20 Conductive Paste (3-pack)| $25    | For calibration sessions       |
| Gold-cup electrodes (10-pack) | $30     | For gel calibration            |
| NuPrep skin prep gel          | $10     | Lowers impedance               |
| **Total**                     | **~$915**|                               |

### Budget: 4-Channel (Ganglion)

| Item                          | Price   | Notes                          |
|-------------------------------|---------|--------------------------------|
| OpenBCI Ganglion Board        | $250    | 4 channels, 200 Hz            |
| Ganglion electrode kit        | $50     | Snap electrodes + headband     |
| **Total**                     | **~$300**| Attention monitoring only      |

### Extended: 16-Channel (Cyton + Daisy)

| Item                          | Price   | Notes                          |
|-------------------------------|---------|--------------------------------|
| OpenBCI Cyton Board           | $500    | Base 8 channels                |
| OpenBCI Daisy Module          | $450    | Adds 8 channels (16 total)     |
| Ultracortex Mark IV (16ch)    | $450    | Extended frame + electrodes    |
| **Total**                     | **~$1,400**| Full paradigm support        |

## Hardware Setup

### Board Assembly

1. Unpack the Cyton board. Verify the blue LED blinks when powered.
2. Insert the USB dongle into your host machine.
3. If using Ultracortex: attach the Cyton board to the headset frame
   using the provided standoffs and screws.
4. Attach electrode leads to the board headers (channels 1-8 map to
   N1P-N8P pins; bias to BIAS; reference to SRB2).

### USB Dongle Driver (FTDI)

The OpenBCI USB dongle uses an FTDI FT231X chip.

**NixOS** (if using our module):
```nix
services.exwm-vr.bci.enable = true;
# This automatically adds FTDI udev rules and firmware
```

**Manual (any Linux)**:
```bash
# Verify the dongle is detected
lsusb | grep FTDI
# Expected: "Future Technology Devices International, Ltd FT231X"

# Check that /dev/ttyUSB0 exists
ls -la /dev/ttyUSB0
```

### udev Rules

If not using the NixOS module, add the udev rule manually:

```
# /etc/udev/rules.d/99-openbci.rules
SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", \
  MODE="0666", GROUP="plugdev", SYMLINK+="openbci"
```

Then reload:
```bash
sudo udevadm control --reload-rules
sudo udevadm trigger
```

## Electrode Placement

### 10-20 System (8-Channel Montage)

```
         Nasion
           |
     Fp1---Fpz---Fp2
    /   \       /   \
   F7    F3--Fz--F4   F8
   |    / |     | \    |
   T3  C3--Cz--C4  T4
   |    \ |     | /    |
   T5    P3--Pz--P4   T6
    \   /       /   \
     O1---Oz---O2
           |
         Inion

  8-channel positions (marked with *):

     *Fp1             *Fp2

          *C3    *C4

          *P3    *P4

      *O1             *O2

  Reference: A1 or A2 (earlobe)
  Ground: Fpz (center forehead)
```

### Placement Guide

1. Measure from nasion (bridge of nose) to inion (bump at back of skull).
2. Fpz is at 10% of this distance from nasion.
3. Fp1/Fp2 are 5% lateral from Fpz along the circumference.
4. C3/C4 are at 50% nasion-inion, 20% lateral from midline.
5. P3/P4 are at 70% nasion-inion, same lateral positions.
6. O1/O2 are at 90% nasion-inion, same lateral positions.

With the Ultracortex Mark IV, the electrode positions are fixed by the
frame geometry. Adjust the frame size to fit your head, then tighten
the thumb screws until the dry electrodes make firm scalp contact.

## Impedance Check

After placing electrodes, verify signal quality:

```
M-x ewwm-bci-hardware-check
```

This command starts a BrainFlow session, reads impedance from all
channels, and displays results in a dedicated buffer:

```
Channel  Electrode  Impedance  Status
-------  ---------  ---------  ------
1        Fp1        8.2 kOhm   OK
2        Fp2        6.1 kOhm   OK
3        C3         12.4 kOhm  WARN (>10 kOhm)
4        C4         7.8 kOhm   OK
5        P3         9.1 kOhm   OK
6        P4         11.2 kOhm  WARN (>10 kOhm)
7        O1         5.3 kOhm   OK
8        O2         6.7 kOhm   OK
REF      A1         3.2 kOhm   OK
```

**Target**: all channels below 10 k-ohm. Channels showing WARN will
still work but with reduced signal quality.

### Troubleshooting High Impedance

- **Dry electrodes**: press electrode more firmly, twist slightly to part
  hair, ensure pins are contacting scalp not hair
- **Gel electrodes**: add more paste, abrade skin lightly with NuPrep,
  check lead connection to board
- **Reference electrode**: clip earclip firmly, add gel if needed
- **Persistent >50 k-ohm**: check cable continuity, try different channel

## Software Setup

### NixOS (Recommended)

In your NixOS configuration or home-manager:

```nix
# configuration.nix
services.exwm-vr = {
  enable = true;
  bci = {
    enable = true;
    board = "cyton";       # or "ganglion", "cyton_daisy", "synthetic"
    serialPort = "/dev/ttyUSB0";  # auto-detected if omitted
    sampleRate = 250;
  };
};
```

Rebuild and switch:
```bash
sudo nixos-rebuild switch
```

### Manual Installation

If not on NixOS, install BrainFlow manually:

```bash
# Python (for analysis tools and calibration scripts)
pip install brainflow numpy scipy

# Verify installation
python -c "from brainflow.board_shim import BoardShim; print('OK')"
```

The Rust compositor links BrainFlow via the `brainflow` crate (compiled
automatically with the `bci` feature flag):

```bash
cargo build --features bci
```

### Emacs Configuration

Add to your init.el or ewwm configuration:

```elisp
(require 'ewwm-bci)

;; Basic configuration
(setq ewwm-bci-board-type 'cyton)
(setq ewwm-bci-serial-port "/dev/ttyUSB0")  ; auto-detected if nil

;; Enable mode-line indicator
(ewwm-bci-mode-line-mode 1)

;; Key bindings
(global-set-key (kbd "C-c b s") #'ewwm-bci-start)
(global-set-key (kbd "C-c b q") #'ewwm-bci-stop)
(global-set-key (kbd "C-c b c") #'ewwm-bci-calibrate)
(global-set-key (kbd "C-c b h") #'ewwm-bci-hardware-check)
```

## First Session

### 1. Start BCI Streaming

```
M-x ewwm-bci-start
```

The mode-line should show `[BCI:connecting...]` then `[BCI:streaming]`.
If it shows `[BCI:error]`, check the `*ewwm-bci*` buffer for details.

### 2. Verify Signal Quality

```
M-x ewwm-bci-signal-monitor
```

Opens a live signal quality display showing:
- Raw waveforms per channel (should show alpha oscillations when relaxed)
- Power spectrum (should show peak at 8-13 Hz with eyes closed)
- Noise floor (should be <5 uV RMS for gel, <10 uV for dry)

**Quick test**: close your eyes for 10 seconds. You should see a clear
increase in alpha power (8-13 Hz) in the occipital channels (O1, O2).
This confirms the system is capturing real brain signals.

### 3. Run Attention Calibration

```
M-x ewwm-bci-calibrate-attention
```

This runs a 2-minute calibration protocol:

1. **Relaxation baseline** (60 seconds): you are prompted to relax, close
   eyes, and let your mind wander. The system captures your baseline
   alpha/theta ratio.

2. **Focus task** (60 seconds): you are prompted to perform mental
   arithmetic (e.g., count backwards from 300 by 7s). The system captures
   your focused-state beta/alpha ratio.

3. **Threshold computation**: the system computes your personal attention
   threshold as the midpoint between relaxed and focused band-power ratios.

After calibration, the mode-line indicator changes to show real-time
attention state: `[BCI:focused]` or `[BCI:relaxed]`.

### 4. Motor Imagery Calibration (16-Channel Only)

```
M-x ewwm-bci-calibrate-mi
```

This runs a 5-minute motor imagery calibration:

- 20 trials each of: left hand, right hand, feet, rest
- Each trial: 2s cue + 4s imagery + 2s rest
- CSP filter computation + LDA classifier training
- Cross-validation accuracy reported at end

Expected accuracy: 70-85% for 2-class (left/right), 55-70% for 4-class.

## Troubleshooting

### USB Dongle Not Detected

```
$ lsusb | grep -i ftdi
(no output)
```

- Unplug and replug the dongle.
- Try a different USB port (avoid hubs).
- Check `dmesg | tail -20` for errors.
- On NixOS: verify `hardware.enableRedistributableFirmware = true`.

### Board Not Connecting

```
ewwm-bci-start: BrainFlow error: BOARD_NOT_READY
```

- Verify serial port: `ls /dev/ttyUSB*`
- Check board power: blue LED should be solid (not blinking)
- Reset board: power cycle, wait 5 seconds, retry
- Try synthetic board first: `(setq ewwm-bci-board-type 'synthetic)`

### Noisy Signal (>20 uV RMS)

- Move away from monitors and power supplies (60/50 Hz noise source)
- Check ground electrode connection
- Enable notch filter: `(setq ewwm-bci-notch-freq 60)` (US) or 50 (EU)
- Verify impedance with `M-x ewwm-bci-hardware-check`

### Signal Drift (Baseline Wanders)

- Normal for first 2-3 minutes (electrode stabilization)
- Enable highpass filter: included in default pipeline (0.5 Hz cutoff)
- If persistent: re-seat electrode, add gel

### Calibration Accuracy Below 70%

- Re-do impedance check; fix any channels >15 k-ohm
- Ensure quiet environment (muscle artifacts from talking/chewing)
- Try eyes-closed calibration (reduces EOG artifacts)
- Use gel electrodes for calibration session

## Expected Accuracy by Paradigm

| Paradigm          | 4-Channel | 8-Channel | 16-Channel |
|-------------------|-----------|-----------|------------|
| Attention (2-class)| 78-85%   | 85-92%    | 88-94%     |
| SSVEP (4-class)   | 82-90%   | 90-96%    | 94-98%     |
| P300 speller      | N/A       | 82-90%    | 90-96%     |
| Motor imagery (2) | N/A       | 72-85%    | 80-92%     |
| Biometric auth    | 78-85%   | 88-93%    | 93-97%     |

## Safety Notes

- This is **not a medical device**. Do not use for diagnosis or treatment.
- **Stop immediately** if you experience headache, dizziness, nausea, or
  skin irritation at electrode sites.
- **Skin prep**: do not abrade skin if you have cuts, rashes, or skin
  conditions at electrode sites.
- **Electrical safety**: the OpenBCI board is battery-powered and
  electrically isolated. Do not modify the board or bypass isolation.
- **Hygiene**: clean electrodes with alcohol wipes between sessions.
  Do not share electrode caps without thorough cleaning.
- **Session duration**: take breaks every 2 hours. Remove electrodes if
  pressure points become uncomfortable.
- **Not for seizure-prone individuals**: flashing visual stimuli (used in
  SSVEP paradigm) may trigger photosensitive seizures. Consult a physician
  if you have a history of epilepsy.
