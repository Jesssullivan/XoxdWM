# Honey Kernel DSC Truth - 2026-05-10

## Scope

This note records the bounded kernel/driver evidence gathered during the Honey
black-goggles lab. It separates verified kernel DSC patch state from the still
unproven panel visual path.

Host state under test:

- Host: `honey`
- Kernel: `6.19.5-10.xr.el10`
- HMD connector: `card0-DP-1`
- USB HMD: `35bd:0101 Bigscreen Beyond`, serial `XCNL4M25CA001675`
- Monado mode after removing forced mode 1: `5088x2544@75`
- OpenXR session state: HelloXR selects `Head: 'Bigscreen Beyond'` and creates
  `3561x3561` eye swapchains.

## Module And EDID Truth

The repo now carries a read-only helper:

```bash
/usr/libexec/exwm-vr/kernel-dsc-truth
```

It verifies the installed kernel modules and live connector state without
writing debugfs, restarting services, detaching HID, or retraining the link.

Live Honey output from `/tmp/exwm-vr-kernel-dsc-truth`:

```text
kernel_release=6.19.5-10.xr.el10
amdgpu_sha256_prefix=a80c9f5babf3b1cf
qp_table_444_8bpc_max_bpp8_patched=true
qp_table_444_8bpc_min_bpp8_patched=true
qp_table_444_8bpc_max_bpp8_stock=false
qp_table_444_8bpc_min_bpp8_stock=false
get_ofs_set_signature=present_minus12_store_signature
drm_sha256_prefix=ec12c9e658942bde
drm_parse_vesa_specific_block=true
edid_0_vesa_dsc_0_offset=176
edid_0_vesa_dsc_0_oui=3a:02:92
edid_0_vesa_dsc_0_dp_type=1
edid_0_vesa_dsc_0_dsc_bpp_x16=128
```

This proves the live module is not missing the CachyOS-style DisplayID DSC BPP
parser or the targeted BPP=8 QP table changes. The helper also finds the
`get_ofs_set` `-12` store signature for the RC offset path.

Later in the same May 10 lab pass, the repo helper was extended to resolve the
connector dynamically and decode a packed DSC PPS if the kernel exposes one.
Running it through the SOPS sudo wrapper:

```bash
just honey-kernel-dsc-truth honey auto
```

returned:

```text
resolved_connector=DP-1
resolved_connector_source=edid-bigscreen
resolved_connector_sysfs=/sys/class/drm/card0-DP-1
debugfs_connector=/sys/kernel/debug/dri/0/DP-1
debugfs_dsc_clock_en=1
debugfs_dsc_bits_per_pixel=128
debugfs_dsc_pic_width=5088
debugfs_dsc_pic_height=2544
debugfs_dsc_slice_width=1272
debugfs_dsc_slice_height=159
debugfs_link_settings=Current:  4  0x14  0  Verified:  4  0x14  16  Reported:  4  0x14  16  Preferred:  4  0x14  0
pps_available=false
kernel_dsc_truth=pass
```

The live debugfs directory does not expose `dsc_pic_parameter_set`, `dpcd`,
`dp_dpcd`, `dpcd_registers`, or `aux_dpcd`. The actual connector files present
were scalar controls such as `dsc_bits_per_pixel`, `dsc_clock_en`,
`dsc_pic_width`, `dsc_pic_height`, `dsc_slice_width`, `dsc_slice_height`,
`dsc_chunk_size`, `dsc_slice_bpg`, `link_settings`, `phy_settings`, and
`dp_dsc_fec_support`.

## Live DSC State

Before the link canary, the live DP-1 debugfs state was:

```text
Current:  4  0xa  0
Verified:  4  0x14  16
Reported:  4  0x14  16
dsc_clock_en=1
dsc_bits_per_pixel=128
dsc_pic_width=5088
dsc_pic_height=2544
dsc_slice_width=1272
dsc_slice_height=159
dsc_chunk_size=1272
dsc_slice_bpg=70
```

A single explicit HBR2 canary was then run:

```bash
echo "4 0x14" > /sys/kernel/debug/dri/0000:05:00.0/DP-1/link_settings
```

The write took effect:

```text
Current:  4  0x14  0
Verified:  4  0x14  16
Reported:  4  0x14  16
Preferred:  4  0x14  0
```

DSC remained active with the same BPP and geometry. During the HBR2 active
HelloXR window, KMS still showed:

```text
plane[110]: plane-2
  crtc=crtc-1
  fb=343
  format=XR24
  crtc-pos=5088x2544+0+0
crtc[287]: crtc-1
  enable=1
  active=1
connector[297]: DP-1
  crtc=crtc-1
```

## Panel Controller State

The HID-only panel init path successfully talks to the `35bd:0101` controller:

- `OnHidOpen` response: `04782938010000000000000000000000`
- `SetVideoConfig` command sent with `dp_training=1`
- backlight/unmute commands accepted at the USB transfer level

But the controller remained stuck at:

```text
video_state=0 (DP Init)
final_video_state=0 (DP Init)
```

This was true before HBR2, during active HelloXR presentation, and after the
single HBR2 retrain canary.

## Classification

Current proof ladder remains:

- `P3 OpenXR Session`: pass
- `P4 Visual First Frame`: fail unless the operator observed goggles output
  during the HBR2 window

The current failure is no longer explained by:

- missing DRM lease
- missing Monado session
- missing DisplayID DSC BPP parser
- stock BPP=8 QP table rows
- forced 3840x1920 mode choice
- current DP link staying at HBR instead of HBR2

## Next Evidence Gap

The next kernel/driver lane should prove what PPS and stream-control state is
actually sent to the DP sink. The current live debugfs surface exposes DSC
geometry and BPP but not the full PPS bytes.

## Cross-Repo Boundary

Sibling repo checks on May 10 keep the ownership split as follows:

- `linux-xr` is the full kernel source tree for patch development and
  `git apply --check` validation. Its local checkout has unrelated dirty
  netfilter/litmus edits, so it was not used as the release-carry surface.
- `linux-xr-fast` is the release-carry authority for XR kernel RPM production.
  The PPS debugfs patch belongs there for a test kernel build, with static
  `series`/RPM-spec wiring checks and tarball dry-runs before any tag or CI
  build is trusted.
- `blahaj` remains the live Honey/RKE2 cluster authority. Its current docs keep
  Honey as the single RKE2 server and mark kernel XR artifacts as adopted-live
  but not fully converged into host IaC. Do not use cluster drain, reboot, or
  rke2 operations as part of PPS evidence capture.

The repo now carries a read-only kernel patch draft:

```text
patches/amdgpu-dsc-pps-debugfs.patch
```

It adds a connector debugfs file named `dsc_pic_parameter_set` that dumps the
cached `dc_stream_state.dsc_packed_pps[128]` as hex bytes. This is the payload
AMD DC already generates in `dsc_get_packed_pps()` and stores on the stream
before sending the PPS SDP; the patch only reads it back. It has been checked
with:

```bash
git -C /Users/jess/git/linux-xr apply --check \
  /Users/jess/git/XoxdWM/patches/amdgpu-dsc-pps-debugfs.patch
```

The diagnostic patch has also been staged in the `linux-xr-fast` release-carry
repo as `xr/patches/amdgpu-dsc-pps-debugfs.patch`, listed after the existing
DSC and Beyond EDID patches in `xr/patches/series`, and applied from
`xr/specs/kernel-xr.spec` as `Patch20`.

The staged carry was validated with:

```bash
cd /Users/jess/git/linux-xr-fast

xr/scripts/check-rpm-patch-wiring.sh

perl /Users/jess/git/linux-xr/scripts/checkpatch.pl --no-tree --strict \
  xr/patches/amdgpu-dsc-pps-debugfs.patch

xr/scripts/check-kernel-carry.sh --kernel-version 6.19.5

xr/scripts/check-kernel-carry.sh --kernel-version 6.19.14
```

The `linux-xr-fast` build and carry-check scripts now run
`xr/scripts/check-rpm-patch-wiring.sh` before staging or dry-running carry
patches, so a future `series` update fails if the RPM spec does not declare and
apply the patch. Both kernel carry dry-runs apply the full `series` with zero
fuzz against kernel.org tarballs. This does not make the patch a visual fix; it
only gives the next test kernel a read-only way to prove the exact packed PPS
bytes that AMD DC cached for the live stream.

Useful next steps:

1. Build a separate linux-xr test kernel with the read-only PPS debugfs patch,
   then compare the live PPS against the corrected Linux PPS layout for
   BPP=128.
2. Capture the DPCD/link-training state around the HBR2 retrain and panel HID
   `video_state` polling.
3. Audit Watchman `28de:2300` video/display-state traffic. The proprietary
   `35bd:0101` power/backlight path is reachable, but it may not be sufficient
   to move the display controller out of `DP Init`.
4. Keep DSC sweeps and broader link retraining separate from the safe read-only
   truth path.
