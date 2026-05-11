//! VR subsystem — OpenXR runtime integration and 3D scene management.
//!
//! Provides:
//! - `VrState`: OpenXR lifecycle (gated behind `vr` feature)
//! - `scene`: 3D scene graph for Wayland surfaces in VR
//! - `texture`: DMA-BUF texture import pipeline (gated behind `vr` feature)
//! - `vr_renderer`: Stereo rendering to OpenXR swapchains (gated behind `vr` feature)

#[cfg(feature = "vr")]
pub mod openxr_state;

#[cfg(feature = "vr")]
pub mod frame_timing;

#[cfg(feature = "vr")]
pub mod texture;

#[cfg(feature = "vr")]
pub mod vr_renderer;

#[cfg(feature = "vr")]
pub use openxr_state::{ReferenceSpaceType, VrState};

#[cfg(not(feature = "vr"))]
pub mod stub;

#[cfg(not(feature = "vr"))]
pub use stub::{ReferenceSpaceType, VrState};

// Scene graph, DRM lease, interaction, and eye tracking are always available (no openxrs dependency).
pub mod attention;
pub mod bci_state;
pub mod beyond_hid;
pub mod blink_wink;
pub mod capture_visibility;
pub mod drm_lease;
pub mod eye_capture;
pub mod eye_tracking;
pub mod fatigue;
pub mod fatigue_eeg;
pub mod follow_mode;
pub mod gaze_focus;
pub mod gaze_scroll;
pub mod gaze_zone;
pub mod gesture;
pub mod gpu_power;
pub mod hand_tracking;
pub mod link_hints;
pub mod motor_imagery;
pub mod overlay;
pub mod p300;
pub mod pupil_detect;
pub mod radial_menu;
pub mod scene;
pub mod ssvep;
pub mod transient_3d;
pub mod virtual_keyboard;
pub mod vr_interaction;
