//! Wayland protocol handler implementations.

pub mod compositor;
pub mod data_control;
pub mod dmabuf;
pub mod dpms;
#[cfg(feature = "full-backend")]
pub mod drm_lease;
pub mod foreign_toplevel;
pub mod idle;
pub mod layer_shell;
pub mod output_management;
pub mod pointer_constraints;
pub mod screencopy;
pub mod seat;
pub mod session_lock;
pub mod shm;
pub mod xdg_activation;
pub mod xdg_shell;
#[cfg(feature = "xwayland")]
pub mod xwayland;
