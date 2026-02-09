//! Backend abstraction — DRM, Winit, and headless backends.

use std::path::PathBuf;

pub mod drm;
pub mod headless;
pub mod winit;

/// Backend type selector.
#[derive(Debug, Clone, Copy)]
pub enum BackendType {
    Winit,
    Drm,
    Headless,
}

/// IPC configuration passed to backends.
pub struct IpcConfig {
    pub socket_path: Option<PathBuf>,
    pub trace: bool,
}

/// Run the compositor with the selected backend.
pub fn run(
    backend: BackendType,
    socket_name: Option<String>,
    headless_exit_after: Option<u64>,
    ipc_socket: Option<PathBuf>,
    ipc_trace: bool,
) -> anyhow::Result<()> {
    let ipc = IpcConfig {
        socket_path: ipc_socket,
        trace: ipc_trace,
    };
    match backend {
        BackendType::Winit => winit::run(socket_name, ipc),
        BackendType::Drm => drm::run(socket_name, ipc),
        BackendType::Headless => headless::run(socket_name, headless_exit_after, ipc),
    }
}
