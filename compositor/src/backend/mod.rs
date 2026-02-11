//! Backend abstraction — DRM, Winit, and headless backends.

use std::path::PathBuf;

#[cfg(feature = "full-backend")]
pub mod drm;
pub mod headless;
#[cfg(feature = "full-backend")]
pub mod winit;

/// Backend type selector.
#[derive(Debug, Clone, Copy)]
pub enum BackendType {
    #[cfg(feature = "full-backend")]
    Winit,
    #[cfg(feature = "full-backend")]
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
    headless_config: headless::HeadlessConfig,
    ipc_socket: Option<PathBuf>,
    ipc_trace: bool,
) -> anyhow::Result<()> {
    let ipc = IpcConfig {
        socket_path: ipc_socket,
        trace: ipc_trace,
    };
    match backend {
        #[cfg(feature = "full-backend")]
        BackendType::Winit => winit::run(socket_name, ipc),
        #[cfg(feature = "full-backend")]
        BackendType::Drm => drm::run(socket_name, ipc),
        BackendType::Headless => headless::run(socket_name, headless_exit_after, ipc, headless_config),
    }
}
