//! DRM backend — production mode, direct hardware rendering.
//!
//! Requires libseat session, DRM device access, and KMS/GBM.
//! This is the backend used when running as a display server.

use super::IpcConfig;
use tracing::{error, info};

pub fn run(socket_name: Option<String>, _ipc_config: IpcConfig) -> anyhow::Result<()> {
    info!("DRM backend selected");
    error!(
        "DRM backend not yet fully implemented. \
         Use --backend winit for development or --backend headless for CI."
    );

    Err(anyhow::anyhow!(
        "DRM backend requires Linux with libseat. Use --backend winit for development."
    ))
}
