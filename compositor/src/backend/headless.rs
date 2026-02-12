//! Headless backend — CI testing and production headless mode.
//!
//! Supports multi-output virtual displays for s390x/mainframe use,
//! configurable poll intervals, graceful signal handling, and
//! periodic status logging.

use crate::{ipc, state::EwwmState};
use super::IpcConfig;
use smithay::{
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::{
        calloop::{
            signals::{Signal, Signals},
            timer::{TimeoutAction, Timer},
            EventLoop,
        },
        wayland_server::{Display, ListeningSocket},
    },
    utils::Transform,
};
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;
use tracing::info;

/// Global flag set by SIGTERM/SIGINT handlers.
static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);

/// Headless output configuration.
#[derive(Debug, Clone)]
pub struct HeadlessConfig {
    /// Number of virtual outputs to create.
    pub output_count: u32,
    /// Virtual output width in pixels.
    pub width: i32,
    /// Virtual output height in pixels.
    pub height: i32,
    /// Poll interval in milliseconds (higher = less CPU).
    pub poll_interval_ms: u64,
}

impl Default for HeadlessConfig {
    fn default() -> Self {
        Self {
            output_count: 1,
            width: 1920,
            height: 1080,
            poll_interval_ms: 100,
        }
    }
}

impl HeadlessConfig {
    /// Parse a "WxH" resolution string. Returns (width, height) or None.
    pub fn parse_resolution(s: &str) -> Option<(i32, i32)> {
        let parts: Vec<&str> = s.split('x').collect();
        if parts.len() != 2 {
            return None;
        }
        let w = parts[0].parse::<i32>().ok()?;
        let h = parts[1].parse::<i32>().ok()?;
        if w > 0 && h > 0 {
            Some((w, h))
        } else {
            None
        }
    }
}

/// Create a virtual output with the given index and resolution.
fn create_virtual_output(
    state: &mut EwwmState,
    index: u32,
    width: i32,
    height: i32,
) -> Output {
    let mode = OutputMode {
        size: (width, height).into(),
        refresh: 60_000,
    };
    let output = Output::new(
        format!("headless-{}", index),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Headless".into(),
        },
    );
    output.change_current_state(
        Some(mode),
        Some(Transform::Normal),
        None,
        Some(((index as i32) * width, 0).into()),
    );
    output.set_preferred(mode);
    state.space.map_output(&output, ((index as i32) * width, 0));
    info!(
        "Created virtual output headless-{}: {}x{} at offset ({}, 0)",
        index, width, height, (index as i32) * width
    );
    output
}

/// Run the compositor in headless mode.
///
/// Parameters:
/// - `socket_name`: Override Wayland socket name (or auto-assign).
/// - `exit_after`: Exit after N seconds (for CI).
/// - `ipc_config`: IPC socket configuration.
/// - `config`: Headless backend configuration (outputs, resolution, poll).
pub fn run(
    socket_name: Option<String>,
    exit_after: Option<u64>,
    ipc_config: IpcConfig,
    config: HeadlessConfig,
) -> anyhow::Result<()> {
    let mut event_loop = EventLoop::<EwwmState>::try_new()?;
    let mut display = Display::<EwwmState>::new()?;

    let mut state = EwwmState::new(&mut display, event_loop.handle());

    // Store headless config in state for IPC queries
    state.headless_active = true;
    state.headless_output_count = config.output_count;
    state.headless_width = config.width;
    state.headless_height = config.height;

    // Configure IPC
    state.ipc_server.ipc_trace = ipc_config.trace;
    let ipc_path = ipc_config
        .socket_path
        .unwrap_or_else(|| ipc::IpcServer::default_socket_path());
    state.ipc_server.socket_path = ipc_path.clone();
    ipc::IpcServer::bind(&ipc_path, &event_loop.handle())?;

    // Create virtual outputs
    let output_count = config.output_count.max(1);
    let mut outputs = Vec::new();
    for i in 0..output_count {
        let output = create_virtual_output(&mut state, i, config.width, config.height);
        outputs.push(output);
    }
    info!(
        "Created {} virtual output(s) at {}x{}",
        output_count, config.width, config.height
    );

    // Set up Wayland socket via ListeningSocket (Smithay 0.7 / wayland-server 0.31)
    let listening_socket = if let Some(ref name) = socket_name {
        let runtime_dir = std::env::var("XDG_RUNTIME_DIR")
            .unwrap_or_else(|_| "/tmp".to_string());
        let path = std::path::PathBuf::from(runtime_dir).join(name);
        ListeningSocket::bind(path)
            .map_err(|e| anyhow::anyhow!("failed to bind wayland socket '{}': {}", name, e))?
    } else {
        ListeningSocket::bind_auto("wayland", 0..33)
            .map_err(|e| anyhow::anyhow!("failed to bind wayland socket: {}", e))?
    };
    let socket_name_os = listening_socket
        .socket_name()
        .expect("listening socket must have a name")
        .to_os_string();
    info!("Wayland socket: {}", socket_name_os.to_string_lossy());
    std::env::set_var("WAYLAND_DISPLAY", &socket_name_os);

    // Register ListeningSocket with calloop to accept client connections
    event_loop.handle().insert_source(
        listening_socket,
        |client_stream, _, state: &mut EwwmState| {
            if let Err(e) = state
                .display_handle
                .insert_client(client_stream, std::sync::Arc::new(crate::state::ClientState::default()))
            {
                tracing::warn!("Failed to accept wayland client: {}", e);
            }
        },
    ).map_err(|e| anyhow::anyhow!("failed to register listening socket: {}", e.error))?;

    // Signal handling: SIGTERM and SIGINT for graceful shutdown
    let signals = Signals::new([Signal::SIGTERM, Signal::SIGINT])
        .map_err(|e| anyhow::anyhow!("failed to create signal source: {}", e))?;
    event_loop.handle().insert_source(signals, |event, _, state: &mut EwwmState| {
        info!("Received signal {:?}, initiating graceful shutdown", event.signal());
        SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
        state.running = false;
    }).map_err(|e| anyhow::anyhow!("failed to register signal handler: {}", e.error))?;

    // Exit timer for CI
    if let Some(seconds) = exit_after {
        info!("Will exit after {} seconds", seconds);
        event_loop.handle().insert_source(
            Timer::from_duration(Duration::from_secs(seconds)),
            |_, _, state: &mut EwwmState| {
                info!("Headless exit timer fired");
                state.running = false;
                TimeoutAction::Drop
            },
        ).map_err(|e| anyhow::anyhow!("failed to register exit timer: {}", e.error))?;
    }

    // Periodic status logging (every 60 seconds)
    event_loop.handle().insert_source(
        Timer::from_duration(Duration::from_secs(60)),
        |_, _, state: &mut EwwmState| {
            let surface_count = state.surfaces.len();
            let ipc_client_count = state.ipc_server.clients.len();
            info!(
                "Headless status: {} surface(s), {} IPC client(s), {} output(s)",
                surface_count, ipc_client_count, state.headless_output_count
            );
            TimeoutAction::ToDuration(Duration::from_secs(60))
        },
    ).map_err(|e| anyhow::anyhow!("failed to register status timer: {}", e.error))?;

    let poll_interval = Duration::from_millis(config.poll_interval_ms);
    info!(
        "Headless backend initialized (poll interval: {}ms), entering event loop",
        config.poll_interval_ms
    );

    while state.running {
        // Check global shutdown flag (set by signal handler)
        if SHUTDOWN_REQUESTED.load(Ordering::SeqCst) {
            state.running = false;
            break;
        }

        // Poll IPC clients
        ipc::IpcServer::poll_clients(&mut state);

        event_loop.dispatch(Some(poll_interval), &mut state)?;
        display.flush_clients()?;
    }

    // Clean up IPC socket
    let _ = std::fs::remove_file(&state.ipc_server.socket_path);

    info!(
        "Headless backend shutting down ({} surface(s), {} IPC client(s))",
        state.surfaces.len(),
        state.ipc_server.clients.len()
    );
    Ok(())
}
