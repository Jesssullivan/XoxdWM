//! EWWM Compositor - VR-first Wayland compositor built on Smithay
//!
//! Part of the EXWM-VR project: a transhuman Emacs window manager.

use ewwm_compositor::backend;

use clap::Parser;
use tracing::info;

#[derive(Parser, Debug)]
#[command(name = "ewwm-compositor", about = "EXWM-VR Wayland compositor")]
struct Cli {
    /// Backend to use: winit, drm, or headless
    #[arg(long, default_value = "auto")]
    backend: String,

    /// Wayland socket name (default: auto-assigned)
    #[arg(long)]
    wayland_socket: Option<String>,

    /// Exit after N seconds (headless mode testing)
    #[arg(long)]
    headless_exit_after: Option<u64>,

    /// Number of virtual outputs in headless mode
    #[arg(long, default_value = "1")]
    headless_outputs: u32,

    /// Virtual output resolution (WxH)
    #[arg(long, default_value = "1920x1080")]
    headless_resolution: String,

    /// IPC socket path (default: $XDG_RUNTIME_DIR/ewwm-ipc.sock)
    #[arg(long)]
    ipc_socket: Option<String>,

    /// Log all IPC messages to stderr
    #[arg(long)]
    ipc_trace: bool,

    /// Show version and exit
    #[arg(long)]
    version: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    if cli.version {
        println!("ewwm-compositor {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "ewwm_compositor=info,smithay=warn".into()),
        )
        .init();

    info!("ewwm-compositor v{} starting", env!("CARGO_PKG_VERSION"));
    info!("backend: {}", cli.backend);

    let backend_type = match cli.backend.as_str() {
        #[cfg(feature = "full-backend")]
        "winit" => backend::BackendType::Winit,
        #[cfg(feature = "full-backend")]
        "drm" => backend::BackendType::Drm,
        "headless" => backend::BackendType::Headless,
        "auto" => {
            #[cfg(feature = "full-backend")]
            {
                if std::env::var("DISPLAY").is_ok() || std::env::var("WAYLAND_DISPLAY").is_ok() {
                    info!("auto-detected: running under existing display, using winit backend");
                    backend::BackendType::Winit
                } else {
                    info!("auto-detected: no display found, using drm backend");
                    backend::BackendType::Drm
                }
            }
            #[cfg(not(feature = "full-backend"))]
            {
                info!("auto-detected: full-backend disabled, using headless backend");
                backend::BackendType::Headless
            }
        }
        #[cfg(not(feature = "full-backend"))]
        "winit" | "drm" => {
            eprintln!(
                "Backend '{}' requires the 'full-backend' feature. \
                 Compile with: cargo build --features full-backend",
                cli.backend
            );
            std::process::exit(1);
        }
        other => {
            eprintln!("Unknown backend: {other}. Use: winit, drm, headless, or auto");
            std::process::exit(1);
        }
    };

    // Parse headless resolution
    let (h_width, h_height) = backend::headless::HeadlessConfig::parse_resolution(
        &cli.headless_resolution,
    )
    .unwrap_or_else(|| {
        eprintln!(
            "Invalid headless resolution '{}', using 1920x1080",
            cli.headless_resolution
        );
        (1920, 1080)
    });

    let headless_config = backend::headless::HeadlessConfig {
        output_count: cli.headless_outputs,
        width: h_width,
        height: h_height,
        poll_interval_ms: 100,
    };

    let ipc_socket = cli.ipc_socket.map(std::path::PathBuf::from);

    backend::run(
        backend_type,
        cli.wayland_socket,
        cli.headless_exit_after,
        headless_config,
        ipc_socket,
        cli.ipc_trace,
    )
}
