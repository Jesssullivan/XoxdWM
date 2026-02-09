//! Winit backend — development mode, compositor runs inside a window.

use crate::{ipc, render, state::EwwmState};
use super::IpcConfig;
use smithay::{
    backend::winit::{self as winit_backend, WinitEvent},
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::calloop::{
        timer::{TimeoutAction, Timer},
        EventLoop,
    },
    reexports::wayland_server::Display,
    utils::{Rectangle, Size, Transform},
    xwayland::{XWayland, XWaylandEvent},
    xwayland::xwm::X11Wm,
};
use std::process::Stdio;
use std::time::Duration;
use tracing::{error, info, warn};

pub fn run(socket_name: Option<String>, ipc_config: IpcConfig) -> anyhow::Result<()> {
    let mut event_loop = EventLoop::<EwwmState>::try_new()?;
    let mut display = Display::<EwwmState>::new()?;

    let mut state = EwwmState::new(&mut display, event_loop.handle());

    // Configure IPC
    state.ipc_server.ipc_trace = ipc_config.trace;
    let ipc_path = ipc_config
        .socket_path
        .unwrap_or_else(|| ipc::IpcServer::default_socket_path());
    state.ipc_server.socket_path = ipc_path.clone();
    ipc::IpcServer::bind(&ipc_path, &event_loop.handle())?;

    // Initialize Winit backend
    let (mut backend, mut winit_evt) = winit_backend::init::<
        smithay::backend::renderer::gles::GlesRenderer,
    >()?;

    // Create output matching window size
    let mode = OutputMode {
        size: (1920, 1080).into(),
        refresh: 60_000,
    };
    let output = Output::new(
        "winit-0".to_string(),
        PhysicalProperties {
            size: (0, 0).into(),
            subpixel: Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Winit".into(),
        },
    );
    output.change_current_state(Some(mode), Some(Transform::Normal), None, Some((0, 0).into()));
    output.set_preferred(mode);
    state.space.map_output(&output, (0, 0));

    // Set up Wayland socket
    let socket = if let Some(name) = socket_name {
        display.handle().add_socket_name(name)?
    } else {
        display.handle().add_socket_auto()?
    };
    info!("Wayland socket: {}", socket.to_string_lossy());
    std::env::set_var("WAYLAND_DISPLAY", &socket);

    // Spawn XWayland
    match XWayland::spawn(
        &display.handle(),
        None,
        std::iter::empty::<(String, String)>(),
        true,
        Stdio::null(),
        Stdio::null(),
        |_| (),
    ) {
        Ok((xwayland, client)) => {
            let dh = display.handle();
            event_loop.handle().insert_source(xwayland, move |event, _, state: &mut EwwmState| {
                match event {
                    XWaylandEvent::Ready { x11_socket, display_number } => {
                        info!(display_number, "XWayland ready");
                        match X11Wm::start_wm(
                            state.loop_handle.clone(),
                            &dh,
                            x11_socket,
                            client.clone(),
                        ) {
                            Ok(wm) => {
                                state.xwm = Some(wm);
                                state.xdisplay = Some(display_number);
                                std::env::set_var("DISPLAY", format!(":{}", display_number));
                            }
                            Err(e) => {
                                error!("Failed to start X11 WM: {}", e);
                            }
                        }
                    }
                    XWaylandEvent::Error => {
                        warn!("XWayland crashed on startup (continuing without X11 support)");
                    }
                }
            }).ok();
            info!("XWayland spawning");
        }
        Err(e) => {
            warn!("XWayland not available: {} (continuing without X11 support)", e);
        }
    }

    // Insert Wayland display source into event loop
    event_loop.handle().insert_source(
        smithay::reexports::calloop::generic::Generic::new(
            display.backend().poll_fd(),
            smithay::reexports::calloop::Interest::READ,
            smithay::reexports::calloop::Mode::Level,
        ),
        |_, _, state: &mut EwwmState| {
            Ok(smithay::reexports::calloop::PostAction::Continue)
        },
    )?;

    // Render timer (60 Hz)
    event_loop.handle().insert_source(
        Timer::from_duration(Duration::from_millis(16)),
        move |_, _, state| {
            // Render frame
            render::render_winit(&mut backend, state, &output);
            TimeoutAction::ToDuration(Duration::from_millis(16))
        },
    )?;

    info!("Winit backend initialized, entering event loop");

    // Main event loop
    while state.running {
        // Dispatch Winit events
        winit_evt.dispatch_new_events(|event| match event {
            WinitEvent::Resized { size, .. } => {
                let mode = OutputMode {
                    size,
                    refresh: 60_000,
                };
                output.change_current_state(Some(mode), None, None, None);
            }
            WinitEvent::CloseRequested => {
                state.running = false;
            }
            WinitEvent::Input(event) => {
                crate::input::handle_input(&mut state, event);
            }
            _ => {}
        })?;

        // Poll IPC clients
        ipc::IpcServer::poll_clients(&mut state);

        // Dispatch calloop events
        event_loop.dispatch(Some(Duration::from_millis(1)), &mut state)?;

        // Flush client events
        display.flush_clients()?;
    }

    // Clean up IPC socket
    let _ = std::fs::remove_file(&state.ipc_server.socket_path);

    info!("Winit backend shutting down");
    Ok(())
}
