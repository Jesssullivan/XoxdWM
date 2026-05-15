//! DRM backend — production mode, direct hardware rendering.
//!
//! Requires libseat session, DRM device access, and KMS/GBM.
//! This is the backend used when running as a real display server
//! on a Linux console (TTY) without an existing Wayland/X11 session.
//!
//! Architecture:
//! - LibSeatSession for privilege management and VT switching
//! - UdevBackend for GPU device enumeration and hotplug monitoring
//! - DrmDevice + GbmBufferedSurface for KMS modesetting and buffer management
//! - LibinputInputBackend for keyboard, pointer, and touch input
//! - GlesRenderer (via EGL on GBM) for OpenGL ES rendering
//!
//! The render cycle is page-flip driven: render a frame, queue it for
//! scanout, wait for vblank, then call frame_submitted() and render
//! the next frame.

use super::IpcConfig;
use crate::config::CompositorConfig;
use crate::{ipc, state::EwwmState};

use smithay::{
    backend::{
        allocator::{
            dmabuf::Dmabuf,
            format::FormatSet,
            gbm::{GbmAllocator, GbmBufferFlags, GbmDevice},
            Fourcc,
        },
        drm::{DrmDevice, DrmDeviceFd, DrmEvent, DrmNode, GbmBufferedSurface, NodeType},
        egl::{EGLContext, EGLDisplay},
        libinput::{LibinputInputBackend, LibinputSessionInterface},
        renderer::{damage::OutputDamageTracker, gles::GlesRenderer, Bind},
        session::{libseat::LibSeatSession, Event as SessionEvent, Session},
        udev::{UdevBackend, UdevEvent},
    },
    output::{Mode as OutputMode, Output, PhysicalProperties, Subpixel},
    reexports::{
        calloop::{EventLoop, LoopHandle, RegistrationToken},
        drm::control::{connector, crtc, Device as ControlDevice, ModeTypeFlags},
        input::Libinput,
        wayland_server::Display,
    },
    utils::{DeviceFd, Size, Transform},
};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::time::Duration;

use tracing::{debug, error, info, warn};

#[cfg(feature = "xwayland")]
use smithay::{
    xwayland::xwm::X11Wm,
    xwayland::{XWayland, XWaylandEvent},
};
#[cfg(feature = "xwayland")]
use std::process::Stdio;

/// Global shutdown flag for signal handlers.
static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);

extern "C" fn drm_signal_handler(_sig: libc::c_int) {
    SHUTDOWN_REQUESTED.store(true, Ordering::SeqCst);
}

/// Preferred color formats for GBM buffer allocation, in priority order.
const COLOR_FORMATS: &[Fourcc] = &[
    Fourcc::Argb8888,
    Fourcc::Xrgb8888,
    Fourcc::Abgr8888,
    Fourcc::Xbgr8888,
];

// ---------------------------------------------------------------------------
// Per-output surface state
// ---------------------------------------------------------------------------

/// State for a single DRM output (connector + CRTC + surface).
struct OutputState {
    /// The Smithay output object (advertised to Wayland clients).
    output: Output,
    /// CRTC handle driving this output.
    _crtc: crtc::Handle,
    /// GBM-backed swapchain surface for page flipping.
    surface: GbmBufferedSurface<GbmAllocator<DrmDeviceFd>, ()>,
    /// Damage tracker for efficient partial redraws.
    damage_tracker: OutputDamageTracker,
    /// Whether we have a frame queued and are waiting for vblank.
    pending_frame: bool,
    /// Whether we need to schedule a new render after the current
    /// frame completes.
    render_scheduled: bool,
}

/// Connector properties relevant to desktop-vs-lease routing.
struct ConnectorScan {
    info: connector::Info,
    name: String,
    connected: bool,
    actual_non_desktop: bool,
    override_match: bool,
}

impl ConnectorScan {
    fn lease_candidate(&self) -> bool {
        self.connected && (self.actual_non_desktop || self.override_match)
    }

    fn lease_description(&self) -> String {
        if self.actual_non_desktop {
            format!("{} (kernel non_desktop)", self.name)
        } else if self.override_match {
            format!("{} (explicit lease override)", self.name)
        } else {
            self.name.clone()
        }
    }
}

// ---------------------------------------------------------------------------
// Per-GPU device state
// ---------------------------------------------------------------------------

/// State for a single DRM GPU device.
struct GpuDevice {
    /// DRM node for this GPU.
    node: DrmNode,
    /// The DRM device handle.
    drm: Rc<RefCell<DrmDevice>>,
    /// GBM device for buffer allocation.
    gbm: GbmDevice<DrmDeviceFd>,
    /// EGL display for this GPU.
    _egl_display: EGLDisplay,
    /// OpenGL ES renderer.
    renderer: GlesRenderer,
    /// Active outputs on this device, keyed by CRTC handle.
    outputs: HashMap<crtc::Handle, OutputState>,
    /// Registration token for the DRM event source in calloop.
    drm_token: RegistrationToken,
    /// Device path for re-opening after VT switch.
    _path: PathBuf,
}

// ---------------------------------------------------------------------------
// Backend-wide state
// ---------------------------------------------------------------------------

/// DRM backend state, stored alongside the event loop.
struct DrmBackendData {
    /// Session handle for privilege management.
    session: LibSeatSession,
    /// Active GPU devices, keyed by DRM node.
    devices: HashMap<DrmNode, GpuDevice>,
    /// Primary render node (first GPU found).
    primary_node: Option<DrmNode>,
}

// ---------------------------------------------------------------------------
// Device lifecycle
// ---------------------------------------------------------------------------

/// Add a DRM device: open it, set up GBM/EGL/renderer, scan connectors.
fn device_added(
    node: DrmNode,
    path: &Path,
    backend: &mut DrmBackendData,
    state: &mut EwwmState,
    loop_handle: &LoopHandle<'static, EwwmState>,
) -> anyhow::Result<()> {
    // Only handle render nodes (card devices).
    let node = node
        .node_with_type(NodeType::Render)
        .and_then(|n| n.ok())
        .unwrap_or(node);

    if backend.devices.contains_key(&node) {
        debug!(?node, "device already tracked, skipping");
        return Ok(());
    }

    info!(?path, ?node, "opening DRM device");

    // Open the device through the session (for seatd privilege).
    let fd = backend
        .session
        .open(
            path,
            smithay::reexports::rustix::fs::OFlags::RDWR
                | smithay::reexports::rustix::fs::OFlags::CLOEXEC
                | smithay::reexports::rustix::fs::OFlags::NOCTTY
                | smithay::reexports::rustix::fs::OFlags::NONBLOCK,
        )
        .map_err(|e| anyhow::anyhow!("failed to open DRM device {:?}: {}", path, e))?;

    let drm_fd = DrmDeviceFd::new(DeviceFd::from(fd));

    // Create DRM device (disable_connectors=true so we start clean).
    let (drm, drm_notifier) = DrmDevice::new(drm_fd.clone(), true)
        .map_err(|e| anyhow::anyhow!("DrmDevice::new failed: {}", e))?;
    let drm = Rc::new(RefCell::new(drm));

    // Hybrid laptops can expose render/offload GPUs with no scanout connectors.
    // Avoid initializing GBM/EGL on those devices before the real display GPU.
    let connector_count = match drm.borrow().resource_handles() {
        Ok(resources) => resources.connectors().len(),
        Err(e) => {
            warn!(
                ?path,
                ?node,
                "failed to read DRM connectors before renderer init: {}",
                e
            );
            0
        }
    };
    if connector_count == 0 {
        info!(
            ?path,
            ?node,
            "DRM device has no connectors, skipping renderer initialization"
        );
        return Ok(());
    }

    // Create GBM device on the same fd.
    let gbm = GbmDevice::new(drm_fd.clone())
        .map_err(|e| anyhow::anyhow!("GbmDevice::new failed: {}", e))?;

    // Set up EGL display and GLES renderer.
    let egl_display = unsafe { EGLDisplay::new(gbm.clone()) }
        .map_err(|e| anyhow::anyhow!("EGLDisplay::new failed: {}", e))?;

    let egl_context = EGLContext::new(&egl_display)
        .map_err(|e| anyhow::anyhow!("EGLContext::new failed: {}", e))?;

    let renderer = unsafe { GlesRenderer::new(egl_context) }
        .map_err(|e| anyhow::anyhow!("GlesRenderer::new failed: {}", e))?;

    // Get renderer's supported DMA-BUF render formats for surface creation.
    let renderer_formats = renderer.egl_context().dmabuf_render_formats().clone();

    // Register DRM event source with calloop for vblank notifications.
    let drm_node = node;
    let drm_token = loop_handle
        .insert_source(
            drm_notifier,
            move |event, _metadata, state: &mut EwwmState| match event {
                DrmEvent::VBlank(crtc) => {
                    handle_vblank(state, drm_node, crtc);
                }
                DrmEvent::Error(err) => {
                    error!(?err, "DRM device error");
                }
            },
        )
        .map_err(|e| anyhow::anyhow!("failed to register DRM source: {}", e))?;

    let mut gpu = GpuDevice {
        node,
        drm,
        gbm,
        _egl_display: egl_display,
        renderer,
        outputs: HashMap::new(),
        drm_token,
        _path: path.to_owned(),
    };

    // Track as primary if first device.
    if backend.primary_node.is_none() {
        backend.primary_node = Some(node);
        info!(?node, "set as primary GPU");
        #[cfg(feature = "full-backend")]
        state.ensure_drm_lease_state(node);
    }

    #[cfg(feature = "full-backend")]
    state.register_drm_lease_device(node, gpu.drm.clone());

    // Scan and configure connected outputs.
    scan_connectors(&mut gpu, &renderer_formats, state);

    backend.devices.insert(node, gpu);
    info!(?node, "DRM device added successfully");

    Ok(())
}

/// Check if a DRM connector has the non_desktop property set.
/// Non-desktop connectors are VR headsets and should not be used for desktop output.
fn is_non_desktop_connector(drm: &DrmDevice, conn: &connector::Info) -> bool {
    let handle = conn.handle();
    let props = match drm.get_properties(handle) {
        Ok(p) => p,
        Err(_) => return false,
    };

    for (prop_handle, value) in props.iter() {
        if let Ok(info) = drm.get_property(*prop_handle) {
            if info.name().to_str() == Ok("non_desktop") {
                return *value != 0;
            }
        }
    }

    false
}

fn connector_display_name(conn: &connector::Info) -> String {
    format!(
        "{}-{}",
        connector_type_name(conn.interface()),
        conn.interface_id(),
    )
}

fn add_connector_overrides(names: &mut HashSet<String>, raw: &str) {
    for name in raw.split(',') {
        let trimmed = name.trim();
        if !trimmed.is_empty() {
            names.insert(trimmed.to_string());
        }
    }
}

fn lease_connector_overrides() -> HashSet<String> {
    let mut names = HashSet::new();

    if let Ok(raw) = std::env::var("EWWM_DRM_LEASE_CONNECTORS") {
        add_connector_overrides(&mut names, &raw);
    }

    if let Ok(raw) = std::env::var("XRT_COMPOSITOR_WAYLAND_CONNECTOR") {
        add_connector_overrides(&mut names, &raw);
    }

    names
}

/// Scan DRM connectors and set up outputs for each connected display.
fn scan_connectors(gpu: &mut GpuDevice, renderer_formats: &FormatSet, state: &mut EwwmState) {
    let res = match gpu.drm.borrow().resource_handles() {
        Ok(r) => r,
        Err(e) => {
            error!("failed to get DRM resource handles: {}", e);
            return;
        }
    };

    let lease_overrides = lease_connector_overrides();
    if !lease_overrides.is_empty() {
        info!(
            overrides = ?lease_overrides,
            "using explicit DRM lease connector overrides"
        );
    }

    let connectors: Vec<ConnectorScan> = {
        let drm = gpu.drm.borrow();

        res.connectors()
            .iter()
            .filter_map(|handle| drm.get_connector(*handle, false).ok())
            .map(|conn| {
                let name = connector_display_name(&conn);
                let actual_non_desktop = is_non_desktop_connector(&drm, &conn);
                let override_match = lease_overrides.contains(&name);
                let connected = conn.state() == connector::State::Connected;

                if override_match && !actual_non_desktop {
                    info!(
                        connector = %name,
                        "treating connector as a DRM lease candidate via explicit override"
                    );
                }

                ConnectorScan {
                    info: conn,
                    name,
                    connected,
                    actual_non_desktop,
                    override_match,
                }
            })
            .collect()
    };

    let _crtcs = res.crtcs();
    let mut used_crtcs = std::collections::HashSet::new();

    // Track which CRTCs are already in use.
    for crtc_handle in gpu.outputs.keys() {
        used_crtcs.insert(*crtc_handle);
    }

    // Collect connector info for the HMD manager. The Rocky package does not
    // enable the OpenXR `vr` feature, so this bookkeeping must stay available
    // in the normal full-backend compositor path as well.
    let mut vr_connectors = Vec::new();
    for connector in &connectors {
        let connector_type = match connector.info.interface() {
            connector::Interface::DisplayPort => crate::vr::drm_lease::ConnectorType::DisplayPort,
            connector::Interface::HDMIA | connector::Interface::HDMIB => {
                crate::vr::drm_lease::ConnectorType::Hdmi
            }
            connector::Interface::USB => crate::vr::drm_lease::ConnectorType::UsbC,
            connector::Interface::Virtual => crate::vr::drm_lease::ConnectorType::Virtual,
            _ => crate::vr::drm_lease::ConnectorType::Unknown,
        };
        let modes: Vec<crate::vr::drm_lease::DisplayMode> = connector
            .info
            .modes()
            .iter()
            .map(|m| crate::vr::drm_lease::DisplayMode {
                width: m.size().0 as u32,
                height: m.size().1 as u32,
                refresh_hz: (m.vrefresh() as u32).max(1),
                preferred: m.mode_type().contains(ModeTypeFlags::PREFERRED),
            })
            .collect();

        vr_connectors.push(crate::vr::drm_lease::DrmConnectorInfo {
            connector_id: connector.info.handle().into(),
            connector_name: connector.name.clone(),
            connector_type,
            non_desktop: connector.lease_candidate(),
            connected: connector.connected,
            manufacturer: String::new(),
            model: String::new(),
            serial: String::new(),
            modes,
        });
    }
    state.vr_state.hmd_manager.update_connectors(vr_connectors);

    #[cfg(feature = "full-backend")]
    if let Some(drm_lease_state) = state.drm_lease_state.as_mut() {
        for connector in &connectors {
            if connector.lease_candidate() {
                drm_lease_state.add_connector::<EwwmState>(
                    connector.info.handle(),
                    connector.name.clone(),
                    connector.lease_description(),
                );
            } else {
                drm_lease_state.withdraw_connector(connector.info.handle());
            }
        }
    }

    for connector in &connectors {
        if !connector.connected {
            continue;
        }

        // Skip lease-designated connectors — they're reserved for direct mode.
        if connector.lease_candidate() {
            info!(
                connector = %connector.name,
                actual_non_desktop = connector.actual_non_desktop,
                override_match = connector.override_match,
                "lease-designated connector, skipping desktop output"
            );
            continue;
        }

        // Find a preferred mode (or fall back to the first available).
        let mode = connector
            .info
            .modes()
            .iter()
            .find(|m| m.mode_type().contains(ModeTypeFlags::PREFERRED))
            .or_else(|| connector.info.modes().first());

        let drm_mode = match mode {
            Some(m) => *m,
            None => {
                warn!(
                    connector = ?connector.info.handle(),
                    "connected but no modes available, skipping"
                );
                continue;
            }
        };

        // Find an available CRTC for this connector.
        // Use ResourceHandles::filter_crtcs to get compatible CRTCs.
        let crtc = {
            let drm = gpu.drm.borrow();
            connector
                .info
                .encoders()
                .iter()
                .filter_map(|enc_handle| drm.get_encoder(*enc_handle).ok())
                .flat_map(|enc| res.filter_crtcs(enc.possible_crtcs()))
                .find(|crtc| !used_crtcs.contains(crtc))
        };

        let crtc = match crtc {
            Some(c) => c,
            None => {
                warn!(
                    connector = ?connector.info.handle(),
                    "no available CRTC, skipping"
                );
                continue;
            }
        };
        used_crtcs.insert(crtc);

        // Create DRM surface for this connector + CRTC + mode.
        let drm_surface = {
            let mut drm = gpu.drm.borrow_mut();
            match drm.create_surface(crtc, drm_mode, &[connector.info.handle()]) {
                Ok(s) => s,
                Err(e) => {
                    error!(
                        connector = ?connector.info.handle(),
                        crtc = ?crtc,
                        "failed to create DRM surface: {}", e
                    );
                    continue;
                }
            }
        };

        // Create GBM allocator for this surface's swapchain.
        let allocator = GbmAllocator::new(
            gpu.gbm.clone(),
            GbmBufferFlags::RENDERING | GbmBufferFlags::SCANOUT,
        );

        // Create the GBM buffered surface (double/triple buffer swapchain).
        let surface = match GbmBufferedSurface::new(
            drm_surface,
            allocator,
            COLOR_FORMATS,
            renderer_formats.iter().cloned(),
        ) {
            Ok(s) => s,
            Err(e) => {
                error!(
                    connector = ?connector.info.handle(),
                    "failed to create GbmBufferedSurface: {}", e
                );
                continue;
            }
        };

        // Build output name from connector type and index.
        let connector_name = connector.name.clone();

        // Physical size in mm from EDID.
        let (phys_w, phys_h) = connector.info.size().unwrap_or((0, 0));

        let wl_mode = OutputMode {
            size: (drm_mode.size().0 as i32, drm_mode.size().1 as i32).into(),
            refresh: (drm_mode.vrefresh() * 1000) as i32,
        };

        let output = Output::new(
            connector_name.clone(),
            PhysicalProperties {
                size: (phys_w as i32, phys_h as i32).into(),
                subpixel: match connector.info.subpixel() {
                    connector::SubPixel::HorizontalRgb => Subpixel::HorizontalRgb,
                    connector::SubPixel::HorizontalBgr => Subpixel::HorizontalBgr,
                    connector::SubPixel::VerticalRgb => Subpixel::VerticalRgb,
                    connector::SubPixel::VerticalBgr => Subpixel::VerticalBgr,
                    connector::SubPixel::None => Subpixel::None,
                    _ => Subpixel::Unknown,
                },
                make: "EWWM".into(),
                model: connector_name.clone(),
            },
        );

        // Calculate output position (lay out horizontally).
        let x_offset: i32 = state
            .space
            .outputs()
            .map(|o| {
                state
                    .space
                    .output_geometry(o)
                    .map(|g| g.loc.x + g.size.w)
                    .unwrap_or(0)
            })
            .max()
            .unwrap_or(0);

        output.change_current_state(
            Some(wl_mode),
            Some(Transform::Normal),
            None,
            Some((x_offset, 0).into()),
        );
        output.set_preferred(wl_mode);
        state.space.map_output(&output, (x_offset, 0));
        let mut output_config =
            crate::handlers::output_management::OutputConfig::new(connector_name.clone());
        output_config.enabled = true;
        output_config.x = x_offset;
        output_config.y = 0;
        output_config.width = drm_mode.size().0 as i32;
        output_config.height = drm_mode.size().1 as i32;
        output_config.refresh = (drm_mode.vrefresh() * 1000) as i32;
        state
            .output_management_state
            .upsert_detected_output(output_config);

        info!(
            name = %connector_name,
            mode = %format!("{}x{}@{}Hz",
                drm_mode.size().0, drm_mode.size().1, drm_mode.vrefresh()),
            crtc = ?crtc,
            "output configured"
        );

        let damage_tracker = OutputDamageTracker::from_output(&output);

        gpu.outputs.insert(
            crtc,
            OutputState {
                output,
                _crtc: crtc,
                surface,
                damage_tracker,
                pending_frame: false,
                render_scheduled: false,
            },
        );
    }

    #[cfg(feature = "full-backend")]
    state.set_drm_output_crtcs(gpu.node, used_crtcs.iter().copied());
}

/// Human-readable connector type name.
fn connector_type_name(interface: connector::Interface) -> &'static str {
    match interface {
        connector::Interface::VGA => "VGA",
        connector::Interface::DVII => "DVI-I",
        connector::Interface::DVID => "DVI-D",
        connector::Interface::DVIA => "DVI-A",
        connector::Interface::SVideo => "S-Video",
        connector::Interface::LVDS => "LVDS",
        connector::Interface::Component => "Component",
        connector::Interface::DisplayPort => "DP",
        connector::Interface::HDMIA => "HDMI-A",
        connector::Interface::HDMIB => "HDMI-B",
        // eDP variant name varies across drm crate versions
        connector::Interface::DSI => "DSI",
        connector::Interface::DPI => "DPI",
        _ => "Unknown",
    }
}

/// Remove a DRM device and its outputs.
fn device_removed(
    node: DrmNode,
    backend: &mut DrmBackendData,
    state: &mut EwwmState,
    loop_handle: &LoopHandle<'static, EwwmState>,
) {
    if let Some(gpu) = backend.devices.remove(&node) {
        info!(?node, "removing DRM device");

        // Unmap all outputs from the space.
        for (_crtc, output_state) in &gpu.outputs {
            let output_name = output_state.output.name();
            state
                .output_management_state
                .remove_detected_output(&output_name);
            state.space.unmap_output(&output_state.output);
        }

        // Remove DRM event source from calloop.
        loop_handle.remove(gpu.drm_token);

        if backend.primary_node == Some(node) {
            backend.primary_node = backend.devices.keys().next().copied();
            if let Some(new_primary) = backend.primary_node {
                info!(?new_primary, "primary GPU changed after removal");
            } else {
                warn!("no GPU devices remaining");
            }
        }

        #[cfg(feature = "full-backend")]
        state.unregister_drm_lease_device(node);
    }
}

// ---------------------------------------------------------------------------
// Frame rendering and page flip handling
// ---------------------------------------------------------------------------

/// Render a frame for the specified output and queue it for scanout.
fn render_output(gpu: &mut GpuDevice, crtc: crtc::Handle, state: &mut EwwmState) {
    let output_state = match gpu.outputs.get_mut(&crtc) {
        Some(s) => s,
        None => return,
    };

    // Don't render if a frame is already pending scanout.
    if output_state.pending_frame {
        output_state.render_scheduled = true;
        return;
    }

    let output = &output_state.output;
    let output_geometry = match state.space.output_geometry(output) {
        Some(g) => g,
        None => return,
    };
    let _size = output_geometry.size.to_physical(1);

    // Get next buffer from the swapchain.
    let (mut dmabuf, age) = match output_state.surface.next_buffer() {
        Ok(buf) => buf,
        Err(e) => {
            warn!(crtc = ?crtc, "failed to get next buffer: {}", e);
            return;
        }
    };

    // Bind the dmabuf as our render target.
    let mut framebuffer = match gpu.renderer.bind(&mut dmabuf) {
        Ok(fb) => fb,
        Err(e) => {
            warn!(crtc = ?crtc, "failed to bind dmabuf: {}", e);
            return;
        }
    };

    // Render frame content using the shared render pipeline.
    let damage = crate::render::render_drm(
        &mut gpu.renderer,
        &mut framebuffer,
        &mut output_state.damage_tracker,
        &mut state.space,
        output,
        age as usize,
    );

    // Must drop the framebuffer borrow before queuing.
    drop(framebuffer);

    // Queue the rendered buffer for scanout via page flip.
    // Pass damage rectangles so the kernel can optimize the flip.
    match output_state.surface.queue_buffer(None, damage, ()) {
        Ok(()) => {
            output_state.pending_frame = true;
            output_state.render_scheduled = false;
        }
        Err(e) => {
            warn!(crtc = ?crtc, "failed to queue buffer: {}", e);
        }
    }
}

/// Render every configured DRM output.
fn render_all_outputs(backend: &mut DrmBackendData, state: &mut EwwmState) {
    let nodes: Vec<DrmNode> = backend.devices.keys().copied().collect();
    for node in nodes {
        if let Some(gpu) = backend.devices.get_mut(&node) {
            let crtcs: Vec<crtc::Handle> = gpu.outputs.keys().copied().collect();
            for crtc in crtcs {
                render_output(gpu, crtc, state);
            }
        }
    }
}

/// Handle a vblank event from a calloop DRM source callback.
///
/// Since the calloop callback only receives `&mut EwwmState` (not the
/// DRM backend data), we record the vblank in thread-local storage for
/// the main loop to process via [`process_vblanks`].
fn handle_vblank(_state: &mut EwwmState, node: DrmNode, crtc: crtc::Handle) {
    debug!(?node, ?crtc, "vblank received");
    record_vblank(node, crtc);
}

// Thread-local storage for pending vblank events (node, crtc pairs).
// This bridges the calloop callback (which only has &mut EwwmState)
// with the main loop (which has access to DrmBackendData).
thread_local! {
    static PENDING_VBLANKS: std::cell::RefCell<Vec<(DrmNode, crtc::Handle)>> =
        std::cell::RefCell::new(Vec::new());
}

/// Record a vblank for later processing in the main loop.
fn record_vblank(node: DrmNode, crtc: crtc::Handle) {
    PENDING_VBLANKS.with(|v| v.borrow_mut().push((node, crtc)));
}

/// Drain and process pending vblank events.
fn process_vblanks(backend: &mut DrmBackendData, state: &mut EwwmState) {
    let vblanks: Vec<(DrmNode, crtc::Handle)> =
        PENDING_VBLANKS.with(|v| std::mem::take(&mut *v.borrow_mut()));

    for (node, crtc) in vblanks {
        if let Some(gpu) = backend.devices.get_mut(&node) {
            if let Some(output_state) = gpu.outputs.get_mut(&crtc) {
                // Mark frame as submitted, freeing the buffer.
                let _user_data = output_state.surface.frame_submitted();
                output_state.pending_frame = false;

                // If a render was scheduled while we were waiting,
                // kick off a new render now.
                if output_state.render_scheduled {
                    render_output(gpu, crtc, state);
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Session (VT switch) handling
// ---------------------------------------------------------------------------

/// Pause all DRM devices and libinput on VT switch away.
fn session_paused(backend: &mut DrmBackendData, state: &mut EwwmState) {
    info!("session paused (VT switch away)");

    #[cfg(feature = "full-backend")]
    if let Some(drm_lease_state) = state.drm_lease_state.as_mut() {
        drm_lease_state.suspend();
    }

    for (_node, gpu) in backend.devices.iter_mut() {
        gpu.drm.borrow_mut().pause();
        for (_crtc, output_state) in gpu.outputs.iter_mut() {
            output_state.pending_frame = false;
            output_state.render_scheduled = false;
        }
    }
}

/// Resume all DRM devices and libinput on VT switch back.
fn session_activated(backend: &mut DrmBackendData, state: &mut EwwmState) {
    info!("session activated (VT switch back)");
    for (_node, gpu) in backend.devices.iter_mut() {
        // Re-activate the DRM device (preserve connector state).
        if let Err(e) = gpu.drm.borrow_mut().activate(false) {
            error!("failed to reactivate DRM device: {}", e);
            continue;
        }

        // Reset buffer state on all surfaces and re-render.
        let crtcs: Vec<crtc::Handle> = gpu.outputs.keys().copied().collect();
        for crtc in crtcs {
            if let Some(output_state) = gpu.outputs.get_mut(&crtc) {
                output_state.surface.reset_buffers();
            }
            render_output(gpu, crtc, state);
        }
    }

    #[cfg(feature = "full-backend")]
    if let Some(drm_lease_state) = state.drm_lease_state.as_mut() {
        drm_lease_state.resume::<EwwmState>();
    }
}

// ---------------------------------------------------------------------------
// Main entry point
// ---------------------------------------------------------------------------

/// Run the compositor with the DRM backend.
///
/// This function blocks until the compositor shuts down (signal, IPC
/// command, or session loss). It returns `Ok(())` on clean shutdown.
pub fn run(
    socket_name: Option<String>,
    ipc_config: IpcConfig,
    compositor_config: CompositorConfig,
) -> anyhow::Result<()> {
    info!("DRM backend starting");

    // ── 1. Create calloop event loop and Wayland display ──────────────

    let mut event_loop = EventLoop::<EwwmState>::try_new()?;
    let mut display = Display::<EwwmState>::new()?;
    let mut state =
        EwwmState::new_with_config(&mut display, event_loop.handle(), compositor_config);

    // ── 2. Configure IPC ──────────────────────────────────────────────

    state.ipc_server.ipc_trace = ipc_config.trace;
    let ipc_path = ipc_config
        .socket_path
        .unwrap_or_else(|| ipc::IpcServer::default_socket_path());
    state.ipc_server.socket_path = ipc_path.clone();
    ipc::IpcServer::bind(&ipc_path, &event_loop.handle())?;

    // ── 3. Initialize libseat session ─────────────────────────────────

    let (session, session_notifier) = LibSeatSession::new().map_err(|e| {
        anyhow::anyhow!(
            "failed to initialize libseat session: {}. \
             Is seatd/logind running? Try: export LIBSEAT_BACKEND=builtin",
            e
        )
    })?;

    let seat_name = session.seat();
    info!(seat = %seat_name, "libseat session initialized");

    let mut backend_data = DrmBackendData {
        session,
        devices: HashMap::new(),
        primary_node: None,
    };

    // ── 4. Set up udev device monitoring ──────────────────────────────

    let udev_backend = UdevBackend::new(&seat_name)
        .map_err(|e| anyhow::anyhow!("failed to initialize udev backend: {}", e))?;

    // Process initially present devices.
    for (device_id, path) in udev_backend.device_list() {
        match DrmNode::from_dev_id(device_id) {
            Ok(node) => {
                if let Err(e) = device_added(
                    node,
                    &path,
                    &mut backend_data,
                    &mut state,
                    &event_loop.handle(),
                ) {
                    warn!(
                        device_id = device_id,
                        path = ?path,
                        "skipping device: {}", e
                    );
                }
            }
            Err(e) => {
                debug!(device_id = device_id, "not a DRM device: {}", e);
            }
        }
    }

    if backend_data.devices.is_empty() {
        return Err(anyhow::anyhow!(
            "no usable DRM devices found. Is a GPU available and are \
             permissions correct? Check /dev/dri/ and seatd."
        ));
    }

    info!(
        device_count = backend_data.devices.len(),
        output_count = backend_data
            .devices
            .values()
            .map(|g| g.outputs.len())
            .sum::<usize>(),
        "DRM devices initialized"
    );

    // ── 4a. Initialize VR (OpenXR) if available ──────────────────────
    //
    // Wire EGL context from the primary GPU to VrState::create_session()
    // so OpenXR can share the GL context for swapchain rendering.
    // Initialize VR (OpenXR) if available.
    // Wire EGL context from the primary GPU to VrState::create_session()
    // so OpenXR can share the GL context for swapchain rendering.
    #[cfg(feature = "vr")]
    {
        match state.vr_state.initialize() {
            Ok(true) => {
                if let Some(primary) = backend_data.primary_node {
                    if let Some(gpu) = backend_data.devices.get(&primary) {
                        // Extract raw EGL pointers from Smithay's EGL context.
                        //
                        // Smithay 0.7 API:
                        //   display() → &EGLDisplay
                        //   get_display_handle() → Arc<EGLDisplayHandle>
                        //     (deref → &ffi::egl::types::EGLDisplay = &*const c_void)
                        //   config_id() → ffi::egl::types::EGLConfig (*const c_void)
                        //   get_context_handle() → ffi::egl::types::EGLContext (*const c_void)
                        let egl_ctx = gpu.renderer.egl_context();
                        let display_handle = egl_ctx.display().get_display_handle();
                        // EGLDisplayHandle derefs to &ffi::egl::types::EGLDisplay
                        // which is &*const c_void. Deref twice to get the raw ptr.
                        let raw_display: *mut std::ffi::c_void =
                            **display_handle as *mut std::ffi::c_void;
                        let raw_config = egl_ctx.config_id() as *mut std::ffi::c_void;
                        let raw_context = egl_ctx.get_context_handle() as *mut std::ffi::c_void;
                        if let Err(e) = state.vr_state.create_session(
                            raw_display as *mut std::ffi::c_void,
                            raw_config,
                            raw_context,
                        ) {
                            warn!("VR: failed to create OpenXR session: {}", e);
                        }
                    }
                }
            }
            Ok(false) => {
                info!("VR: not available, continuing in 2D mode");
            }
            Err(e) => {
                warn!("VR: initialization failed: {}", e);
            }
        }
    }

    // ── 5. Set up libinput for input events ───────────────────────────

    let mut libinput_context = Libinput::new_with_udev::<LibinputSessionInterface<LibSeatSession>>(
        backend_data.session.clone().into(),
    );

    if let Err(e) = libinput_context.udev_assign_seat(&seat_name) {
        return Err(anyhow::anyhow!(
            "failed to assign seat '{}' to libinput: {:?}",
            seat_name,
            e
        ));
    }

    let libinput_backend = LibinputInputBackend::new(libinput_context.clone());

    event_loop
        .handle()
        .insert_source(libinput_backend, |event, _, state: &mut EwwmState| {
            crate::input::handle_input(state, event);
        })
        .map_err(|e| anyhow::anyhow!("failed to register libinput source: {}", e))?;

    info!("libinput initialized on seat '{}'", seat_name);

    // ── 6. Register session notifier for VT switching ─────────────────

    // We need the libinput context in the session callback for
    // suspend/resume. Clone it since it's Rc-based internally.
    let mut libinput_for_session = libinput_context.clone();

    event_loop
        .handle()
        .insert_source(session_notifier, move |event, _, _state: &mut EwwmState| {
            match event {
                SessionEvent::PauseSession => {
                    // Suspend libinput to release evdev fds.
                    libinput_for_session.suspend();
                    info!("session paused: libinput suspended");
                    // DRM pause/activate handled in main loop via flag.
                    SESSION_PAUSED.store(true, Ordering::SeqCst);
                }
                SessionEvent::ActivateSession => {
                    // Resume libinput.
                    if let Err(e) = libinput_for_session.resume() {
                        error!("failed to resume libinput: {:?}", e);
                    }
                    info!("session activated: libinput resumed");
                    SESSION_ACTIVATED.store(true, Ordering::SeqCst);
                }
            }
        })
        .map_err(|e| anyhow::anyhow!("failed to register session source: {}", e))?;

    // ── 7. Register udev hotplug monitoring ───────────────────────────

    event_loop
        .handle()
        .insert_source(
            udev_backend,
            move |event, _, _state: &mut EwwmState| match event {
                UdevEvent::Added { device_id, path } => {
                    info!(device_id, ?path, "udev: device added");
                    UDEV_EVENTS.with(|v| {
                        v.borrow_mut().push(UdevAction::Added { device_id, path });
                    });
                }
                UdevEvent::Changed { device_id } => {
                    debug!(device_id, "udev: device changed");
                    UDEV_EVENTS.with(|v| {
                        v.borrow_mut().push(UdevAction::Changed { device_id });
                    });
                }
                UdevEvent::Removed { device_id } => {
                    info!(device_id, "udev: device removed");
                    UDEV_EVENTS.with(|v| {
                        v.borrow_mut().push(UdevAction::Removed { device_id });
                    });
                }
            },
        )
        .map_err(|e| anyhow::anyhow!("failed to register udev source: {}", e))?;

    // ── 8. Set up Wayland socket ──────────────────────────────────────

    let xdg_runtime_dir = std::env::var("XDG_RUNTIME_DIR")
        .unwrap_or_else(|_| format!("/run/user/{}", unsafe { libc::getuid() }));
    let socket_name_str = socket_name.unwrap_or_else(|| "wayland-0".to_string());
    let socket_path = format!("{}/{}", xdg_runtime_dir, socket_name_str);
    let _ = std::fs::remove_file(&socket_path);
    let wayland_listener = std::os::unix::net::UnixListener::bind(&socket_path)?;
    wayland_listener.set_nonblocking(true)?;
    info!("Wayland socket: {}", socket_path);
    std::env::set_var("WAYLAND_DISPLAY", &socket_name_str);
    state.apply_real_session_startup_policy();

    // Accept Wayland client connections
    event_loop
        .handle()
        .insert_source(
            smithay::reexports::calloop::generic::Generic::new(
                wayland_listener,
                smithay::reexports::calloop::Interest::READ,
                smithay::reexports::calloop::Mode::Level,
            ),
            |_, source, state: &mut EwwmState| {
                match source.accept() {
                    Ok((stream, _)) => {
                        if let Err(e) = state.display_handle.insert_client(
                            stream,
                            std::sync::Arc::new(crate::state::ClientState::default()),
                        ) {
                            warn!("failed to insert Wayland client: {}", e);
                        }
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {}
                    Err(e) => {
                        warn!("Wayland socket accept error: {}", e);
                    }
                }
                Ok(smithay::reexports::calloop::PostAction::Continue)
            },
        )
        .map_err(|e| anyhow::anyhow!("failed to insert socket source: {:?}", e))?;

    // ── 9. Optional XWayland compatibility bridge ─────────────────────
    #[cfg(feature = "xwayland")]
    {
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
                event_loop
                    .handle()
                    .insert_source(
                        xwayland,
                        move |event, _, state: &mut EwwmState| match event {
                            XWaylandEvent::Ready {
                                x11_socket,
                                display_number,
                            } => {
                                info!(display_number, "XWayland ready");
                                match X11Wm::start_wm(
                                    state.loop_handle.clone(),
                                    x11_socket,
                                    client.clone(),
                                ) {
                                    Ok(wm) => {
                                        state.xwm = Some(wm);
                                        state.xdisplay = Some(display_number);
                                        std::env::set_var(
                                            "DISPLAY",
                                            format!(":{}", display_number),
                                        );
                                    }
                                    Err(e) => {
                                        error!("failed to start X11 WM: {}", e);
                                    }
                                }
                            }
                            XWaylandEvent::Error => {
                                warn!(
                                    "XWayland crashed on startup \
                                     (continuing without X11 support)"
                                );
                            }
                        },
                    )
                    .ok();
                info!("XWayland spawning");
            }
            Err(e) => {
                warn!(
                    "XWayland not available: {} \
                     (continuing without X11 support)",
                    e
                );
            }
        }
    }
    #[cfg(not(feature = "xwayland"))]
    {
        info!("XWayland compatibility feature disabled; not starting X11 bridge");
    }

    // ── 10. Wayland display dispatch ─────────────────────────────────
    // Instead of inserting a calloop source for the display poll fd
    // (which borrows `display` and conflicts with flush_clients),
    // we dispatch clients directly in the main loop.

    // ── 11. Signal handling for graceful shutdown ──────────────────────
    unsafe {
        libc::signal(libc::SIGTERM, drm_signal_handler as libc::sighandler_t);
        libc::signal(libc::SIGINT, drm_signal_handler as libc::sighandler_t);
    }

    // ── 12. Initial render for all outputs ────────────────────────────

    // Kick off the first frame on every output.
    render_all_outputs(&mut backend_data, &mut state);

    info!("DRM backend initialized, entering event loop");

    // ── 13. Main event loop ───────────────────────────────────────────

    while state.running {
        // Check global shutdown flag.
        if SHUTDOWN_REQUESTED.load(Ordering::SeqCst) {
            state.running = false;
            break;
        }

        // Handle session state transitions (VT switching).
        if SESSION_PAUSED.swap(false, Ordering::SeqCst) {
            session_paused(&mut backend_data, &mut state);
        }
        if SESSION_ACTIVATED.swap(false, Ordering::SeqCst) {
            session_activated(&mut backend_data, &mut state);
        }

        // Process pending vblank events from DRM.
        process_vblanks(&mut backend_data, &mut state);

        // Process udev hotplug events.
        process_udev_events(&mut backend_data, &mut state, &event_loop.handle());

        // Poll IPC clients.
        ipc::IpcServer::poll_clients(&mut state);

        // ── VR frame submission ─────────────────────────────────────
        // Poll OpenXR events, run the VR frame loop, and submit frames
        // to the headset via Monado.
        #[cfg(feature = "vr")]
        if state.vr_state.enabled {
            state.vr_state.poll_events();

            if let Some(frame_data) = state.vr_state.tick_frame() {
                if frame_data.should_render {
                    let view_configs = state.vr_state.view_config_views().to_vec();

                    // Render scene content into swapchain images (or
                    // fall back to black frames if renderer not ready).
                    state
                        .vr_state
                        .render_vr_frame(&frame_data.views, &view_configs);

                    // Build projection layers and submit frame.
                    let swapchain_count = state.vr_state.swapchain_images().len();
                    let view_count = frame_data.views.len();
                    if swapchain_count >= 2 && view_count >= 2 {
                        state.vr_state.submit_projection_frame(
                            &frame_data.views,
                            &view_configs,
                            frame_data.predicted_display_time,
                        );
                    }
                }
            }
        }

        // Dispatch calloop sources (DRM, libinput, session, udev, Wayland).
        event_loop.dispatch(Some(Duration::from_millis(1)), &mut state)?;

        // Dispatch and flush pending Wayland client events.
        display.dispatch_clients(&mut state)?;
        display.flush_clients()?;

        if state.take_render_request() {
            render_all_outputs(&mut backend_data, &mut state);
        }
    }

    // ── 14. Cleanup ───────────────────────────────────────────────────

    info!("DRM backend shutting down");

    // Remove IPC socket.
    let _ = std::fs::remove_file(&state.ipc_server.socket_path);

    // Unmap all outputs.
    for (_node, gpu) in backend_data.devices.iter_mut() {
        for (_crtc, output_state) in gpu.outputs.iter() {
            let output_name = output_state.output.name();
            state
                .output_management_state
                .remove_detected_output(&output_name);
            state.space.unmap_output(&output_state.output);
        }
    }

    // Drop GPU devices (releases DRM master, GBM buffers, EGL).
    backend_data.devices.clear();

    // Close the session.
    drop(backend_data.session);

    info!(
        "DRM backend shut down ({} surface(s), {} IPC client(s))",
        state.surfaces.len(),
        state.ipc_server.clients.len(),
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// Session state flags (atomic, set in calloop callbacks)
// ---------------------------------------------------------------------------

static SESSION_PAUSED: AtomicBool = AtomicBool::new(false);
static SESSION_ACTIVATED: AtomicBool = AtomicBool::new(false);

// ---------------------------------------------------------------------------
// Udev event bridge (calloop callback -> main loop)
// ---------------------------------------------------------------------------

/// Deferred udev action for the main loop to process.
enum UdevAction {
    Added { device_id: dev_t, path: PathBuf },
    Changed { device_id: dev_t },
    Removed { device_id: dev_t },
}

use libc::dev_t;

thread_local! {
    static UDEV_EVENTS: std::cell::RefCell<Vec<UdevAction>> =
        std::cell::RefCell::new(Vec::new());
}

/// Process queued udev hotplug events in the main loop.
fn process_udev_events(
    backend: &mut DrmBackendData,
    state: &mut EwwmState,
    loop_handle: &LoopHandle<'static, EwwmState>,
) {
    let events: Vec<UdevAction> = UDEV_EVENTS.with(|v| std::mem::take(&mut *v.borrow_mut()));

    for action in events {
        match action {
            UdevAction::Added { device_id, path } => match DrmNode::from_dev_id(device_id) {
                Ok(node) => {
                    if let Err(e) = device_added(node, &path, backend, state, loop_handle) {
                        warn!(
                            device_id = device_id,
                            "hotplug: failed to add device: {}", e
                        );
                    }
                }
                Err(e) => {
                    debug!(device_id, "hotplug: not a DRM device: {}", e);
                }
            },
            UdevAction::Changed { device_id } => {
                // Connector hotplug (display plugged/unplugged).
                if let Ok(node) = DrmNode::from_dev_id(device_id) {
                    let node = node
                        .node_with_type(NodeType::Render)
                        .and_then(|n| n.ok())
                        .unwrap_or(node);

                    if let Some(gpu) = backend.devices.get_mut(&node) {
                        info!(?node, "connector hotplug detected, rescanning");

                        // Unmap existing outputs.
                        for (_crtc, output_state) in gpu.outputs.drain() {
                            let output_name = output_state.output.name();
                            state
                                .output_management_state
                                .remove_detected_output(&output_name);
                            state.space.unmap_output(&output_state.output);
                        }

                        // Rescan connectors.
                        let renderer_formats =
                            gpu.renderer.egl_context().dmabuf_render_formats().clone();
                        scan_connectors(gpu, &renderer_formats, state);

                        // Render initial frames on new outputs.
                        let crtcs: Vec<crtc::Handle> = gpu.outputs.keys().copied().collect();
                        for crtc in crtcs {
                            render_output(gpu, crtc, state);
                        }
                    }
                }
            }
            UdevAction::Removed { device_id } => {
                if let Ok(node) = DrmNode::from_dev_id(device_id) {
                    device_removed(node, backend, state, loop_handle);
                }
            }
        }
    }
}
