//! Compositor state — the central struct holding all Smithay state.
//!
//! Follows niri pattern: single `EwwmState` struct owns everything,
//! passed as `&mut self` to all handler trait implementations.

#[cfg(feature = "full-backend")]
use smithay::{
    backend::drm::{DrmDevice, DrmNode},
    reexports::drm::control::crtc,
    wayland::drm_lease::{DrmLease, DrmLeaseState},
};
use smithay::{
    desktop::{PopupManager, Space, Window},
    input::{Seat, SeatState},
    reexports::{
        calloop::{generic::Generic, Interest, LoopHandle, Mode, PostAction},
        wayland_server::{
            backend::{ClientData, ClientId, DisconnectReason},
            protocol::wl_surface::WlSurface,
            Client, Display, DisplayHandle,
        },
    },
    utils::{Logical, Rectangle, Size, SERIAL_COUNTER},
    wayland::{
        compositor::{CompositorClientState, CompositorState},
        cursor_shape::CursorShapeManagerState,
        dmabuf::DmabufState,
        foreign_toplevel_list::ForeignToplevelListState,
        idle_inhibit::IdleInhibitManagerState,
        idle_notify::IdleNotifierState,
        output::OutputManagerState,
        pointer_constraints::PointerConstraintsState,
        selection::{data_device::DataDeviceState, primary_selection::PrimarySelectionState},
        session_lock::SessionLockManagerState,
        shell::{wlr_layer::WlrLayerShellState, xdg::XdgShellState},
        shm::ShmState,
        xdg_activation::XdgActivationState,
    },
};
#[cfg(feature = "xwayland")]
use smithay::{wayland::xwayland_shell::XWaylandShellState, xwayland::xwm::X11Wm};
#[cfg(feature = "full-backend")]
use std::{cell::RefCell, rc::Rc};
use std::{
    collections::{HashMap, HashSet},
    process::{Child, Command},
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
};
use tracing::{info, warn};

use crate::autotype::AutoTypeManager;
use crate::clock::{Clock, SystemClock};
use crate::config::CompositorConfig;
use crate::handlers::dpms::DpmsState;
use crate::handlers::output_management::OutputManagementState;
use crate::handlers::screencopy::ScreencopyState;
use crate::ipc::IpcServer;
use crate::secure_input::SecureInputState;
use crate::vr::VrState;

/// Monotonically increasing surface ID generator.
static NEXT_SURFACE_ID: AtomicU64 = AtomicU64::new(1);

/// Generate a unique surface ID.
pub fn next_surface_id() -> u64 {
    NEXT_SURFACE_ID.fetch_add(1, Ordering::Relaxed)
}

/// Tracked surface data for Emacs IPC reporting.
#[derive(Debug)]
pub struct SurfaceData {
    pub surface_id: u64,
    pub app_id: Option<String>,
    pub title: Option<String>,
    /// True if this surface comes from XWayland (X11 application).
    pub is_x11: bool,
    /// X11 WM_CLASS class name (only for XWayland surfaces).
    pub x11_class: Option<String>,
    /// X11 WM_CLASS instance name (only for XWayland surfaces).
    pub x11_instance: Option<String>,
    /// Workspace assignment (default 0).
    pub workspace: usize,
    /// Whether this surface is floating (vs tiled).
    pub floating: bool,
    /// Last compositor geometry, used to restore floating/manual surfaces after
    /// workspace visibility changes.
    pub geometry: Option<Rectangle<i32, Logical>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeAutostartOutcome {
    pub target: String,
    pub status: &'static str,
    pub detail: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NativeIdleStatus {
    pub state: &'static str,
    pub pid: Option<u32>,
    pub detail: String,
}

impl SurfaceData {
    pub fn new(surface_id: u64) -> Self {
        Self {
            surface_id,
            app_id: None,
            title: None,
            is_x11: false,
            x11_class: None,
            x11_instance: None,
            workspace: 0,
            floating: false,
            geometry: None,
        }
    }

    pub fn new_x11(surface_id: u64) -> Self {
        let mut data = Self::new(surface_id);
        data.is_x11 = true;
        data
    }
}

/// Usable output area after accounting for layer-shell exclusive zones.
#[derive(Debug, Clone, Copy)]
pub struct UsableArea {
    pub x: i32,
    pub y: i32,
    pub w: i32,
    pub h: i32,
}

impl Default for UsableArea {
    fn default() -> Self {
        Self {
            x: 0,
            y: 0,
            w: 1920,
            h: 1080,
        }
    }
}

/// Central compositor state.
pub struct EwwmState {
    // Native compositor config
    pub config: CompositorConfig,

    // Wayland core
    pub display_handle: DisplayHandle,
    pub loop_handle: LoopHandle<'static, Self>,

    // Protocol states
    pub compositor_state: CompositorState,
    pub xdg_shell_state: XdgShellState,
    pub shm_state: ShmState,
    pub output_state: OutputManagerState,
    pub seat_state: SeatState<Self>,
    pub data_device_state: DataDeviceState,

    // Layer shell
    pub layer_shell_state: WlrLayerShellState,

    // Session lock (ext-session-lock-v1)
    pub session_lock_state: SessionLockManagerState,
    pub session_locked: bool,

    // Idle notification + inhibit
    pub idle_notifier_state: IdleNotifierState<Self>,
    pub idle_inhibit_state: IdleInhibitManagerState,

    // Primary selection (zwp-primary-selection-v1)
    pub primary_selection_state: PrimarySelectionState,

    // DMA-BUF (linux-dmabuf-v1)
    pub dmabuf_state: DmabufState,

    // Cursor shape (wp-cursor-shape-v1)
    pub cursor_shape_state: CursorShapeManagerState,

    // XDG activation (xdg-activation-v1)
    pub xdg_activation_state: XdgActivationState,

    // Foreign toplevel management
    pub foreign_toplevel_state: ForeignToplevelListState,

    // XWayland
    #[cfg(feature = "xwayland")]
    pub xwm: Option<X11Wm>,
    #[cfg(feature = "xwayland")]
    pub xwayland_shell_state: XWaylandShellState,
    #[cfg(feature = "xwayland")]
    pub xdisplay: Option<u32>,

    // Popups
    pub popups: PopupManager,

    // Input
    pub seat: Seat<Self>,

    // Window management
    pub space: Space<Window>,
    pub surfaces: HashMap<u64, SurfaceData>,
    /// Maps surface_id → Window for O(1) dispatch lookups.
    pub surface_to_window: HashMap<u64, Window>,
    pub workspace_count: usize,
    pub active_workspace: usize,
    pub current_layout: String,
    pub layout_master_ratio: f32,

    // Output usable area (after layer-shell exclusive zones)
    pub usable_area: UsableArea,

    // IPC
    pub ipc_server: IpcServer,
    pub grabbed_keys: HashSet<String>,
    pub native_autostart_launched: HashSet<String>,
    pub native_idle_process: Option<Child>,

    // VR subsystem
    pub vr_state: VrState,

    // Auto-type (credential injection)
    pub autotype: AutoTypeManager,

    // Secure input mode
    pub secure_input: SecureInputState,

    // Headless backend state (for IPC queries)
    pub headless_active: bool,
    pub headless_output_count: u32,
    pub headless_width: i32,
    pub headless_height: i32,

    // DPMS output power state
    pub dpms_state: DpmsState,

    // Screencopy (wlr-screencopy-unstable-v1)
    pub screencopy_state: ScreencopyState,

    // Output management (wlr-output-management-unstable-v1)
    pub output_management_state: OutputManagementState,

    // Pointer constraints (pointer-constraints-unstable-v1)
    pub pointer_constraints_state: PointerConstraintsState,

    // DRM lease protocol (wp_drm_lease_v1)
    #[cfg(feature = "full-backend")]
    pub drm_lease_state: Option<DrmLeaseState>,
    #[cfg(feature = "full-backend")]
    pub active_drm_leases: Vec<DrmLease>,
    #[cfg(feature = "full-backend")]
    pub drm_lease_devices: HashMap<DrmNode, Rc<RefCell<DrmDevice>>>,
    #[cfg(feature = "full-backend")]
    pub drm_output_crtcs: HashMap<DrmNode, HashSet<crtc::Handle>>,

    // Focus tracking
    pub focused_surface: Option<u64>,

    // Cursor image status
    pub cursor_status: CursorImageStatus,

    // Shutdown flag
    pub running: bool,

    // Clock (real or test)
    pub clock: Arc<dyn Clock>,
}

/// Simplified cursor image status for tracking.
#[derive(Debug, Clone, PartialEq)]
pub enum CursorImageStatus {
    /// Default cursor (compositor-provided).
    Default,
    /// Client-set cursor surface (tracked by Smithay).
    Surface,
    /// Cursor is hidden.
    Hidden,
}

impl EwwmState {
    pub fn new(display: &mut Display<Self>, loop_handle: LoopHandle<'static, Self>) -> Self {
        Self::new_with_config(display, loop_handle, CompositorConfig::default())
    }

    pub fn new_with_config(
        display: &mut Display<Self>,
        loop_handle: LoopHandle<'static, Self>,
        config: CompositorConfig,
    ) -> Self {
        let display_handle = display.handle();

        let compositor_state = CompositorState::new::<Self>(&display_handle);
        let xdg_shell_state = XdgShellState::new::<Self>(&display_handle);
        let shm_state = ShmState::new::<Self>(&display_handle, vec![]);
        let output_state = OutputManagerState::new_with_xdg_output::<Self>(&display_handle);
        let mut seat_state = SeatState::new();
        let data_device_state = DataDeviceState::new::<Self>(&display_handle);

        // Layer shell protocol
        let layer_shell_state = WlrLayerShellState::new::<Self>(&display_handle);

        // Session lock protocol (ext-session-lock-v1)
        let session_lock_state = SessionLockManagerState::new::<Self, _>(&display_handle, |_| true);

        // Idle notification (ext-idle-notify-v1)
        let idle_notifier_state = IdleNotifierState::new(&display_handle, loop_handle.clone());

        // Idle inhibit (zwp-idle-inhibit-v1)
        let idle_inhibit_state = IdleInhibitManagerState::new::<Self>(&display_handle);

        // Primary selection (zwp-primary-selection-v1)
        let primary_selection_state = PrimarySelectionState::new::<Self>(&display_handle);

        // DMA-BUF (linux-dmabuf-v1) — state only, global created when renderer is available
        let dmabuf_state = DmabufState::new();

        // Cursor shape (wp-cursor-shape-v1)
        let cursor_shape_state = CursorShapeManagerState::new::<Self>(&display_handle);

        // XDG activation (xdg-activation-v1)
        let xdg_activation_state = XdgActivationState::new::<Self>(&display_handle);

        // Foreign toplevel list protocol
        let foreign_toplevel_state = ForeignToplevelListState::new::<Self>(&display_handle);

        // XWayland shell protocol (for surface serial matching)
        #[cfg(feature = "xwayland")]
        let xwayland_shell_state = XWaylandShellState::new::<Self>(&display_handle);

        // Pointer constraints (pointer-constraints-unstable-v1)
        let pointer_constraints_state = PointerConstraintsState::new::<Self>(&display_handle);

        let seat = seat_state.new_wl_seat(&display_handle, "ewwm-seat");

        info!(
            "EwwmState initialized (layer-shell, foreign-toplevel, session-lock, \
             idle-notify, idle-inhibit, primary-selection, dmabuf, cursor-shape, \
             xdg-activation, pointer-constraints{} )",
            if cfg!(feature = "xwayland") {
                ", xwayland-shell"
            } else {
                ""
            }
        );

        let ipc_socket_path = IpcServer::default_socket_path();

        let workspace_count = config.normalized_workspace_count();
        let active_workspace = config.normalized_active_workspace();
        let current_layout = config.normalized_layout_default();
        let layout_master_ratio = config.normalized_layout_master_ratio();
        let mut vr_state = VrState::new();
        apply_native_vr_config(&mut vr_state, &config);

        Self {
            config,
            display_handle,
            loop_handle,
            compositor_state,
            xdg_shell_state,
            shm_state,
            output_state,
            seat_state,
            data_device_state,
            layer_shell_state,
            session_lock_state,
            session_locked: false,
            idle_notifier_state,
            idle_inhibit_state,
            primary_selection_state,
            dmabuf_state,
            cursor_shape_state,
            xdg_activation_state,
            foreign_toplevel_state,
            #[cfg(feature = "xwayland")]
            xwm: None,
            #[cfg(feature = "xwayland")]
            xwayland_shell_state,
            #[cfg(feature = "xwayland")]
            xdisplay: None,
            popups: PopupManager::default(),
            seat,
            space: Space::default(),
            surfaces: HashMap::new(),
            surface_to_window: HashMap::new(),
            workspace_count,
            active_workspace,
            current_layout,
            layout_master_ratio,
            usable_area: UsableArea::default(),
            ipc_server: IpcServer::new(ipc_socket_path),
            grabbed_keys: HashSet::new(),
            native_autostart_launched: HashSet::new(),
            native_idle_process: None,
            vr_state,
            autotype: AutoTypeManager::new(),
            secure_input: SecureInputState::new(),
            headless_active: false,
            headless_output_count: 0,
            headless_width: 1920,
            headless_height: 1080,
            dpms_state: DpmsState::default(),
            screencopy_state: ScreencopyState::new(),
            output_management_state: OutputManagementState::new(),
            pointer_constraints_state,
            #[cfg(feature = "full-backend")]
            drm_lease_state: None,
            #[cfg(feature = "full-backend")]
            active_drm_leases: Vec::new(),
            #[cfg(feature = "full-backend")]
            drm_lease_devices: HashMap::new(),
            #[cfg(feature = "full-backend")]
            drm_output_crtcs: HashMap::new(),
            focused_surface: None,
            cursor_status: CursorImageStatus::Default,
            running: true,
            clock: Arc::new(SystemClock),
        }
    }
}

fn apply_native_vr_config(vr_state: &mut VrState, config: &CompositorConfig) {
    if config.gaze_zone_layout == "custom" {
        if config.gaze_zone_custom_map.trim().is_empty() {
            warn!("config: gaze_zone_layout=custom without gaze_zone_custom_map; using defaults");
            return;
        }

        match crate::vr::gaze_zone::parse_zone_layout_map(&config.gaze_zone_custom_map)
            .and_then(|entries| vr_state.zone_detector.set_custom_layout(entries))
        {
            Ok(()) => {}
            Err(err) => warn!(?err, "config: invalid gaze_zone_custom_map; using defaults"),
        }
        return;
    }

    if let Err(err) = vr_state
        .zone_detector
        .set_layout_preset(&config.gaze_zone_layout)
    {
        warn!(?err, "config: invalid gaze_zone_layout; using defaults");
    }
}

impl EwwmState {
    #[cfg(feature = "full-backend")]
    pub fn ensure_drm_lease_state(&mut self, node: DrmNode) {
        if self.drm_lease_state.is_some() {
            return;
        }

        match DrmLeaseState::new::<Self>(&self.display_handle, &node) {
            Ok(state) => {
                info!(?node, "initialized wp_drm_lease_v1 global");
                self.drm_lease_state = Some(state);
            }
            Err(err) => {
                info!(?node, ?err, "wp_drm_lease_v1 unavailable on this DRM node");
            }
        }
    }

    #[cfg(feature = "full-backend")]
    pub fn register_drm_lease_device(&mut self, node: DrmNode, drm: Rc<RefCell<DrmDevice>>) {
        self.drm_lease_devices.insert(node, drm);
    }

    #[cfg(feature = "full-backend")]
    pub fn unregister_drm_lease_device(&mut self, node: DrmNode) {
        self.drm_lease_devices.remove(&node);
        self.drm_output_crtcs.remove(&node);
    }

    #[cfg(feature = "full-backend")]
    pub fn set_drm_output_crtcs<I>(&mut self, node: DrmNode, crtcs: I)
    where
        I: IntoIterator<Item = crtc::Handle>,
    {
        self.drm_output_crtcs
            .insert(node, crtcs.into_iter().collect());
    }

    /// Look up a Window by its surface_id.
    pub fn find_window(&self, surface_id: u64) -> Option<&Window> {
        self.surface_to_window.get(&surface_id)
    }

    /// Initial visible rectangle for newly mapped application windows.
    pub fn initial_window_geometry(&self) -> Rectangle<i32, Logical> {
        let area = self.usable_area;
        let width = if area.w > 0 { area.w } else { 1920 };
        let height = if area.h > 0 { area.h } else { 1080 };
        Rectangle::new((area.x, area.y).into(), (width, height).into())
    }

    /// Reflow non-floating surfaces on the active workspace using native policy.
    pub fn apply_native_layout(&mut self) -> usize {
        self.apply_workspace_visibility();

        let windows = self.active_tiled_windows();
        if windows.is_empty() || self.current_layout == "floating" {
            return 0;
        }

        let geometries = self.native_layout_geometries(windows.len());
        let applied = geometries.len();
        for ((surface_id, window), geometry) in windows.into_iter().zip(geometries.into_iter()) {
            self.apply_window_geometry(surface_id, window, geometry);
        }

        self.space.refresh();
        applied
    }

    /// Keep compositor visibility aligned with native workspace membership.
    pub fn apply_workspace_visibility(&mut self) {
        let ids = self.sorted_surface_ids();
        for surface_id in ids {
            let Some(window) = self.surface_to_window.get(&surface_id).cloned() else {
                continue;
            };

            if self.surface_is_on_active_workspace(surface_id) {
                if self.space.element_geometry(&window).is_none() {
                    let geometry = self
                        .surfaces
                        .get(&surface_id)
                        .and_then(|data| data.geometry)
                        .unwrap_or_else(|| self.initial_window_geometry());
                    self.apply_window_geometry(surface_id, window, geometry);
                }
                continue;
            }

            if let Some(geometry) = self.space.element_geometry(&window) {
                if let Some(data) = self.surfaces.get_mut(&surface_id) {
                    data.geometry = Some(geometry);
                }
                self.space.unmap_elem(&window);
            }
        }
        self.space.refresh();
    }

    fn sorted_surface_ids(&self) -> Vec<u64> {
        let mut ids = self.surfaces.keys().copied().collect::<Vec<_>>();
        ids.sort_unstable();
        ids
    }

    fn surface_is_on_active_workspace(&self, surface_id: u64) -> bool {
        self.surfaces
            .get(&surface_id)
            .map(|data| data.workspace == self.active_workspace)
            .unwrap_or(false)
    }

    fn active_tiled_windows(&self) -> Vec<(u64, Window)> {
        self.sorted_surface_ids()
            .into_iter()
            .filter(|id| {
                self.surfaces
                    .get(id)
                    .map(|data| data.workspace == self.active_workspace && !data.floating)
                    .unwrap_or(false)
            })
            .filter_map(|id| {
                self.surface_to_window
                    .get(&id)
                    .cloned()
                    .map(|window| (id, window))
            })
            .collect()
    }

    fn native_layout_geometries(&self, count: usize) -> Vec<Rectangle<i32, Logical>> {
        let area = self.initial_window_geometry();
        match self.current_layout.as_str() {
            "monocle" => vec![area; count],
            "grid" => grid_layout(area, count),
            "tiling" => tiling_layout(area, count, self.layout_master_ratio),
            _ => tiling_layout(area, count, self.layout_master_ratio),
        }
    }

    fn apply_window_geometry(
        &mut self,
        _surface_id: u64,
        window: Window,
        geometry: Rectangle<i32, Logical>,
    ) {
        self.space
            .map_element(window.clone(), (geometry.loc.x, geometry.loc.y), false);
        if let Some(data) = self.surfaces.get_mut(&_surface_id) {
            data.geometry = Some(geometry);
        }

        if let Some(toplevel) = window.toplevel() {
            toplevel.with_pending_state(|state| {
                state.size = Some(Size::from((geometry.size.w, geometry.size.h)));
            });
            toplevel.send_pending_configure();
        }

        #[cfg(feature = "xwayland")]
        if let Some(surface) = window.x11_surface() {
            if let Err(err) = surface.configure(Some(geometry)) {
                warn!(
                    surface_id = _surface_id,
                    ?err,
                    "native layout XWayland configure failed"
                );
            }
        }
    }

    /// Apply native compositor manage policy to a newly known surface.
    pub fn apply_native_manage_policy(&self, data: &mut SurfaceData, transient: bool) {
        let candidates = [
            data.app_id.as_deref(),
            data.x11_class.as_deref(),
            data.x11_instance.as_deref(),
        ];

        if let Some(workspace) = self.config.workspace_for_app_candidates(&candidates) {
            if workspace < self.workspace_count {
                data.workspace = workspace;
            } else {
                warn!(
                    surface_id = data.surface_id,
                    workspace,
                    workspace_count = self.workspace_count,
                    "ignoring native manage rule outside configured workspace range"
                );
            }
        }

        if transient || self.config.should_float_app_candidates(&candidates) {
            data.floating = true;
        }
    }

    pub fn focus_surface(&mut self, surface_id: u64) -> Result<(), String> {
        if !self.surfaces.contains_key(&surface_id) {
            return Err(format!("unknown surface: {surface_id}"));
        }

        let window = self.find_window(surface_id).cloned();
        if let Some(window) = window {
            self.space.raise_element(&window, true);
            let keyboard = self.seat.get_keyboard().unwrap();
            let serial = SERIAL_COUNTER.next_serial();
            let wl_surface = window.toplevel().map(|t| t.wl_surface().clone());
            if let Some(surface) = wl_surface {
                keyboard.set_focus(self, Some(surface), serial);
            }
        }

        Ok(())
    }

    pub fn focus_adjacent_surface(&mut self, direction: FocusDirection) -> Option<u64> {
        let mut ids = self
            .surfaces
            .iter()
            .filter_map(|(id, data)| (data.workspace == self.active_workspace).then_some(*id))
            .collect::<Vec<_>>();
        ids.sort_unstable();

        if ids.is_empty() {
            return None;
        }

        let current = self
            .focused_surface
            .and_then(|focused| ids.iter().position(|id| *id == focused));
        let next_index = match (direction, current) {
            (FocusDirection::Next, Some(index)) => (index + 1) % ids.len(),
            (FocusDirection::Previous, Some(0)) => ids.len() - 1,
            (FocusDirection::Previous, Some(index)) => index - 1,
            (_, None) => 0,
        };
        let surface_id = ids[next_index];

        if self.focus_surface(surface_id).is_ok() {
            Some(surface_id)
        } else {
            None
        }
    }

    pub fn reload_native_config(&mut self) -> Result<String, String> {
        let (config, source) = CompositorConfig::load_default_path_strict()?;
        self.apply_runtime_config(config);
        Ok(source)
    }

    pub fn launch_configured_app(&self, name: &str) -> Result<String, String> {
        let command = self
            .config
            .app_launch_command(name)
            .ok_or_else(|| format!("unknown app launch target: {name}"))?
            .to_string();

        Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch {name}: {err}"))?;
        Ok(format!("launch:{name}"))
    }

    pub fn launch_session_lock(&self) -> Result<String, String> {
        let command = self
            .config
            .session_lock_command()
            .ok_or_else(|| "session lock command is not configured".to_string())?
            .to_string();

        Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch session lock: {err}"))?;
        Ok("session-lock".to_string())
    }

    pub fn run_native_autostart(&mut self, force: bool) -> Vec<NativeAutostartOutcome> {
        let targets = self.config.configured_autostart_targets().to_vec();
        let mut outcomes = Vec::with_capacity(targets.len());

        for target in targets {
            if !force && self.native_autostart_launched.contains(&target) {
                outcomes.push(NativeAutostartOutcome {
                    target,
                    status: "skipped",
                    detail: "already-launched".to_string(),
                });
                continue;
            }

            match self.launch_configured_app(&target) {
                Ok(detail) => {
                    self.native_autostart_launched.insert(target.clone());
                    outcomes.push(NativeAutostartOutcome {
                        target,
                        status: "launched",
                        detail,
                    });
                }
                Err(detail) => {
                    outcomes.push(NativeAutostartOutcome {
                        target,
                        status: "error",
                        detail,
                    });
                }
            }
        }

        outcomes
    }

    pub fn run_startup_autostart(&mut self) {
        if !self.config.autostart_enabled {
            return;
        }

        let outcomes = self.run_native_autostart(false);
        if outcomes.is_empty() {
            info!("native autostart enabled with no configured targets");
            return;
        }

        for outcome in outcomes {
            match outcome.status {
                "launched" => info!(
                    target = %outcome.target,
                    detail = %outcome.detail,
                    "native autostart launched configured target"
                ),
                "skipped" => info!(
                    target = %outcome.target,
                    detail = %outcome.detail,
                    "native autostart skipped configured target"
                ),
                _ => warn!(
                    target = %outcome.target,
                    detail = %outcome.detail,
                    "native autostart target failed"
                ),
            }
        }
    }

    pub fn native_idle_status(&mut self) -> NativeIdleStatus {
        let Some(child) = self.native_idle_process.as_mut() else {
            return NativeIdleStatus {
                state: "stopped",
                pid: None,
                detail: "not-running".to_string(),
            };
        };

        match child.try_wait() {
            Ok(Some(status)) => {
                let detail = format!("exited:{status}");
                self.native_idle_process = None;
                NativeIdleStatus {
                    state: "stopped",
                    pid: None,
                    detail,
                }
            }
            Ok(None) => NativeIdleStatus {
                state: "running",
                pid: Some(child.id()),
                detail: "running".to_string(),
            },
            Err(err) => {
                let detail = format!("status-error:{err}");
                self.native_idle_process = None;
                NativeIdleStatus {
                    state: "error",
                    pid: None,
                    detail,
                }
            }
        }
    }

    pub fn start_native_idle(&mut self) -> Result<NativeIdleStatus, String> {
        let current = self.native_idle_status();
        if current.state == "running" {
            return Ok(current);
        }

        let command = self
            .config
            .session_idle_command()
            .ok_or_else(|| "session idle command is not configured".to_string())?
            .to_string();

        let child = Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch session idle daemon: {err}"))?;
        let pid = child.id();
        self.native_idle_process = Some(child);
        Ok(NativeIdleStatus {
            state: "running",
            pid: Some(pid),
            detail: "started".to_string(),
        })
    }

    pub fn stop_native_idle(&mut self) -> Result<NativeIdleStatus, String> {
        let Some(mut child) = self.native_idle_process.take() else {
            return Ok(NativeIdleStatus {
                state: "stopped",
                pid: None,
                detail: "not-running".to_string(),
            });
        };

        let pid = child.id();
        if let Err(err) = child.kill() {
            return Err(format!("failed to stop session idle daemon: {err}"));
        }
        let detail = match child.wait() {
            Ok(status) => format!("stopped:{status}"),
            Err(err) => format!("stopped-wait-error:{err}"),
        };
        Ok(NativeIdleStatus {
            state: "stopped",
            pid: Some(pid),
            detail,
        })
    }

    pub fn run_startup_idle(&mut self) {
        if !self.config.session_idle_enabled {
            return;
        }

        match self.start_native_idle() {
            Ok(status) => info!(
                state = status.state,
                pid = status.pid,
                detail = %status.detail,
                "native session idle daemon started"
            ),
            Err(reason) => warn!(%reason, "native session idle daemon failed to start"),
        }
    }

    pub fn apply_runtime_config(&mut self, config: CompositorConfig) {
        let workspace_count = config.normalized_workspace_count();
        let active_workspace = config.normalized_active_workspace();
        self.native_autostart_launched
            .retain(|target| config.configured_autostart_targets().contains(target));
        self.active_workspace = active_workspace;
        self.workspace_count = workspace_count;
        self.current_layout = config.normalized_layout_default();
        self.layout_master_ratio = config.normalized_layout_master_ratio();
        apply_native_vr_config(&mut self.vr_state, &config);
        self.config = config;
        self.apply_native_layout();
    }

    /// Find the surface_id for a given WlSurface.
    pub fn surface_id_for_wl_surface(&self, wl_surface: &WlSurface) -> Option<u64> {
        self.surface_to_window.iter().find_map(|(id, window)| {
            if window
                .toplevel()
                .map(|t| *t.wl_surface() == *wl_surface)
                .unwrap_or(false)
            {
                return Some(*id);
            }

            #[cfg(feature = "xwayland")]
            if window
                .x11_surface()
                .and_then(|surface| surface.wl_surface())
                .map(|surface| surface == *wl_surface)
                .unwrap_or(false)
            {
                return Some(*id);
            }

            None
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FocusDirection {
    Next,
    Previous,
}

fn tiling_layout(
    area: Rectangle<i32, Logical>,
    count: usize,
    master_ratio: f32,
) -> Vec<Rectangle<i32, Logical>> {
    if count == 0 {
        return Vec::new();
    }
    if count == 1 {
        return vec![area];
    }

    let x = area.loc.x;
    let y = area.loc.y;
    let width = area.size.w.max(1);
    let height = area.size.h.max(1);
    let ratio = master_ratio.clamp(0.1, 0.9);
    let master_width = ((width as f32 * ratio).round() as i32).clamp(1, width.saturating_sub(1));
    let stack_width = (width - master_width).max(1);
    let stack_count = count - 1;

    let mut geometries = Vec::with_capacity(count);
    geometries.push(Rectangle::new((x, y).into(), (master_width, height).into()));

    for index in 0..stack_count {
        let top = y + ((index as i32 * height) / stack_count as i32);
        let bottom = y + (((index + 1) as i32 * height) / stack_count as i32);
        let row_height = (bottom - top).max(1);
        geometries.push(Rectangle::new(
            (x + master_width, top).into(),
            (stack_width, row_height).into(),
        ));
    }

    geometries
}

fn grid_layout(area: Rectangle<i32, Logical>, count: usize) -> Vec<Rectangle<i32, Logical>> {
    if count == 0 {
        return Vec::new();
    }

    let x = area.loc.x;
    let y = area.loc.y;
    let width = area.size.w.max(1);
    let height = area.size.h.max(1);
    let columns = (count as f64).sqrt().ceil().max(1.0) as i32;
    let rows = ((count as i32 + columns - 1) / columns).max(1);

    (0..count)
        .map(|index| {
            let index = index as i32;
            let column = index % columns;
            let row = index / columns;
            let left = x + ((column * width) / columns);
            let right = x + (((column + 1) * width) / columns);
            let top = y + ((row * height) / rows);
            let bottom = y + (((row + 1) * height) / rows);
            Rectangle::new(
                (left, top).into(),
                ((right - left).max(1), (bottom - top).max(1)).into(),
            )
        })
        .collect()
}

/// Per-client state required by Smithay's CompositorHandler.
#[derive(Default)]
pub struct ClientState {
    pub compositor_state: CompositorClientState,
}

impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}
    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use smithay::reexports::calloop::EventLoop;

    fn test_state_with_config(config: CompositorConfig) -> EwwmState {
        let event_loop = EventLoop::<EwwmState>::try_new().unwrap();
        let mut display = Display::<EwwmState>::new().unwrap();
        EwwmState::new_with_config(&mut display, event_loop.handle(), config)
    }

    #[test]
    fn default_workspace_state_comes_from_native_config() {
        let state = test_state_with_config(CompositorConfig::default());
        assert_eq!(state.workspace_count, 4);
        assert_eq!(state.active_workspace, 0);
    }

    #[test]
    fn configured_workspace_state_comes_from_native_config() {
        let mut config = CompositorConfig::default();
        config.workspace_count = 7;
        config.active_workspace = 3;

        let state = test_state_with_config(config);
        assert_eq!(state.workspace_count, 7);
        assert_eq!(state.active_workspace, 3);
    }

    #[test]
    fn active_workspace_is_clamped_to_native_workspace_count() {
        let mut config = CompositorConfig::default();
        config.workspace_count = 3;
        config.active_workspace = 99;

        let state = test_state_with_config(config);
        assert_eq!(state.workspace_count, 3);
        assert_eq!(state.active_workspace, 2);
    }

    #[test]
    fn tiling_layout_places_master_and_stack_deterministically() {
        let area = Rectangle::new((0, 0).into(), (1000, 600).into());
        let geometries = tiling_layout(area, 3, 0.6);

        assert_eq!(geometries.len(), 3);
        assert_eq!(geometries[0].loc.x, 0);
        assert_eq!(geometries[0].loc.y, 0);
        assert_eq!(geometries[0].size.w, 600);
        assert_eq!(geometries[0].size.h, 600);
        assert_eq!(geometries[1].loc.x, 600);
        assert_eq!(geometries[1].loc.y, 0);
        assert_eq!(geometries[1].size.w, 400);
        assert_eq!(geometries[1].size.h, 300);
        assert_eq!(geometries[2].loc.x, 600);
        assert_eq!(geometries[2].loc.y, 300);
        assert_eq!(geometries[2].size.w, 400);
        assert_eq!(geometries[2].size.h, 300);
    }

    #[test]
    fn grid_layout_places_surfaces_in_stable_cells() {
        let area = Rectangle::new((10, 20).into(), (900, 600).into());
        let geometries = grid_layout(area, 4);

        assert_eq!(geometries.len(), 4);
        assert_eq!(
            geometries[0],
            Rectangle::new((10, 20).into(), (450, 300).into())
        );
        assert_eq!(
            geometries[1],
            Rectangle::new((460, 20).into(), (450, 300).into())
        );
        assert_eq!(
            geometries[2],
            Rectangle::new((10, 320).into(), (450, 300).into())
        );
        assert_eq!(
            geometries[3],
            Rectangle::new((460, 320).into(), (450, 300).into())
        );
    }
}
