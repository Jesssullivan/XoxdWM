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
    utils::{Logical, Rectangle},
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
use tracing::info;

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
    /// Last native compositor geometry for remapping/reflow.
    pub geometry: Rectangle<i32, Logical>,
    /// Whether this surface is currently mapped into the compositor space.
    pub visible: bool,
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
            geometry: Rectangle::new((0, 0).into(), (800, 600).into()),
            visible: true,
        }
    }

    pub fn new_x11(surface_id: u64) -> Self {
        let mut data = Self::new(surface_id);
        data.is_x11 = true;
        data
    }

    pub fn protocol(&self) -> &'static str {
        if self.is_x11 {
            "xwayland"
        } else {
            "wayland"
        }
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

    // Output usable area (after layer-shell exclusive zones)
    pub usable_area: UsableArea,

    // IPC
    pub ipc_server: IpcServer,
    pub grabbed_keys: HashSet<String>,
    pub autostart_launched: HashSet<String>,
    pub session_lock_child: Option<Child>,
    pub session_idle_child: Option<Child>,

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

        let mut seat = seat_state.new_wl_seat(&display_handle, "ewwm-seat");
        seat.add_keyboard(Default::default(), 200, 25)
            .expect("failed to initialize keyboard capability");
        seat.add_pointer();

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
        let current_layout = config.layout_mode.clone();

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
            usable_area: UsableArea::default(),
            ipc_server: IpcServer::new(ipc_socket_path),
            grabbed_keys: HashSet::new(),
            autostart_launched: HashSet::new(),
            session_lock_child: None,
            session_idle_child: None,
            vr_state: VrState::new(),
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

    pub fn set_surface_geometry(&mut self, surface_id: u64, geometry: Rectangle<i32, Logical>) {
        if let Some(data) = self.surfaces.get_mut(&surface_id) {
            data.geometry = geometry;
        }

        if let Some(window) = self.surface_to_window.get(&surface_id).cloned() {
            self.space
                .map_element(window.clone(), (geometry.loc.x, geometry.loc.y), false);
            if let Some(toplevel) = window.toplevel() {
                toplevel.with_pending_state(|state| {
                    state.size = Some((geometry.size.w, geometry.size.h).into());
                });
                toplevel.send_pending_configure();
            }
            #[cfg(feature = "xwayland")]
            if let Some(x11) = window.x11_surface() {
                let _ = x11.configure(Some(geometry));
            }
        }
    }

    pub fn apply_workspace_visibility(&mut self) {
        let surface_ids: Vec<u64> = self.surfaces.keys().copied().collect();
        for surface_id in surface_ids {
            let should_show = self
                .surfaces
                .get(&surface_id)
                .map(|data| data.workspace == self.active_workspace)
                .unwrap_or(false);
            let window = self.surface_to_window.get(&surface_id).cloned();

            if should_show {
                let geometry = self
                    .surfaces
                    .get(&surface_id)
                    .map(|data| data.geometry)
                    .unwrap_or_else(|| self.initial_window_geometry());
                if let Some(window) = window {
                    self.space
                        .map_element(window, (geometry.loc.x, geometry.loc.y), false);
                }
                if let Some(data) = self.surfaces.get_mut(&surface_id) {
                    data.visible = true;
                }
            } else {
                if let Some(window) = window {
                    self.space.unmap_elem(&window);
                }
                if let Some(data) = self.surfaces.get_mut(&surface_id) {
                    data.visible = false;
                }
            }
        }
    }

    pub fn reflow_native_layout(&mut self) {
        self.apply_workspace_visibility();
        let mut tiled: Vec<u64> = self
            .surfaces
            .iter()
            .filter_map(|(id, data)| {
                (data.workspace == self.active_workspace && !data.floating).then_some(*id)
            })
            .collect();
        tiled.sort_unstable();
        if tiled.is_empty() {
            return;
        }

        let rects = self.layout_rects(tiled.len());
        for (surface_id, geometry) in tiled.into_iter().zip(rects) {
            self.set_surface_geometry(surface_id, geometry);
        }
    }

    fn layout_rects(&self, count: usize) -> Vec<Rectangle<i32, Logical>> {
        let area = self.usable_area;
        let x = area.x;
        let y = area.y;
        let w = if area.w > 0 { area.w } else { 1920 };
        let h = if area.h > 0 { area.h } else { 1080 };

        match self.current_layout.as_str() {
            "monocle" => (0..count)
                .map(|_| Rectangle::new((x, y).into(), (w, h).into()))
                .collect(),
            "grid" => {
                let cols = (count as f64).sqrt().ceil() as i32;
                let rows = ((count as f64) / cols as f64).ceil() as i32;
                let cell_w = (w / cols.max(1)).max(1);
                let cell_h = (h / rows.max(1)).max(1);
                (0..count)
                    .map(|index| {
                        let index = index as i32;
                        let col = index % cols;
                        let row = index / cols;
                        Rectangle::new(
                            (x + col * cell_w, y + row * cell_h).into(),
                            (cell_w, cell_h).into(),
                        )
                    })
                    .collect()
            }
            _ if count == 1 => vec![Rectangle::new((x, y).into(), (w, h).into())],
            _ => {
                let master_w = ((w as f32) * 0.55).round() as i32;
                let stack_w = (w - master_w).max(1);
                let stack_count = (count as i32 - 1).max(1);
                let stack_h = (h / stack_count).max(1);
                let mut rects = Vec::with_capacity(count);
                rects.push(Rectangle::new((x, y).into(), (master_w, h).into()));
                for index in 1..count {
                    let row = index as i32 - 1;
                    rects.push(Rectangle::new(
                        (x + master_w, y + row * stack_h).into(),
                        (stack_w, stack_h).into(),
                    ));
                }
                rects
            }
        }
    }

    pub fn set_native_layout(&mut self, layout: &str) {
        self.current_layout = layout.to_string();
        self.reflow_native_layout();
    }

    pub fn cycle_native_layout(&mut self) {
        let next = match self.current_layout.as_str() {
            "tiling" => "monocle",
            "monocle" => "grid",
            "grid" => "tiling",
            _ => "tiling",
        };
        self.set_native_layout(next);
    }

    pub fn reload_native_config(&mut self) -> Result<(), String> {
        let config = CompositorConfig::load_or_default();
        self.workspace_count = config.normalized_workspace_count();
        self.active_workspace = config.normalized_active_workspace();
        self.current_layout = config.layout_mode.clone();
        self.config = config;
        self.reflow_native_layout();
        Ok(())
    }

    pub fn launch_configured_app(&mut self, target: &str) -> Result<u32, String> {
        let command = self
            .config
            .app_launch_commands
            .get(target)
            .cloned()
            .ok_or_else(|| format!("unknown app launch target: {target}"))?;
        let child = Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch {target}: {err}"))?;
        let pid = child.id();
        info!(target, command, pid, "native app launch");
        Ok(pid)
    }

    pub fn run_autostart(&mut self, force: bool) -> (Vec<String>, Vec<String>, Vec<String>) {
        let mut launched = Vec::new();
        let mut skipped = Vec::new();
        let mut errors = Vec::new();
        for target in self.config.autostart_targets.clone() {
            if !force && self.autostart_launched.contains(&target) {
                skipped.push(target);
                continue;
            }
            match self.launch_configured_app(&target) {
                Ok(_) => {
                    self.autostart_launched.insert(target.clone());
                    launched.push(target);
                }
                Err(err) => errors.push(err),
            }
        }
        (launched, skipped, errors)
    }

    pub fn apply_real_session_startup_policy(&mut self) {
        if self.config.autostart_enabled {
            let (launched, skipped, errors) = self.run_autostart(false);
            info!(?launched, ?skipped, ?errors, "native autostart policy");
        }
        if self.config.session_idle_enabled {
            if let Err(err) = self.start_session_idle() {
                info!(error = err, "native idle supervision not started");
            }
        }
    }

    pub fn start_session_lock(&mut self) -> Result<(), String> {
        if Self::child_running(&mut self.session_lock_child) {
            return Ok(());
        }
        let command = self
            .config
            .session_lock_command
            .clone()
            .ok_or_else(|| "session_lock_command is not configured".to_string())?;
        let child = Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch session lock: {err}"))?;
        self.session_locked = true;
        self.session_lock_child = Some(child);
        Ok(())
    }

    pub fn start_session_idle(&mut self) -> Result<(), String> {
        if Self::child_running(&mut self.session_idle_child) {
            return Ok(());
        }
        let command = self
            .config
            .session_idle_command
            .clone()
            .ok_or_else(|| "session_idle_command is not configured".to_string())?;
        let child = Command::new("sh")
            .arg("-lc")
            .arg(&command)
            .spawn()
            .map_err(|err| format!("failed to launch session idle command: {err}"))?;
        self.session_idle_child = Some(child);
        Ok(())
    }

    pub fn stop_session_idle(&mut self) -> Result<(), String> {
        if let Some(child) = self.session_idle_child.as_mut() {
            child
                .kill()
                .map_err(|err| format!("failed to stop session idle command: {err}"))?;
        }
        self.session_idle_child = None;
        Ok(())
    }

    pub fn child_running(child: &mut Option<Child>) -> bool {
        child
            .as_mut()
            .map(|child| child.try_wait().ok().flatten().is_none())
            .unwrap_or(false)
    }

    pub fn handle_native_key_action(&mut self, key: &str) -> bool {
        let action = match self.config.key_actions.get(key).cloned() {
            Some(action) => action,
            None => return false,
        };
        let ok = self.run_native_action(&action).is_ok();
        let safe_key = key.replace('"', "\\\"");
        let safe_action = action.replace('"', "\\\"");
        let event = crate::ipc::dispatch::format_event(
            "native-key-action",
            &[
                ("key", &format!("\"{}\"", safe_key)),
                ("action", &format!("\"{}\"", safe_action)),
                ("status", if ok { ":ok" } else { ":error" }),
            ],
        );
        crate::ipc::server::IpcServer::broadcast_event(self, &event);
        true
    }

    pub fn run_native_action(&mut self, action: &str) -> Result<(), String> {
        if let Some(workspace) = action.strip_prefix("workspace-switch:") {
            let workspace = workspace
                .parse::<usize>()
                .map_err(|_| format!("invalid workspace action: {action}"))?;
            if workspace >= self.workspace_count {
                return Err(format!(
                    "workspace {} out of range (count {})",
                    workspace, self.workspace_count
                ));
            }
            self.active_workspace = workspace;
            self.reflow_native_layout();
            return Ok(());
        }
        if let Some(target) = action.strip_prefix("app-launch:") {
            self.launch_configured_app(target)?;
            return Ok(());
        }
        if let Some(layout) = action.strip_prefix("layout-set:") {
            self.set_native_layout(layout);
            return Ok(());
        }
        match action {
            "focus-next" => self.focus_next_surface(1),
            "focus-previous" => self.focus_next_surface(-1),
            "layout-cycle" => {
                self.cycle_native_layout();
                Ok(())
            }
            "config-reload" => self.reload_native_config(),
            "compositor-exit" => {
                self.running = false;
                Ok(())
            }
            _ => Err(format!("unknown native key action: {action}")),
        }
    }

    fn focus_next_surface(&mut self, direction: i32) -> Result<(), String> {
        let mut ids: Vec<u64> = self
            .surfaces
            .iter()
            .filter_map(|(id, data)| (data.workspace == self.active_workspace).then_some(*id))
            .collect();
        ids.sort_unstable();
        if ids.is_empty() {
            return Err("no focusable surfaces on active workspace".to_string());
        }
        let current = self
            .focused_surface
            .and_then(|id| ids.iter().position(|candidate| *candidate == id))
            .unwrap_or(0);
        let len = ids.len() as i32;
        let next = (current as i32 + direction).rem_euclid(len) as usize;
        self.focus_surface(ids[next]);
        Ok(())
    }

    pub fn focus_surface(&mut self, surface_id: u64) {
        if let Some(window) = self.surface_to_window.get(&surface_id).cloned() {
            self.space.raise_element(&window, true);
            if let Some(keyboard) = self.seat.get_keyboard() {
                let serial = smithay::utils::SERIAL_COUNTER.next_serial();
                let wl_surface = window.toplevel().map(|t| t.wl_surface().clone());
                if let Some(surface) = wl_surface {
                    keyboard.set_focus(self, Some(surface), serial);
                }
            }
        }
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

/// Per-client state required by Smithay's CompositorHandler.
#[derive(Default)]
pub struct ClientState {
    pub compositor_state: CompositorClientState,
}

impl ClientData for ClientState {
    fn initialized(&self, _client_id: ClientId) {}
    fn disconnected(&self, _client_id: ClientId, _reason: DisconnectReason) {}
}
