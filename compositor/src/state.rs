//! Compositor state — the central struct holding all Smithay state.
//!
//! Follows niri pattern: single `EwwmState` struct owns everything,
//! passed as `&mut self` to all handler trait implementations.

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
    utils::Rectangle,
    wayland::{
        compositor::{CompositorClientState, CompositorState},
        foreign_toplevel_list::ForeignToplevelListState,
        output::OutputManagerState,
        selection::data_device::DataDeviceState,
        shell::{
            wlr_layer::WlrLayerShellState,
            xdg::XdgShellState,
        },
        shm::ShmState,
    },
    xwayland::xwm::X11Wm,
    wayland::xwayland_shell::XWaylandShellState,
};
use std::{
    collections::{HashMap, HashSet},
    sync::{atomic::{AtomicU64, Ordering}, Arc},
};
use tracing::info;

use crate::autotype::AutoTypeManager;
use crate::clock::{Clock, SystemClock};
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

    // Foreign toplevel management
    pub foreign_toplevel_state: ForeignToplevelListState,

    // XWayland
    pub xwm: Option<X11Wm>,
    pub xwayland_shell_state: XWaylandShellState,
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
    pub active_workspace: usize,

    // Output usable area (after layer-shell exclusive zones)
    pub usable_area: UsableArea,

    // IPC
    pub ipc_server: IpcServer,
    pub grabbed_keys: HashSet<String>,

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

    // Shutdown flag
    pub running: bool,

    // Clock (real or test)
    pub clock: Arc<dyn Clock>,
}

impl EwwmState {
    pub fn new(
        display: &mut Display<Self>,
        loop_handle: LoopHandle<'static, Self>,
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

        // Foreign toplevel list protocol
        let foreign_toplevel_state = ForeignToplevelListState::new::<Self>(&display_handle);

        // XWayland shell protocol (for surface serial matching)
        let xwayland_shell_state = XWaylandShellState::new::<Self>(&display_handle);

        let seat = seat_state.new_wl_seat(&display_handle, "ewwm-seat");

        info!("EwwmState initialized (with layer-shell, foreign-toplevel, xwayland-shell)");

        let ipc_socket_path = IpcServer::default_socket_path();

        Self {
            display_handle,
            loop_handle,
            compositor_state,
            xdg_shell_state,
            shm_state,
            output_state,
            seat_state,
            data_device_state,
            layer_shell_state,
            foreign_toplevel_state,
            xwm: None,
            xwayland_shell_state,
            xdisplay: None,
            popups: PopupManager::default(),
            seat,
            space: Space::default(),
            surfaces: HashMap::new(),
            surface_to_window: HashMap::new(),
            active_workspace: 0,
            usable_area: UsableArea::default(),
            ipc_server: IpcServer::new(ipc_socket_path),
            grabbed_keys: HashSet::new(),
            vr_state: VrState::new(),
            autotype: AutoTypeManager::new(),
            secure_input: SecureInputState::new(),
            headless_active: false,
            headless_output_count: 0,
            headless_width: 1920,
            headless_height: 1080,
            running: true,
            clock: Arc::new(SystemClock),
        }
    }
}

impl EwwmState {
    /// Look up a Window by its surface_id.
    pub fn find_window(&self, surface_id: u64) -> Option<&Window> {
        self.surface_to_window.get(&surface_id)
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
