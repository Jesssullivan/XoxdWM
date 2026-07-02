//! wl_compositor and wl_buffer handler.

use crate::ipc::{dispatch::format_event, server::IpcServer};
use crate::state::{ClientState, EwwmState};
#[cfg(feature = "xwayland")]
use smithay::xwayland::XWaylandClientData;
use smithay::{
    backend::renderer::utils::on_commit_buffer_handler,
    delegate_compositor,
    reexports::wayland_server::{protocol::wl_surface::WlSurface, Client},
    wayland::{
        compositor::{
            get_parent, is_sync_subsurface, CompositorClientState, CompositorHandler,
            CompositorState,
        },
        shell::xdg::XdgToplevelSurfaceData,
    },
};
use tracing::trace;

impl CompositorHandler for EwwmState {
    fn compositor_state(&mut self) -> &mut CompositorState {
        &mut self.compositor_state
    }

    fn client_compositor_state<'a>(&self, client: &'a Client) -> &'a CompositorClientState {
        if let Some(state) = client.get_data::<ClientState>() {
            return &state.compositor_state;
        }

        #[cfg(feature = "xwayland")]
        if let Some(state) = client.get_data::<XWaylandClientData>() {
            return &state.compositor_state;
        }

        panic!("Wayland client is missing compositor state")
    }

    fn commit(&mut self, surface: &WlSurface) {
        on_commit_buffer_handler::<Self>(surface);

        // Check for app_id / title changes on xdg_toplevel surfaces.
        self.update_surface_metadata(surface);

        // Mark VR texture dirty so the renderer re-imports the surface content.
        #[cfg(feature = "vr")]
        if self.vr_state.enabled {
            if let Some(surface_id) = self.surface_id_for_wl_surface(surface) {
                if let Some(renderer) = self.vr_state.renderer_mut() {
                    renderer.texture_manager.mark_dirty(surface_id);
                }
            }
        }
    }
}

impl EwwmState {
    /// Check if a committed surface has new app_id or title, and emit
    /// IPC events if they changed.
    fn update_surface_metadata(&mut self, wl_surface: &WlSurface) {
        let surface_id = match self.surface_id_for_wl_surface(wl_surface) {
            Some(id) => id,
            None => return,
        };

        // Read current app_id and title from xdg toplevel data.
        let (app_id, title) = smithay::wayland::compositor::with_states(wl_surface, |states| {
            states
                .data_map
                .get::<XdgToplevelSurfaceData>()
                .map(|data| {
                    let guard = data.lock().unwrap();
                    (guard.app_id.clone(), guard.title.clone())
                })
                .unwrap_or((None, None))
        });

        let config = &self.config;
        let workspace_count = self.workspace_count;
        let data = match self.surfaces.get_mut(&surface_id) {
            Some(d) => d,
            None => return,
        };

        let mut changed = false;
        let old_workspace = data.workspace;
        let old_floating = data.floating;

        if app_id != data.app_id && app_id.is_some() {
            data.app_id = app_id.clone();
            changed = true;
        }

        if title != data.title && title.is_some() {
            data.title = title.clone();
            changed = true;
        }

        if changed {
            let candidates = [
                data.app_id.as_deref(),
                data.x11_class.as_deref(),
                data.x11_instance.as_deref(),
            ];
            if let Some(workspace) = config.workspace_for_app_candidates(&candidates) {
                if workspace < workspace_count {
                    data.workspace = workspace;
                }
            }
            if config.should_float_app_candidates(&candidates) {
                data.floating = true;
            }
        }

        let new_workspace = data.workspace;
        let new_floating = data.floating;

        if changed {
            let aid = app_id.as_deref().unwrap_or("");
            let ttl = title.as_deref().unwrap_or("");
            trace!(
                surface_id,
                app_id = aid,
                title = ttl,
                "surface metadata updated"
            );

            let event = format_event(
                "surface-updated",
                &[
                    ("id", &surface_id.to_string()),
                    ("app-id", &format!("\"{}\"", aid)),
                    ("title", &format!("\"{}\"", ttl)),
                ],
            );
            IpcServer::broadcast_event(self, &event);

            if old_workspace != new_workspace {
                let event = format_event(
                    "surface-workspace-changed",
                    &[
                        ("id", &surface_id.to_string()),
                        ("old-workspace", &old_workspace.to_string()),
                        ("new-workspace", &new_workspace.to_string()),
                    ],
                );
                IpcServer::broadcast_event(self, &event);
            }

            if old_floating != new_floating {
                let event = format_event(
                    "surface-float-changed",
                    &[
                        ("id", &surface_id.to_string()),
                        ("floating", if new_floating { "t" } else { "nil" }),
                    ],
                );
                IpcServer::broadcast_event(self, &event);
            }
        }
    }
}

impl smithay::wayland::buffer::BufferHandler for EwwmState {
    fn buffer_destroyed(
        &mut self,
        _buffer: &smithay::reexports::wayland_server::protocol::wl_buffer::WlBuffer,
    ) {
    }
}

delegate_compositor!(EwwmState);
