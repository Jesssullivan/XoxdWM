//! wp_drm_lease_v1 handler.
//!
//! The current slice wires the protocol global into the compositor so direct
//! mode clients can discover lease support and candidate connectors.
//! Actual lease granting still needs backend-owned DRM device access, so
//! requests are rejected for now instead of pretending the bridge is complete.

use crate::state::EwwmState;
use smithay::{
    backend::drm::DrmNode,
    delegate_drm_lease,
    wayland::drm_lease::{
        DrmLease, DrmLeaseBuilder, DrmLeaseHandler, DrmLeaseRequest, DrmLeaseState,
        LeaseRejected,
    },
};
use tracing::{info, warn};

impl DrmLeaseHandler for EwwmState {
    fn drm_lease_state(&mut self, _node: DrmNode) -> &mut DrmLeaseState {
        self.drm_lease_state
            .as_mut()
            .expect("drm lease state requested before initialization")
    }

    fn lease_request(
        &mut self,
        node: DrmNode,
        request: DrmLeaseRequest,
    ) -> Result<DrmLeaseBuilder, LeaseRejected> {
        warn!(
            ?node,
            connectors = ?request.connectors,
            "rejecting DRM lease request: lease granting is not wired yet"
        );
        Err(LeaseRejected::default())
    }

    fn new_active_lease(&mut self, node: DrmNode, lease: DrmLease) {
        info!(?node, lease_id = lease.id(), "new DRM lease became active");
        self.active_drm_leases.push(lease);
    }

    fn lease_destroyed(&mut self, node: DrmNode, lease_id: u32) {
        info!(?node, lease_id, "DRM lease destroyed");
        self.active_drm_leases.retain(|lease| lease.id() != lease_id);
    }
}

delegate_drm_lease!(EwwmState);
