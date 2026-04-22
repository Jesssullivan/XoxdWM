//! wp_drm_lease_v1 handler.
//!
//! This handler bridges Smithay's wp_drm_lease_v1 protocol to the live DRM
//! backend state. It grants leases only when a requested connector can be
//! driven by a currently-unused CRTC with a claimable primary plane.

use crate::state::EwwmState;
use smithay::{
    backend::drm::DrmNode,
    delegate_drm_lease,
    reexports::drm::control::connector as drm_connector,
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
        let Some(drm) = self.drm_lease_devices.get(&node).cloned() else {
            warn!(
                ?node,
                connectors = ?request.connectors,
                "rejecting DRM lease request: no live DRM device for node"
            );
            return Err(LeaseRejected::default());
        };

        let requested_connectors = request.connectors.clone();
        let mut reserved_crtcs = self
            .drm_output_crtcs
            .get(&node)
            .cloned()
            .unwrap_or_default();

        let drm = drm.borrow();
        let resources = drm.resource_handles().map_err(|err| {
            warn!(?node, ?err, "rejecting DRM lease request: resource query failed");
            LeaseRejected::default()
        })?;

        let mut builder = DrmLeaseBuilder::new(&drm);
        let mut granted = Vec::new();

        for connector in request.connectors {
            let info = drm.get_connector(connector, false).map_err(|err| {
                warn!(
                    ?node,
                    ?connector,
                    ?err,
                    "rejecting DRM lease request: failed to inspect connector"
                );
                LeaseRejected::default()
            })?;

            if info.state() != drm_connector::State::Connected {
                warn!(
                    ?node,
                    ?connector,
                    "rejecting DRM lease request: connector is not connected"
                );
                return Err(LeaseRejected::default());
            }

            let Some(crtc) = info
                .encoders()
                .iter()
                .filter_map(|enc_handle| drm.get_encoder(*enc_handle).ok())
                .flat_map(|enc| resources.filter_crtcs(enc.possible_crtcs()))
                .find(|crtc| !reserved_crtcs.contains(crtc))
            else {
                warn!(
                    ?node,
                    ?connector,
                    reserved_crtcs = ?reserved_crtcs,
                    "rejecting DRM lease request: no free compatible CRTC"
                );
                return Err(LeaseRejected::default());
            };

            let planes = drm.planes(&crtc).map_err(|err| {
                warn!(
                    ?node,
                    ?connector,
                    ?crtc,
                    ?err,
                    "rejecting DRM lease request: failed to enumerate planes"
                );
                LeaseRejected::default()
            })?;

            let Some((primary_plane, claim)) = planes
                .primary
                .iter()
                .find_map(|plane| drm.claim_plane(plane.handle, crtc).map(|claim| (plane.handle, claim)))
            else {
                warn!(
                    ?node,
                    ?connector,
                    ?crtc,
                    "rejecting DRM lease request: no claimable primary plane"
                );
                return Err(LeaseRejected::default());
            };

            builder.add_connector(connector);
            builder.add_crtc(crtc);
            builder.add_plane(primary_plane, claim);
            reserved_crtcs.insert(crtc);
            granted.push((connector, crtc, primary_plane));
        }

        info!(
            ?node,
            connectors = ?requested_connectors,
            assignments = ?granted,
            "granting DRM lease request"
        );

        Ok(builder)
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
