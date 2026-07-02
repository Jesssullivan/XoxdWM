//! IPC message dispatch — parse s-expressions and route to handlers.

use super::server::IpcServer;
use crate::config::{is_valid_layout_mode, LAYOUT_CYCLE};
use crate::state::EwwmState;
use lexpr::Value;
use tracing::{debug, warn};

/// Parse an s-expression message and dispatch to the appropriate handler.
/// Returns an optional response string (s-expression).
pub fn handle_message(state: &mut EwwmState, client_id: u64, raw: &str) -> Option<String> {
    let value = match lexpr::from_str(raw) {
        Ok(v) => v,
        Err(e) => {
            warn!(client_id, "malformed s-expression: {}", e);
            return Some(error_response(0, &format!("malformed s-expression: {e}")));
        }
    };

    let msg_type = get_keyword(&value, "type");
    let msg_id = get_int(&value, "id").unwrap_or(0);

    // Check authentication (hello must be first message)
    let is_authenticated = state
        .ipc_server
        .clients
        .get(&client_id)
        .map(|c| c.authenticated)
        .unwrap_or(false);

    match msg_type.as_deref() {
        Some("hello") => handle_hello(state, client_id, msg_id, &value),
        _ if !is_authenticated => Some(error_response(msg_id, "hello handshake required")),
        Some("ping") => handle_ping(state, msg_id, &value),
        Some("surface-list") => handle_surface_list(state, msg_id),
        Some("surface-info") => handle_surface_info(state, msg_id, &value),
        Some("focused-surface") => handle_focused_surface(state, msg_id),
        Some("surface-focus") => handle_surface_focus(state, msg_id, &value),
        Some("focus-surface") => handle_surface_focus(state, msg_id, &value),
        Some("surface-close") => handle_surface_close(state, msg_id, &value),
        Some("surface-move") => handle_surface_move(state, msg_id, &value),
        Some("surface-resize") => handle_surface_resize(state, msg_id, &value),
        Some("surface-move-interactive") => handle_surface_move_interactive(state, msg_id, &value),
        Some("surface-resize-interactive") => {
            handle_surface_resize_interactive(state, msg_id, &value)
        }
        Some("surface-fullscreen") => handle_surface_fullscreen(state, msg_id, &value),
        Some("surface-float") => handle_surface_float(state, msg_id, &value),
        Some("workspace-switch") => handle_workspace_switch(state, msg_id, &value),
        Some("workspace-list") => handle_workspace_list(state, msg_id),
        Some("workspace-move-surface") => handle_workspace_move_surface(state, msg_id, &value),
        Some("layout-get") => handle_layout_get(state, msg_id),
        Some("layout-set") => handle_layout_set(state, msg_id, &value),
        Some("layout-cycle") => handle_layout_cycle(state, msg_id),
        Some("app-launch-list") => handle_app_launch_list(state, msg_id),
        Some("app-launch") => handle_app_launch(state, msg_id, &value),
        Some("launch-app") => handle_app_launch(state, msg_id, &value),
        Some("config-reload") => handle_config_reload(state, msg_id),
        Some("reload-config") => handle_config_reload(state, msg_id),
        Some("autostart-list") => handle_autostart_list(state, msg_id),
        Some("autostart-run") => handle_autostart_run(state, msg_id, &value),
        Some("session-status") => handle_session_status(state, msg_id),
        Some("session-lock") => handle_session_lock(state, msg_id),
        Some("session-logout") => handle_compositor_exit(state, msg_id),
        Some("session-idle-status") => handle_session_idle_status(state, msg_id),
        Some("session-idle-start") => handle_session_idle_start(state, msg_id),
        Some("session-idle-stop") => handle_session_idle_stop(state, msg_id),
        Some("key-grab") => handle_key_grab(state, msg_id, &value),
        Some("key-ungrab") => handle_key_ungrab(state, msg_id, &value),
        Some("vr-status") => handle_vr_status(state, msg_id),
        Some("vr-diagnostics") => handle_vr_diagnostics(state, msg_id),
        Some("vr-set-reference-space") => handle_vr_set_reference_space(state, msg_id, &value),
        Some("vr-restart") => handle_vr_restart(state, msg_id),
        Some("vr-get-frame-timing") => handle_vr_get_frame_timing(state, msg_id),
        Some("vr-scene-status") => handle_vr_scene_status(state, msg_id),
        Some("vr-scene-set-layout") => handle_vr_scene_set_layout(state, msg_id, &value),
        Some("vr-scene-set-ppu") => handle_vr_scene_set_ppu(state, msg_id, &value),
        Some("vr-scene-set-background") => handle_vr_scene_set_background(state, msg_id, &value),
        Some("passthrough-enable") => handle_passthrough_enable(state, msg_id, &value),
        Some("passthrough-disable") => handle_passthrough_disable(state, msg_id),
        Some("passthrough-status") => handle_passthrough_status(state, msg_id),
        Some("passthrough-set-blend-mode") => {
            handle_passthrough_set_blend_mode(state, msg_id, &value)
        }
        Some("passthrough-set-opacity") => handle_passthrough_set_opacity(state, msg_id, &value),
        Some("vr-scene-set-projection") => handle_vr_scene_set_projection(state, msg_id, &value),
        Some("vr-scene-focus") => handle_vr_scene_focus(state, msg_id, &value),
        Some("vr-scene-move") => handle_vr_scene_move(state, msg_id, &value),
        Some("vr-display-info") => handle_vr_display_info(state, msg_id),
        Some("vr-display-set-mode") => handle_vr_display_set_mode(state, msg_id, &value),
        Some("vr-display-select-hmd") => handle_vr_display_select_hmd(state, msg_id, &value),
        Some("vr-display-set-refresh-rate") => {
            handle_vr_display_set_refresh_rate(state, msg_id, &value)
        }
        Some("vr-display-auto-detect") => handle_vr_display_auto_detect(state, msg_id),
        Some("vr-display-list-connectors") => handle_vr_display_list_connectors(state, msg_id),
        Some("vr-pointer-state") => handle_vr_pointer_state(state, msg_id),
        Some("vr-click") => handle_vr_click(state, msg_id, &value),
        Some("vr-grab") => handle_vr_grab(state, msg_id),
        Some("vr-grab-release") => handle_vr_grab_release(state, msg_id),
        Some("vr-adjust-depth") => handle_vr_adjust_depth(state, msg_id, &value),
        Some("vr-set-follow") => handle_vr_set_follow(state, msg_id, &value),
        Some("vr-set-gaze-offset") => handle_vr_set_gaze_offset(state, msg_id, &value),
        Some("vr-calibrate-confirm") => handle_vr_calibrate_confirm(state, msg_id),
        // Eye tracking and gaze control
        Some("gaze-status") => handle_gaze_status(state, msg_id),
        Some("gaze-set-source") => handle_gaze_set_source(state, msg_id, &value),
        Some("gaze-calibrate-start") => handle_gaze_calibrate_start(state, msg_id, &value),
        Some("gaze-calibrate-point") => handle_gaze_calibrate_point(state, msg_id, &value),
        Some("gaze-set-visualization") => handle_gaze_set_visualization(state, msg_id, &value),
        Some("gaze-set-smoothing") => handle_gaze_set_smoothing(state, msg_id, &value),
        Some("gaze-simulate") => handle_gaze_simulate(state, msg_id, &value),
        Some("gaze-health") => handle_gaze_health(state, msg_id),
        // Gaze focus and dwell
        Some("gaze-focus-config") => handle_gaze_focus_config(state, msg_id),
        Some("gaze-focus-status") => handle_gaze_focus_status(state, msg_id),
        Some("gaze-focus-set-policy") => handle_gaze_focus_set_policy(state, msg_id, &value),
        Some("gaze-focus-set-dwell") => handle_gaze_focus_set_dwell(state, msg_id, &value),
        Some("gaze-focus-set-cooldown") => handle_gaze_focus_set_cooldown(state, msg_id, &value),
        Some("gaze-focus-analytics") => handle_gaze_focus_analytics(state, msg_id),
        Some("gaze-focus-back") => handle_gaze_focus_back(state, msg_id),
        // Blink/wink detection
        Some("wink-status") => handle_wink_status(state, msg_id),
        Some("wink-config") => handle_wink_config(state, msg_id),
        Some("wink-calibrate-start") => handle_wink_calibrate_start(state, msg_id, &value),
        Some("wink-set-confidence") => handle_wink_set_confidence(state, msg_id, &value),
        // Gaze zones
        Some("gaze-zone-status") => handle_gaze_zone_status(state, msg_id),
        Some("gaze-zone-config") => handle_gaze_zone_config(state, msg_id),
        Some("gaze-zone-set-dwell") => handle_gaze_zone_set_dwell(state, msg_id, &value),
        Some("gaze-zone-set-layout") => handle_gaze_zone_set_layout(state, msg_id, &value),
        // Eye fatigue monitoring
        Some("fatigue-status") => handle_fatigue_status(state, msg_id),
        Some("fatigue-config") => handle_fatigue_config(state, msg_id),
        Some("fatigue-metrics") => handle_fatigue_metrics(state, msg_id),
        Some("fatigue-reset") => handle_fatigue_reset(state, msg_id),
        // Headless backend
        Some("headless-status") => handle_headless_status(state, msg_id),
        Some("headless-set-resolution") => handle_headless_set_resolution(state, msg_id, &value),
        Some("headless-add-output") => handle_headless_add_output(state, msg_id),
        Some("headless-remove-output") => handle_headless_remove_output(state, msg_id),
        // Auto-type and secure input
        Some("autotype") => handle_autotype(state, msg_id, &value),
        Some("autotype-status") => handle_autotype_status(state, msg_id),
        Some("autotype-abort") => handle_autotype_abort(state, msg_id),
        Some("autotype-pause") => handle_autotype_pause(state, msg_id, &value),
        Some("autotype-resume") => handle_autotype_resume(state, msg_id),
        Some("command") => handle_compat_command(state, msg_id, &value),
        Some("secure-input-mode") => handle_secure_input_mode(state, msg_id, &value),
        Some("secure-input-status") => handle_secure_input_status(state, msg_id),
        Some("gaze-away-monitor") => handle_gaze_away_monitor(state, msg_id, &value),
        // Gaze scroll and link hints
        Some("gaze-scroll-status") => handle_gaze_scroll_status(state, msg_id),
        Some("gaze-scroll-config") => handle_gaze_scroll_config(state, msg_id, &value),
        Some("gaze-scroll-set-speed") => handle_gaze_scroll_set_speed(state, msg_id, &value),
        Some("link-hints-load") => handle_link_hints_load(state, msg_id, &value),
        Some("link-hints-confirm") => handle_link_hints_confirm(state, msg_id),
        Some("link-hints-clear") => handle_link_hints_clear(state, msg_id),
        Some("link-hints-status") => handle_link_hints_status(state, msg_id),
        // Hand tracking
        Some("hand-tracking-status") => handle_hand_tracking_status(state, msg_id),
        Some("hand-tracking-config") => handle_hand_tracking_config(state, msg_id, &value),
        Some("hand-tracking-configure") => handle_hand_tracking_config(state, msg_id, &value),
        Some("hand-tracking-toggle") => handle_hand_tracking_toggle(state, msg_id, &value),
        Some("hand-tracking-joint") => handle_hand_tracking_joint(state, msg_id, &value),
        Some("hand-tracking-skeleton") => handle_hand_tracking_skeleton(state, msg_id, &value),
        Some("hand-tracking-distance") => handle_hand_tracking_distance(state, msg_id, &value),
        // Gesture recognition
        Some("gesture-status") => handle_gesture_status(state, msg_id),
        Some("gesture-config") => handle_gesture_config(state, msg_id, &value),
        Some("gesture-bind") => handle_gesture_bind(state, msg_id, &value),
        Some("gesture-unbind") => handle_gesture_unbind(state, msg_id, &value),
        Some("gesture-bindings") => handle_gesture_bindings(state, msg_id),
        // Virtual keyboard
        Some("keyboard-show") => handle_keyboard_show(state, msg_id),
        Some("keyboard-hide") => handle_keyboard_hide(state, msg_id),
        Some("keyboard-toggle") => handle_keyboard_toggle(state, msg_id),
        Some("keyboard-layout") => handle_keyboard_layout(state, msg_id, &value),
        Some("keyboard-status") => handle_keyboard_status(state, msg_id),
        // BCI core
        Some("bci-status") => handle_bci_status(state, msg_id),
        Some("bci-hardware-check") => handle_bci_hardware_check(state, msg_id),
        Some("bci-start") => handle_bci_start(state, msg_id),
        Some("bci-stop") => handle_bci_stop(state, msg_id),
        Some("bci-restart") => handle_bci_restart(state, msg_id),
        Some("bci-signal-quality") => handle_bci_signal_quality(state, msg_id),
        Some("bci-config") => handle_bci_config(state, msg_id, &value),
        Some("bci-inject-synthetic") => handle_bci_inject_synthetic(state, msg_id, &value),
        Some("bci-data-list") => handle_bci_data_list(state, msg_id),
        Some("bci-data-delete") => handle_bci_data_delete(state, msg_id, &value),
        // BCI attention
        Some("bci-attention-status") => handle_bci_attention_status(state, msg_id),
        Some("bci-attention-config") => handle_bci_attention_config(state, msg_id, &value),
        Some("bci-attention-calibrate") => handle_bci_attention_calibrate_start(state, msg_id),
        Some("bci-attention-toggle") => handle_bci_attention_toggle(state, msg_id, &value),
        Some("bci-dnd-enable") => handle_bci_dnd_compat(state, msg_id, true),
        Some("bci-dnd-disable") => handle_bci_dnd_compat(state, msg_id, false),
        Some("bci-attention-calibrate-start") => {
            handle_bci_attention_calibrate_start(state, msg_id)
        }
        Some("bci-attention-calibrate-finish") => {
            handle_bci_attention_calibrate_finish(state, msg_id)
        }
        // BCI SSVEP
        Some("bci-ssvep-status") => handle_bci_ssvep_status(state, msg_id),
        Some("bci-ssvep-config") => handle_bci_ssvep_config(state, msg_id, &value),
        Some("bci-ssvep-configure") => handle_bci_ssvep_configure_compat(state, msg_id, &value),
        Some("bci-ssvep-start") => handle_bci_ssvep_start(state, msg_id),
        Some("bci-ssvep-stop") => handle_bci_ssvep_stop(state, msg_id),
        // BCI P300
        Some("bci-p300-status") => handle_bci_p300_status(state, msg_id),
        Some("bci-p300-config") => handle_bci_p300_config(state, msg_id, &value),
        Some("bci-p300-start") => handle_bci_p300_start(state, msg_id, &value),
        Some("bci-p300-stop") => handle_bci_p300_stop(state, msg_id),
        Some("bci-p300-cancel") => handle_bci_p300_stop(state, msg_id),
        // BCI motor imagery
        Some("bci-mi-status") => handle_bci_mi_status(state, msg_id),
        Some("bci-mi-config") => handle_bci_mi_config(state, msg_id, &value),
        Some("bci-mi-calibrate") => handle_bci_mi_calibrate_start(state, msg_id),
        Some("bci-mi-toggle") => handle_bci_mi_toggle(state, msg_id, &value),
        Some("bci-mi-calibrate-start") => handle_bci_mi_calibrate_start(state, msg_id),
        Some("bci-mi-calibrate-finish") => handle_bci_mi_calibrate_finish(state, msg_id),
        // BCI fatigue EEG
        Some("bci-fatigue-eeg-status") => handle_bci_fatigue_eeg_status(state, msg_id),
        Some("bci-fatigue-eeg-config") => handle_bci_fatigue_eeg_config(state, msg_id, &value),
        // App-layer BCI surfaces without native compositor product authority yet.
        Some("bci-nfb-start") => handle_unsupported_app_surface(
            msg_id,
            "bci-nfb",
            "native neurofeedback session streaming is not implemented",
        ),
        Some("bci-nfb-stop") => handle_unsupported_app_surface(
            msg_id,
            "bci-nfb",
            "native neurofeedback session streaming is not implemented",
        ),
        Some("multimodal-enable") => handle_unsupported_app_surface(
            msg_id,
            "multimodal",
            "native multimodal fusion is not implemented",
        ),
        Some("multimodal-disable") => handle_unsupported_app_surface(
            msg_id,
            "multimodal",
            "native multimodal fusion is not implemented",
        ),
        Some("multimodal-set-dwell") => handle_unsupported_app_surface(
            msg_id,
            "multimodal",
            "native multimodal dwell policy is not implemented",
        ),
        Some("multimodal-three-factor-start") => handle_unsupported_app_surface(
            msg_id,
            "multimodal",
            "native three-factor verification is not implemented",
        ),
        Some("passkey-response") => handle_unsupported_app_surface(
            msg_id,
            "passkey",
            "passkey browser response plumbing remains app-layer",
        ),
        // DPMS output power
        Some("dpms-get") => handle_dpms_get(state, msg_id),
        Some("dpms-set") => handle_dpms_set(state, msg_id, &value),
        // Screencopy (wlr-screencopy-unstable-v1)
        Some("screencopy-status") => handle_screencopy_status(state, msg_id),
        // Output management (wlr-output-management-unstable-v1)
        Some("output-list") => handle_output_list(state, msg_id),
        Some("output-configure") => handle_output_configure(state, msg_id, &value),
        // Pointer constraints (pointer-constraints-unstable-v1)
        Some("pointer-constraints-status") => handle_pointer_constraints_status(state, msg_id),
        // IPC recording (v0.2.0)
        Some("ipc-record-start") => handle_ipc_record_start(state, msg_id, &value),
        Some("ipc-record-stop") => handle_ipc_record_stop(state, msg_id),
        Some("ipc-record-status") => handle_ipc_record_status(state, msg_id),
        // IPC security (v0.3.1)
        Some("ipc-client-info") => handle_ipc_client_info(state, client_id, msg_id),
        Some("ipc-rate-limit") => handle_ipc_rate_limit(state, client_id, msg_id, &value),
        Some("input-latency-probe") => handle_input_latency_probe(msg_id, &value),
        // VR follow mode
        Some("vr-follow-status") => handle_vr_follow_status(state, msg_id),
        Some("follow-status") => handle_vr_follow_status(state, msg_id),
        Some("vr-follow-set-policy") => handle_vr_follow_set_policy(state, msg_id, &value),
        Some("follow-set-policy") => handle_vr_follow_set_policy(state, msg_id, &value),
        Some("follow-configure") => handle_follow_configure(state, msg_id, &value),
        Some("vr-follow-recenter") => handle_vr_follow_recenter(state, msg_id),
        Some("follow-recenter") => handle_vr_follow_recenter(state, msg_id),
        Some("vr-follow-grab-all") => handle_vr_follow_grab_all(state, msg_id),
        // Compatibility names for the Emacs focus-routing app layer. Native
        // authority lives in the compositor gaze-focus policy state.
        Some("focus-routing-status") => handle_focus_routing_status(state, msg_id),
        Some("focus-routing-set-mode") => handle_focus_routing_set_mode(state, msg_id, &value),
        Some("focus-routing-set-dwell") => handle_gaze_focus_set_dwell(state, msg_id, &value),
        Some("focus-routing-configure") => handle_focus_routing_configure(state, msg_id, &value),
        // VR transient chains
        Some("vr-transient-add") => handle_vr_transient_add(state, msg_id, &value),
        Some("vr-transient-remove") => handle_vr_transient_remove(state, msg_id, &value),
        Some("vr-transient-list") => handle_vr_transient_list(state, msg_id),
        Some("transient-list") => handle_vr_transient_list(state, msg_id),
        Some("transient-status") => handle_transient_status(state, msg_id),
        Some("transient-configure") => handle_transient_configure(state, msg_id, &value),
        Some("transient-set-offset") => handle_transient_set_offset(state, msg_id, &value),
        Some("transient-set-placement") => handle_transient_set_placement(state, msg_id, &value),
        // Compositor-local spatial anchors. These are native scene anchors,
        // not XR_EXT_spatial_anchor runtime persistence.
        Some("anchor-create") => handle_anchor_create(state, msg_id, &value),
        Some("anchor-restore") => handle_anchor_restore(state, msg_id, &value),
        Some("anchor-remove") => handle_anchor_remove(state, msg_id, &value),
        Some("anchor-list") => handle_anchor_list(state, msg_id),
        Some("anchor-status") => handle_anchor_status(state, msg_id),
        Some("anchor-goto") => handle_anchor_goto(state, msg_id, &value),
        // VR overlays
        Some("vr-overlay-create") => handle_vr_overlay_create(state, msg_id, &value),
        Some("overlay-create") => handle_vr_overlay_create(state, msg_id, &value),
        Some("vr-overlay-remove") => handle_vr_overlay_remove(state, msg_id, &value),
        Some("overlay-remove") => handle_vr_overlay_remove(state, msg_id, &value),
        Some("vr-overlay-list") => handle_vr_overlay_list(state, msg_id),
        Some("overlay-list") => handle_vr_overlay_list(state, msg_id),
        Some("overlay-status") => handle_vr_overlay_list(state, msg_id),
        Some("vr-overlay-configure") => handle_vr_overlay_configure(state, msg_id, &value),
        Some("overlay-set-alpha") => handle_vr_overlay_configure(state, msg_id, &value),
        Some("overlay-set-visible") => handle_vr_overlay_configure(state, msg_id, &value),
        Some("overlay-link-surface") => handle_vr_overlay_configure(state, msg_id, &value),
        // VR radial menu
        Some("vr-radial-open") => handle_vr_radial_open(state, msg_id),
        Some("vr-radial-close") => handle_vr_radial_close(state, msg_id),
        Some("vr-radial-toggle") => handle_vr_radial_toggle(state, msg_id),
        Some("vr-radial-configure") => handle_vr_radial_configure(state, msg_id, &value),
        Some("vr-radial-status") => handle_vr_radial_status(state, msg_id),
        // VR capture visibility
        Some("vr-capture-set") => handle_vr_capture_set(state, msg_id, &value),
        Some("vr-capture-get") => handle_vr_capture_get(state, msg_id, &value),
        Some("vr-capture-status") => handle_vr_capture_status(state, msg_id),
        // GPU power management
        Some("gpu-power-status") => handle_gpu_power_status(state, msg_id),
        Some("gpu-power-set-profile") => handle_gpu_power_set_profile(state, msg_id, &value),
        Some("gpu-power-detect") => handle_gpu_power_detect(state, msg_id),
        // Bigscreen Beyond HID control
        Some("beyond-status") => handle_beyond_status(state, msg_id),
        Some("beyond-detect") => handle_beyond_detect(state, msg_id),
        Some("beyond-power-on") => handle_beyond_power_on(state, msg_id),
        Some("beyond-set-brightness") => handle_beyond_set_brightness(state, msg_id, &value),
        Some("beyond-set-fan-speed") => handle_beyond_set_fan_speed(state, msg_id, &value),
        Some("beyond-set-led-color") => handle_beyond_set_led_color(state, msg_id, &value),
        Some("beyond-firmware-version") => handle_beyond_firmware_version(state, msg_id),
        // VR device listing
        Some("vr-list-devices") => handle_vr_list_devices(state, msg_id),
        Some("compositor-exit") => handle_compositor_exit(state, msg_id),
        Some(other) => Some(error_response(
            msg_id,
            &format!("unknown message type: {other}"),
        )),
        None => Some(error_response(msg_id, "missing :type field")),
    }
}

// ── Handlers ────────────────────────────────────────────────

fn handle_hello(
    state: &mut EwwmState,
    client_id: u64,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let version = get_int(value, "version").unwrap_or(0);
    if version != 1 {
        return Some(error_response(
            msg_id,
            &format!("unsupported protocol version: {version}"),
        ));
    }

    // SO_PEERCRED: verify peer UID matches compositor UID.
    // This prevents other users on the same host from connecting.
    if let Some(client) = state.ipc_server.clients.get(&client_id) {
        if let Some(peer_uid) = client.peer_uid {
            let our_uid = unsafe { libc::getuid() };
            if peer_uid != our_uid {
                warn!(
                    client_id,
                    peer_uid, our_uid, "rejecting client: UID mismatch"
                );
                return Some(error_response(
                    msg_id,
                    "authentication failed: UID mismatch",
                ));
            }
        }
    }

    let client_name = get_string(value, "client").unwrap_or_default();
    debug!(client_id, client_name, "hello handshake (authenticated)");

    // Store peer info and mark authenticated.
    let peer_pid = state
        .ipc_server
        .clients
        .get(&client_id)
        .and_then(|c| c.peer_pid);
    if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
        client.authenticated = true;
    }

    let vr_flag = if state.vr_state.enabled { "t" } else { "nil" };
    let pid_field = peer_pid
        .map(|p| format!(" :peer-pid {}", p))
        .unwrap_or_default();
    let xwayland_flag = if cfg!(feature = "xwayland") {
        "t"
    } else {
        "nil"
    };
    Some(format!(
        "(:type :hello :id {} :version 1 :server \"ewwm-compositor\" :features (:xwayland {} :vr {}){})",
        msg_id, xwayland_flag, vr_flag, pid_field
    ))
}

fn handle_ping(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let client_ts = get_int(value, "timestamp").unwrap_or(0);
    let server_ts = state.clock.unix_millis();

    Some(format!(
        "(:type :response :id {} :status :ok :client-timestamp {} :server-timestamp {})",
        msg_id, client_ts, server_ts
    ))
}

fn handle_surface_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut surfaces_sexp = String::from("(");

    for (id, data) in &state.surfaces {
        let app_id = data.app_id.as_deref().unwrap_or("");
        let title = data.title.as_deref().unwrap_or("");
        let x11_flag = if data.is_x11 { "t" } else { "nil" };
        let x11_class = data.x11_class.as_deref().unwrap_or("");
        let x11_instance = data.x11_instance.as_deref().unwrap_or("");
        // Get geometry for this specific surface's Window
        let geo = state
            .surface_to_window
            .get(id)
            .and_then(|w| state.space.element_geometry(w))
            .map(|g| (g.loc.x, g.loc.y, g.size.w, g.size.h))
            .unwrap_or((0, 0, 0, 0));

        let focused = state.focused_surface == Some(*id);
        surfaces_sexp.push_str(&format!(
            "(:id {} :app-id \"{}\" :title \"{}\" :x11 {} :x11-class \"{}\" :x11-instance \"{}\" :geometry (:x {} :y {} :w {} :h {}) :workspace {} :floating {} :focused {})",
            id,
            escape_string(app_id),
            escape_string(title),
            x11_flag,
            escape_string(x11_class),
            escape_string(x11_instance),
            geo.0, geo.1, geo.2, geo.3,
            data.workspace,
            if data.floating { "t" } else { "nil" },
            if focused { "t" } else { "nil" },
        ));
    }
    surfaces_sexp.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :surfaces {})",
        msg_id, surfaces_sexp
    ))
}

fn handle_focused_surface(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.focused_surface {
        Some(id) => {
            let data = state.surfaces.get(&id);
            let app_id = data.and_then(|d| d.app_id.as_deref()).unwrap_or("");
            let title = data.and_then(|d| d.title.as_deref()).unwrap_or("");
            Some(format!(
                "(:type :response :id {} :status :ok :surface-id {} :app-id \"{}\" :title \"{}\")",
                msg_id,
                id,
                escape_string(app_id),
                escape_string(title),
            ))
        }
        None => Some(format!(
            "(:type :response :id {} :status :ok :surface-id nil)",
            msg_id,
        )),
    }
}

fn handle_surface_info(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let data = match state.surfaces.get(&surface_id) {
        Some(d) => d,
        None => return Some(error_response(msg_id, "unknown surface")),
    };

    let app_id = data.app_id.as_deref().unwrap_or("");
    let title = data.title.as_deref().unwrap_or("");
    let x11_flag = if data.is_x11 { "t" } else { "nil" };
    let x11_class = data.x11_class.as_deref().unwrap_or("");
    let x11_instance = data.x11_instance.as_deref().unwrap_or("");
    let focused = state.focused_surface == Some(surface_id);

    let geo = state
        .surface_to_window
        .get(&surface_id)
        .and_then(|w| state.space.element_geometry(w))
        .map(|g| (g.loc.x, g.loc.y, g.size.w, g.size.h))
        .unwrap_or((0, 0, 0, 0));

    Some(format!(
        "(:type :response :id {} :status :ok :surface-id {} :app-id \"{}\" :title \"{}\" :x11 {} :x11-class \"{}\" :x11-instance \"{}\" :geometry (:x {} :y {} :w {} :h {}) :workspace {} :floating {} :focused {})",
        msg_id,
        surface_id,
        escape_string(app_id),
        escape_string(title),
        x11_flag,
        escape_string(x11_class),
        escape_string(x11_instance),
        geo.0, geo.1, geo.2, geo.3,
        data.workspace,
        if data.floating { "t" } else { "nil" },
        if focused { "t" } else { "nil" },
    ))
}

fn handle_surface_focus(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    if let Err(reason) = state.focus_surface(surface_id) {
        return Some(error_response(msg_id, &reason));
    }

    Some(ok_response(msg_id))
}

fn handle_surface_close(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Send close request to the correct toplevel
    if let Some(w) = state.find_window(surface_id) {
        if let Some(toplevel) = w.toplevel() {
            toplevel.send_close();
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_move(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let x = get_int(value, "x").unwrap_or(0) as i32;
    let y = get_int(value, "y").unwrap_or(0) as i32;

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    // Find window by surface_id and remap at new location
    if let Some(w) = state.find_window(surface_id).cloned() {
        let current_geo = state.space.element_geometry(&w);
        let width = current_geo.map(|geo| geo.size.w).unwrap_or(800);
        let height = current_geo.map(|geo| geo.size.h).unwrap_or(600);
        state.space.map_element(w, (x, y), false);
        if let Some(data) = state.surfaces.get_mut(&surface_id) {
            data.geometry = Some(smithay::utils::Rectangle::new(
                (x, y).into(),
                (width, height).into(),
            ));
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_resize(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    let current_geo = state
        .surface_to_window
        .get(&surface_id)
        .and_then(|window| state.space.element_geometry(window));
    let default_x = current_geo.map(|geo| geo.loc.x).unwrap_or(0);
    let default_y = current_geo.map(|geo| geo.loc.y).unwrap_or(0);
    let default_w = current_geo.map(|geo| geo.size.w).unwrap_or(800);
    let default_h = current_geo.map(|geo| geo.size.h).unwrap_or(600);

    let x = get_int(value, "x")
        .or_else(|| get_nested_int(value, "geometry", "x"))
        .unwrap_or(default_x as i64) as i32;
    let y = get_int(value, "y")
        .or_else(|| get_nested_int(value, "geometry", "y"))
        .unwrap_or(default_y as i64) as i32;
    let w = get_int(value, "w")
        .or_else(|| get_nested_int(value, "geometry", "w"))
        .unwrap_or(default_w as i64) as i32;
    let h = get_int(value, "h")
        .or_else(|| get_nested_int(value, "geometry", "h"))
        .unwrap_or(default_h as i64) as i32;

    // Resize via pending state using proper surface_id lookup
    if let Some(win) = state.find_window(surface_id).cloned() {
        state.space.map_element(win.clone(), (x, y), false);
        if let Some(data) = state.surfaces.get_mut(&surface_id) {
            data.geometry = Some(smithay::utils::Rectangle::new((x, y).into(), (w, h).into()));
        }
        if let Some(toplevel) = win.toplevel() {
            toplevel.with_pending_state(|s| {
                s.size = Some(smithay::utils::Size::from((w, h)));
            });
            toplevel.send_pending_configure();
        }
    }

    let geometry = format!("(:x {} :y {} :w {} :h {})", x, y, w, h);
    let event = format_event(
        "surface-geometry-changed",
        &[("id", &surface_id.to_string()), ("geometry", &geometry)],
    );
    IpcServer::broadcast_event(state, &event);

    Some(ok_response(msg_id))
}

fn handle_surface_move_interactive(
    _state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    Some(error_response(
        msg_id,
        &format!(
            "surface-move-interactive is not implemented in native compositor yet for surface {surface_id}"
        ),
    ))
}

fn handle_surface_resize_interactive(
    _state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    Some(error_response(
        msg_id,
        &format!(
            "surface-resize-interactive is not implemented in native compositor yet for surface {surface_id}"
        ),
    ))
}

fn handle_surface_fullscreen(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use smithay::reexports::wayland_protocols::xdg::shell::server::xdg_toplevel::State as ToplevelState;

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let enable = get_bool(value, "enable").unwrap_or(true);

    if !state.surfaces.contains_key(&surface_id) {
        return Some(error_response(
            msg_id,
            &format!("unknown surface: {surface_id}"),
        ));
    }

    if let Some(win) = state.find_window(surface_id) {
        if let Some(toplevel) = win.toplevel() {
            toplevel.with_pending_state(|s| {
                if enable {
                    s.states.set(ToplevelState::Fullscreen);
                    // Set size to full output
                } else {
                    s.states.unset(ToplevelState::Fullscreen);
                }
            });
            toplevel.send_pending_configure();
            debug!(surface_id, fullscreen = enable, "fullscreen toggle");
        }
    }

    Some(ok_response(msg_id))
}

fn handle_surface_float(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let enable = get_bool(value, "enable").unwrap_or(true);

    let data = match state.surfaces.get_mut(&surface_id) {
        Some(d) => d,
        None => {
            return Some(error_response(
                msg_id,
                &format!("unknown surface: {surface_id}"),
            ))
        }
    };

    data.floating = enable;
    debug!(surface_id, floating = enable, "float toggle");
    state.apply_native_layout();

    let event = format_event(
        "surface-float-changed",
        &[
            ("id", &surface_id.to_string()),
            ("floating", if enable { "t" } else { "nil" }),
        ],
    );
    IpcServer::broadcast_event(state, &event);

    Some(ok_response(msg_id))
}

fn handle_workspace_switch(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let workspace = match get_int(value, "workspace") {
        Some(w) if w >= 0 => w as usize,
        Some(w) => return Some(error_response(msg_id, &format!("invalid workspace: {w}"))),
        None => 0,
    };
    if workspace >= state.workspace_count {
        return Some(error_response(
            msg_id,
            &format!(
                "workspace {} out of range (count {})",
                workspace, state.workspace_count
            ),
        ));
    }
    debug!(workspace, "workspace switch");
    let previous = state.active_workspace;
    state.active_workspace = workspace;
    state.apply_native_layout();
    if previous != workspace {
        let event = format_event(
            "workspace-changed",
            &[
                ("workspace", &workspace.to_string()),
                ("previous", &previous.to_string()),
            ],
        );
        IpcServer::broadcast_event(state, &event);
    }
    Some(ok_response(msg_id))
}

fn handle_workspace_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut workspaces = String::from("(");
    for i in 0..state.workspace_count {
        let active = if i == state.active_workspace {
            "t"
        } else {
            "nil"
        };
        // Collect surface IDs on this workspace
        let surface_ids: Vec<String> = state
            .surfaces
            .iter()
            .filter(|(_, d)| d.workspace == i)
            .map(|(id, _)| id.to_string())
            .collect();
        let surfaces_sexp = format!("({})", surface_ids.join(" "));
        workspaces.push_str(&format!(
            "(:index {} :name \"{}\" :surfaces {} :active {} :count {})",
            i,
            i + 1,
            surfaces_sexp,
            active,
            surface_ids.len(),
        ));
    }
    workspaces.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :workspaces {})",
        msg_id, workspaces
    ))
}

fn handle_workspace_move_surface(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let workspace = match get_int(value, "workspace") {
        Some(w) if w >= 0 => w as usize,
        Some(w) => return Some(error_response(msg_id, &format!("invalid workspace: {w}"))),
        None => return Some(error_response(msg_id, "missing :workspace")),
    };
    if workspace >= state.workspace_count {
        return Some(error_response(
            msg_id,
            &format!(
                "workspace {} out of range (count {})",
                workspace, state.workspace_count
            ),
        ));
    }

    let old_workspace = {
        let data = match state.surfaces.get_mut(&surface_id) {
            Some(d) => d,
            None => {
                return Some(error_response(
                    msg_id,
                    &format!("unknown surface: {surface_id}"),
                ))
            }
        };
        let old_workspace = data.workspace;
        data.workspace = workspace;
        old_workspace
    };
    debug!(
        surface_id,
        from = old_workspace,
        to = workspace,
        "workspace move"
    );
    state.apply_native_layout();

    let event = format_event(
        "surface-workspace-changed",
        &[
            ("id", &surface_id.to_string()),
            ("old-workspace", &old_workspace.to_string()),
            ("new-workspace", &workspace.to_string()),
        ],
    );
    IpcServer::broadcast_event(state, &event);

    Some(ok_response(msg_id))
}

fn handle_layout_get(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    Some(layout_response(msg_id, state))
}

fn handle_layout_set(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let layout = match get_string(value, "layout") {
        Some(layout) if is_valid_layout_mode(&layout) => layout,
        Some(layout) => return Some(error_response(msg_id, &format!("invalid layout: {layout}"))),
        None => return Some(error_response(msg_id, "missing :layout")),
    };
    set_current_layout(state, layout, msg_id)
}

fn handle_layout_cycle(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let pos = LAYOUT_CYCLE
        .iter()
        .position(|layout| *layout == state.current_layout)
        .unwrap_or(0);
    let next = LAYOUT_CYCLE[(pos + 1) % LAYOUT_CYCLE.len()].to_string();
    set_current_layout(state, next, msg_id)
}

fn set_current_layout(state: &mut EwwmState, layout: String, msg_id: i64) -> Option<String> {
    let previous = std::mem::replace(&mut state.current_layout, layout);
    debug!(
        layout = %state.current_layout,
        previous = %previous,
        "native layout policy updated"
    );
    state.apply_native_layout();

    let layout_kw = format!(":{}", state.current_layout);
    let previous_kw = format!(":{}", previous);
    let event = format_event(
        "layout-changed",
        &[
            ("layout", layout_kw.as_str()),
            ("previous", previous_kw.as_str()),
        ],
    );
    IpcServer::broadcast_event(state, &event);

    Some(layout_response(msg_id, state))
}

fn layout_response(msg_id: i64, state: &EwwmState) -> String {
    format!(
        "(:type :response :id {} :status :ok :layout :{} :master-ratio {})",
        msg_id, state.current_layout, state.layout_master_ratio
    )
}

fn handle_app_launch(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let name = match get_string(value, "name")
        .or_else(|| get_string(value, "target"))
        .or_else(|| get_string(value, "app"))
    {
        Some(name) if !name.trim().is_empty() => name,
        _ => return Some(error_response(msg_id, "missing :name")),
    };

    match state.launch_configured_app(&name) {
        Ok(detail) => Some(format!(
            "(:type :response :id {} :status :ok :name \"{}\" :detail \"{}\")",
            msg_id,
            escape_string(&name),
            escape_string(&detail)
        )),
        Err(reason) => Some(error_response(msg_id, &reason)),
    }
}

fn handle_app_launch_list(state: &EwwmState, msg_id: i64) -> Option<String> {
    let targets = format_string_list(state.config.app_launch_commands.keys());
    Some(format!(
        "(:type :response :id {} :status :ok :targets {})",
        msg_id, targets
    ))
}

fn handle_config_reload(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let source = match state.reload_native_config() {
        Ok(source) => source,
        Err(reason) => return Some(error_response(msg_id, &reason)),
    };

    let workspace_count = state.workspace_count.to_string();
    let active_workspace = state.active_workspace.to_string();
    let layout_kw = format!(":{}", state.current_layout);
    let event = format_event(
        "config-reloaded",
        &[
            ("workspace-count", workspace_count.as_str()),
            ("active-workspace", active_workspace.as_str()),
            ("layout", layout_kw.as_str()),
        ],
    );
    IpcServer::broadcast_event(state, &event);

    Some(format!(
        "(:type :response :id {} :status :ok :detail \"config-reloaded\" :source \"{}\" :workspace-count {} :active-workspace {} :layout :{})",
        msg_id, escape_string(&source), state.workspace_count, state.active_workspace, state.current_layout
    ))
}

fn handle_autostart_list(state: &EwwmState, msg_id: i64) -> Option<String> {
    let targets = format_string_list(state.config.configured_autostart_targets().iter());
    let mut launched = state.native_autostart_launched.iter().collect::<Vec<_>>();
    launched.sort();
    let launched = format_string_list(launched.into_iter());

    Some(format!(
        "(:type :response :id {} :status :ok :enabled {} :targets {} :launched {})",
        msg_id,
        bool_atom(state.config.autostart_enabled),
        targets,
        launched
    ))
}

fn handle_autostart_run(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let force = get_bool(value, "force").unwrap_or(false);
    let outcomes = state.run_native_autostart(force);
    let launched_count = outcomes
        .iter()
        .filter(|outcome| outcome.status == "launched")
        .count()
        .to_string();
    let skipped_count = outcomes
        .iter()
        .filter(|outcome| outcome.status == "skipped")
        .count()
        .to_string();
    let error_count = outcomes
        .iter()
        .filter(|outcome| outcome.status == "error")
        .count()
        .to_string();
    let event = format_event(
        "autostart-ran",
        &[
            ("launched", launched_count.as_str()),
            ("skipped", skipped_count.as_str()),
            ("errors", error_count.as_str()),
        ],
    );
    IpcServer::broadcast_event(state, &event);

    Some(format!(
        "(:type :response :id {} :status :ok :force {} :results {})",
        msg_id,
        bool_atom(force),
        format_autostart_outcomes(&outcomes)
    ))
}

fn handle_session_status(state: &EwwmState, msg_id: i64) -> Option<String> {
    Some(format!(
        "(:type :response :id {} :status :ok :locked {} :lock-command-configured {})",
        msg_id,
        bool_atom(state.session_locked),
        bool_atom(state.config.session_lock_command().is_some())
    ))
}

fn handle_session_lock(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.launch_session_lock() {
        Ok(detail) => Some(format!(
            "(:type :response :id {} :status :ok :detail \"{}\")",
            msg_id,
            escape_string(&detail)
        )),
        Err(reason) => Some(error_response(msg_id, &reason)),
    }
}

fn handle_session_idle_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.native_idle_status();
    Some(session_idle_response(msg_id, &status))
}

fn handle_session_idle_start(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.start_native_idle() {
        Ok(status) => Some(session_idle_response(msg_id, &status)),
        Err(reason) => Some(error_response(msg_id, &reason)),
    }
}

fn handle_session_idle_stop(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.stop_native_idle() {
        Ok(status) => Some(session_idle_response(msg_id, &status)),
        Err(reason) => Some(error_response(msg_id, &reason)),
    }
}

fn session_idle_response(msg_id: i64, status: &crate::state::NativeIdleStatus) -> String {
    let pid = status
        .pid
        .map(|pid| pid.to_string())
        .unwrap_or_else(|| "nil".to_string());
    format!(
        "(:type :response :id {} :status :ok :idle :{} :pid {} :detail \"{}\")",
        msg_id,
        status.state,
        pid,
        escape_string(&status.detail)
    )
}

fn handle_key_grab(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let key = match get_string(value, "key") {
        Some(k) => k,
        None => return Some(error_response(msg_id, "missing :key")),
    };
    debug!(key, "registering key grab");
    state.grabbed_keys.insert(key);
    Some(ok_response(msg_id))
}

fn handle_key_ungrab(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let key = match get_string(value, "key") {
        Some(k) => k,
        None => return Some(error_response(msg_id, "missing :key")),
    };
    debug!(key, "removing key grab");
    state.grabbed_keys.remove(&key);
    Some(ok_response(msg_id))
}

fn handle_vr_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let session = state.vr_state.session_state_str();
    let hmd = state.vr_state.hmd_name();
    let headless = if state.vr_state.is_headless() {
        "t"
    } else {
        "nil"
    };
    let frame_stats = state.vr_state.frame_stats_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :session :{} :hmd \"{}\" :headless {} :frame-stats {})",
        msg_id, session, escape_string(hmd), headless, frame_stats
    ))
}

fn handle_vr_diagnostics(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let diagnostics = state.vr_state.diagnostics_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :vr-diagnostics {})",
        msg_id, diagnostics
    ))
}

fn handle_vr_set_reference_space(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let space_type = get_keyword(value, "space-type");
    match space_type.as_deref() {
        Some("local") => {
            state
                .vr_state
                .set_reference_space(crate::vr::ReferenceSpaceType::Local);
            Some(ok_response(msg_id))
        }
        Some("stage") => {
            state
                .vr_state
                .set_reference_space(crate::vr::ReferenceSpaceType::Stage);
            Some(ok_response(msg_id))
        }
        Some("view") => {
            state
                .vr_state
                .set_reference_space(crate::vr::ReferenceSpaceType::View);
            Some(ok_response(msg_id))
        }
        _ => Some(error_response(
            msg_id,
            "invalid :space-type (use local, stage, or view)",
        )),
    }
}

fn handle_vr_restart(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.shutdown();
    // Re-initialize is deferred to the next frame tick
    Some(ok_response(msg_id))
}

fn handle_vr_get_frame_timing(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let timing = state.vr_state.frame_stats_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :timing {})",
        msg_id, timing
    ))
}

fn handle_vr_scene_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.scene.scene_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :scene {})",
        msg_id, sexp
    ))
}

fn handle_vr_scene_set_layout(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::scene::VrLayoutMode;

    let layout = get_keyword(value, "layout");
    let mode = match layout.as_deref() {
        Some("arc") => VrLayoutMode::Arc,
        Some("stack") => VrLayoutMode::Stack,
        Some("freeform") => VrLayoutMode::Freeform,
        Some(g) if g.starts_with("grid-") => {
            let cols = g[5..].parse::<u32>().unwrap_or(2);
            VrLayoutMode::Grid { columns: cols }
        }
        Some("grid") => VrLayoutMode::Grid {
            columns: get_int(value, "columns").unwrap_or(2) as u32,
        },
        _ => {
            return Some(error_response(
                msg_id,
                "invalid :layout (use arc, grid, stack, freeform)",
            ))
        }
    };

    state.vr_state.scene.set_layout(mode);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_set_ppu(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let ppu = match get_int(value, "ppu") {
        Some(p) if p > 0 => p as f32,
        _ => {
            return Some(error_response(
                msg_id,
                "invalid :ppu (must be positive integer)",
            ))
        }
    };

    let surface_id = get_int(value, "surface-id");
    match surface_id {
        Some(id) => state.vr_state.scene.set_surface_ppu(id as u64, ppu),
        None => state.vr_state.scene.set_global_ppu(ppu),
    }

    Some(ok_response(msg_id))
}

fn handle_vr_scene_set_background(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::VrBackground;

    let bg = get_keyword(value, "background");
    let background = match bg.as_deref() {
        Some("dark") => VrBackground::Dark,
        Some("gradient") => VrBackground::Gradient,
        Some("grid") => VrBackground::Grid,
        Some("passthrough") => VrBackground::Passthrough,
        _ => {
            return Some(error_response(
                msg_id,
                "invalid :background (use dark, gradient, grid, passthrough)",
            ))
        }
    };

    state.vr_state.scene.background = background;
    Some(ok_response(msg_id))
}

fn handle_passthrough_enable(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::scene::VrBackground;

    if let Some(blend_mode) = get_keyword(value, "blend-mode") {
        if !is_valid_passthrough_blend_mode(&blend_mode) {
            return Some(error_response(
                msg_id,
                "invalid :blend-mode (use opaque, additive, or alpha-blend)",
            ));
        }
        state.config.passthrough_blend_mode = blend_mode;
    }
    if let Some(opacity) = get_float(value, "opacity") {
        if let Err(reason) = set_passthrough_opacity(state, opacity) {
            return Some(error_response(msg_id, reason));
        }
    }

    state.vr_state.scene.background = VrBackground::Passthrough;
    handle_passthrough_status(state, msg_id)
}

fn handle_passthrough_disable(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    use crate::vr::scene::VrBackground;

    state.vr_state.scene.background = VrBackground::Dark;
    handle_passthrough_status(state, msg_id)
}

fn handle_passthrough_set_blend_mode(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let blend_mode = match get_keyword(value, "blend-mode") {
        Some(mode) if is_valid_passthrough_blend_mode(&mode) => mode,
        Some(_) => {
            return Some(error_response(
                msg_id,
                "invalid :blend-mode (use opaque, additive, or alpha-blend)",
            ))
        }
        None => return Some(error_response(msg_id, "missing :blend-mode")),
    };
    state.config.passthrough_blend_mode = blend_mode;
    handle_passthrough_status(state, msg_id)
}

fn handle_passthrough_set_opacity(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let opacity = match get_float(value, "opacity") {
        Some(opacity) => opacity,
        None => return Some(error_response(msg_id, "missing :opacity")),
    };
    if let Err(reason) = set_passthrough_opacity(state, opacity) {
        return Some(error_response(msg_id, reason));
    }
    handle_passthrough_status(state, msg_id)
}

fn handle_passthrough_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    use crate::vr::scene::VrBackground;

    let enabled = matches!(state.vr_state.scene.background, VrBackground::Passthrough);
    Some(format!(
        "(:type :response :id {} :status :ok :passthrough (:enabled {} :blend-mode :{} :opacity {:.2}))",
        msg_id,
        if enabled { "t" } else { "nil" },
        state.config.passthrough_blend_mode,
        state.config.passthrough_opacity,
    ))
}

fn set_passthrough_opacity(state: &mut EwwmState, opacity: f64) -> Result<(), &'static str> {
    if !(0.0..=1.0).contains(&opacity) {
        return Err("invalid :opacity (0.0-1.0)");
    }
    state.config.passthrough_opacity = opacity as f32;
    Ok(())
}

fn is_valid_passthrough_blend_mode(mode: &str) -> bool {
    matches!(mode, "opaque" | "additive" | "alpha-blend")
}

fn handle_vr_scene_set_projection(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::ProjectionType;

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let proj = get_keyword(value, "projection");
    let projection = match proj.as_deref() {
        Some("flat") => ProjectionType::Flat,
        Some("cylinder") => ProjectionType::Cylinder,
        _ => {
            return Some(error_response(
                msg_id,
                "invalid :projection (use flat, cylinder)",
            ))
        }
    };

    state.vr_state.scene.set_projection(surface_id, projection);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_focus(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = get_int(value, "surface-id").map(|id| id as u64);
    state.vr_state.scene.set_focus(surface_id);
    Some(ok_response(msg_id))
}

fn handle_vr_scene_move(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::scene::{Quat, Transform3D, Vec3};

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let x = get_int(value, "x").unwrap_or(0) as f32 / 100.0; // cm to meters
    let y = get_int(value, "y").unwrap_or(0) as f32 / 100.0;
    let z = get_int(value, "z").unwrap_or(-200) as f32 / 100.0;

    let transform = Transform3D {
        position: Vec3::new(x, y, z),
        rotation: Quat::IDENTITY,
        scale: Vec3::ONE,
    };

    state.vr_state.scene.set_transform(surface_id, transform);
    Some(ok_response(msg_id))
}

// ── VR Display handlers ───────────────────────────────────

fn handle_vr_display_info(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.hmd_manager.display_info_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :display {})",
        msg_id, sexp
    ))
}

fn handle_vr_display_set_mode(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::drm_lease::VrDisplayMode;

    let mode_str = get_keyword(value, "mode");
    let mode = match mode_str.as_deref().and_then(VrDisplayMode::from_str) {
        Some(m) => m,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :mode (use headset, preview, headless, off)",
            ))
        }
    };

    state.vr_state.hmd_manager.set_display_mode(mode);
    Some(ok_response(msg_id))
}

fn handle_vr_display_select_hmd(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let connector_id = match get_int(value, "connector-id") {
        Some(id) => id as u32,
        None => return Some(error_response(msg_id, "missing :connector-id")),
    };

    if state.vr_state.hmd_manager.select_hmd(connector_id) {
        Some(ok_response(msg_id))
    } else {
        Some(error_response(
            msg_id,
            &format!("connector {} not found or not an HMD", connector_id),
        ))
    }
}

fn handle_vr_display_set_refresh_rate(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let target = match get_int(value, "rate") {
        Some(r) if r > 0 => r as u32,
        _ => {
            return Some(error_response(
                msg_id,
                "invalid :rate (must be positive integer)",
            ))
        }
    };

    let actual = state.vr_state.hmd_manager.set_target_refresh_rate(target);
    Some(format!(
        "(:type :response :id {} :status :ok :target {} :actual {})",
        msg_id, target, actual
    ))
}

fn handle_vr_display_auto_detect(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mode = state.vr_state.hmd_manager.auto_detect_mode();
    Some(format!(
        "(:type :response :id {} :status :ok :mode :{})",
        msg_id,
        mode.as_str()
    ))
}

fn handle_vr_display_list_connectors(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut list = String::from("(");
    for conn in &state.vr_state.hmd_manager.connectors {
        list.push_str(&conn.to_sexp());
    }
    list.push(')');

    Some(format!(
        "(:type :response :id {} :status :ok :connectors {})",
        msg_id, list
    ))
}

// ── VR Interaction handlers ────────────────────────────────

fn handle_vr_pointer_state(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.interaction.pointer_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :pointer {})",
        msg_id, sexp
    ))
}

fn handle_vr_click(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::vr_interaction::ClickType;

    let button_str = get_keyword(value, "button").unwrap_or_else(|| "left".to_string());
    let click = match ClickType::from_str(&button_str) {
        Some(c) => c,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :button (use left, right, middle, double)",
            ))
        }
    };

    let target = state.vr_state.interaction.current_hit.map(|h| h.surface_id);
    let ptr = &state.vr_state.interaction.active_pointer;
    let (px, py) = ptr
        .as_ref()
        .map(|p| (p.pixel_x, p.pixel_y))
        .unwrap_or((0, 0));

    Some(format!(
        "(:type :response :id {} :status :ok :button :{} :surface-id {} :x {} :y {})",
        msg_id,
        click.as_str(),
        target
            .map(|id| id.to_string())
            .unwrap_or_else(|| "nil".to_string()),
        px,
        py
    ))
}

fn handle_vr_grab(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.interaction.start_grab(&state.vr_state.scene) {
        Some(sid) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {})",
            msg_id, sid
        )),
        None => Some(error_response(msg_id, "no surface under ray to grab")),
    }
}

fn handle_vr_grab_release(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.interaction.end_grab() {
        Some((sid, pos)) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {} :position (:x {:.3} :y {:.3} :z {:.3}))",
            msg_id, sid, pos.x, pos.y, pos.z
        )),
        None => Some(error_response(msg_id, "no active grab")),
    }
}

fn handle_vr_adjust_depth(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::vr_interaction::{adjust_depth, DEPTH_MAX, DEPTH_MIN};

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let delta = get_int(value, "delta").unwrap_or(-20) as f32 / 100.0; // cm to meters

    if let Some(node) = state.vr_state.scene.nodes.get_mut(&surface_id) {
        let new_z = adjust_depth(node.transform.position.z, delta, DEPTH_MIN, DEPTH_MAX);
        node.transform.position.z = new_z;
        Some(format!(
            "(:type :response :id {} :status :ok :surface-id {} :distance {:.2})",
            msg_id, surface_id, -new_z
        ))
    } else {
        Some(error_response(
            msg_id,
            &format!("unknown surface: {}", surface_id),
        ))
    }
}

fn handle_vr_set_follow(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::vr_interaction::FollowMode;

    let surface_id = match get_int(value, "surface-id") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface-id")),
    };

    let mode_str = get_keyword(value, "mode");
    let mode = match mode_str.as_deref().and_then(FollowMode::from_str) {
        Some(m) => m,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :mode (use none, lazy, sticky, locked)",
            ))
        }
    };

    state.vr_state.interaction.set_follow_mode(surface_id, mode);
    Some(ok_response(msg_id))
}

fn handle_vr_set_gaze_offset(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::scene::Vec3;

    let x = get_int(value, "x").unwrap_or(15) as f32 / 100.0;
    let y = get_int(value, "y").unwrap_or(-10) as f32 / 100.0;
    let z = get_int(value, "z").unwrap_or(-5) as f32 / 100.0;

    state.vr_state.interaction.gaze_config.offset = Vec3::new(x, y, z);
    Some(ok_response(msg_id))
}

fn handle_vr_calibrate_confirm(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let head_pose = state.vr_state.interaction.head_pose;
    let done =
        state
            .vr_state
            .interaction
            .calibration
            .record_point(crate::vr::vr_interaction::HeadPose {
                position: head_pose.position,
                rotation: head_pose.rotation,
            });

    if done {
        // Compute new offset
        if let Some(offset) = state.vr_state.interaction.calibration.compute_offset() {
            let rms = state
                .vr_state
                .interaction
                .calibration
                .rms_error_deg(&offset);
            state.vr_state.interaction.gaze_config.offset = offset;
            Some(format!(
                "(:type :response :id {} :status :ok :calibration :complete :rms-error {:.1} :offset (:x {:.3} :y {:.3} :z {:.3}))",
                msg_id, rms, offset.x, offset.y, offset.z
            ))
        } else {
            Some(error_response(
                msg_id,
                "calibration failed: insufficient data",
            ))
        }
    } else {
        let next = state.vr_state.interaction.calibration.current_target;
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :point-recorded :next {})",
            msg_id, next
        ))
    }
}

// ── Eye tracking handlers ──────────────────────────────────

fn handle_gaze_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.eye_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze {})",
        msg_id, status
    ))
}

fn handle_gaze_set_source(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let source_str = get_keyword(value, "source").unwrap_or_default();
    use crate::vr::eye_tracking::GazeSource;
    let source = if source_str == "auto" {
        None
    } else {
        match GazeSource::from_str(&source_str) {
            Some(s) => Some(s),
            None => {
                return Some(error_response(
                    msg_id,
                    &format!("unknown gaze source: {source_str}"),
                ))
            }
        }
    };
    state.vr_state.eye_tracking.set_source(source);
    Some(ok_response(msg_id))
}

fn handle_gaze_calibrate_start(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let points = get_int(value, "points").unwrap_or(5) as usize;
    state.vr_state.eye_tracking.start_calibration(points);
    Some(format!(
        "(:type :response :id {} :status :ok :calibration :started :points {})",
        msg_id, points
    ))
}

fn handle_gaze_calibrate_point(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::scene::Vec3;
    let tx = get_int(value, "target-x").unwrap_or(0) as f32 / 100.0;
    let ty = get_int(value, "target-y").unwrap_or(0) as f32 / 100.0;
    let tz = get_int(value, "target-z").unwrap_or(-200) as f32 / 100.0;
    let target = Vec3::new(tx, ty, tz);

    let gaze_dir = state
        .vr_state
        .eye_tracking
        .current_gaze
        .map(|g| g.ray.direction)
        .unwrap_or(Vec3::new(0.0, 0.0, -1.0));

    let timestamp = state
        .vr_state
        .eye_tracking
        .current_gaze
        .map(|g| g.timestamp_s)
        .unwrap_or(0.0);

    let complete = state
        .vr_state
        .eye_tracking
        .record_calibration_point(target, gaze_dir, timestamp);

    if complete {
        let rms = state
            .vr_state
            .eye_tracking
            .calibration
            .rms_error()
            .unwrap_or(0.0);
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :complete :rms-error {:.1})",
            msg_id, rms
        ))
    } else {
        let next = state
            .vr_state
            .eye_tracking
            .calibration
            .current_point_index()
            .unwrap_or(0);
        Some(format!(
            "(:type :response :id {} :status :ok :calibration :point-recorded :next {})",
            msg_id, next
        ))
    }
}

fn handle_gaze_set_visualization(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let vis_str = get_keyword(value, "mode").unwrap_or_default();
    use crate::vr::eye_tracking::GazeVisualization;
    match GazeVisualization::from_str(&vis_str) {
        Some(vis) => {
            state.vr_state.eye_tracking.set_visualization(vis);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(
            msg_id,
            &format!("unknown visualization: {vis_str}"),
        )),
    }
}

fn handle_gaze_set_smoothing(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let alpha = get_int(value, "alpha").unwrap_or(30) as f32 / 100.0;
    state.vr_state.eye_tracking.set_smoothing(alpha);
    Some(ok_response(msg_id))
}

fn handle_gaze_simulate(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let mode_str = get_keyword(value, "mode").unwrap_or_default();
    use crate::vr::eye_tracking::SimulatedGazeMode;
    if mode_str == "off" || mode_str == "nil" {
        state.vr_state.eye_tracking.set_simulate(None);
        return Some(ok_response(msg_id));
    }
    match SimulatedGazeMode::from_str(&mode_str) {
        Some(mode) => {
            state.vr_state.eye_tracking.set_simulate(Some(mode));
            Some(ok_response(msg_id))
        }
        None => Some(error_response(
            msg_id,
            &format!("unknown simulate mode: {mode_str}"),
        )),
    }
}

fn handle_gaze_health(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let h = &state.vr_state.eye_tracking.health;
    Some(format!(
        "(:type :response :id {} :status :ok :health (:rate {:.0} :expected-rate {:.0} :confidence {:.2} :tracking-lost {} :calibration-error {} :consecutive-lost {}))",
        msg_id,
        h.actual_rate_hz,
        h.expected_rate_hz,
        h.avg_confidence,
        if h.tracking_lost { "t" } else { "nil" },
        h.calibration_error_deg.map(|e| format!("{:.1}", e)).unwrap_or_else(|| "nil".to_string()),
        h.consecutive_lost_frames,
    ))
}

// ── Gaze focus handlers ────────────────────────────────────

fn handle_gaze_focus_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.gaze_focus.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_gaze_focus_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gaze_focus.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :focus {})",
        msg_id, status
    ))
}

fn handle_gaze_focus_set_policy(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::gaze_focus::FocusPolicy;

    let policy_str = get_keyword(value, "policy").unwrap_or_default();
    match FocusPolicy::from_str(&policy_str) {
        Some(policy) => {
            state.vr_state.gaze_focus.set_policy(policy);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(
            msg_id,
            &format!("invalid :policy (use gaze-only, gaze-primary, gaze-assist, disabled): {policy_str}"),
        )),
    }
}

fn handle_gaze_focus_set_dwell(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let threshold = match get_int(value, "threshold-ms") {
        Some(t) if t >= 50 && t <= 2000 => t as f64,
        _ => return Some(error_response(msg_id, "invalid :threshold-ms (50-2000)")),
    };

    state.vr_state.gaze_focus.set_dwell_threshold(threshold);
    Some(format!(
        "(:type :response :id {} :status :ok :threshold-ms {:.0})",
        msg_id, threshold
    ))
}

fn handle_gaze_focus_set_cooldown(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let cooldown = match get_int(value, "cooldown-ms") {
        Some(c) if c >= 0 && c <= 5000 => c as f64,
        _ => return Some(error_response(msg_id, "invalid :cooldown-ms (0-5000)")),
    };

    state.vr_state.gaze_focus.set_cooldown(cooldown);
    Some(format!(
        "(:type :response :id {} :status :ok :cooldown-ms {:.0})",
        msg_id, cooldown
    ))
}

fn handle_gaze_focus_analytics(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let analytics = state.vr_state.gaze_focus.analytics.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :analytics {})",
        msg_id, analytics
    ))
}

fn handle_gaze_focus_back(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.gaze_focus.focus_back() {
        Some(surface_id) => Some(format!(
            "(:type :response :id {} :status :ok :surface-id {})",
            msg_id, surface_id
        )),
        None => Some(error_response(msg_id, "no focus history available")),
    }
}

// ── Blink/wink handlers ────────────────────────────────────

fn handle_wink_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.blink_wink.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :wink {})",
        msg_id, status
    ))
}

fn handle_wink_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.blink_wink.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_wink_calibrate_start(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let trials = get_int(value, "trials").unwrap_or(10);
    state.vr_state.blink_wink.calibration.reset();
    Some(format!(
        "(:type :response :id {} :status :ok :calibration :started :trials {})",
        msg_id, trials
    ))
}

fn handle_wink_set_confidence(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let threshold = get_int(value, "threshold").unwrap_or(70) as f32 / 100.0;
    if !(0.0..=1.0).contains(&threshold) {
        return Some(error_response(msg_id, "invalid :threshold (0-100)"));
    }
    state
        .vr_state
        .blink_wink
        .blink_detector
        .confidence_threshold = threshold;
    Some(format!(
        "(:type :response :id {} :status :ok :threshold {:.2})",
        msg_id, threshold
    ))
}

// ── Gaze zone handlers ─────────────────────────────────────

fn handle_gaze_zone_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.zone_detector.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :zone {})",
        msg_id, status
    ))
}

fn handle_gaze_zone_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.zone_detector.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_gaze_zone_set_dwell(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let dwell_ms = match get_int(value, "dwell-ms") {
        Some(d) if d >= 50 && d <= 2000 => d as f64,
        _ => return Some(error_response(msg_id, "invalid :dwell-ms (50-2000)")),
    };

    state.vr_state.zone_detector.config.dwell_ms = dwell_ms;
    Some(format!(
        "(:type :response :id {} :status :ok :dwell-ms {:.0})",
        msg_id, dwell_ms
    ))
}

fn handle_gaze_zone_set_layout(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let layout = get_keyword(value, "layout").unwrap_or_else(|| "default".to_string());

    let result = if layout == "custom" {
        parse_gaze_zone_layout(value)
            .and_then(|entries| state.vr_state.zone_detector.set_custom_layout(entries))
    } else {
        state.vr_state.zone_detector.set_layout_preset(&layout)
    };

    match result {
        Ok(()) => handle_gaze_zone_config(state, msg_id),
        Err(reason) => Some(error_response(msg_id, &reason)),
    }
}

fn parse_gaze_zone_layout(
    value: &Value,
) -> Result<Vec<(crate::vr::gaze_zone::GazeZone, String)>, String> {
    let zones =
        get_value(value, "zones").ok_or_else(|| "missing :zones for custom layout".to_string())?;
    let mut entries = Vec::new();
    let mut current = zones;

    loop {
        match current {
            Value::Cons(pair) => {
                entries.push(parse_gaze_zone_layout_entry(pair.car())?);
                current = pair.cdr();
            }
            Value::Null => break,
            other => {
                entries.push(parse_gaze_zone_layout_entry(other)?);
                break;
            }
        }
    }

    Ok(entries)
}

fn parse_gaze_zone_layout_entry(
    entry: &Value,
) -> Result<(crate::vr::gaze_zone::GazeZone, String), String> {
    use crate::vr::gaze_zone::GazeZone;

    let pair = match entry {
        Value::Cons(pair) => pair,
        _ => return Err("invalid custom zone entry".to_string()),
    };
    let zone_name = value_to_keyword_string(pair.car());
    let modifier_value = match pair.cdr() {
        Value::Cons(next) => next.car(),
        Value::Null => return Err(format!("missing modifier for zone: {}", zone_name)),
        other => other,
    };
    let zone = GazeZone::from_str(&zone_name)
        .ok_or_else(|| format!("invalid gaze zone: {}", zone_name))?;
    Ok((zone, value_to_keyword_string(modifier_value)))
}

// ── Eye fatigue handlers ───────────────────────────────────

fn handle_fatigue_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.fatigue_monitor.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :fatigue {})",
        msg_id, status
    ))
}

fn handle_fatigue_config(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let config = state.vr_state.fatigue_monitor.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_fatigue_metrics(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let metrics = state.vr_state.fatigue_monitor.metrics_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :metrics {})",
        msg_id, metrics
    ))
}

fn handle_fatigue_reset(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.fatigue_monitor.teardown();
    Some(ok_response(msg_id))
}

// ── Auto-type handlers ─────────────────────────────────────

fn handle_autotype(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let text = match get_string(value, "text") {
        Some(t) => t,
        None => return Some(error_response(msg_id, "missing :text")),
    };
    let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;
    let delay_ms = get_int(value, "delay-ms");
    let verify = get_keyword(value, "verify-surface");

    if let Some(d) = delay_ms {
        state.autotype.config.delay_ms = d as u64;
    }
    if let Some(v) = verify {
        state.autotype.config.verify_surface = v != "nil";
    }

    match state.autotype.start_typing(&text, surface_id) {
        Ok(()) => Some(format!(
            "(:type :response :id {} :status :ok :chars {} :surface-id {})",
            msg_id,
            text.len(),
            surface_id
        )),
        Err(msg) => Some(error_response(msg_id, &msg)),
    }
}

fn handle_autotype_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.autotype.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :autotype {})",
        msg_id, status
    ))
}

fn handle_autotype_abort(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.autotype.abort();
    Some(ok_response(msg_id))
}

fn handle_autotype_pause(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::autotype::PauseReason;

    let reason_str = get_keyword(value, "reason").unwrap_or_else(|| "user-requested".to_string());
    let reason = match reason_str.as_str() {
        "gaze-away" => PauseReason::GazeAway,
        _ => PauseReason::UserRequested,
    };
    state.autotype.pause(reason);
    Some(ok_response(msg_id))
}

fn handle_autotype_resume(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.autotype.resume();
    Some(ok_response(msg_id))
}

fn handle_compat_command(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let command = match get_string(value, "command") {
        Some(command) => command,
        None => return Some(error_response(msg_id, "missing :command")),
    };

    match command.as_str() {
        "autotype" => handle_autotype(state, msg_id, value),
        "autotype-status" => handle_autotype_status(state, msg_id),
        "autotype-abort" => handle_autotype_abort(state, msg_id),
        "autotype-pause" => handle_autotype_pause(state, msg_id, value),
        "autotype-resume" => handle_autotype_resume(state, msg_id),
        other => Some(error_response(
            msg_id,
            &format!("unsupported compatibility command: {other}"),
        )),
    }
}

// ── Secure input handlers ──────────────────────────────────

fn handle_secure_input_mode(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let enable = get_keyword(value, "enable")
        .map(|v| v != "nil")
        .unwrap_or(true);

    if enable {
        let reason = get_string(value, "reason").unwrap_or_else(|| "ipc".to_string());
        let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;
        let timeout = get_int(value, "timeout");
        if let Some(t) = timeout {
            state.secure_input.config.auto_exit_timeout_secs = t as u64;
        }
        let now = state.clock.now();
        state.secure_input.enter(&reason, surface_id, now);
        Some(ok_response(msg_id))
    } else {
        state.secure_input.exit();
        Some(ok_response(msg_id))
    }
}

fn handle_secure_input_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let now = state.clock.now();
    let status = state.secure_input.status_sexp(now);
    Some(format!(
        "(:type :response :id {} :status :ok :secure-input {})",
        msg_id, status
    ))
}

fn handle_gaze_away_monitor(_state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let enable = get_keyword(value, "enable")
        .map(|v| v != "nil")
        .unwrap_or(true);
    let surface_id = get_int(value, "surface-id").unwrap_or(0);
    let _pause_ms = get_int(value, "pause-ms").unwrap_or(500);
    let _resume_ms = get_int(value, "resume-ms").unwrap_or(300);
    let _abort_ms = get_int(value, "abort-ms").unwrap_or(5000);

    debug!(
        enable,
        surface_id,
        "gaze-away monitor {}",
        if enable { "started" } else { "stopped" }
    );

    // Gaze-away monitoring state is tracked on the Emacs side;
    // the compositor acknowledges the request and will emit
    // gaze-target-changed events as needed.
    Some(ok_response(msg_id))
}

// ── Headless backend handlers ──────────────────────────────

fn handle_headless_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let active = if state.headless_active { "t" } else { "nil" };
    let surface_count = state.surfaces.len();
    let ipc_client_count = state.ipc_server.clients.len();
    Some(format!(
        "(:type :response :id {} :status :ok :headless {} :outputs {} :resolution \"{}x{}\" :surfaces {} :ipc-clients {})",
        msg_id,
        active,
        state.headless_output_count,
        state.headless_width,
        state.headless_height,
        surface_count,
        ipc_client_count,
    ))
}

fn handle_headless_set_resolution(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    let w = match get_int(value, "w") {
        Some(v) if v > 0 && v <= 7680 => v as i32,
        _ => return Some(error_response(msg_id, "invalid :w (must be 1-7680)")),
    };
    let h = match get_int(value, "h") {
        Some(v) if v > 0 && v <= 4320 => v as i32,
        _ => return Some(error_response(msg_id, "invalid :h (must be 1-4320)")),
    };

    state.headless_width = w;
    state.headless_height = h;
    for cfg in &mut state.output_management_state.configs {
        if cfg.name.starts_with("headless-") {
            cfg.width = w;
            cfg.height = h;
        }
    }

    debug!(w, h, "headless resolution updated");
    Some(format!(
        "(:type :response :id {} :status :ok :resolution \"{}x{}\")",
        msg_id, w, h
    ))
}

fn handle_headless_add_output(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    let new_index = state.headless_output_count;
    state.headless_output_count += 1;

    // Create the virtual output in the Smithay space
    let mode = smithay::output::Mode {
        size: (state.headless_width, state.headless_height).into(),
        refresh: 60_000,
    };
    let output = smithay::output::Output::new(
        format!("headless-{}", new_index),
        smithay::output::PhysicalProperties {
            size: (0, 0).into(),
            subpixel: smithay::output::Subpixel::Unknown,
            make: "EWWM".into(),
            model: "Headless".into(),
        },
    );
    let x_offset = (new_index as i32) * state.headless_width;
    output.change_current_state(
        Some(mode),
        Some(smithay::utils::Transform::Normal),
        None,
        Some((x_offset, 0).into()),
    );
    output.set_preferred(mode);
    state.space.map_output(&output, (x_offset, 0));
    let mut output_config =
        crate::handlers::output_management::OutputConfig::new(format!("headless-{}", new_index));
    output_config.x = x_offset;
    output_config.width = state.headless_width;
    output_config.height = state.headless_height;
    output_config.refresh = 60_000;
    state
        .output_management_state
        .upsert_detected_output(output_config);

    debug!(index = new_index, "added headless output");
    Some(format!(
        "(:type :response :id {} :status :ok :output-index {} :outputs {})",
        msg_id, new_index, state.headless_output_count
    ))
}

fn handle_headless_remove_output(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    if !state.headless_active {
        return Some(error_response(msg_id, "not running in headless mode"));
    }

    if state.headless_output_count <= 1 {
        return Some(error_response(msg_id, "cannot remove last output"));
    }

    state.headless_output_count -= 1;
    let removed_index = state.headless_output_count;

    // Find and unmap the output from the space
    let target_name = format!("headless-{}", removed_index);
    let output = state
        .space
        .outputs()
        .find(|o| o.name() == target_name)
        .cloned();
    if let Some(o) = output {
        state.space.unmap_output(&o);
    }
    state
        .output_management_state
        .remove_detected_output(&target_name);

    debug!(index = removed_index, "removed headless output");
    Some(format!(
        "(:type :response :id {} :status :ok :removed-index {} :outputs {})",
        msg_id, removed_index, state.headless_output_count
    ))
}

// ── DPMS handlers ──────────────────────────────────────────

fn handle_dpms_get(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    Some(format!(
        "(:type :response :id {} :status :ok :dpms-state \"{}\")",
        msg_id, state.dpms_state,
    ))
}

fn handle_dpms_set(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::handlers::dpms::DpmsState;

    let state_str = match get_string(value, "state") {
        Some(s) => s,
        None => return Some(error_response(msg_id, "missing :state field")),
    };

    match DpmsState::from_str_ipc(&state_str) {
        Some(new_state) => {
            let old = state.dpms_state;
            state.dpms_state = new_state;
            debug!(?old, ?new_state, "DPMS state changed");

            // Notify Emacs of the state change
            let event = format_event("dpms-changed", &[("state", &new_state.to_string())]);
            IpcServer::broadcast_event(state, &event);

            Some(format!(
                "(:type :response :id {} :status :ok :dpms-state \"{}\")",
                msg_id, new_state,
            ))
        }
        None => Some(error_response(
            msg_id,
            &format!(
                "invalid DPMS state: {} (use on/standby/suspend/off)",
                state_str
            ),
        )),
    }
}

// ── Screencopy handlers ────────────────────────────────────

fn handle_screencopy_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let count = state.screencopy_state.get_active_count();
    let frames: Vec<String> = state
        .screencopy_state
        .active_frames
        .iter()
        .map(|f| f.to_string())
        .collect();
    let frames_sexp = if frames.is_empty() {
        "nil".to_string()
    } else {
        format!("({})", frames.join(" "))
    };
    Some(format!(
        "(:type :response :id {} :status :ok :active-count {} :frame-counter {} :frames {})",
        msg_id, count, state.screencopy_state.frame_counter, frames_sexp,
    ))
}

// ── Output management handlers ─────────────────────────────

fn handle_output_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let configs: Vec<String> = state
        .output_management_state
        .get_configurations()
        .iter()
        .map(|c| c.to_sexp())
        .collect();
    let list_sexp = if configs.is_empty() {
        "nil".to_string()
    } else {
        format!("({})", configs.join(" "))
    };
    Some(format!(
        "(:type :response :id {} :status :ok :serial {} :outputs {})",
        msg_id, state.output_management_state.serial, list_sexp,
    ))
}

fn handle_output_configure(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::handlers::output_management::{OutputConfig, OutputTransform};

    let name = match get_string(value, "name") {
        Some(n) => n,
        None => return Some(error_response(msg_id, "missing :name field")),
    };

    let test_only = get_bool(value, "test-only").unwrap_or(false);

    // Build config from IPC values, defaulting to existing config if present.
    let base = state
        .output_management_state
        .configs
        .iter()
        .find(|c| c.name == name)
        .cloned()
        .unwrap_or_else(|| OutputConfig::new(name.clone()));

    let config = OutputConfig {
        name,
        enabled: get_bool(value, "enabled").unwrap_or(base.enabled),
        x: get_int(value, "x").map(|v| v as i32).unwrap_or(base.x),
        y: get_int(value, "y").map(|v| v as i32).unwrap_or(base.y),
        width: get_int(value, "width")
            .map(|v| v as i32)
            .unwrap_or(base.width),
        height: get_int(value, "height")
            .map(|v| v as i32)
            .unwrap_or(base.height),
        refresh: get_int(value, "refresh")
            .map(|v| v as i32)
            .unwrap_or(base.refresh),
        scale: get_float(value, "scale").unwrap_or(base.scale),
        transform: get_string(value, "transform")
            .and_then(|s| OutputTransform::from_str_ipc(&s))
            .unwrap_or(base.transform),
    };

    if test_only {
        match state.output_management_state.test_config(&config) {
            Ok(serial) => Some(format!(
                "(:type :response :id {} :status :ok :serial {} :test t)",
                msg_id, serial,
            )),
            Err(e) => Some(error_response(msg_id, &e)),
        }
    } else {
        match state.output_management_state.apply_config(config) {
            Ok(serial) => {
                // Notify Emacs of the configuration change.
                let event = format_event("output-configured", &[("serial", &serial.to_string())]);
                IpcServer::broadcast_event(state, &event);

                Some(format!(
                    "(:type :response :id {} :status :ok :serial {})",
                    msg_id, serial,
                ))
            }
            Err(e) => Some(error_response(msg_id, &e)),
        }
    }
}

// ── Pointer constraints handlers ───────────────────────────

fn handle_pointer_constraints_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let active = if state.pointer_constraint_active() {
        "t"
    } else {
        "nil"
    };
    Some(format!(
        "(:type :response :id {} :status :ok :constraint-active {})",
        msg_id, active,
    ))
}

// ── Gaze scroll handlers ───────────────────────────────────

fn handle_gaze_scroll_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gaze_scroll.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze-scroll {})",
        msg_id, status
    ))
}

fn handle_gaze_scroll_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(enabled_str) = get_keyword(value, "enable") {
        state.vr_state.gaze_scroll.config.enabled = enabled_str != "nil";
    }
    if let Some(edge) = get_int(value, "edge-pct") {
        let pct = (edge as f32 / 100.0).clamp(0.01, 0.50);
        state.vr_state.gaze_scroll.config.edge_pct = pct;
    }
    if let Some(speed) = get_int(value, "speed") {
        let s = (speed as f32).clamp(0.1, 50.0);
        state.vr_state.gaze_scroll.config.speed = s;
    }
    if let Some(delay) = get_int(value, "activation-delay-ms") {
        state.vr_state.gaze_scroll.config.activation_delay_ms = (delay as f64).max(0.0);
    }
    if let Some(horiz_str) = get_keyword(value, "horizontal") {
        state.vr_state.gaze_scroll.config.horizontal = horiz_str != "nil";
    }

    let status = state.vr_state.gaze_scroll.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gaze-scroll {})",
        msg_id, status
    ))
}

fn handle_gaze_scroll_set_speed(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let speed = match get_int(value, "speed") {
        Some(s) if s > 0 => s as f32,
        _ => return Some(error_response(msg_id, "invalid :speed (must be positive)")),
    };

    state.vr_state.gaze_scroll.config.speed = speed.clamp(0.1, 50.0);
    Some(format!(
        "(:type :response :id {} :status :ok :speed {:.1})",
        msg_id, state.vr_state.gaze_scroll.config.speed
    ))
}

// ── Link hint handlers ─────────────────────────────────────

fn handle_link_hints_load(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let json = match get_string(value, "hints") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :hints (JSON array)")),
    };
    let surface_id = get_int(value, "surface-id").unwrap_or(0) as u64;

    match state.vr_state.link_hints.load_hints(&json, surface_id) {
        Ok(count) => Some(format!(
            "(:type :response :id {} :status :ok :hint-count {} :surface-id {})",
            msg_id, count, surface_id
        )),
        Err(e) => Some(error_response(msg_id, &format!("hint load failed: {}", e))),
    }
}

fn handle_link_hints_confirm(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.link_hints.confirm() {
        Some(crate::vr::link_hints::LinkHintEvent::Confirmed { hint_id, url }) => Some(format!(
            "(:type :response :id {} :status :ok :hint-id {} :url \"{}\")",
            msg_id,
            hint_id,
            escape_string(&url)
        )),
        _ => Some(error_response(msg_id, "no hint currently highlighted")),
    }
}

fn handle_link_hints_clear(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.link_hints.clear();
    Some(ok_response(msg_id))
}

fn handle_link_hints_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.link_hints.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :link-hints {})",
        msg_id, status
    ))
}

// ── Hand tracking handlers ─────────────────────────────────

fn handle_hand_tracking_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.hand_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hand-tracking {})",
        msg_id, status
    ))
}

fn handle_hand_tracking_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled").or_else(|| get_bool(value, "enable")) {
        state.vr_state.hand_tracking.config.enabled = enabled;
    }
    if let Some(min_conf) = get_float(value, "min-confidence") {
        state.vr_state.hand_tracking.config.min_confidence = min_conf as f32;
    }
    if let Some(smoothing) = get_float(value, "smoothing") {
        state.vr_state.hand_tracking.config.smoothing = smoothing as f32;
    }
    if let Some(prediction) = get_float(value, "prediction-ms") {
        state.vr_state.hand_tracking.config.prediction_ms = prediction as f32;
    }

    let status = state.vr_state.hand_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hand-tracking {})",
        msg_id, status
    ))
}

fn handle_hand_tracking_toggle(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let enabled = get_bool(value, "enable")
        .or_else(|| get_bool(value, "enabled"))
        .unwrap_or(!state.vr_state.hand_tracking.config.enabled);
    state.vr_state.hand_tracking.config.enabled = enabled;

    let status = state.vr_state.hand_tracking.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hand-tracking {})",
        msg_id, status
    ))
}

fn handle_hand_tracking_joint(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let joint_name = match get_string(value, "joint") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint")),
    };

    match state
        .vr_state
        .hand_tracking
        .get_joint(&hand_str, &joint_name)
    {
        Some(joint) => {
            let pos = joint.position;
            let rot = joint.orientation;
            Some(format!(
                "(:type :response :id {} :status :ok :hand :{} :joint \"{}\" :position (:x {:.4} :y {:.4} :z {:.4}) :orientation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}) :radius {:.4})",
                msg_id, hand_str, escape_string(&joint_name),
                pos.x, pos.y, pos.z,
                rot.x, rot.y, rot.z, rot.w,
                joint.radius,
            ))
        }
        None => Some(error_response(
            msg_id,
            &format!("joint not found: {} {}", hand_str, joint_name),
        )),
    }
}

fn handle_hand_tracking_skeleton(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };

    match state.vr_state.hand_tracking.get_skeleton(&hand_str) {
        Some(joints) => {
            let mut sexp = String::from("(");
            for joint in &joints {
                let pos = joint.position.clone();
                let rot = joint.orientation.clone();
                sexp.push_str(&format!(
                    "(:name \"{}\" :position (:x {:.4} :y {:.4} :z {:.4}) :orientation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}) :radius {:.4})",
                    escape_string(&joint.name),
                    pos.x, pos.y, pos.z,
                    rot.x, rot.y, rot.z, rot.w,
                    joint.radius,
                ));
            }
            sexp.push(')');
            Some(format!(
                "(:type :response :id {} :status :ok :hand :{} :joint-count {} :joints {})",
                msg_id,
                hand_str,
                joints.len(),
                sexp
            ))
        }
        None => Some(error_response(
            msg_id,
            &format!("hand not tracked: {}", hand_str),
        )),
    }
}

fn handle_hand_tracking_distance(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let hand_str = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let joint_a = match get_string(value, "joint-a") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint-a")),
    };
    let joint_b = match get_string(value, "joint-b") {
        Some(j) => j,
        None => return Some(error_response(msg_id, "missing :joint-b")),
    };

    match state.vr_state.hand_tracking.joint_distance_by_name(&hand_str, &joint_a, &joint_b) {
        Some(distance) => Some(format!(
            "(:type :response :id {} :status :ok :hand :{} :joint-a \"{}\" :joint-b \"{}\" :distance {:.4})",
            msg_id, hand_str,
            escape_string(&joint_a),
            escape_string(&joint_b),
            distance,
        )),
        None => Some(error_response(
            msg_id,
            &format!("could not compute distance: {} {} <-> {}", hand_str, joint_a, joint_b),
        )),
    }
}

// ── Gesture handlers ───────────────────────────────────────

fn handle_gesture_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.gesture.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture {})",
        msg_id, status
    ))
}

fn handle_gesture_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(pinch) = get_float(value, "pinch-threshold") {
        state.vr_state.gesture.config.pinch_threshold_m = pinch as f32;
    }
    if let Some(grab) = get_float(value, "grab-threshold") {
        state.vr_state.gesture.config.grab_threshold_m = grab as f32;
    }
    if let Some(swipe) = get_float(value, "swipe-min-velocity") {
        state.vr_state.gesture.config.swipe_min_velocity = swipe as f32;
    }
    if let Some(debounce) = get_float(value, "debounce-ms") {
        state.vr_state.gesture.config.debounce_ms = debounce;
    }
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.gesture.config.enabled = enabled;
    }

    let status = state.vr_state.gesture.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture {})",
        msg_id, status
    ))
}

fn handle_gesture_bind(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let gesture = match get_string(value, "gesture") {
        Some(g) => g,
        None => return Some(error_response(msg_id, "missing :gesture")),
    };
    let hand = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };
    let action = match get_string(value, "action") {
        Some(a) => a,
        None => return Some(error_response(msg_id, "missing :action")),
    };

    state.vr_state.gesture.add_binding(&gesture, &hand, &action);
    let count = state.vr_state.gesture.binding_count();
    Some(format!(
        "(:type :response :id {} :status :ok :gesture \"{}\" :hand :{} :action \"{}\" :bindings {})",
        msg_id,
        escape_string(&gesture),
        hand,
        escape_string(&action),
        count,
    ))
}

fn handle_gesture_unbind(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let gesture = match get_string(value, "gesture") {
        Some(g) => g,
        None => return Some(error_response(msg_id, "missing :gesture")),
    };
    let hand = match get_string(value, "hand") {
        Some(h) => h,
        None => return Some(error_response(msg_id, "missing :hand (left or right)")),
    };

    let removed = state.vr_state.gesture.remove_binding(&gesture, &hand);
    if removed {
        Some(ok_response(msg_id))
    } else {
        Some(error_response(
            msg_id,
            &format!("no binding for gesture {} hand {}", gesture, hand),
        ))
    }
}

fn handle_gesture_bindings(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let bindings = state.vr_state.gesture.bindings_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :bindings {})",
        msg_id, bindings
    ))
}

// ── Virtual keyboard handlers ──────────────────────────────

fn handle_keyboard_show(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.show();
    Some(ok_response(msg_id))
}

fn handle_keyboard_hide(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.hide();
    Some(ok_response(msg_id))
}

fn handle_keyboard_toggle(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.virtual_keyboard.toggle();
    let visible = if state.vr_state.virtual_keyboard.visible {
        "t"
    } else {
        "nil"
    };
    Some(format!(
        "(:type :response :id {} :status :ok :visible {})",
        msg_id, visible
    ))
}

fn handle_keyboard_layout(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let layout_str = match get_string(value, "layout") {
        Some(l) => l,
        None => return Some(error_response(msg_id, "missing :layout")),
    };

    match state
        .vr_state
        .virtual_keyboard
        .set_layout_by_name(&layout_str)
    {
        Ok(()) => Some(format!(
            "(:type :response :id {} :status :ok :layout \"{}\")",
            msg_id,
            escape_string(&layout_str)
        )),
        Err(msg) => Some(error_response(
            msg_id,
            &format!("invalid :layout (use qwerty, dvorak, colemak): {}", msg),
        )),
    }
}

fn handle_keyboard_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.virtual_keyboard.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :keyboard {})",
        msg_id, status
    ))
}

// ── BCI handlers ───────────────────────────────────────────

fn handle_bci_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :bci {})",
        msg_id, status
    ))
}

fn handle_bci_start(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.bci.start() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_bci_stop(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.bci.stop() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_bci_restart(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.bci.restart() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_bci_signal_quality(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let quality = state.vr_state.bci.signal_quality_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :quality {})",
        msg_id, quality
    ))
}

fn handle_bci_hardware_check(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.status_sexp();
    let quality = state.vr_state.bci.signal_quality_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :hardware (:acquisition :unproven :status {} :quality {}))",
        msg_id, status, quality
    ))
}

fn handle_bci_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(board) = get_string(value, "board") {
        if let Err(e) = state.vr_state.bci.set_board_by_name(&board) {
            return Some(error_response(msg_id, &e));
        }
    }
    if let Some(port) = get_string(value, "serial-port") {
        state.vr_state.bci.set_serial_port(&port);
    }
    if let Some(freq) = get_float(value, "notch-frequency") {
        if let Err(e) = state.vr_state.bci.set_notch(freq) {
            return Some(error_response(msg_id, &e));
        }
    }
    if let Some(enabled) = get_bool(value, "artifact-rejection") {
        state.vr_state.bci.config.artifact_rejection = enabled;
    }
    if let Some(days) = get_int(value, "data-retention-days") {
        state.vr_state.bci.config.data_retention_days = days as u32;
    }
    Some(ok_response(msg_id))
}

fn handle_bci_inject_synthetic(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let event_type = match get_string(value, "event") {
        Some(t) => t,
        None => return Some(error_response(msg_id, "missing :event")),
    };
    // Collect optional params
    let mut params = Vec::new();
    if let Some(v) = get_string(value, "amplitude") {
        params.push(("amplitude".to_string(), v));
    }
    if let Some(v) = get_string(value, "latency") {
        params.push(("latency".to_string(), v));
    }
    if let Some(v) = get_string(value, "frequency") {
        params.push(("frequency".to_string(), v));
    }
    if let Some(v) = get_string(value, "class") {
        params.push(("class".to_string(), v));
    }
    match state
        .vr_state
        .bci
        .inject_synthetic_event(&event_type, &params)
    {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_bci_data_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let data = state.vr_state.bci.data_list();
    Some(format!(
        "(:type :response :id {} :status :ok :sessions {})",
        msg_id, data
    ))
}

fn handle_bci_data_delete(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let session_id = match get_string(value, "session-id") {
        Some(s) => s,
        None => return Some(error_response(msg_id, "missing :session-id")),
    };
    match state.vr_state.bci.data_delete(&session_id) {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

// ── BCI attention handlers ──────────────────────────────────

fn handle_bci_attention_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.attention.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :attention {})",
        msg_id, status
    ))
}

fn handle_bci_attention_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.bci.attention.config.enabled = enabled;
    }
    if let Some(name) = get_string(value, "threshold-name") {
        if let Some(val) = get_float(value, "threshold-value") {
            if let Err(e) = state.vr_state.bci.attention.set_threshold(&name, val) {
                return Some(error_response(msg_id, &e));
            }
        }
    }
    let config = state.vr_state.bci.attention.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_bci_attention_toggle(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let enabled = get_bool(value, "enable")
        .or_else(|| get_bool(value, "enabled"))
        .unwrap_or(!state.vr_state.bci.attention.config.enabled);
    state.vr_state.bci.attention.config.enabled = enabled;
    handle_bci_attention_status(state, msg_id)
}

fn handle_bci_dnd_compat(state: &mut EwwmState, msg_id: i64, enabled: bool) -> Option<String> {
    let status = state.vr_state.bci.attention.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :dnd (:requested {} :native nil :reason \"{}\" :attention {}))",
        msg_id,
        if enabled { "t" } else { "nil" },
        "notification/DND policy is not native yet",
        status,
    ))
}

fn handle_bci_attention_calibrate_start(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.bci.attention.start_calibration();
    Some(ok_response(msg_id))
}

fn handle_bci_attention_calibrate_finish(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.bci.attention.finish_calibration() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

// ── BCI SSVEP handlers ─────────────────────────────────────

fn handle_bci_ssvep_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.ssvep.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :ssvep {})",
        msg_id, status
    ))
}

fn handle_bci_ssvep_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.bci.ssvep.config.enabled = enabled;
    }
    if let Some(window) = get_float(value, "window-seconds") {
        state.vr_state.bci.ssvep.config.window_seconds = window;
    }
    if let Some(snr) = get_float(value, "min-snr-db") {
        state.vr_state.bci.ssvep.config.min_snr_db = snr;
    }
    if let Some(conf) = get_float(value, "min-confidence") {
        state.vr_state.bci.ssvep.config.min_confidence = conf;
    }
    let config = state.vr_state.bci.ssvep.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_bci_ssvep_configure_compat(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(window) = get_float(value, "window").or_else(|| get_float(value, "window-seconds"))
    {
        state.vr_state.bci.ssvep.config.window_seconds = window;
    }
    if let Some(conf) = get_float(value, "min-confidence") {
        state.vr_state.bci.ssvep.config.min_confidence = conf;
    }
    if let Some(snr) = get_float(value, "min-snr-db") {
        state.vr_state.bci.ssvep.config.min_snr_db = snr;
    }
    if let Some(freqs_value) = get_value(value, "frequencies") {
        match parse_ssvep_frequencies(freqs_value) {
            Some(freqs) if !freqs.is_empty() => {
                state.vr_state.bci.ssvep.set_frequencies(freqs);
            }
            Some(_) => {}
            None => return Some(error_response(msg_id, "invalid :frequencies alist")),
        }
    }

    let config = state.vr_state.bci.ssvep.config_sexp();
    let ignored_cooldown = if get_int(value, "cooldown-ms").is_some() {
        "(:cooldown-ms)"
    } else {
        "()"
    };
    Some(format!(
        "(:type :response :id {} :status :ok :config {} :ignored-fields {})",
        msg_id, config, ignored_cooldown
    ))
}

fn handle_bci_ssvep_start(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.bci.ssvep.start();
    Some(ok_response(msg_id))
}

fn handle_bci_ssvep_stop(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.bci.ssvep.stop();
    Some(ok_response(msg_id))
}

// ── BCI P300 handlers ───────────────────────────────────────

fn handle_bci_p300_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.p300.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :p300 {})",
        msg_id, status
    ))
}

fn handle_bci_p300_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.bci.p300.config.enabled = enabled;
    }
    if let Some(reps) = get_int(value, "repetitions") {
        state.vr_state.bci.p300.config.repetitions = reps as u32;
    }
    if let Some(soa) = get_float(value, "soa-ms") {
        state.vr_state.bci.p300.config.soa_ms = soa;
    }
    if let Some(conf) = get_float(value, "min-confidence") {
        state.vr_state.bci.p300.config.min_confidence = conf;
    }
    let config = state.vr_state.bci.p300.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_bci_p300_start(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(reps) = get_int(value, "repetitions") {
        state.vr_state.bci.p300.config.repetitions = reps as u32;
    }
    if let Some(soa) = get_float(value, "soa-ms") {
        state.vr_state.bci.p300.config.soa_ms = soa;
    }
    if let Some(duration) =
        get_float(value, "flash-duration-ms").or_else(|| get_float(value, "stimulus-duration-ms"))
    {
        state.vr_state.bci.p300.config.stimulus_duration_ms = duration;
    }
    if let Some(conf) = get_float(value, "min-confidence") {
        state.vr_state.bci.p300.config.min_confidence = conf;
    }
    let num_targets = get_int(value, "num-targets")
        .map(|n| n as usize)
        .or_else(|| get_value(value, "targets").map(|targets| flatten_list(targets).len()))
        .unwrap_or(6);
    state.vr_state.bci.p300.start(num_targets);
    Some(ok_response(msg_id))
}

fn handle_bci_p300_stop(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.bci.p300.stop();
    Some(ok_response(msg_id))
}

// ── BCI motor imagery handlers ──────────────────────────────

fn handle_bci_mi_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.motor_imagery.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :motor-imagery {})",
        msg_id, status
    ))
}

fn handle_bci_mi_config(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.bci.motor_imagery.config.enabled = enabled;
    }
    if let Some(conf) = get_float(value, "min-confidence") {
        state.vr_state.bci.motor_imagery.config.min_confidence = conf;
    }
    let config = state.vr_state.bci.motor_imagery.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

fn handle_bci_mi_toggle(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let enabled = get_bool(value, "enable")
        .or_else(|| get_bool(value, "enabled"))
        .unwrap_or(!state.vr_state.bci.motor_imagery.config.enabled);
    state.vr_state.bci.motor_imagery.config.enabled = enabled;
    handle_bci_mi_status(state, msg_id)
}

fn handle_bci_mi_calibrate_start(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.bci.motor_imagery.start_calibration();
    Some(ok_response(msg_id))
}

fn handle_bci_mi_calibrate_finish(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.bci.motor_imagery.finish_calibration() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

// ── BCI fatigue EEG handlers ────────────────────────────────

fn handle_bci_fatigue_eeg_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.bci.fatigue_eeg.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :fatigue-eeg {})",
        msg_id, status
    ))
}

fn handle_bci_fatigue_eeg_config(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if let Some(enabled) = get_bool(value, "enabled") {
        state.vr_state.bci.fatigue_eeg.config.enabled = enabled;
    }
    if let Some(mild) = get_float(value, "mild-threshold") {
        state.vr_state.bci.fatigue_eeg.config.mild_threshold = mild;
    }
    if let Some(moderate) = get_float(value, "moderate-threshold") {
        state.vr_state.bci.fatigue_eeg.config.moderate_threshold = moderate;
    }
    if let Some(severe) = get_float(value, "severe-threshold") {
        state.vr_state.bci.fatigue_eeg.config.severe_threshold = severe;
    }
    if let Some(auto_save) = get_bool(value, "auto-save-on-severe") {
        state.vr_state.bci.fatigue_eeg.config.auto_save_on_severe = auto_save;
    }
    let config = state.vr_state.bci.fatigue_eeg.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :config {})",
        msg_id, config
    ))
}

// ── IPC recording handlers ─────────────────────────────────

fn handle_ipc_record_start(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let session_name = get_string(value, "session-name");
    state.ipc_server.recorder.start(session_name);
    Some(format!("(:type :response :id {} :status :ok)", msg_id))
}

fn handle_ipc_record_stop(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.ipc_server.recorder.stop();
    let recording = state.ipc_server.recorder.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :recording {})",
        msg_id, recording
    ))
}

fn handle_ipc_record_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.ipc_server.recorder.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :recorder {})",
        msg_id, status
    ))
}

// ── IPC security handlers ────────────────────────────────

fn handle_ipc_client_info(state: &mut EwwmState, client_id: u64, msg_id: i64) -> Option<String> {
    if let Some(client) = state.ipc_server.clients.get(&client_id) {
        let uid = client
            .peer_uid
            .map(|u| u.to_string())
            .unwrap_or_else(|| "nil".to_string());
        let pid = client
            .peer_pid
            .map(|p| p.to_string())
            .unwrap_or_else(|| "nil".to_string());
        let rate = client.rate_limiter.max_per_second;
        Some(format!(
            "(:type :response :id {} :status :ok :client-id {} :peer-uid {} :peer-pid {} :authenticated t :rate-limit {})",
            msg_id, client_id, uid, pid, rate
        ))
    } else {
        Some(error_response(msg_id, "client not found"))
    }
}

fn handle_ipc_rate_limit(
    state: &mut EwwmState,
    client_id: u64,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let new_limit = match get_int(value, "limit") {
        Some(n) if n > 0 && n <= 10000 => n as u32,
        Some(_) => return Some(error_response(msg_id, "limit must be 1-10000")),
        None => return Some(error_response(msg_id, "missing :limit parameter")),
    };

    if let Some(client) = state.ipc_server.clients.get_mut(&client_id) {
        client.rate_limiter.max_per_second = new_limit;
        debug!(client_id, new_limit, "rate limit updated");
    }
    Some(ok_response(msg_id))
}

fn handle_input_latency_probe(msg_id: i64, value: &Value) -> Option<String> {
    let client_timestamp = get_int(value, "timestamp")
        .map(|timestamp| timestamp.to_string())
        .unwrap_or_else(|| "nil".to_string());
    let server_timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_millis())
        .unwrap_or(0);

    Some(format!(
        "(:type :response :id {} :status :ok :client-timestamp {} :server-timestamp {})",
        msg_id, client_timestamp, server_timestamp
    ))
}

// ── VR follow mode handlers ──────────────────────────────

fn handle_vr_follow_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.follow_mode.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :follow {})",
        msg_id, status
    ))
}

fn handle_vr_follow_set_policy(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::follow_mode::FollowPolicy;

    let policy_str = get_keyword(value, "policy");
    match policy_str.as_deref().and_then(FollowPolicy::from_str) {
        Some(policy) => {
            state.vr_state.follow_mode.set_policy(policy);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(
            msg_id,
            "invalid :policy (use disabled, focused-only, grab-all, or threshold-only)",
        )),
    }
}

fn handle_follow_configure(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(policy_str) = get_keyword(value, "policy") {
        use crate::vr::follow_mode::FollowPolicy;
        let policy = match FollowPolicy::from_str(&policy_str) {
            Some(policy) => policy,
            None => {
                return Some(error_response(
                    msg_id,
                    "invalid :policy (use disabled, focused-only, grab-all, or threshold-only)",
                ))
            }
        };
        state.vr_state.follow_mode.set_policy(policy);
    }

    if let Some(h_fov) = get_float(value, "h-fov") {
        if !(1.0..=180.0).contains(&h_fov) {
            return Some(error_response(msg_id, "invalid :h-fov (1-180)"));
        }
        state.vr_state.follow_mode.config.h_fov_threshold = h_fov as f32;
    }

    if let Some(v_fov) = get_float(value, "v-fov") {
        if !(1.0..=180.0).contains(&v_fov) {
            return Some(error_response(msg_id, "invalid :v-fov (1-180)"));
        }
        state.vr_state.follow_mode.config.v_fov_threshold = v_fov as f32;
    }

    if let Some(speed) = get_float(value, "speed") {
        if !(0.0..=1.0).contains(&speed) {
            return Some(error_response(msg_id, "invalid :speed (0.0-1.0)"));
        }
        state.vr_state.follow_mode.config.follow_speed = speed as f32;
    }

    if let Some(distance) = get_float(value, "distance") {
        if !(0.1..=20.0).contains(&distance) {
            return Some(error_response(msg_id, "invalid :distance (0.1-20.0)"));
        }
        state.vr_state.follow_mode.config.follow_distance = distance as f32;
    }

    if let Some(suppress) = get_bool(value, "suppress-reading") {
        state.vr_state.follow_mode.config.suppress_during_reading = suppress;
    }

    handle_vr_follow_status(state, msg_id)
}

fn handle_vr_follow_recenter(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let head_pos = state.vr_state.interaction.head_pose.position;
    let head_rot = state.vr_state.interaction.head_pose.rotation;
    let vr = &mut state.vr_state;
    vr.follow_mode.recenter(head_pos, head_rot, &mut vr.scene);
    Some(ok_response(msg_id))
}

fn handle_vr_follow_grab_all(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let head_pos = state.vr_state.interaction.head_pose.position;
    let head_rot = state.vr_state.interaction.head_pose.rotation;
    let vr = &mut state.vr_state;
    vr.follow_mode.grab_all(&mut vr.scene, head_pos, head_rot);
    Some(ok_response(msg_id))
}

fn handle_focus_routing_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let focus = state.vr_state.gaze_focus.status_sexp();
    let config = state.vr_state.gaze_focus.config_sexp();
    let mode = state.vr_state.gaze_focus.policy.as_str();
    let dwell_ms = state.vr_state.gaze_focus.config.threshold_ms;
    Some(format!(
        "(:type :response :id {} :status :ok :routing (:mode :{} :dwell-ms {:.0} :focus {} :config {}))",
        msg_id, mode, dwell_ms, focus, config
    ))
}

fn handle_focus_routing_set_mode(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::gaze_focus::FocusPolicy;

    let mode = get_keyword(value, "mode")
        .or_else(|| get_keyword(value, "policy"))
        .unwrap_or_default();
    match FocusPolicy::from_str(&mode) {
        Some(policy) => {
            state.vr_state.gaze_focus.set_policy(policy);
            handle_focus_routing_status(state, msg_id)
        }
        None => Some(error_response(
            msg_id,
            &format!("invalid :mode (use gaze-only, gaze-primary, gaze-assist, disabled): {mode}"),
        )),
    }
}

fn handle_focus_routing_configure(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    if get_keyword(value, "mode").is_some() || get_keyword(value, "policy").is_some() {
        if let Some(response) = handle_focus_routing_set_mode(state, msg_id, value) {
            if response.contains(":status :error") {
                return Some(response);
            }
        }
    }

    if get_int(value, "threshold-ms").is_some() || get_int(value, "dwell-ms").is_some() {
        let threshold = get_int(value, "threshold-ms").or_else(|| get_int(value, "dwell-ms"));
        let threshold = match threshold {
            Some(t) if (50..=2000).contains(&t) => t as f64,
            _ => return Some(error_response(msg_id, "invalid dwell threshold (50-2000)")),
        };
        state.vr_state.gaze_focus.set_dwell_threshold(threshold);
    }

    handle_focus_routing_status(state, msg_id)
}

// ── VR Transient Chains ──────────────────────────────────

fn handle_vr_transient_add(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::transient_3d::TransientPlacement;

    let child_id = match get_int(value, "child") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :child")),
    };
    let parent_id = match get_int(value, "parent") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :parent")),
    };
    let placement = match get_keyword(value, "placement") {
        Some(placement) => match TransientPlacement::from_str(&placement) {
            Some(parsed) => parsed,
            None => {
                return Some(error_response(
                    msg_id,
                    "invalid :placement (use front, above, below, or auto)",
                ))
            }
        },
        None => state.vr_state.transient_chains.default_placement,
    };

    match state
        .vr_state
        .transient_chains
        .add_transient(child_id, parent_id, placement)
    {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_vr_transient_remove(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let child_id = match get_int(value, "child") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :child")),
    };
    state.vr_state.transient_chains.remove_transient(child_id);
    Some(ok_response(msg_id))
}

fn handle_vr_transient_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.transient_chains.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :transients {})",
        msg_id, sexp
    ))
}

fn handle_transient_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let transients = state.vr_state.transient_chains.to_sexp();
    let config = state.vr_state.transient_chains.config_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :transients {} :config {})",
        msg_id, transients, config
    ))
}

fn handle_transient_configure(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    if let Some(offset) = get_float(value, "z-offset") {
        if let Err(reason) = set_transient_z_offset(state, offset) {
            return Some(error_response(msg_id, reason));
        }
    }

    if let Some(max_depth) = get_int(value, "max-depth") {
        if !(1..=32).contains(&max_depth) {
            return Some(error_response(msg_id, "invalid :max-depth (1-32)"));
        }
        state.vr_state.transient_chains.max_depth = max_depth as u32;
    }

    if let Some(placement) = get_keyword(value, "placement") {
        if let Err(reason) = set_transient_placement(state, &placement) {
            return Some(error_response(msg_id, reason));
        }
    }

    handle_transient_status(state, msg_id)
}

fn handle_transient_set_offset(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let offset = match get_float(value, "z-offset") {
        Some(offset) => offset,
        None => return Some(error_response(msg_id, "missing :z-offset")),
    };
    if let Err(reason) = set_transient_z_offset(state, offset) {
        return Some(error_response(msg_id, reason));
    }
    handle_transient_status(state, msg_id)
}

fn handle_transient_set_placement(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let placement = match get_keyword(value, "placement") {
        Some(placement) => placement,
        None => return Some(error_response(msg_id, "missing :placement")),
    };
    if let Err(reason) = set_transient_placement(state, &placement) {
        return Some(error_response(msg_id, reason));
    }
    handle_transient_status(state, msg_id)
}

fn set_transient_z_offset(state: &mut EwwmState, offset: f64) -> Result<(), &'static str> {
    if !(0.01..=1.0).contains(&offset) {
        return Err("invalid :z-offset (0.01-1.0)");
    }
    state.vr_state.transient_chains.z_offset_per_level = offset as f32;
    Ok(())
}

fn set_transient_placement(state: &mut EwwmState, placement: &str) -> Result<(), &'static str> {
    use crate::vr::transient_3d::TransientPlacement;

    let placement = TransientPlacement::from_str(placement)
        .ok_or("invalid :placement (use front, above, below, or auto)")?;
    state.vr_state.transient_chains.default_placement = placement;
    Ok(())
}

// ── Compositor-local spatial anchors ────────────────────────

fn handle_anchor_create(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let name = match get_string(value, "name") {
        Some(name) if !name.trim().is_empty() => name,
        _ => return Some(error_response(msg_id, "missing :name")),
    };
    let surface_id = match get_int(value, "surface-id") {
        Some(id) if id >= 0 => id as u64,
        _ => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let transform = match state.vr_state.scene.nodes.get(&surface_id) {
        Some(node) => node.transform,
        None => {
            return Some(error_response(
                msg_id,
                "surface has no native VR scene node to anchor",
            ))
        }
    };

    let anchor = state
        .vr_state
        .anchors
        .create_or_update(name, surface_id, transform)
        .clone();
    broadcast_anchor_event(state, "anchor-created", &anchor);
    Some(anchor_response(msg_id, &anchor))
}

fn handle_anchor_restore(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::anchor::transform_from_parts;

    let name = match get_string(value, "name") {
        Some(name) if !name.trim().is_empty() => name,
        _ => return Some(error_response(msg_id, "missing :name")),
    };
    let surface_id = match get_int(value, "surface-id") {
        Some(id) if id >= 0 => id as u64,
        _ => return Some(error_response(msg_id, "missing :surface-id")),
    };
    let position = match parse_vec3(value, "position") {
        Ok(position) => position,
        Err(reason) => return Some(error_response(msg_id, &reason)),
    };
    let rotation = match parse_quat(value, "rotation") {
        Ok(rotation) => rotation,
        Err(reason) => return Some(error_response(msg_id, &reason)),
    };
    let transform = transform_from_parts(position, rotation);

    if state.vr_state.scene.nodes.contains_key(&surface_id) {
        state.vr_state.scene.set_transform(surface_id, transform);
    }

    let anchor = state
        .vr_state
        .anchors
        .create_or_update(name, surface_id, transform)
        .clone();
    broadcast_anchor_event(state, "anchor-created", &anchor);
    Some(anchor_response(msg_id, &anchor))
}

fn handle_anchor_remove(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let name = match get_string(value, "name") {
        Some(name) if !name.trim().is_empty() => name,
        _ => return Some(error_response(msg_id, "missing :name")),
    };

    match state.vr_state.anchors.remove(&name) {
        Some(anchor) => {
            let event = format!(
                "(:type :event :event :anchor-removed :name \"{}\" :surface-id {})",
                escape_string(&anchor.name),
                anchor.surface_id,
            );
            IpcServer::broadcast_event(state, &event);
            Some(ok_response(msg_id))
        }
        None => Some(error_response(msg_id, &format!("unknown anchor: {}", name))),
    }
}

fn handle_anchor_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let anchors = state.vr_state.anchors.to_sexp();
    let event = format!("(:type :event :event :anchor-list :anchors {})", anchors);
    IpcServer::broadcast_event(state, &event);
    Some(format!(
        "(:type :response :id {} :status :ok :anchors {})",
        msg_id, anchors
    ))
}

fn handle_anchor_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let status = state.vr_state.anchors.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :anchors {})",
        msg_id, status
    ))
}

fn handle_anchor_goto(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let name = match get_string(value, "name") {
        Some(name) if !name.trim().is_empty() => name,
        _ => return Some(error_response(msg_id, "missing :name")),
    };

    let anchor = match state.vr_state.anchors.activate(&name).cloned() {
        Some(anchor) => anchor,
        None => return Some(error_response(msg_id, &format!("unknown anchor: {}", name))),
    };

    if state.vr_state.scene.nodes.contains_key(&anchor.surface_id) {
        state
            .vr_state
            .scene
            .set_transform(anchor.surface_id, anchor.transform);
        state.vr_state.scene.set_focus(Some(anchor.surface_id));
    }

    Some(anchor_response(msg_id, &anchor))
}

fn anchor_response(msg_id: i64, anchor: &crate::vr::anchor::SpatialAnchor) -> String {
    format!(
        "(:type :response :id {} :status :ok :anchor {})",
        msg_id,
        anchor.to_sexp()
    )
}

fn broadcast_anchor_event(
    state: &mut EwwmState,
    event_name: &str,
    anchor: &crate::vr::anchor::SpatialAnchor,
) {
    let pos = anchor.transform.position;
    let rot = anchor.transform.rotation;
    let event = format!(
        "(:type :event :event :{} :name \"{}\" :surface-id {} :position (:x {:.3} :y {:.3} :z {:.3}) :rotation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}))",
        event_name,
        escape_string(&anchor.name),
        anchor.surface_id,
        pos.x,
        pos.y,
        pos.z,
        rot.x,
        rot.y,
        rot.z,
        rot.w,
    );
    IpcServer::broadcast_event(state, &event);
}

fn parse_vec3(value: &Value, key: &str) -> Result<crate::vr::scene::Vec3, String> {
    use crate::vr::scene::Vec3;

    let raw = get_value(value, key).ok_or_else(|| format!("missing :{}", key))?;
    if let (Some(x), Some(y), Some(z)) = (
        get_float(raw, "x"),
        get_float(raw, "y"),
        get_float(raw, "z"),
    ) {
        return Ok(Vec3::new(x as f32, y as f32, z as f32));
    }

    let values = value_to_float_list(raw);
    if values.len() < 3 {
        return Err(format!("invalid :{} (expected x y z)", key));
    }
    Ok(Vec3::new(values[0], values[1], values[2]))
}

fn parse_quat(value: &Value, key: &str) -> Result<crate::vr::scene::Quat, String> {
    use crate::vr::scene::Quat;

    let raw = get_value(value, key).ok_or_else(|| format!("missing :{}", key))?;
    if let (Some(x), Some(y), Some(z), Some(w)) = (
        get_float(raw, "x"),
        get_float(raw, "y"),
        get_float(raw, "z"),
        get_float(raw, "w"),
    ) {
        return Ok(Quat {
            x: x as f32,
            y: y as f32,
            z: z as f32,
            w: w as f32,
        });
    }

    let values = value_to_float_list(raw);
    if values.len() < 4 {
        return Err(format!("invalid :{} (expected x y z w)", key));
    }
    Ok(Quat {
        x: values[0],
        y: values[1],
        z: values[2],
        w: values[3],
    })
}

fn value_to_float_list(value: &Value) -> Vec<f32> {
    flatten_list(value)
        .into_iter()
        .filter_map(|value| value_to_keyword_string(value).parse::<f32>().ok())
        .collect()
}

// ── VR Overlays ─────────────────────────────────────────────

fn handle_vr_overlay_create(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::overlay::OverlayType;

    let type_str = get_keyword(value, "overlay-type").unwrap_or_else(|| "head-locked".to_string());
    let overlay_type = match compat_overlay_type(&type_str).and_then(OverlayType::from_str) {
        Some(t) => t,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :overlay-type (use world-locked, head-locked, hand-locked, hud, notification, or status-bar)",
            ))
        }
    };
    let width = get_float(value, "width").unwrap_or(0.4) as f32;
    let height = get_float(value, "height").unwrap_or(0.3) as f32;
    let alpha = get_float(value, "alpha").unwrap_or(1.0) as f32;
    let sort_order = get_int(value, "sort-order").unwrap_or(0) as i32;

    let id = state.vr_state.overlay_manager.create_overlay(
        overlay_type,
        width,
        height,
        alpha,
        sort_order,
    );

    if id == 0 {
        Some(error_response(msg_id, "max overlays reached"))
    } else {
        Some(format!(
            "(:type :response :id {} :status :ok :overlay-id {})",
            msg_id, id
        ))
    }
}

fn handle_vr_overlay_remove(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let overlay_id = match get_overlay_id(value) {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :overlay-id")),
    };
    if state.vr_state.overlay_manager.remove_overlay(overlay_id) {
        Some(ok_response(msg_id))
    } else {
        Some(error_response(msg_id, "overlay not found"))
    }
}

fn handle_vr_overlay_list(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.overlay_manager.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :overlays {})",
        msg_id, sexp
    ))
}

fn handle_vr_overlay_configure(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let overlay_id = match get_overlay_id(value) {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :overlay-id")),
    };

    let mgr = &mut state.vr_state.overlay_manager;

    if mgr.get_overlay(overlay_id).is_none() {
        return Some(error_response(msg_id, "overlay not found"));
    }

    // Apply any provided configuration fields.
    if let Some(alpha) = get_float(value, "alpha") {
        mgr.set_alpha(overlay_id, alpha as f32);
    }
    if let Some(visible) = get_bool(value, "visible") {
        mgr.set_visible(overlay_id, visible);
    }
    if let Some(surface_id) = get_int(value, "surface").or_else(|| get_int(value, "surface-id")) {
        mgr.link_surface(overlay_id, surface_id as u64);
    }

    // Position update: check for :x :y :z fields.
    let has_pos = get_float(value, "x").is_some()
        || get_float(value, "y").is_some()
        || get_float(value, "z").is_some();
    if has_pos {
        use crate::vr::scene::Transform3D;
        let current = mgr.get_overlay(overlay_id).unwrap().transform;
        let new_transform = Transform3D {
            position: crate::vr::scene::Vec3::new(
                get_float(value, "x").unwrap_or(current.position.x as f64) as f32,
                get_float(value, "y").unwrap_or(current.position.y as f64) as f32,
                get_float(value, "z").unwrap_or(current.position.z as f64) as f32,
            ),
            rotation: current.rotation,
            scale: current.scale,
        };
        mgr.set_transform(overlay_id, new_transform);
    }

    Some(ok_response(msg_id))
}

fn get_overlay_id(value: &Value) -> Option<i64> {
    get_int(value, "overlay-id").or_else(|| get_int(value, "id"))
}

fn compat_overlay_type(type_str: &str) -> Option<&'static str> {
    match type_str {
        "hud" | "notification" | "status-bar" => Some("head-locked"),
        "world-locked" => Some("world-locked"),
        "head-locked" => Some("head-locked"),
        "hand-locked" => Some("hand-locked"),
        _ => None,
    }
}

// ── VR Radial Menu ──────────────────────────────────────────

fn handle_vr_radial_open(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let center = state.vr_state.interaction.head_pose.position;
    state.vr_state.radial_menu.open(center);
    Some(ok_response(msg_id))
}

fn handle_vr_radial_close(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.radial_menu.close();
    Some(ok_response(msg_id))
}

fn handle_vr_radial_toggle(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let center = state.vr_state.interaction.head_pose.position;
    state.vr_state.radial_menu.toggle(center);
    Some(ok_response(msg_id))
}

fn handle_vr_radial_configure(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    // Apply radius if provided.
    if let Some(radius) = get_float(value, "radius") {
        state.vr_state.radial_menu.radius = radius as f32;
    }
    if let Some(inner) = get_float(value, "inner-radius") {
        state.vr_state.radial_menu.inner_radius = inner as f32;
    }

    // Parse items list if provided: each item is (:id "x" :label "y").
    // For simplicity, also accept flat pairs via repeated id/label fields
    // encoded by the Elisp side as a single configure message.
    // The items are sent as serialized pairs in the s-expression.
    // We rebuild the item list from scratch.
    if let Some(_items_raw) = get_keyword(value, "items") {
        // Simple fallback: items are not easily parsed from the raw keyword.
        // Instead, we accept them as a sequence of id-N / label-N keys.
        debug!("radial-configure: items field present (raw parse)");
    }

    // Accept bulk items via indexed keys: id-0, label-0, id-1, label-1, ...
    let mut items: Vec<(String, String)> = Vec::new();
    for i in 0..32 {
        let id_key = format!("id-{}", i);
        let label_key = format!("label-{}", i);
        match (get_string(value, &id_key), get_string(value, &label_key)) {
            (Some(id), Some(label)) => items.push((id, label)),
            _ => break,
        }
    }
    if !items.is_empty() {
        state.vr_state.radial_menu.set_items(items);
    }

    Some(ok_response(msg_id))
}

fn handle_vr_radial_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.radial_menu.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :radial {})",
        msg_id, sexp
    ))
}

// ── VR Capture Visibility ───────────────────────────────────

fn handle_vr_capture_set(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    use crate::vr::capture_visibility::CaptureVisibility;

    let surface_id = match get_int(value, "surface") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface")),
    };
    let vis_str = get_keyword(value, "visibility").unwrap_or_else(|| "visible".to_string());
    let visibility = match CaptureVisibility::from_str(&vis_str) {
        Some(v) => v,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :visibility (use visible, hidden, or sensitive)",
            ))
        }
    };

    state
        .vr_state
        .capture_visibility
        .set_visibility(surface_id, visibility);
    Some(ok_response(msg_id))
}

fn handle_vr_capture_get(state: &mut EwwmState, msg_id: i64, value: &Value) -> Option<String> {
    let surface_id = match get_int(value, "surface") {
        Some(id) => id as u64,
        None => return Some(error_response(msg_id, "missing :surface")),
    };
    let vis = state.vr_state.capture_visibility.get_visibility(surface_id);
    Some(format!(
        "(:type :response :id {} :status :ok :surface {} :visibility :{})",
        msg_id,
        surface_id,
        vis.as_str()
    ))
}

fn handle_vr_capture_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.capture_visibility.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :capture {})",
        msg_id, sexp
    ))
}

fn handle_gpu_power_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.gpu_power.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gpu-power {})",
        msg_id, sexp
    ))
}

fn handle_gpu_power_set_profile(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    use crate::vr::gpu_power::GpuPowerProfile;

    let profile_str = match get_keyword(value, "profile") {
        Some(s) => s,
        None => return Some(error_response(msg_id, "missing :profile")),
    };
    let profile = match GpuPowerProfile::from_str(&profile_str) {
        Some(p) => p,
        None => {
            return Some(error_response(
                msg_id,
                "invalid :profile (use auto, low, normal, or high)",
            ))
        }
    };
    match state.vr_state.gpu_power.set_profile(profile) {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_gpu_power_detect(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.vr_state.gpu_power.run_detect();
    let sexp = state.vr_state.gpu_power.to_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :gpu-power {})",
        msg_id, sexp
    ))
}

// ── Beyond HID handlers ────────────────────────────────────

fn handle_beyond_status(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let sexp = state.vr_state.beyond_hid.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :beyond {})",
        msg_id, sexp
    ))
}

fn handle_beyond_detect(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    // Scan for connected Beyond headset. In a full implementation this
    // would enumerate /dev/hidraw* for BEYOND_VENDOR_ID:BEYOND_PRODUCT_ID_HMD.
    // For now, assume connected if we get this far (user explicitly asking).
    state.vr_state.beyond_hid.detect(true, None);
    let sexp = state.vr_state.beyond_hid.status_sexp();
    Some(format!(
        "(:type :response :id {} :status :ok :beyond {})",
        msg_id, sexp
    ))
}

fn handle_beyond_power_on(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    match state.vr_state.beyond_hid.power_on_display() {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_beyond_set_brightness(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let pct = match get_int(value, "value") {
        Some(v) => v as u8,
        None => return Some(error_response(msg_id, "missing :value (0-100)")),
    };
    match state.vr_state.beyond_hid.set_brightness(pct) {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_beyond_set_fan_speed(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let pct = match get_int(value, "value") {
        Some(v) => v as u8,
        None => return Some(error_response(msg_id, "missing :value (40-100)")),
    };
    match state.vr_state.beyond_hid.set_fan_speed(pct) {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_beyond_set_led_color(
    state: &mut EwwmState,
    msg_id: i64,
    value: &Value,
) -> Option<String> {
    let r = match get_int(value, "r") {
        Some(v) => v as u8,
        None => return Some(error_response(msg_id, "missing :r (0-255)")),
    };
    let g = match get_int(value, "g") {
        Some(v) => v as u8,
        None => return Some(error_response(msg_id, "missing :g (0-255)")),
    };
    let b = match get_int(value, "b") {
        Some(v) => v as u8,
        None => return Some(error_response(msg_id, "missing :b (0-255)")),
    };
    match state.vr_state.beyond_hid.set_led_color(r, g, b) {
        Ok(()) => Some(ok_response(msg_id)),
        Err(e) => Some(error_response(msg_id, &e)),
    }
}

fn handle_beyond_firmware_version(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let version = state.vr_state.beyond_hid.firmware_version_str();
    Some(format!(
        "(:type :response :id {} :status :ok :firmware-version \"{}\")",
        msg_id, version,
    ))
}

// ── VR device listing ──────────────────────────────────────

fn handle_vr_list_devices(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    let mut devices = Vec::new();

    // HMD info from VR state
    if state.vr_state.enabled {
        let hmd = &state.vr_state.hmd_info;
        devices.push(format!(
            "(:type :hmd :name \"{}\" :vendor {} :tracking-orient {} :tracking-pos {} :resolution (:w {} :h {}))",
            escape_string(&hmd.system_name),
            hmd.vendor_id,
            if hmd.orientation_tracking { "t" } else { "nil" },
            if hmd.position_tracking { "t" } else { "nil" },
            hmd.recommended_width,
            hmd.recommended_height,
        ));
    }

    // Beyond HID devices
    let beyond = &state.vr_state.beyond_hid;
    if beyond.is_detected() {
        devices.push(format!(
            "(:type :controller :name \"Bigscreen Beyond\" :subtype :hid :firmware \"{}\")",
            escape_string(&beyond.firmware_version_str()),
        ));
    }

    let device_list = devices.join(" ");
    Some(format!(
        "(:type :response :id {} :status :ok :devices ({}))",
        msg_id, device_list,
    ))
}

fn handle_compositor_exit(state: &mut EwwmState, msg_id: i64) -> Option<String> {
    state.running = false;
    Some(ok_response(msg_id))
}

// ── Helpers ────────────────────────────────────────────────

fn handle_unsupported_app_surface(msg_id: i64, surface: &str, reason: &str) -> Option<String> {
    Some(format!(
        "(:type :response :id {} :status :error :surface :{} :reason \"{}\")",
        msg_id,
        surface,
        escape_string(reason),
    ))
}

fn ok_response(id: i64) -> String {
    format!("(:type :response :id {} :status :ok)", id)
}

fn error_response(id: i64, reason: &str) -> String {
    format!(
        "(:type :response :id {} :status :error :reason \"{}\")",
        id,
        escape_string(reason)
    )
}

fn bool_atom(value: bool) -> &'static str {
    if value {
        "t"
    } else {
        "nil"
    }
}

fn format_string_list<'a, I>(items: I) -> String
where
    I: IntoIterator<Item = &'a String>,
{
    let values = items
        .into_iter()
        .map(|item| format!("\"{}\"", escape_string(item)))
        .collect::<Vec<_>>();
    format!("({})", values.join(" "))
}

fn format_autostart_outcomes(outcomes: &[crate::state::NativeAutostartOutcome]) -> String {
    let values = outcomes
        .iter()
        .map(|outcome| {
            format!(
                "(:target \"{}\" :status :{} :detail \"{}\")",
                escape_string(&outcome.target),
                outcome.status,
                escape_string(&outcome.detail)
            )
        })
        .collect::<Vec<_>>();
    format!("({})", values.join(" "))
}

/// Escape a string for s-expression output.
fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Extract a raw value from an s-expression plist.
/// Walks cons pairs directly to find `:key` followed by its value.
/// Handles both `Value::Keyword("key")` (elisp parser) and
/// `Value::Symbol(":key")` (default parser) forms.
fn get_value<'a>(value: &'a Value, key: &str) -> Option<&'a Value> {
    let prefixed = format!(":{}", key);
    let mut current = value;
    loop {
        match current {
            Value::Cons(pair) => {
                let car = pair.car();
                let is_key = match car {
                    Value::Keyword(k) => k.as_ref() == key,
                    Value::Symbol(s) => s.as_ref() == prefixed,
                    _ => false,
                };
                if is_key {
                    // Value is the car of the next cons cell
                    if let Value::Cons(next) = pair.cdr() {
                        return Some(next.car());
                    }
                    return None;
                }
                current = pair.cdr();
            }
            _ => break,
        }
    }
    None
}

fn value_to_keyword_string(value: &Value) -> String {
    match value {
        Value::Keyword(v) => v.to_string(),
        Value::Symbol(v) => {
            let s = v.to_string();
            s.strip_prefix(':').unwrap_or(&s).to_string()
        }
        Value::String(v) => v.to_string(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => {
            if *b {
                "t".to_string()
            } else {
                "nil".to_string()
            }
        }
        Value::Null => "nil".to_string(),
        _ => value.to_string(),
    }
}

fn get_keyword(value: &Value, key: &str) -> Option<String> {
    get_value(value, key).map(value_to_keyword_string)
}

/// Extract an integer value from an s-expression plist.
fn get_int(value: &Value, key: &str) -> Option<i64> {
    get_keyword(value, key).and_then(|s| s.parse().ok())
}

fn get_nested_int(value: &Value, nested_key: &str, key: &str) -> Option<i64> {
    get_value(value, nested_key).and_then(|nested| get_int(nested, key))
}

/// Extract a string value from an s-expression plist.
fn get_string(value: &Value, key: &str) -> Option<String> {
    get_keyword(value, key)
}

/// Extract a boolean value from an s-expression plist.
/// Treats "t" as true, "nil" as false.
fn get_bool(value: &Value, key: &str) -> Option<bool> {
    get_keyword(value, key).map(|s| !matches!(s.as_str(), "nil" | "false"))
}

/// Extract a floating-point value from an s-expression plist.
fn get_float(value: &Value, key: &str) -> Option<f64> {
    get_keyword(value, key).and_then(|s| s.parse().ok())
}

/// Flatten a possibly nested list/cons structure into a Vec of leaf values.
fn flatten_list(value: &Value) -> Vec<&Value> {
    let mut result = Vec::new();
    fn walk<'a>(v: &'a Value, out: &mut Vec<&'a Value>) {
        match v {
            Value::Cons(pair) => {
                walk(pair.car(), out);
                walk(pair.cdr(), out);
            }
            Value::Null => {} // end of list
            other => out.push(other),
        }
    }
    walk(value, &mut result);
    result
}

fn parse_ssvep_frequencies(value: &Value) -> Option<Vec<(usize, f64)>> {
    let leaves = flatten_list(value);
    if leaves.len() % 2 != 0 {
        return None;
    }

    let mut freqs = Vec::new();
    for pair in leaves.chunks(2) {
        let workspace = value_to_keyword_string(pair[0]).parse::<usize>().ok()?;
        let frequency = value_to_keyword_string(pair[1]).parse::<f64>().ok()?;
        freqs.push((workspace, frequency));
    }
    Some(freqs)
}

/// Format an IPC event s-expression.
pub fn format_event(event_type: &str, fields: &[(&str, &str)]) -> String {
    let mut s = format!("(:type :event :event :{}", event_type);
    for (key, val) in fields {
        s.push_str(&format!(" :{} {}", key, val));
    }
    s.push(')');
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── ok_response / error_response ────────────────────────

    #[test]
    fn test_ok_response_format() {
        let r = ok_response(42);
        assert!(r.contains(":type :response"));
        assert!(r.contains(":id 42"));
        assert!(r.contains(":status :ok"));
    }

    #[test]
    fn test_error_response_format() {
        let r = error_response(7, "bad input");
        assert!(r.contains(":type :response"));
        assert!(r.contains(":id 7"));
        assert!(r.contains(":status :error"));
        assert!(r.contains(":reason \"bad input\""));
    }

    #[test]
    fn test_error_response_escapes_quotes() {
        let r = error_response(1, "say \"hello\"");
        assert!(r.contains("say \\\"hello\\\""));
    }

    // ── escape_string ───────────────────────────────────────

    #[test]
    fn test_escape_string_plain() {
        assert_eq!(escape_string("hello"), "hello");
    }

    #[test]
    fn test_escape_string_quotes() {
        assert_eq!(escape_string("say \"hi\""), "say \\\"hi\\\"");
    }

    #[test]
    fn test_escape_string_backslash() {
        assert_eq!(escape_string("a\\b"), "a\\\\b");
    }

    #[test]
    fn test_escape_string_both() {
        assert_eq!(escape_string("\"\\\""), "\\\"\\\\\\\"");
    }

    // ── get_keyword ─────────────────────────────────────────

    #[test]
    fn test_get_keyword_from_plist() {
        let v = lexpr::from_str("(:type :hello :version 1)").unwrap();
        assert_eq!(get_keyword(&v, "type"), Some("hello".to_string()));
        assert_eq!(get_keyword(&v, "version"), Some("1".to_string()));
    }

    #[test]
    fn test_get_keyword_string_value() {
        let v = lexpr::from_str("(:type :hello :client \"emacs\")").unwrap();
        assert_eq!(get_keyword(&v, "client"), Some("emacs".to_string()));
    }

    #[test]
    fn test_get_keyword_missing_key() {
        let v = lexpr::from_str("(:type :hello)").unwrap();
        assert_eq!(get_keyword(&v, "nonexistent"), None);
    }

    #[test]
    fn test_get_keyword_empty_list() {
        let v = lexpr::from_str("()").unwrap();
        assert_eq!(get_keyword(&v, "type"), None);
    }

    // ── get_int ─────────────────────────────────────────────

    #[test]
    fn test_get_int_positive() {
        let v = lexpr::from_str("(:id 42)").unwrap();
        assert_eq!(get_int(&v, "id"), Some(42));
    }

    #[test]
    fn test_get_int_negative() {
        let v = lexpr::from_str("(:x -100)").unwrap();
        assert_eq!(get_int(&v, "x"), Some(-100));
    }

    #[test]
    fn test_get_int_missing() {
        let v = lexpr::from_str("(:type :hello)").unwrap();
        assert_eq!(get_int(&v, "id"), None);
    }

    #[test]
    fn test_get_int_non_numeric() {
        let v = lexpr::from_str("(:id :hello)").unwrap();
        assert_eq!(get_int(&v, "id"), None);
    }

    // ── get_bool ────────────────────────────────────────────

    #[test]
    fn test_get_bool_true() {
        let v = lexpr::from_str("(:enable t)").unwrap();
        assert_eq!(get_bool(&v, "enable"), Some(true));
    }

    #[test]
    fn test_get_bool_nil() {
        let v = lexpr::from_str("(:enable nil)").unwrap();
        assert_eq!(get_bool(&v, "enable"), Some(false));
    }

    // ── get_float ───────────────────────────────────────────

    #[test]
    fn test_get_float_integer() {
        let v = lexpr::from_str("(:speed 10)").unwrap();
        assert_eq!(get_float(&v, "speed"), Some(10.0));
    }

    // ── format_event ────────────────────────────────────────

    #[test]
    fn test_format_event_no_fields() {
        let e = format_event("test", &[]);
        assert_eq!(e, "(:type :event :event :test)");
    }

    #[test]
    fn test_format_event_with_fields() {
        let e = format_event("surface-created", &[("id", "1"), ("app-id", "\"firefox\"")]);
        assert!(e.starts_with("(:type :event :event :surface-created"));
        assert!(e.contains(":id 1"));
        assert!(e.contains(":app-id \"firefox\""));
        assert!(e.ends_with(')'));
    }

    // ── flatten_list ────────────────────────────────────────

    #[test]
    fn test_flatten_list_simple() {
        let v = lexpr::from_str("(:a 1 :b 2)").unwrap();
        let flat = flatten_list(&v);
        assert!(flat.len() >= 4); // :a, 1, :b, 2
    }

    #[test]
    fn test_flatten_list_empty() {
        let v = lexpr::from_str("()").unwrap();
        let flat = flatten_list(&v);
        assert!(flat.is_empty());
    }

    #[test]
    fn test_parse_ssvep_frequencies_from_alist() {
        let v = lexpr::from_str("((1 . 12.0) (2 . 15.0))").unwrap();
        let freqs = parse_ssvep_frequencies(&v).unwrap();
        assert_eq!(freqs, vec![(1, 12.0), (2, 15.0)]);
    }

    #[test]
    fn test_parse_ssvep_frequencies_rejects_odd_values() {
        let v = lexpr::from_str("(1 12.0 2)").unwrap();
        assert!(parse_ssvep_frequencies(&v).is_none());
    }

    // ── Protocol round-trip ─────────────────────────────────

    #[test]
    fn test_ok_response_is_valid_sexp() {
        let r = ok_response(1);
        let parsed = lexpr::from_str(&r);
        assert!(
            parsed.is_ok(),
            "ok_response should produce valid s-expression"
        );
    }

    #[test]
    fn test_error_response_is_valid_sexp() {
        let r = error_response(1, "test error");
        let parsed = lexpr::from_str(&r);
        assert!(
            parsed.is_ok(),
            "error_response should produce valid s-expression"
        );
    }

    #[test]
    fn test_unsupported_app_surface_response_is_valid_sexp() {
        let r = handle_unsupported_app_surface(7, "multimodal", "native fusion not implemented")
            .unwrap();
        let v = lexpr::from_str(&r).unwrap();
        assert_eq!(get_keyword(&v, "type"), Some("response".to_string()));
        assert_eq!(get_int(&v, "id"), Some(7));
        assert_eq!(get_keyword(&v, "status"), Some("error".to_string()));
        assert_eq!(get_keyword(&v, "surface"), Some("multimodal".to_string()));
    }

    #[test]
    fn test_format_event_is_valid_sexp() {
        let e = format_event("test", &[("key", "123")]);
        let parsed = lexpr::from_str(&e);
        assert!(
            parsed.is_ok(),
            "format_event should produce valid s-expression"
        );
    }

    #[test]
    fn test_ok_response_parseable_fields() {
        let r = ok_response(99);
        let v = lexpr::from_str(&r).unwrap();
        assert_eq!(get_keyword(&v, "type"), Some("response".to_string()));
        assert_eq!(get_int(&v, "id"), Some(99));
        assert_eq!(get_keyword(&v, "status"), Some("ok".to_string()));
    }

    #[test]
    fn test_error_response_parseable_fields() {
        let r = error_response(5, "missing field");
        let v = lexpr::from_str(&r).unwrap();
        assert_eq!(get_keyword(&v, "type"), Some("response".to_string()));
        assert_eq!(get_int(&v, "id"), Some(5));
        assert_eq!(get_keyword(&v, "status"), Some("error".to_string()));
        assert_eq!(get_keyword(&v, "reason"), Some("missing field".to_string()));
    }
}
