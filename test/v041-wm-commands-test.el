;;; v041-wm-commands-test.el --- v0.4.1 WM command tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for fullscreen, float, workspace-move, and workspace-list.

;;; Code:

(require 'ert)

(defvar v041-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

;; ── Fullscreen ─────────────────────────────────────────────

(ert-deftest v041/fullscreen-uses-toplevel-state ()
  "surface-fullscreen sets ToplevelState::Fullscreen."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "ToplevelState::Fullscreen" nil t)))))

(ert-deftest v041/fullscreen-sends-configure ()
  "surface-fullscreen sends pending configure."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "send_pending_configure" nil t)))))

(ert-deftest v041/fullscreen-accepts-enable-param ()
  "surface-fullscreen reads :enable parameter."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "get_bool(value, \"enable\")" nil t)))))

;; ── Resize geometry ────────────────────────────────────────

(ert-deftest v041/resize-accepts-geometry-payload ()
  "surface-resize should accept Lisp's nested :geometry payload."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "get_nested_int(value, \"geometry\", \"x\")" nil t))
      (should (search-forward "get_nested_int(value, \"geometry\", \"y\")" nil t))
      (should (search-forward "get_nested_int(value, \"geometry\", \"w\")" nil t))
      (should (search-forward "get_nested_int(value, \"geometry\", \"h\")" nil t)))))

(ert-deftest v041/resize-emits-geometry-event ()
  "surface-resize should publish the geometry it applied."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((content (buffer-string)))
        (should (string-match-p "surface-geometry-changed" content))
        (should (string-match-p "IpcServer::broadcast_event(state, &event)" content))))))

;; ── Float toggle ───────────────────────────────────────────

(ert-deftest v041/float-updates-data ()
  "surface-float updates SurfaceData.floating."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "data.floating = enable" nil t)))))

(ert-deftest v041/float-emits-event ()
  "surface-float emits surface-float-changed IPC event."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-float-changed" nil t)))))

;; ── Workspace move ─────────────────────────────────────────

(ert-deftest v041/workspace-move-updates-data ()
  "workspace-move-surface updates SurfaceData.workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "data.workspace = workspace" nil t)))))

(ert-deftest v041/workspace-move-emits-event ()
  "workspace-move-surface emits surface-workspace-changed event."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-workspace-changed" nil t)))))

(ert-deftest v041/workspace-move-has-old-new ()
  "workspace-move event includes old-workspace and new-workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "old-workspace" nil t))
      (should (search-forward "new-workspace" nil t)))))

;; ── Workspace list ─────────────────────────────────────────

(ert-deftest v041/workspace-list-has-surface-ids ()
  "workspace-list includes surface IDs per workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_ids" nil t)))))

(ert-deftest v041/workspace-list-has-count ()
  "workspace-list includes :count per workspace."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":count" nil t)))))

(ert-deftest v041/workspace-list-uses-native-workspace-count ()
  "workspace-list should use native state rather than hard-coded four."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((content (buffer-string)))
        (should (string-match-p "0\\.\\.state\\.workspace_count" content))
        (should-not (string-match-p "for i in 0\\.\\.4" content))))))

(ert-deftest v041/workspace-switch-emits-native-event ()
  "workspace-switch should broadcast compositor-owned workspace state."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((content (buffer-string)))
        (should (string-match-p "workspace-changed" content))
        (should (string-match-p "(\"previous\", &previous\\.to_string())" content))
        (should (string-match-p "IpcServer::broadcast_event(state, &event)" content))))))

;; ── Layout policy ──────────────────────────────────────────

(ert-deftest v041/layout-set-reads-layout-param ()
  "layout-set reads :layout parameter."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "get_string(value, \"layout\")" nil t)))))

(ert-deftest v041/layout-state-is-native ()
  "layout-set should update native compositor state."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((content (buffer-string)))
        (should (string-match-p "state\\.current_layout" content))
        (should (string-match-p "native layout policy updated" content))
        (should-not (string-match-p (concat "Emacs" "-driven") content))))))

(ert-deftest v041/layout-set-validates-mode ()
  "layout-set should reject unknown layout names."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "is_valid_layout_mode(&layout)" nil t))
      (should (search-forward "invalid layout" nil t)))))

(ert-deftest v041/layout-cycle-uses-native-cycle ()
  "layout-cycle should use the native layout cycle."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "LAYOUT_CYCLE" nil t))
      (should (search-forward "set_current_layout(state, next, msg_id)" nil t)))))

(ert-deftest v041/layout-get-reports-native-state ()
  "layout-get should report the compositor's current layout."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "Some(\"layout-get\") => handle_layout_get(state, msg_id)" nil t))
      (should (search-forward ":master-ratio" nil t)))))

(ert-deftest v041/native-compat-aliases-cover-emacs-app-layer ()
  "Legacy Emacs app-layer command names should dispatch to native handlers."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("Some(\"focus-surface\") => handle_surface_focus"
                        "Some(\"compositor-exit\") => handle_compositor_exit"
                        "Some(\"follow-status\") => handle_vr_follow_status"
                        "Some(\"follow-set-policy\") => handle_vr_follow_set_policy"
                        "Some(\"follow-configure\") => handle_follow_configure"
                        "Some(\"follow-recenter\") => handle_vr_follow_recenter"
                        "Some(\"focus-routing-status\") => handle_focus_routing_status"
                        "Some(\"focus-routing-set-mode\") => handle_focus_routing_set_mode"
                        "Some(\"focus-routing-set-dwell\") => handle_gaze_focus_set_dwell"
                        "Some(\"focus-routing-configure\") => handle_focus_routing_configure"
                        "Some(\"surface-move-interactive\") => handle_surface_move_interactive"
                        "handle_surface_resize_interactive(state, msg_id, &value)"
                        "Some(\"overlay-create\") => handle_vr_overlay_create"
                        "Some(\"overlay-remove\") => handle_vr_overlay_remove"
                        "Some(\"overlay-list\") => handle_vr_overlay_list"
                        "Some(\"overlay-status\") => handle_vr_overlay_list"
                        "Some(\"overlay-set-alpha\") => handle_vr_overlay_configure"
                        "Some(\"overlay-set-visible\") => handle_vr_overlay_configure"
                        "Some(\"overlay-link-surface\") => handle_vr_overlay_configure"
                        "Some(\"passthrough-enable\") => handle_passthrough_enable"
                        "Some(\"passthrough-disable\") => handle_passthrough_disable"
                        "Some(\"passthrough-status\") => handle_passthrough_status"
                        "Some(\"passthrough-set-blend-mode\") =>"
                        "Some(\"passthrough-set-opacity\") => handle_passthrough_set_opacity"
                        "Some(\"transient-list\") => handle_vr_transient_list"
                        "Some(\"transient-status\") => handle_transient_status"
                        "Some(\"transient-configure\") => handle_transient_configure"
                        "Some(\"transient-set-offset\") => handle_transient_set_offset"
                        "Some(\"transient-set-placement\") => handle_transient_set_placement"
                        "Some(\"hand-tracking-configure\") => handle_hand_tracking_config"
                        "Some(\"hand-tracking-toggle\") => handle_hand_tracking_toggle"
                        "Some(\"command\") => handle_compat_command"
                        "Some(\"input-latency-probe\") => handle_input_latency_probe"
                        "Some(\"gaze-zone-set-layout\") => handle_gaze_zone_set_layout"
                        "Some(\"anchor-create\") => handle_anchor_create"
                        "Some(\"anchor-restore\") => handle_anchor_restore"
                        "Some(\"anchor-remove\") => handle_anchor_remove"
                        "Some(\"anchor-list\") => handle_anchor_list"
                        "Some(\"anchor-status\") => handle_anchor_status"
                        "Some(\"anchor-goto\") => handle_anchor_goto"
                        "state.running = false"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/overlay-compat-fields-map-to-native-overlay-config ()
  "Legacy overlay command fields should be accepted by native overlay handlers."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("fn get_overlay_id(value: &Value) -> Option<i64>"
                        "get_int(value, \"overlay-id\").or_else(|| get_int(value, \"id\"))"
                        "get_int(value, \"surface\").or_else(|| get_int(value, \"surface-id\"))"
                        "\"hud\" | \"notification\" | \"status-bar\" => Some(\"head-locked\")"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/passthrough-compat-is-native-config-backed ()
  "Passthrough aliases should use native compositor state/config."
  (let ((dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root))
        (config (expand-file-name "compositor/src/config.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (dolist (needle '("is_valid_passthrough_blend_mode"
                        "set_passthrough_opacity(state, opacity)"
                        "state.config.passthrough_blend_mode"
                        "state.config.passthrough_opacity"
                        "VrBackground::Passthrough"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents config)
      (dolist (needle '("pub passthrough_opacity: f32"
                        "\"passthrough_opacity\""
                        "\"passthrough_blend_mode\""))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/transient-compat-is-native-config-backed ()
  "Transient aliases should mutate the native transient chain manager."
  (let ((dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root))
        (transient (expand-file-name "compositor/src/vr/transient_3d.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (dolist (needle '("state.vr_state.transient_chains.default_placement"
                        "state.vr_state.transient_chains.z_offset_per_level"
                        "state.vr_state.transient_chains.max_depth"
                        "config_sexp()"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents transient)
      (dolist (needle '("pub default_placement: TransientPlacement"
                        "pub fn config_sexp(&self) -> String"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/compat-command-is-autotype-only ()
  "The generic command wrapper should only preserve known autotype aliases."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("fn handle_compat_command"
                        "\"autotype\" => handle_autotype"
                        "\"autotype-status\" => handle_autotype_status"
                        "\"autotype-abort\" => handle_autotype_abort"
                        "unsupported compatibility command"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/hand-tracking-compat-is-config-only ()
  "Hand compatibility commands should mutate native config, not claim acquisition."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("fn handle_hand_tracking_toggle"
                        "get_bool(value, \"enabled\").or_else(|| get_bool(value, \"enable\"))"
                        "state.vr_state.hand_tracking.config.enabled = enabled"
                        "state.vr_state.hand_tracking.status_sexp()"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/bci-compat-is-native-state-or-explicit-boundary ()
  "BCI compatibility commands should either hit native state or state the boundary."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("Some(\"bci-hardware-check\") => handle_bci_hardware_check"
                        "Some(\"bci-attention-calibrate\") => handle_bci_attention_calibrate_start"
                        "Some(\"bci-attention-toggle\") => handle_bci_attention_toggle"
                        "Some(\"bci-dnd-enable\") => handle_bci_dnd_compat"
                        "Some(\"bci-dnd-disable\") => handle_bci_dnd_compat"
                        "Some(\"bci-ssvep-configure\") => handle_bci_ssvep_configure_compat"
                        "Some(\"bci-p300-cancel\") => handle_bci_p300_stop"
                        "Some(\"bci-mi-calibrate\") => handle_bci_mi_calibrate_start"
                        "Some(\"bci-mi-toggle\") => handle_bci_mi_toggle"
                        ":acquisition :unproven"
                        "parse_ssvep_frequencies"
                        "state.vr_state.bci.motor_imagery.config.enabled = enabled"
                        "state.vr_state.bci.attention.config.enabled = enabled"
                        "notification/DND policy is not native yet"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/unproven-app-surfaces-return-explicit-native-errors ()
  "Unproven app-layer IPC should not fall through as unknown commands."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("Some(\"bci-nfb-start\") => handle_unsupported_app_surface"
                        "Some(\"bci-nfb-stop\") => handle_unsupported_app_surface"
                        "Some(\"multimodal-enable\") => handle_unsupported_app_surface"
                        "Some(\"multimodal-disable\") => handle_unsupported_app_surface"
                        "Some(\"multimodal-set-dwell\") => handle_unsupported_app_surface"
                        "Some(\"multimodal-three-factor-start\") => handle_unsupported_app_surface"
                        "Some(\"passkey-response\") => handle_unsupported_app_surface"
                        "native neurofeedback session streaming is not implemented"
                        "native multimodal fusion is not implemented"
                        "passkey browser response plumbing remains app-layer"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/input-latency-probe-is-ipc-roundtrip-only ()
  "Input latency probe should report timestamps without pretending HID proof."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("fn handle_input_latency_probe"
                        ":client-timestamp"
                        ":server-timestamp"
                        "std::time::SystemTime::now()"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/gaze-zone-layout-is-native-config-backed ()
  "Gaze zone layout compatibility should mutate native detector layout state."
  (let ((dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root))
        (gaze-zone (expand-file-name "compositor/src/vr/gaze_zone.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (dolist (needle '("fn handle_gaze_zone_set_layout"
                        "parse_gaze_zone_layout(value)"
                        "state.vr_state.zone_detector.set_custom_layout(entries)"
                        "state.vr_state.zone_detector.set_layout_preset(&layout)"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents gaze-zone)
      (dolist (needle '("pub layout_name: String"
                        "pub zone_modifiers: Vec<(GazeZone, String)>"
                        "pub fn set_layout_preset"
                        "pub fn set_custom_layout"
                        "pub fn modifier_for_zone"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/gaze-zone-layout-presets-are-native ()
  "Native zone layout presets should preserve the app-layer preset names."
  (let ((file (expand-file-name "compositor/src/vr/gaze_zone.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (needle '("fn default_zone_layout()"
                        "fn vim_like_zone_layout()"
                        "fn spacemacs_zone_layout()"
                        "\"vim-like\" => vim_like_zone_layout()"
                        "\"spacemacs\" => spacemacs_zone_layout()"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/gaze-zone-layout-loads-from-native-config ()
  "Native compositor config should seed gaze-zone layout before app clients connect."
  (let ((config (expand-file-name "compositor/src/config.rs" v041-test--root))
        (state (expand-file-name "compositor/src/state.rs" v041-test--root))
        (gaze-zone (expand-file-name "compositor/src/vr/gaze_zone.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents config)
      (dolist (needle '("pub gaze_zone_layout: String"
                        "pub gaze_zone_custom_map: String"
                        "\"gaze_zone_layout\""
                        "\"gaze_zone_custom_map\""
                        "is_valid_gaze_zone_layout"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents state)
      (dolist (needle '("let mut vr_state = VrState::new()"
                        "apply_native_vr_config(&mut vr_state, &config)"
                        "parse_zone_layout_map(&config.gaze_zone_custom_map)"
                        "set_layout_preset(&config.gaze_zone_layout)"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents gaze-zone)
      (should (search-forward "pub fn parse_zone_layout_map" nil t)))))

(ert-deftest v041/gaze-zone-events-use-canonical-app-layer-names ()
  "Native gaze-zone events should match the Lisp event subscriptions."
  (let ((gaze-zone (expand-file-name "compositor/src/vr/gaze_zone.rs" v041-test--root))
        (lisp-gaze-zone (expand-file-name "lisp/vr/ewwm-vr-gaze-zone.el" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents gaze-zone)
      (dolist (needle '(":event :gaze-zone-entered"
                        ":event :gaze-zone-activated"
                        ":event :gaze-zone-deactivated"
                        ":event :gaze-zone-dwell-progress"))
        (should (search-forward needle nil t))
        (goto-char (point-min)))
      (dolist (retired '(":event :zone-entered"
                         ":event :zone-activated"
                         ":event :zone-deactivated"
                         ":event :zone-dwell-progress"))
        (should-not (search-forward retired nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents lisp-gaze-zone)
      (dolist (needle '(":gaze-zone-entered"
                        ":gaze-zone-activated"
                        ":gaze-zone-deactivated"
                        ":gaze-zone-dwell-progress"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/autotype-aborted-is-consumed-by-app-client ()
  "Rust autotype abort events should have a Lisp app-layer handler."
  (let ((rust-autotype (expand-file-name "compositor/src/autotype.rs" v041-test--root))
        (lisp-client (expand-file-name "lisp/vr/ewwm-secrets-compositor.el" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents rust-autotype)
      (should (search-forward ":event :autotype-aborted" nil t)))
    (with-temp-buffer
      (insert-file-contents lisp-client)
      (dolist (needle '("ewwm-secrets-compositor--on-autotype-aborted"
                        "(:autotype-aborted   . ewwm-secrets-compositor--on-autotype-aborted)"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))))

(ert-deftest v041/anchors-are-native-scene-state-not-xr-ext-claim ()
  "Anchor commands should use compositor-local scene state and avoid XR_EXT claims."
  (let ((dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" v041-test--root))
        (anchor (expand-file-name "compositor/src/vr/anchor.rs" v041-test--root))
        (vr-mod (expand-file-name "compositor/src/vr/mod.rs" v041-test--root))
        (stub (expand-file-name "compositor/src/vr/stub.rs" v041-test--root))
        (openxr (expand-file-name "compositor/src/vr/openxr_state.rs" v041-test--root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (dolist (needle '("fn handle_anchor_create"
                        "surface has no native VR scene node to anchor"
                        "fn handle_anchor_restore"
                        "state.vr_state.scene.set_transform"
                        "fn handle_anchor_goto"
                        "state.vr_state.scene.set_focus"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents anchor)
      (dolist (needle '("pub struct AnchorManager"
                        "pub struct SpatialAnchor"
                        "This is not XR_EXT_spatial_anchor support"))
        (should (search-forward needle nil t))
        (goto-char (point-min))))
    (with-temp-buffer
      (insert-file-contents vr-mod)
      (should (search-forward "pub mod anchor" nil t)))
    (dolist (file (list stub openxr))
      (with-temp-buffer
        (insert-file-contents file)
        (should (search-forward "AnchorManager" nil t))))))

(provide 'v041-wm-commands-test)
;;; v041-wm-commands-test.el ends here
