;;; week9-integration-test.el --- Week 9 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-display)

(defvar ewwm-ipc--event-handlers)

;; ── Rust module structure ───────────────────────────────────

(ert-deftest week9/drm-lease-module-exists ()
  "drm_lease.rs exists."
  (should (file-exists-p
           (expand-file-name "compositor/src/vr/drm_lease.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week9/mod-rs-exports-drm-lease ()
  "vr/mod.rs declares drm_lease module."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/vr/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod drm_lease;" nil t)))))

(ert-deftest week9/handlers-mod-rs-exports-drm-lease ()
  "handlers/mod.rs declares drm_lease module for full-backend builds."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (mod-rs (expand-file-name "compositor/src/handlers/mod.rs" root)))
    (with-temp-buffer
      (insert-file-contents mod-rs)
      (should (search-forward "pub mod drm_lease;" nil t)))))

;; ── IPC dispatch ────────────────────────────────────────────

(ert-deftest week9/dispatch-has-display-info ()
  "dispatch.rs handles vr-display-info."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-info\"" nil t)))))

(ert-deftest week9/dispatch-has-display-set-mode ()
  "dispatch.rs handles vr-display-set-mode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-set-mode\"" nil t)))))

(ert-deftest week9/dispatch-has-display-select-hmd ()
  "dispatch.rs handles vr-display-select-hmd."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-select-hmd\"" nil t)))))

(ert-deftest week9/dispatch-has-display-refresh-rate ()
  "dispatch.rs handles vr-display-set-refresh-rate."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-set-refresh-rate\"" nil t)))))

(ert-deftest week9/dispatch-has-display-auto-detect ()
  "dispatch.rs handles vr-display-auto-detect."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-auto-detect\"" nil t)))))

(ert-deftest week9/dispatch-has-display-list-connectors ()
  "dispatch.rs handles vr-display-list-connectors."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (dispatch (expand-file-name "compositor/src/ipc/dispatch.rs" root)))
    (with-temp-buffer
      (insert-file-contents dispatch)
      (should (search-forward "\"vr-display-list-connectors\"" nil t)))))

;; ── Emacs integration ───────────────────────────────────────

(ert-deftest week9/ewwm-requires-vr-display ()
  "ewwm.el requires ewwm-vr-display."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(require 'ewwm-vr-display)" nil t)))))

(ert-deftest week9/ewwm-init-calls-display-init ()
  "ewwm.el init calls ewwm-vr-display-init."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-display-init)" nil t)))))

(ert-deftest week9/ewwm-exit-calls-display-teardown ()
  "ewwm.el exit calls ewwm-vr-display-teardown."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ewwm (expand-file-name "lisp/vr/ewwm.el" root)))
    (with-temp-buffer
      (insert-file-contents ewwm)
      (should (search-forward "(ewwm-vr-display-teardown)" nil t)))))

;; ── Rust VrState integration ────────────────────────────────

(ert-deftest week9/openxr-state-has-hmd-manager ()
  "openxr_state.rs has hmd_manager field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (ostate (expand-file-name "compositor/src/vr/openxr_state.rs" root)))
    (with-temp-buffer
      (insert-file-contents ostate)
      (should (search-forward "pub hmd_manager: HmdManager" nil t)))))

(ert-deftest week9/stub-has-hmd-manager ()
  "stub.rs has hmd_manager field."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (stub (expand-file-name "compositor/src/vr/stub.rs" root)))
    (with-temp-buffer
      (insert-file-contents stub)
      (should (search-forward "pub hmd_manager: HmdManager" nil t)))))

;; ── Rust drm_lease structure ────────────────────────────────

(ert-deftest week9/drm-lease-has-connector-type ()
  "drm_lease.rs defines ConnectorType."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum ConnectorType" nil t)))))

(ert-deftest week9/drm-lease-has-hmd-manager ()
  "drm_lease.rs defines HmdManager."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct HmdManager" nil t)))))

(ert-deftest week9/drm-lease-has-lease-state ()
  "drm_lease.rs defines LeaseState."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct LeaseState" nil t)))))

(ert-deftest week9/drm-lease-has-display-mode ()
  "drm_lease.rs defines VrDisplayMode."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum VrDisplayMode" nil t)))))

(ert-deftest week9/drm-lease-has-rust-tests ()
  "drm_lease.rs has unit tests."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "#[cfg(test)]" nil t)))))

(ert-deftest week9/handler-drm-lease-module-exists ()
  "Smithay protocol handler module exists for DRM lease wiring."
  (should (file-exists-p
           (expand-file-name "compositor/src/handlers/drm_lease.rs"
                             (locate-dominating-file default-directory ".git")))))

(ert-deftest week9/handler-drm-lease-delegates-smithay-protocol ()
  "drm_lease handler delegates the Smithay protocol."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/handlers/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "delegate_drm_lease!(EwwmState);" nil t)))))

(ert-deftest week9/state-has-drm-lease-state ()
  "EwwmState tracks DRM lease protocol state."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/state.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub drm_lease_state: Option<DrmLeaseState>" nil t))
      (should (search-forward "pub active_drm_leases: Vec<DrmLease>" nil t))
      (should (search-forward "pub drm_lease_devices: HashMap<DrmNode, Rc<RefCell<DrmDevice>>>" nil t))
      (should (search-forward "pub drm_output_crtcs: HashMap<DrmNode, HashSet<crtc::Handle>>" nil t))
      (should (search-forward "pub fn register_drm_lease_device(" nil t))
      (should (search-forward "pub fn set_drm_output_crtcs<I>(&mut self, node: DrmNode, crtcs: I)" nil t))
      (should (search-forward "pub fn ensure_drm_lease_state(&mut self, node: DrmNode)" nil t)))))

(ert-deftest week9/backend-initializes-drm-lease-state ()
  "DRM backend initializes the lease global on the primary node."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/backend/drm.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "state.ensure_drm_lease_state(node);" nil t))
      (should (search-forward "state.register_drm_lease_device(node, gpu.drm.clone());" nil t))
      (should (search-forward "state.set_drm_output_crtcs(gpu.node, used_crtcs.iter().copied());" nil t)))))

(ert-deftest week9/lease-handler-builds-real-leases ()
  "Lease handler uses the live DRM backend to build real leases."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/handlers/drm_lease.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "self.drm_lease_devices.get(&node).cloned()" nil t))
      (should (search-forward "self.drm_output_crtcs" nil t))
      (should (search-forward "let mut builder = DrmLeaseBuilder::new(&drm);" nil t))
      (should (search-forward "builder.add_crtc(crtc);" nil t))
      (should (search-forward "builder.add_plane(primary_plane, claim);" nil t))
      (should (search-forward "\"granting DRM lease request\"" nil t)))))

(ert-deftest week9/backend-recognizes-lease-connector-overrides ()
  "DRM backend recognizes explicit connector override env vars."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/backend/drm.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "\"EWWM_DRM_LEASE_CONNECTORS\"" nil t))
      (should (search-forward "\"XRT_COMPOSITOR_WAYLAND_CONNECTOR\"" nil t)))))

(ert-deftest week9/backend-skips-lease-candidate-connectors ()
  "DRM backend reserves lease-designated connectors instead of mapping them."
  (let* ((root (locate-dominating-file default-directory ".git"))
         (file (expand-file-name "compositor/src/backend/drm.rs" root)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (should (search-forward "lease-designated connector, skipping desktop output" nil t))
      (goto-char (point-min))
      (should (search-forward "add_connector::<EwwmState>(" nil t))
      (goto-char (point-min))
      (should (search-forward "drm_lease_state.withdraw_connector" nil t)))))

;; ── Cross-module consistency ────────────────────────────────

(ert-deftest week9/display-modes-consistent ()
  "Elisp display modes match Rust VrDisplayMode."
  (let ((elisp-modes '(headset preview headless off)))
    (let* ((root (locate-dominating-file default-directory ".git"))
           (file (expand-file-name "compositor/src/vr/drm_lease.rs" root)))
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (m elisp-modes)
          (goto-char (point-min))
          (should (search-forward (format "\"%s\"" (symbol-name m)) nil t)))))))

;;; week9-integration-test.el ends here
