;;; honey-display-regression-test.el --- Honey display regression tests -*- lexical-binding: t -*-

;;; Commentary:
;; Static checks for black-screen regressions found during the Honey XR lab.

;;; Code:

(require 'ert)

(defconst honey-display-regression--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun honey-display-regression--read-file (relative)
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative honey-display-regression--root))
    (buffer-string)))

(ert-deftest honey-display-regression/compositor-accepts-xwayland-client-state ()
  "XWayland clients should not crash client_compositor_state."
  (let ((content (honey-display-regression--read-file
                  "compositor/src/handlers/compositor.rs")))
    (should (string-match-p "XWaylandClientData" content))
    (should (string-match-p "get_data::<ClientState>()" content))
    (should (string-match-p "get_data::<XWaylandClientData>()" content))
    (should-not
     (string-match-p
      (regexp-quote "client.get_data::<ClientState>().unwrap().compositor_state")
      content))))

(ert-deftest honey-display-regression/xdg-toplevels-get-visible-initial-configure ()
  "New native Wayland windows should receive a non-zero initial size."
  (let ((content (honey-display-regression--read-file
                  "compositor/src/handlers/xdg_shell.rs")))
    (should (string-match-p "initial_window_geometry" content))
    (should (string-match-p "state.size = Some" content))
    (should (string-match-p "State::Activated" content))
    (should (string-match-p "surface.send_configure()" content))))

(ert-deftest honey-display-regression/xwayland-maps-with-visible-initial-geometry ()
  "XWayland windows should not use a zero bbox as first configure geometry."
  (let ((content (honey-display-regression--read-file
                  "compositor/src/handlers/xwayland.rs")))
    (should (string-match-p "initial_window_geometry" content))
    (should (string-match-p "window.configure(Some(initial_geometry))" content))
    (should-not
     (string-match-p
      (regexp-quote ".and_then(|w| self.space.element_geometry(w))")
      content))))

(ert-deftest honey-display-regression/surface-id-lookup-includes-x11-surfaces ()
  "Commit metadata/VR dirty tracking should find XWayland wl_surfaces too."
  (let ((content (honey-display-regression--read-file "compositor/src/state.rs")))
    (should (string-match-p "initial_window_geometry" content))
    (should (string-match-p "x11_surface()" content))
    (should (string-match-p "surface.wl_surface()" content))))

(ert-deftest honey-display-regression/output-list-is-backed-by-detected-outputs ()
  "IPC output-list should be populated from backend-mapped outputs."
  (let ((output-management
         (honey-display-regression--read-file
          "compositor/src/handlers/output_management.rs"))
        (drm (honey-display-regression--read-file "compositor/src/backend/drm.rs"))
        (headless
         (honey-display-regression--read-file "compositor/src/backend/headless.rs"))
        (winit (honey-display-regression--read-file "compositor/src/backend/winit.rs")))
    (should (string-match-p "upsert_detected_output" output-management))
    (should (string-match-p "remove_detected_output" output-management))
    (dolist (content (list drm headless winit))
      (should (string-match-p "upsert_detected_output" content)))))

(ert-deftest honey-display-regression/drm-has-operator-visible-test-pattern ()
  "DRM lab mode should support an explicit visible framebuffer pattern."
  (let ((content (honey-display-regression--read-file "compositor/src/render.rs")))
    (should (string-match-p "EWWM_DRM_TEST_PATTERN" content))
    (should (string-match-p "solid-red" content))
    (should (string-match-p "Color32F::new(1.0, 0.0, 0.0, 1.0)" content))
    (should (string-match-p "render_age" content))))

(ert-deftest honey-display-regression/drm-readback-logs-framebuffer-hash ()
  "DRM lab mode should prove compositor-side framebuffer pixels."
  (let ((content (honey-display-regression--read-file "compositor/src/render.rs")))
    (should (string-match-p "EWWM_DRM_READBACK" content))
    (should (string-match-p "copy_framebuffer" content))
    (should (string-match-p "map_texture" content))
    (should (string-match-p "drm framebuffer readback" content))
    (should (string-match-p "center_pixel" content))))

(provide 'honey-display-regression-test)
;;; honey-display-regression-test.el ends here
