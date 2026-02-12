;;; v030-dispatch-test.el --- v0.3.0 IPC dispatch tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the surface_id → Window correlation fix and rendering pipeline.

;;; Code:

(require 'ert)

(defvar v030-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

;; ── surface_to_window mapping in state.rs ─────────────────────

(ert-deftest v030/state-has-surface-to-window-field ()
  "state.rs has surface_to_window field."
  (let ((file (expand-file-name "compositor/src/state.rs" v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_to_window" nil t)))))

(ert-deftest v030/state-has-find-window-method ()
  "state.rs has find_window method."
  (let ((file (expand-file-name "compositor/src/state.rs" v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "fn find_window" nil t)))))

;; ── dispatch.rs uses find_window ──────────────────────────────

(ert-deftest v030/dispatch-focus-uses-find-window ()
  "handle_surface_focus uses find_window."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "find_window(surface_id)" nil t)))))

(ert-deftest v030/dispatch-no-todo-window-correlation ()
  "dispatch.rs has no TODO about window correlation."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (search-forward "TODO: proper window-to-surface-id" nil t)))))

(ert-deftest v030/dispatch-no-todo-proper-lookup ()
  "dispatch.rs has no 'TODO: proper lookup' comments."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (search-forward "TODO: proper lookup" nil t)))))

(ert-deftest v030/dispatch-no-elements-next-hack ()
  "dispatch.rs does not use elements().next() hack."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (search-forward "elements().next().cloned()" nil t)))))

(ert-deftest v030/dispatch-no-true-todo-hack ()
  "dispatch.rs does not use 'true // TODO' hack."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should-not
       (search-forward "true // TODO" nil t)))))

;; ── xdg_shell handler populates mapping ───────────────────────

(ert-deftest v030/xdg-shell-inserts-surface-to-window ()
  "xdg_shell.rs inserts into surface_to_window."
  (let ((file (expand-file-name "compositor/src/handlers/xdg_shell.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_to_window.insert" nil t)))))

(ert-deftest v030/xdg-shell-removes-surface-to-window ()
  "xdg_shell.rs removes from surface_to_window on destroy."
  (let ((file (expand-file-name "compositor/src/handlers/xdg_shell.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_to_window.remove" nil t)))))

;; ── xwayland handler populates mapping ────────────────────────

(ert-deftest v030/xwayland-inserts-surface-to-window ()
  "xwayland.rs inserts into surface_to_window."
  (let ((file (expand-file-name "compositor/src/handlers/xwayland.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_to_window.insert" nil t)))))

(ert-deftest v030/xwayland-removes-surface-to-window ()
  "xwayland.rs removes from surface_to_window on unmap."
  (let ((file (expand-file-name "compositor/src/handlers/xwayland.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface_to_window.remove" nil t)))))

;; ── Rendering pipeline ────────────────────────────────────────

(ert-deftest v030/render-has-surface-rendering ()
  "render.rs renders actual surface content (not just clear)."
  (let ((file (expand-file-name "compositor/src/render.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Should use space render elements, not just clear
      (should (search-forward "render_elements" nil t)))))

(ert-deftest v030/render-has-damage-tracker ()
  "render.rs uses OutputDamageTracker."
  (let ((file (expand-file-name "compositor/src/render.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "DamageTracker" nil t)))))

(ert-deftest v030/render-has-drm-function ()
  "render.rs has a DRM render function."
  (let ((file (expand-file-name "compositor/src/render.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "render_drm" nil t)))))

;; ── DRM backend ───────────────────────────────────────────────

(ert-deftest v030/drm-backend-not-stub ()
  "drm.rs is not a one-function stub."
  (let ((file (expand-file-name "compositor/src/backend/drm.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Real DRM backend should have session management
      (should (search-forward "LibSeatSession" nil t)))))

(ert-deftest v030/drm-backend-has-event-loop ()
  "drm.rs has an event loop."
  (let ((file (expand-file-name "compositor/src/backend/drm.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "EventLoop" nil t)))))

(ert-deftest v030/drm-backend-has-output ()
  "drm.rs creates an Output."
  (let ((file (expand-file-name "compositor/src/backend/drm.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "Output::new" nil t)))))

;; ── surface-list returns per-surface geometry ─────────────────

(ert-deftest v030/surface-list-per-surface-geometry ()
  "dispatch.rs surface-list uses per-surface geometry lookup."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs"
                                v030-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Should use surface_to_window.get(id) not elements().find_map
      (should (search-forward "surface_to_window" nil t)))))

(provide 'v030-dispatch-test)
;;; v030-dispatch-test.el ends here
