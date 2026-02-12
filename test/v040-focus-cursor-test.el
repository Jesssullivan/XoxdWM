;;; v040-focus-cursor-test.el --- v0.4.0 focus & cursor tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for focus tracking, cursor handling, click-to-focus, and
;; surface metadata (app_id/title) tracking.

;;; Code:

(require 'ert)

(defvar v040-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

;; ── Focus tracking ─────────────────────────────────────────

(ert-deftest v040/state-has-focused-surface ()
  "EwwmState has focused_surface field."
  (let ((file (expand-file-name "compositor/src/state.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "focused_surface" nil t)))))

(ert-deftest v040/state-has-cursor-status ()
  "EwwmState has cursor_status field."
  (let ((file (expand-file-name "compositor/src/state.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "cursor_status" nil t)))))

(ert-deftest v040/cursor-image-status-enum ()
  "CursorImageStatus enum is defined."
  (let ((file (expand-file-name "compositor/src/state.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub enum CursorImageStatus" nil t)))))

(ert-deftest v040/seat-focus-changed-emits-ipc ()
  "focus_changed handler broadcasts IPC event."
  (let ((file (expand-file-name "compositor/src/handlers/seat.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "focus-changed" nil t)))))

(ert-deftest v040/seat-focus-changed-updates-state ()
  "focus_changed handler updates focused_surface state."
  (let ((file (expand-file-name "compositor/src/handlers/seat.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "self.focused_surface = new_focus" nil t)))))

(ert-deftest v040/seat-cursor-image-updates-state ()
  "cursor_image handler updates cursor_status."
  (let ((file (expand-file-name "compositor/src/handlers/seat.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "self.cursor_status = new_status" nil t)))))

;; ── Click-to-focus ─────────────────────────────────────────

(ert-deftest v040/click-to-focus ()
  "Pointer button handler sets keyboard focus on click."
  (let ((file (expand-file-name "compositor/src/input.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "keyboard.set_focus" nil t)))))

;; ── Surface metadata tracking ──────────────────────────────

(ert-deftest v040/compositor-commit-updates-metadata ()
  "Compositor commit handler updates surface metadata."
  (let ((file (expand-file-name "compositor/src/handlers/compositor.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "update_surface_metadata" nil t)))))

(ert-deftest v040/compositor-reads-xdg-toplevel-data ()
  "Compositor reads XdgToplevelSurfaceData for app_id/title."
  (let ((file (expand-file-name "compositor/src/handlers/compositor.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "XdgToplevelSurfaceData" nil t)))))

(ert-deftest v040/compositor-emits-surface-updated ()
  "Compositor emits surface-updated IPC event on metadata change."
  (let ((file (expand-file-name "compositor/src/handlers/compositor.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-updated" nil t)))))

;; ── IPC commands ───────────────────────────────────────────

(ert-deftest v040/dispatch-has-focused-surface ()
  "dispatch.rs handles focused-surface command."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "focused-surface" nil t)))))

(ert-deftest v040/dispatch-has-surface-info ()
  "dispatch.rs handles surface-info command."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "surface-info" nil t)))))

(ert-deftest v040/surface-list-reports-focused ()
  "surface-list response includes real :focused status."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "focused_surface == Some(*id)" nil t)))))

;; ── Surface ID lookup ──────────────────────────────────────

(ert-deftest v040/state-has-surface-id-lookup ()
  "EwwmState has surface_id_for_wl_surface helper."
  (let ((file (expand-file-name "compositor/src/state.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "fn surface_id_for_wl_surface" nil t)))))

;; ── Focus event format ─────────────────────────────────────

(ert-deftest v040/focus-event-has-old-and-new ()
  "focus-changed event includes :old and :new fields."
  (let ((file (expand-file-name "compositor/src/handlers/seat.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "(\"old\"" nil t))
      (should (search-forward "(\"new\"" nil t)))))

(ert-deftest v040/focused-surface-returns-metadata ()
  "focused-surface response includes :app-id and :title."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v040-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "fn handle_focused_surface" nil t))
      (should (search-forward ":app-id" nil t)))))

(provide 'v040-focus-cursor-test)
;;; v040-focus-cursor-test.el ends here
