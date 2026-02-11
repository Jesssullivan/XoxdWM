;;; ewwm-qutebrowser-tabs.el --- Tab-as-buffer for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Maps qutebrowser tabs to Emacs buffers.  Each tab is represented as
;; a buffer with metadata (URL, title, index).  Provides tab switching,
;; creation, and killing that syncs bidirectionally with qutebrowser.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send "ewwm-qutebrowser-ipc")
(declare-function ewwm-qutebrowser-current-surface
                  "ewwm-qutebrowser")

;; Forward-declare from ewwm-qutebrowser
(defvar ewwm-qutebrowser-app-id)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-tabs nil
  "Tab-as-buffer integration for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-tab-")

(defcustom ewwm-qutebrowser-tab-buffer-prefix "*qb:"
  "Prefix for qutebrowser tab buffer names."
  :type 'string
  :group 'ewwm-qutebrowser-tabs)

(defcustom ewwm-qutebrowser-tab-close-on-kill t
  "Non-nil to close qutebrowser tab when its buffer is killed."
  :type 'boolean
  :group 'ewwm-qutebrowser-tabs)

(defcustom ewwm-qutebrowser-tab-sync-on-switch t
  "Non-nil to sync qutebrowser focus when switching tab buffers."
  :type 'boolean
  :group 'ewwm-qutebrowser-tabs)

(defcustom ewwm-qutebrowser-tab-show-favicons nil
  "Non-nil to show favicons in tab buffer names."
  :type 'boolean
  :group 'ewwm-qutebrowser-tabs)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-tab--buffers nil
  "Alist of (TAB-INDEX . BUFFER) for qutebrowser tabs.")

(defvar ewwm-qutebrowser-tab--active-index nil
  "Index of the currently active qutebrowser tab.")

;; ── Buffer-local variables ───────────────────────────────────

(defvar-local ewwm-qutebrowser-tab-url nil
  "URL for this tab buffer.")
(put 'ewwm-qutebrowser-tab-url 'permanent-local t)

(defvar-local ewwm-qutebrowser-tab-title nil
  "Title for this tab buffer.")
(put 'ewwm-qutebrowser-tab-title 'permanent-local t)

(defvar-local ewwm-qutebrowser-tab-index nil
  "Tab index for this tab buffer.")
(put 'ewwm-qutebrowser-tab-index 'permanent-local t)

;; ── Tab buffer creation ──────────────────────────────────────

(defun ewwm-qutebrowser-tab--create (index url title)
  "Create a buffer for qutebrowser tab at INDEX with URL and TITLE.
Returns the created buffer."
  (let* ((display-title (or title url "untitled"))
         (name (format "%s%s*" ewwm-qutebrowser-tab-buffer-prefix
                       display-title))
         (buf (generate-new-buffer name)))
    (with-current-buffer buf
      (setq ewwm-qutebrowser-tab-index index
            ewwm-qutebrowser-tab-url (or url "")
            ewwm-qutebrowser-tab-title (or title "")))
    (setf (alist-get index ewwm-qutebrowser-tab--buffers) buf)
    buf))

(defun ewwm-qutebrowser-tab--find (index)
  "Find the buffer for tab INDEX.
Returns nil if no buffer exists for that index."
  (let ((buf (alist-get index ewwm-qutebrowser-tab--buffers)))
    (when (and buf (buffer-live-p buf))
      buf)))

(defun ewwm-qutebrowser-tab--update (index &rest props)
  "Update tab buffer at INDEX with PROPS.
PROPS is a plist with :url and/or :title keys."
  (when-let ((buf (ewwm-qutebrowser-tab--find index)))
    (with-current-buffer buf
      (when (plist-member props :url)
        (setq ewwm-qutebrowser-tab-url (plist-get props :url)))
      (when (plist-member props :title)
        (setq ewwm-qutebrowser-tab-title (plist-get props :title))
        (let ((new-name (format "%s%s*"
                                ewwm-qutebrowser-tab-buffer-prefix
                                (plist-get props :title))))
          (ignore-errors (rename-buffer new-name t)))))
    buf))

(defun ewwm-qutebrowser-tab--kill (index)
  "Kill the buffer for tab INDEX.
Returns non-nil if a buffer was killed."
  (when-let ((buf (ewwm-qutebrowser-tab--find index)))
    (setf (alist-get index ewwm-qutebrowser-tab--buffers nil 'remove) nil)
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf))
      t)))

(defun ewwm-qutebrowser-tab--all ()
  "Return list of all live tab buffers."
  (cl-loop for (_idx . buf) in ewwm-qutebrowser-tab--buffers
           when (buffer-live-p buf) collect buf))

(defun ewwm-qutebrowser-tab--count ()
  "Return the number of live tab buffers."
  (length (ewwm-qutebrowser-tab--all)))

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-qutebrowser-tab-switch (index)
  "Switch to qutebrowser tab INDEX."
  (interactive "nTab index: ")
  (let ((buf (ewwm-qutebrowser-tab--find index)))
    (if buf
        (progn
          (setq ewwm-qutebrowser-tab--active-index index)
          (switch-to-buffer buf)
          (when ewwm-qutebrowser-tab-sync-on-switch
            (ewwm-qutebrowser-ipc-send
             (format ":tab-select %d" (1+ index)))))
      (user-error "No tab buffer for index %d" index))))

(defun ewwm-qutebrowser-tab-next ()
  "Switch to the next qutebrowser tab."
  (interactive)
  (let* ((current (or ewwm-qutebrowser-tab--active-index 0))
         (count (ewwm-qutebrowser-tab--count))
         (next (if (< (1+ current) count) (1+ current) 0)))
    (ewwm-qutebrowser-tab-switch next)))

(defun ewwm-qutebrowser-tab-prev ()
  "Switch to the previous qutebrowser tab."
  (interactive)
  (let* ((current (or ewwm-qutebrowser-tab--active-index 0))
         (count (ewwm-qutebrowser-tab--count))
         (prev (if (> current 0) (1- current) (1- count))))
    (ewwm-qutebrowser-tab-switch prev)))

(defun ewwm-qutebrowser-tab-close (&optional index)
  "Close qutebrowser tab at INDEX (default: current)."
  (interactive)
  (let ((idx (or index ewwm-qutebrowser-tab--active-index 0)))
    (when ewwm-qutebrowser-tab-close-on-kill
      (ewwm-qutebrowser-ipc-send
       (format ":tab-close %d" (1+ idx))))
    (ewwm-qutebrowser-tab--kill idx)))

(defun ewwm-qutebrowser-tab-list ()
  "Display a list of all qutebrowser tab buffers."
  (interactive)
  (let ((tabs (ewwm-qutebrowser-tab--all)))
    (if (null tabs)
        (message "No qutebrowser tabs")
      (message "Tabs: %s"
               (mapconcat
                (lambda (buf)
                  (with-current-buffer buf
                    (format "%d:%s"
                            ewwm-qutebrowser-tab-index
                            (or ewwm-qutebrowser-tab-title "?"))))
                tabs ", ")))))

(provide 'ewwm-qutebrowser-tabs)
;;; ewwm-qutebrowser-tabs.el ends here
