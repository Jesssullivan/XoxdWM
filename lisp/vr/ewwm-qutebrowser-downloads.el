;;; ewwm-qutebrowser-downloads.el --- Download manager for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Download management for qutebrowser within EWWM.  Tracks active
;; downloads, provides progress in the mode-line, sends desktop
;; notifications on completion, and offers interactive commands
;; for listing, canceling, and opening downloads.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send
                  "ewwm-qutebrowser-ipc")
(declare-function notifications-notify "notifications")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-downloads nil
  "Download management for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-download-")

(defcustom ewwm-qutebrowser-download-dir "~/Downloads"
  "Default download directory."
  :type 'directory
  :group 'ewwm-qutebrowser-downloads)

(defcustom ewwm-qutebrowser-download-notify t
  "Non-nil to send notifications on download completion."
  :type 'boolean
  :group 'ewwm-qutebrowser-downloads)

(defcustom ewwm-qutebrowser-download-mode-line t
  "Non-nil to show download progress in the mode-line."
  :type 'boolean
  :group 'ewwm-qutebrowser-downloads)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-downloads--active nil
  "Alist of (ID . plist) for active downloads.
Each plist: :url :filename :progress :total :state.")

(defvar ewwm-qutebrowser-downloads--mode-line-string ""
  "Current mode-line string for download progress.")

;; ── Event handlers ───────────────────────────────────────────

(defun ewwm-qutebrowser-downloads--on-started (event)
  "Handle download start EVENT.
EVENT is a plist with :id :url :filename."
  (let ((id (plist-get event :id))
        (url (plist-get event :url))
        (filename (plist-get event :filename)))
    (setf (alist-get id
            ewwm-qutebrowser-downloads--active)
          (list :url url :filename filename
                :progress 0 :total 0 :state 'active))
    (ewwm-qutebrowser-downloads--update-mode-line)
    (message "Download started: %s"
             (file-name-nondirectory
              (or filename url)))))

(defun ewwm-qutebrowser-downloads--on-progress (event)
  "Update download progress from EVENT.
EVENT is a plist with :id :progress :total."
  (let ((id (plist-get event :id))
        (progress (plist-get event :progress))
        (total (plist-get event :total)))
    (when-let ((info (alist-get
                      id
                      ewwm-qutebrowser-downloads--active)))
      (plist-put info :progress progress)
      (plist-put info :total total)
      (ewwm-qutebrowser-downloads--update-mode-line))))

(defun ewwm-qutebrowser-downloads--on-completed (event)
  "Handle download completion EVENT.
EVENT is a plist with :id :filename."
  (let* ((id (plist-get event :id))
         (info (alist-get
                id ewwm-qutebrowser-downloads--active))
         (filename (or (plist-get event :filename)
                       (plist-get info :filename)))
         (basename (file-name-nondirectory
                    (or filename ""))))
    (when info
      (plist-put info :state 'completed))
    (setf (alist-get id
            ewwm-qutebrowser-downloads--active
            nil 'remove)
          nil)
    (ewwm-qutebrowser-downloads--update-mode-line)
    (when (and ewwm-qutebrowser-download-notify
               (fboundp 'notifications-notify))
      (notifications-notify
       :title "Download Complete"
       :body basename
       :app-name "ewwm"))
    (message "Download complete: %s" basename)))

;; ── Mode-line ────────────────────────────────────────────────

(defun ewwm-qutebrowser-downloads--update-mode-line ()
  "Format the mode-line string from active downloads."
  (setq ewwm-qutebrowser-downloads--mode-line-string
        (if (and ewwm-qutebrowser-download-mode-line
                 ewwm-qutebrowser-downloads--active)
            (format " DL:%d"
                    (length
                     ewwm-qutebrowser-downloads--active))
          ""))
  (force-mode-line-update t))

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-qutebrowser-downloads-cancel (id)
  "Cancel the download with ID via IPC."
  (interactive
   (list (string-to-number
          (completing-read
           "Cancel download ID: "
           (mapcar (lambda (e)
                     (number-to-string (car e)))
                   ewwm-qutebrowser-downloads--active)))))
  (when (fboundp 'ewwm-qutebrowser-ipc-send)
    (ewwm-qutebrowser-ipc-send
     (format ":download-cancel %d" id)))
  (setf (alist-get id
          ewwm-qutebrowser-downloads--active nil 'remove)
        nil)
  (ewwm-qutebrowser-downloads--update-mode-line)
  (message "Download %d canceled" id))

(defun ewwm-qutebrowser-downloads-list ()
  "Show a buffer listing active downloads."
  (interactive)
  (let ((buf (get-buffer-create "*ewwm-downloads*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "ewwm qutebrowser downloads\n")
        (insert (make-string 40 ?-) "\n\n")
        (if (null ewwm-qutebrowser-downloads--active)
            (insert "No active downloads.\n")
          (dolist (entry
                   ewwm-qutebrowser-downloads--active)
            (let* ((id (car entry))
                   (info (cdr entry))
                   (fname (plist-get info :filename))
                   (prog (plist-get info :progress))
                   (total (plist-get info :total))
                   (pct (if (and total (> total 0))
                            (/ (* 100 prog) total)
                          0)))
              (insert
               (format "  [%d] %s  %d%%  (%s)\n"
                       id
                       (file-name-nondirectory
                        (or fname "unknown"))
                       pct
                       (plist-get info :state)))))))
      (special-mode))
    (pop-to-buffer buf)))

(defun ewwm-qutebrowser-downloads-open (id)
  "Open the downloaded file for download ID."
  (interactive
   (list (string-to-number
          (completing-read
           "Open download ID: "
           (mapcar (lambda (e)
                     (number-to-string (car e)))
                   ewwm-qutebrowser-downloads--active)))))
  (let ((info (alist-get
               id ewwm-qutebrowser-downloads--active)))
    (if (not info)
        (message "Download %d not found" id)
      (let ((filename (plist-get info :filename)))
        (if (and filename (file-exists-p filename))
            (find-file filename)
          (message "File not found: %s" filename))))))

(provide 'ewwm-qutebrowser-downloads)
;;; ewwm-qutebrowser-downloads.el ends here
