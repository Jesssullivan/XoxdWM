;;; ewwm-qutebrowser-adblock.el --- Ad blocker for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Ad blocker configuration and management for qutebrowser.
;; Manages filter lists, generates Python config for the built-in
;; ad blocker, and tracks blocked request counts.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send
                  "ewwm-qutebrowser-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-adblock nil
  "Ad blocker integration for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-adblock-")

(defcustom ewwm-qutebrowser-adblock-lists
  '("https://easylist.to/easylist/easylist.txt"
    "https://easylist.to/easylist/easyprivacy.txt")
  "List of ad blocker filter list URLs."
  :type '(repeat string)
  :group 'ewwm-qutebrowser-adblock)

(defcustom ewwm-qutebrowser-adblock-method 'both
  "Ad blocking method for qutebrowser.
`both'    - use both Brave adblock and hosts blocking.
`adblock' - use only Brave adblock engine.
`hosts'   - use only hosts-based blocking."
  :type '(choice (const :tag "Both methods" both)
                 (const :tag "Adblock only" adblock)
                 (const :tag "Hosts only" hosts))
  :group 'ewwm-qutebrowser-adblock)

(defcustom ewwm-qutebrowser-adblock-mode-line nil
  "Non-nil to show blocked count in the mode-line."
  :type 'boolean
  :group 'ewwm-qutebrowser-adblock)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-adblock--blocked-count 0
  "Number of requests blocked in this session.")

(defvar ewwm-qutebrowser-adblock--mode-line-string ""
  "Current mode-line string for blocked count.")

;; ── Config generation ────────────────────────────────────────

(defun ewwm-qutebrowser-adblock--generate-config ()
  "Generate Python config for qutebrowser ad blocking.
Returns a string of c.content.blocking.* settings."
  (let ((method-str
         (pcase ewwm-qutebrowser-adblock-method
           ('both "'adblock', 'hosts'")
           ('adblock "'adblock'")
           ('hosts "'hosts'"))))
    (concat
     "# Ad blocker config - ewwm-qutebrowser-adblock.el\n"
     "c.content.blocking.enabled = True\n"
     (format "c.content.blocking.method = [%s]\n"
             method-str)
     "c.content.blocking.adblock.lists = [\n"
     (mapconcat (lambda (url)
                  (format "    '%s'" url))
                ewwm-qutebrowser-adblock-lists
                ",\n")
     "\n]\n")))

;; ── Commands ─────────────────────────────────────────────────

(defun ewwm-qutebrowser-adblock-update ()
  "Send :adblock-update to qutebrowser via IPC.
Triggers re-download and compilation of filter lists."
  (interactive)
  (when (fboundp 'ewwm-qutebrowser-ipc-send)
    (ewwm-qutebrowser-ipc-send ":adblock-update"))
  (message "ewwm-qutebrowser-adblock: update triggered"))

(defun ewwm-qutebrowser-adblock-write-config ()
  "Write ad blocker config to the qutebrowser config dir.
Generates Python config and writes it to a file."
  (interactive)
  (let* ((config
          (ewwm-qutebrowser-adblock--generate-config))
         (path (expand-file-name
                "~/.config/qutebrowser/\
exwm-vr-adblock.py")))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert config))
    (when (fboundp 'ewwm-qutebrowser-ipc-send)
      (ewwm-qutebrowser-ipc-send
       (format ":config-source %s" path)))
    (message
     "ewwm-qutebrowser-adblock: config written to %s"
     path)))

;; ── Event handler ────────────────────────────────────────────

(defun ewwm-qutebrowser-adblock--on-blocked (event)
  "Handle blocked request EVENT.
Increments blocked counter and updates mode-line."
  (ignore event)
  (cl-incf ewwm-qutebrowser-adblock--blocked-count)
  (when ewwm-qutebrowser-adblock-mode-line
    (setq ewwm-qutebrowser-adblock--mode-line-string
          (format " Block:%d"
                  ewwm-qutebrowser-adblock--blocked-count))
    (force-mode-line-update t)))

(provide 'ewwm-qutebrowser-adblock)
;;; ewwm-qutebrowser-adblock.el ends here
