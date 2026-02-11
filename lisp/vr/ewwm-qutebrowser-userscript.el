;;; ewwm-qutebrowser-userscript.el --- Userscript bridge for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Userscript management for qutebrowser within EWWM.  Provides
;; functions to run, install, and manage userscripts that bridge
;; qutebrowser and the Emacs window manager.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send
                  "ewwm-qutebrowser-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-userscript nil
  "Userscript bridge for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-userscript-")

(defcustom ewwm-qutebrowser-userscript-dir
  "~/.local/share/qutebrowser/userscripts/exwm-vr"
  "Directory for EWWM-managed qutebrowser userscripts."
  :type 'directory
  :group 'ewwm-qutebrowser-userscript)

(defcustom ewwm-qutebrowser-userscript-auto-install t
  "Non-nil to auto-install bundled userscripts on init."
  :type 'boolean
  :group 'ewwm-qutebrowser-userscript)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-userscript--results nil
  "Alist of (NAME . result) from userscript runs.")

;; ── Bundled scripts ──────────────────────────────────────────

(defvar ewwm-qutebrowser-userscript--bundled
  '(("emacs-open" .
     "#!/bin/bash\n\
# Open current URL in eww or external Emacs browser\n\
echo \"open-url $QUTE_URL\" \
| socat - UNIX:\"$EWWM_SOCKET\"\n")
    ("yank-url" .
     "#!/bin/bash\n\
# Yank current URL to Emacs kill ring\n\
echo \"yank $QUTE_URL\" \
| socat - UNIX:\"$EWWM_SOCKET\"\n")
    ("dom-to-org" .
     "#!/usr/bin/env python3\n\
\"\"\"Extract page content as org-mode text.\"\"\"\n\
import os\n\
url = os.environ.get('QUTE_URL', '')\n\
title = os.environ.get('QUTE_TITLE', '')\n\
print(f'* [[{url}][{title}]]')\n"))
  "Alist of (NAME . CONTENT) for bundled userscripts.")

;; ── Directory management ─────────────────────────────────────

(defun ewwm-qutebrowser-userscript--ensure-dir ()
  "Ensure the userscript directory exists.
Creates the directory if needed and returns its path."
  (let ((dir (expand-file-name
              ewwm-qutebrowser-userscript-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; ── List installed ───────────────────────────────────────────

(defun ewwm-qutebrowser-userscript--list-installed ()
  "List installed userscripts in the script directory.
Returns a list of file names (without directory)."
  (let ((dir (expand-file-name
              ewwm-qutebrowser-userscript-dir)))
    (when (file-directory-p dir)
      (cl-remove-if
       (lambda (f) (string-prefix-p "." f))
       (directory-files dir nil nil t)))))

;; ── Install ──────────────────────────────────────────────────

(defun ewwm-qutebrowser-userscript-install ()
  "Install bundled userscripts to the script directory.
Writes each bundled script and sets executable permissions."
  (interactive)
  (let ((dir (ewwm-qutebrowser-userscript--ensure-dir))
        (count 0))
    (dolist (entry
             ewwm-qutebrowser-userscript--bundled)
      (let ((path (expand-file-name (car entry) dir)))
        (with-temp-file path
          (insert (cdr entry)))
        (set-file-modes path #o755)
        (cl-incf count)))
    (message
     "ewwm-qutebrowser-userscript: installed %d to %s"
     count dir)))

;; ── Run userscript ───────────────────────────────────────────

(defun ewwm-qutebrowser-userscript-run (name &rest args)
  "Run userscript NAME with optional ARGS via IPC.
Sends :spawn -u to qutebrowser pointing to the script."
  (let* ((dir (expand-file-name
               ewwm-qutebrowser-userscript-dir))
         (path (expand-file-name name dir))
         (cmd (if args
                  (format ":spawn -u %s %s"
                          path
                          (mapconcat #'identity args " "))
                (format ":spawn -u %s" path))))
    (unless (file-exists-p path)
      (user-error "Userscript not found: %s" path))
    (when (fboundp 'ewwm-qutebrowser-ipc-send)
      (ewwm-qutebrowser-ipc-send cmd))
    (message "Running userscript: %s" name)))

;; ── Event handler ────────────────────────────────────────────

(defun ewwm-qutebrowser-userscript--on-result (event)
  "Handle userscript result EVENT.
EVENT is a plist with :name and :result keys."
  (let ((name (plist-get event :name))
        (result (plist-get event :result)))
    (setf (alist-get name
            ewwm-qutebrowser-userscript--results
            nil nil #'string=)
          result)
    (message "Userscript %s: %s" name
             (if result "completed" "no result"))))

(provide 'ewwm-qutebrowser-userscript)
;;; ewwm-qutebrowser-userscript.el ends here
