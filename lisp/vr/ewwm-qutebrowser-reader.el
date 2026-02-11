;;; ewwm-qutebrowser-reader.el --- Reader mode for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Reader mode integration for qutebrowser.  Generates CSS from
;; Emacs-configurable defcustoms and applies it to web pages via
;; qutebrowser jseval commands for a distraction-free reading
;; experience.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send
                  "ewwm-qutebrowser-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-reader nil
  "Reader mode settings for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-reader-")

(defcustom ewwm-qutebrowser-reader-font-size 18
  "Font size in pixels for reader mode."
  :type 'integer
  :group 'ewwm-qutebrowser-reader)

(defcustom ewwm-qutebrowser-reader-line-spacing 1.8
  "Line spacing multiplier for reader mode."
  :type 'float
  :group 'ewwm-qutebrowser-reader)

(defcustom ewwm-qutebrowser-reader-max-width "60ch"
  "Maximum content width for reader mode.
Accepts any valid CSS width value."
  :type 'string
  :group 'ewwm-qutebrowser-reader)

(defcustom ewwm-qutebrowser-reader-dark-mode t
  "Non-nil to use dark background in reader mode."
  :type 'boolean
  :group 'ewwm-qutebrowser-reader)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-reader--active nil
  "Non-nil when reader mode is currently active.")

;; ── CSS generation ───────────────────────────────────────────

(defun ewwm-qutebrowser-reader--generate-css ()
  "Generate CSS string from reader mode defcustoms.
Returns a minified CSS string for injection."
  (let ((bg (if ewwm-qutebrowser-reader-dark-mode
                "#1a1a2e" "#fafafa"))
        (fg (if ewwm-qutebrowser-reader-dark-mode
                "#e0e0e0" "#1a1a1a"))
        (link (if ewwm-qutebrowser-reader-dark-mode
                  "#82aaff" "#1565c0")))
    (format
     "body{background:%s!important;\
color:%s!important;\
font-size:%dpx!important;\
line-height:%s!important;\
max-width:%s!important;\
margin:0 auto!important;\
padding:2em!important;\
font-family:Georgia,serif!important}\
a{color:%s!important}\
img{max-width:100%%!important;height:auto!important}\
nav,header,footer,aside,.sidebar,.ad\
{display:none!important}"
     bg fg
     ewwm-qutebrowser-reader-font-size
     (number-to-string
      ewwm-qutebrowser-reader-line-spacing)
     ewwm-qutebrowser-reader-max-width
     link)))

;; ── Apply / toggle ───────────────────────────────────────────

(defun ewwm-qutebrowser-reader--apply ()
  "Apply reader mode CSS to the current qutebrowser page.
Sends a jseval command to inject the stylesheet."
  (let ((css (ewwm-qutebrowser-reader--generate-css)))
    (when (fboundp 'ewwm-qutebrowser-ipc-send)
      (ewwm-qutebrowser-ipc-send
       (format
        ":jseval -q \
var s=document.createElement('style');\
s.id='ewwm-reader';s.textContent='%s';\
document.head.appendChild(s);"
        css)))))

(defun ewwm-qutebrowser-reader--remove ()
  "Remove reader mode CSS from the current page."
  (when (fboundp 'ewwm-qutebrowser-ipc-send)
    (ewwm-qutebrowser-ipc-send
     ":jseval -q \
var s=document.getElementById('ewwm-reader');\
if(s)s.remove();")))

(defun ewwm-qutebrowser-reader-mode ()
  "Toggle reader mode on the current qutebrowser page.
Injects or removes a CSS stylesheet that reformats the
page for comfortable reading."
  (interactive)
  (if ewwm-qutebrowser-reader--active
      (progn
        (ewwm-qutebrowser-reader--remove)
        (setq ewwm-qutebrowser-reader--active nil)
        (message "Reader mode disabled"))
    (ewwm-qutebrowser-reader--apply)
    (setq ewwm-qutebrowser-reader--active t)
    (message "Reader mode enabled")))

(provide 'ewwm-qutebrowser-reader)
;;; ewwm-qutebrowser-reader.el ends here
