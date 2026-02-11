;;; ewwm-qutebrowser-gaze.el --- Gaze-driven browsing for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Gaze-driven interaction for qutebrowser: gaze scrolling and
;; gaze-based link following.  Bridges the VR eye-tracking pipeline
;; with qutebrowser userscripts.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send "ewwm-qutebrowser-ipc")
(declare-function ewwm-qutebrowser-current-surface
                  "ewwm-qutebrowser")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-gaze nil
  "Gaze-driven browsing for qutebrowser."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-gaze-")

(defcustom ewwm-qutebrowser-gaze-scroll-speed 2.0
  "Scroll speed multiplier for gaze scrolling."
  :type 'number
  :group 'ewwm-qutebrowser-gaze)

(defcustom ewwm-qutebrowser-gaze-scroll-margin 0.15
  "Fraction of viewport height as the scroll trigger margin.
When gaze enters the top or bottom margin, scrolling begins."
  :type 'number
  :group 'ewwm-qutebrowser-gaze)

(defcustom ewwm-qutebrowser-gaze-follow-dwell-ms 600
  "Dwell time in milliseconds to confirm link selection."
  :type 'integer
  :group 'ewwm-qutebrowser-gaze)

(defcustom ewwm-qutebrowser-gaze-follow-enable t
  "Non-nil to enable gaze-based link following."
  :type 'boolean
  :group 'ewwm-qutebrowser-gaze)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-gaze--hints nil
  "Current hint elements from the page.
List of plists: ((:id N :text STR :rect (:x X :y Y :w W :h H) :url URL) ...).")

(defvar ewwm-qutebrowser-gaze--highlighted nil
  "Index of the currently highlighted hint.")

(defvar ewwm-qutebrowser-gaze--dwell-timer nil
  "Timer for gaze dwell confirmation.")

;; ── Gaze scrolling ───────────────────────────────────────────

(defun ewwm-qutebrowser-gaze-scroll-toggle ()
  "Toggle gaze-based scrolling."
  (interactive)
  (message "Gaze scroll: toggled"))

;; ── Gaze link following ──────────────────────────────────────

(defun ewwm-qutebrowser-gaze-follow-toggle ()
  "Toggle gaze-based link following."
  (interactive)
  (setq ewwm-qutebrowser-gaze-follow-enable
        (not ewwm-qutebrowser-gaze-follow-enable))
  (message "Gaze follow: %s"
           (if ewwm-qutebrowser-gaze-follow-enable "on" "off")))

(defun ewwm-qutebrowser-gaze--load-hints (hints)
  "Load HINTS data from the page userscript.
HINTS is a list of plists with :id, :text, :rect, :url."
  (setq ewwm-qutebrowser-gaze--hints hints
        ewwm-qutebrowser-gaze--highlighted nil))

(defun ewwm-qutebrowser-gaze--highlight (index)
  "Highlight hint at INDEX."
  (setq ewwm-qutebrowser-gaze--highlighted index)
  (when (and (integerp index) (< index (length ewwm-qutebrowser-gaze--hints)))
    (ewwm-qutebrowser-ipc-send
     (format ":jseval document.querySelector('[data-ewwm-hint=\"%d\"]').style.outline='2px solid red'" index))))

(defun ewwm-qutebrowser-gaze--confirm ()
  "Confirm the currently highlighted hint."
  (when-let ((idx ewwm-qutebrowser-gaze--highlighted))
    (when (< idx (length ewwm-qutebrowser-gaze--hints))
      (let ((hint (nth idx ewwm-qutebrowser-gaze--hints)))
        (when-let ((url (plist-get hint :url)))
          (ewwm-qutebrowser-ipc-send (format ":open %s" url)))))))

(provide 'ewwm-qutebrowser-gaze)
;;; ewwm-qutebrowser-gaze.el ends here
