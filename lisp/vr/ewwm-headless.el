;;; ewwm-headless.el --- Headless mode for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Headless mode support for ewwm.  Enables running the compositor without
;; a physical display, auto-detects headless environments, disables VR
;; features that require hardware, manages virtual outputs, and provides
;; terminal-friendly keybindings.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-headless nil
  "Headless mode settings for EWWM."
  :group 'ewwm
  :prefix "ewwm-headless-")

(defcustom ewwm-headless-mode nil
  "Non-nil when running in headless mode."
  :type 'boolean
  :group 'ewwm-headless)

(defcustom ewwm-headless-terminal-keys t
  "Non-nil to enable terminal-friendly keybindings in headless mode."
  :type 'boolean
  :group 'ewwm-headless)

(defcustom ewwm-headless-virtual-outputs 1
  "Number of virtual outputs to create in headless mode."
  :type 'integer
  :group 'ewwm-headless)

(defcustom ewwm-headless-resolution "1920x1080"
  "Default resolution for virtual outputs (WIDTHxHEIGHT)."
  :type 'string
  :group 'ewwm-headless)

(defcustom ewwm-headless-status-interval 60
  "Seconds between periodic status log messages.
Set to 0 to disable periodic logging."
  :type 'integer
  :group 'ewwm-headless)

(defcustom ewwm-headless-disable-features '(vr gaze wink fatigue eye-tracking)
  "List of feature symbols to disable in headless mode.
Each symbol corresponds to a subsystem that requires physical hardware."
  :type '(repeat symbol)
  :group 'ewwm-headless)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-headless--status-timer nil
  "Timer for periodic status logging, or nil if not running.")

(defvar ewwm-headless--active-outputs nil
  "List of virtual output plists.
Each entry: (:id N :resolution \"WxH\" :name \"output-N\").")

;; ── Feature disable ──────────────────────────────────────────

(defun ewwm-headless--disable-vr-features ()
  "Disable VR-related features listed in `ewwm-headless-disable-features'.
Sets the enable defcustom of each feature to nil when bound."
  (dolist (feat ewwm-headless-disable-features)
    (let ((var (intern (format "ewwm-vr-%s-enable" feat))))
      (when (boundp var)
        (set var nil)))
    ;; Also try without vr- prefix for top-level features
    (let ((var2 (intern (format "ewwm-%s-enable" feat))))
      (when (boundp var2)
        (set var2 nil)))))

;; ── Terminal-friendly keys ───────────────────────────────────

(defun ewwm-headless--setup-terminal-keys ()
  "Set up terminal-friendly keybindings.
When `ewwm-headless-terminal-keys' is non-nil, bind keys that
work without a graphical display."
  (when (and ewwm-headless-terminal-keys
             (boundp 'ewwm-mode-map))
    (define-key ewwm-mode-map (kbd "C-c w s") #'ewwm-headless-status)
    (define-key ewwm-mode-map (kbd "C-c w a") #'ewwm-headless-add-output)
    (define-key ewwm-mode-map (kbd "C-c w r") #'ewwm-headless-remove-output)
    (define-key ewwm-mode-map (kbd "C-c w R") #'ewwm-headless-set-resolution)))

;; ── Output management ────────────────────────────────────────

(defun ewwm-headless--format-output (output)
  "Format OUTPUT plist for display.
OUTPUT is a plist with :id, :resolution, and :name keys."
  (format "output-%s %s (%s)"
          (or (plist-get output :id) "?")
          (or (plist-get output :name) "unnamed")
          (or (plist-get output :resolution) "unknown")))

(defun ewwm-headless-add-output (&optional resolution)
  "Add a virtual output with RESOLUTION.
RESOLUTION defaults to `ewwm-headless-resolution'.
Sends headless-add-output IPC command to compositor."
  (interactive
   (list (read-string
          (format "Resolution (default %s): " ewwm-headless-resolution)
          nil nil ewwm-headless-resolution)))
  (let ((res (or resolution ewwm-headless-resolution)))
    (when (fboundp 'ewwm-ipc-send)
      (ewwm-ipc-send `(:type :headless-add-output :resolution ,res)))
    (let ((id (1+ (length ewwm-headless--active-outputs))))
      (push (list :id id :resolution res :name (format "virtual-%d" id))
            ewwm-headless--active-outputs))
    (message "ewwm-headless: added virtual output (%s)" res)))

(defun ewwm-headless-remove-output (&optional output-id)
  "Remove virtual output with OUTPUT-ID.
Sends headless-remove-output IPC command to compositor."
  (interactive
   (list (string-to-number
          (read-string "Output ID to remove: "))))
  (let ((id (or output-id 1)))
    (when (fboundp 'ewwm-ipc-send)
      (ewwm-ipc-send `(:type :headless-remove-output :id ,id)))
    (setq ewwm-headless--active-outputs
          (cl-remove-if (lambda (o) (eql (plist-get o :id) id))
                        ewwm-headless--active-outputs))
    (message "ewwm-headless: removed output %d" id)))

(defun ewwm-headless-set-resolution (resolution &optional output-id)
  "Set RESOLUTION on virtual output OUTPUT-ID.
OUTPUT-ID defaults to 1.  Sends headless-set-resolution IPC command."
  (interactive
   (list (read-string
          (format "Resolution (default %s): " ewwm-headless-resolution)
          nil nil ewwm-headless-resolution)
         (string-to-number
          (read-string "Output ID (default 1): " nil nil "1"))))
  (let ((id (or output-id 1)))
    (when (fboundp 'ewwm-ipc-send)
      (ewwm-ipc-send `(:type :headless-set-resolution :id ,id :resolution ,resolution)))
    ;; Update local tracking
    (dolist (output ewwm-headless--active-outputs)
      (when (eql (plist-get output :id) id)
        (plist-put output :resolution resolution)))
    (message "ewwm-headless: output %d resolution set to %s" id resolution)))

;; ── Status ───────────────────────────────────────────────────

(defun ewwm-headless--log-status ()
  "Log periodic headless status.  Called by `ewwm-headless--status-timer'."
  (when ewwm-headless-mode
    (message "ewwm-headless: outputs=%d surfaces=%d"
             (length ewwm-headless--active-outputs)
             (length ewwm--surface-buffer-alist))))

(defun ewwm-headless-status ()
  "Display headless mode status in the minibuffer."
  (interactive)
  (if (not ewwm-headless-mode)
      (message "ewwm-headless: not in headless mode")
    (let ((outputs (length ewwm-headless--active-outputs))
          (surfaces (length ewwm--surface-buffer-alist))
          (disabled (mapconcat #'symbol-name
                               ewwm-headless-disable-features
                               ", ")))
      (message "ewwm-headless: mode=ON outputs=%d surfaces=%d disabled=[%s]"
               outputs surfaces disabled))))

;; ── Mode-line ────────────────────────────────────────────────

(defun ewwm-headless-mode-line-string ()
  "Return a mode-line string for headless status."
  (when ewwm-headless-mode
    " [H]"))

;; ── Auto-detection ───────────────────────────────────────────

(defun ewwm-headless-detect ()
  "Auto-detect whether we are running in headless mode.
Checks for missing DISPLAY and WAYLAND_DISPLAY environment variables,
and queries the compositor for headless flag via IPC.
Returns non-nil if headless mode is detected."
  (let ((no-display (not (getenv "DISPLAY")))
        (no-wayland (not (getenv "WAYLAND_DISPLAY")))
        (compositor-headless nil))
    ;; Query compositor if IPC is available
    (when (and (fboundp 'ewwm-ipc-connected-p)
               (ewwm-ipc-connected-p)
               (fboundp 'ewwm-ipc-send-sync))
      (condition-case nil
          (let ((resp (ewwm-ipc-send-sync '(:type :headless-status))))
            (when (eq (plist-get resp :status) :ok)
              (setq compositor-headless (eq (plist-get resp :headless) t))))
        (error nil)))
    (or compositor-headless (and no-display no-wayland))))

;; ── Init / teardown ──────────────────────────────────────────

(defun ewwm-headless-init ()
  "Initialize headless mode.
Disables VR features, sets up terminal keys, creates virtual outputs,
and starts periodic status logging."
  (setq ewwm-headless-mode t)
  (ewwm-headless--disable-vr-features)
  (ewwm-headless--setup-terminal-keys)
  ;; Create initial virtual outputs
  (setq ewwm-headless--active-outputs nil)
  (dotimes (i ewwm-headless-virtual-outputs)
    (push (list :id (1+ i)
                :resolution ewwm-headless-resolution
                :name (format "virtual-%d" (1+ i)))
          ewwm-headless--active-outputs))
  ;; Start status timer
  (when (> ewwm-headless-status-interval 0)
    (setq ewwm-headless--status-timer
          (run-with-timer ewwm-headless-status-interval
                          ewwm-headless-status-interval
                          #'ewwm-headless--log-status)))
  (message "ewwm-headless: initialized with %d virtual output(s)"
           ewwm-headless-virtual-outputs))

(defun ewwm-headless-teardown ()
  "Clean up headless mode state."
  (when ewwm-headless--status-timer
    (cancel-timer ewwm-headless--status-timer))
  (setq ewwm-headless-mode nil
        ewwm-headless--status-timer nil
        ewwm-headless--active-outputs nil))

(provide 'ewwm-headless)
;;; ewwm-headless.el ends here
