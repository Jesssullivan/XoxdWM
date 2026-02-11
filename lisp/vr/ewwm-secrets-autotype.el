;;; ewwm-secrets-autotype.el --- Auto-type dispatcher for EWWM secrets  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Central auto-type dispatcher for the EWWM secrets subsystem.
;; Selects the best available backend (compositor wl_keyboard injection,
;; or ydotool) and orchestrates credential entry with optional gaze-away
;; safety and secure-input mode integration.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")
;; Backends
(declare-function ewwm-secrets-compositor-available-p "ewwm-secrets-compositor")
(declare-function ewwm-secrets-compositor--type-string "ewwm-secrets-compositor")
(declare-function ewwm-secrets-compositor--type-credentials "ewwm-secrets-compositor")
(declare-function ewwm-secrets-compositor--abort "ewwm-secrets-compositor")
(declare-function ewwm-secrets-ydotool-available-p "ewwm-secrets-ydotool")
(declare-function ewwm-secrets-ydotool--type-string "ewwm-secrets-ydotool")
(declare-function ewwm-secrets-ydotool--type-credentials "ewwm-secrets-ydotool")
(declare-function ewwm-secrets-ydotool--type-with-tab-fields "ewwm-secrets-ydotool")
;; Gaze-away safety
(declare-function ewwm-secrets-gaze-away-start "ewwm-secrets-gaze-away")
(declare-function ewwm-secrets-gaze-away-stop "ewwm-secrets-gaze-away")
;; Secure input
(declare-function ewwm-vr-secure-input-enter "ewwm-vr-secure-input")
(declare-function ewwm-vr-secure-input-exit "ewwm-vr-secure-input")
;; Secret retrieval
(declare-function ewwm-secrets-get "ewwm-secrets")
(declare-function ewwm-secrets-search "ewwm-secrets")
(declare-function ewwm-keepassxc-get-logins "ewwm-keepassxc-browser")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-secrets-autotype nil
  "Auto-type dispatcher for EWWM secrets."
  :group 'ewwm)

(defcustom ewwm-secrets-autotype-backend 'auto
  "Auto-type backend to use.
`auto' selects the best available backend at runtime:
  1. compositor (wl_keyboard injection) — preferred when IPC connected
  2. ydotool — fallback when compositor backend unavailable
`compositor' forces compositor backend only.
`ydotool' forces ydotool backend only."
  :type '(choice (const :tag "Auto-select best available" auto)
                 (const :tag "Compositor (wl_keyboard)" compositor)
                 (const :tag "Ydotool" ydotool))
  :group 'ewwm-secrets-autotype)

(defcustom ewwm-secrets-autotype-secure-input t
  "Non-nil to enter secure input mode during auto-type.
Disables gaze/wink/EEG subsystems while credentials are being typed."
  :type 'boolean
  :group 'ewwm-secrets-autotype)

(defcustom ewwm-secrets-autotype-gaze-away t
  "Non-nil to enable gaze-away safety during auto-type.
Pauses typing if gaze leaves the target surface."
  :type 'boolean
  :group 'ewwm-secrets-autotype)

(defcustom ewwm-secrets-autotype-clear-after t
  "Non-nil to zero credential strings after typing."
  :type 'boolean
  :group 'ewwm-secrets-autotype)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-secrets-autotype-start-hook nil
  "Hook run before an auto-type operation begins.")

(defvar ewwm-secrets-autotype-complete-hook nil
  "Hook run after an auto-type operation completes successfully.")

(defvar ewwm-secrets-autotype-error-hook nil
  "Hook run when an auto-type operation fails.
Functions receive one argument: the error message string.")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-secrets-autotype--active nil
  "Non-nil when an auto-type operation is in progress.")

(defvar ewwm-secrets-autotype--backend-name nil
  "Name of the backend currently in use, or nil.")

;; ── Backend selection ────────────────────────────────────────

(defun ewwm-secrets-autotype--select-backend ()
  "Select and return the auto-type backend to use.
Returns a symbol: `compositor', `ydotool', or nil if none available."
  (pcase ewwm-secrets-autotype-backend
    ('compositor
     (if (and (fboundp 'ewwm-secrets-compositor-available-p)
              (ewwm-secrets-compositor-available-p))
         'compositor
       nil))
    ('ydotool
     (if (and (fboundp 'ewwm-secrets-ydotool-available-p)
              (ewwm-secrets-ydotool-available-p))
         'ydotool
       nil))
    ('auto
     (cond
      ((and (fboundp 'ewwm-secrets-compositor-available-p)
            (ewwm-secrets-compositor-available-p))
       'compositor)
      ((and (fboundp 'ewwm-secrets-ydotool-available-p)
            (ewwm-secrets-ydotool-available-p))
       'ydotool)
      (t nil)))
    (_ nil)))

(defun ewwm-secrets-autotype-available-p ()
  "Return non-nil if any auto-type backend is available."
  (not (null (ewwm-secrets-autotype--select-backend))))

;; ── Core auto-type functions ─────────────────────────────────

(defun ewwm-secrets-autotype-string (string &optional surface-id)
  "Type STRING via the selected auto-type backend.
SURFACE-ID is used by the compositor backend for targeted input.
Handles secure-input and gaze-away activation around the typing."
  (let ((backend (ewwm-secrets-autotype--select-backend)))
    (unless backend
      (error "ewwm-secrets-autotype: no auto-type backend available"))
    (setq ewwm-secrets-autotype--active t
          ewwm-secrets-autotype--backend-name backend)
    (run-hooks 'ewwm-secrets-autotype-start-hook)
    (condition-case err
        (progn
          ;; Enter secure input mode
          (when (and ewwm-secrets-autotype-secure-input
                     (fboundp 'ewwm-vr-secure-input-enter))
            (ewwm-vr-secure-input-enter "autotype" surface-id))
          ;; Start gaze-away monitoring
          (when (and ewwm-secrets-autotype-gaze-away
                     surface-id
                     (fboundp 'ewwm-secrets-gaze-away-start))
            (ewwm-secrets-gaze-away-start surface-id))
          ;; Dispatch to backend
          (pcase backend
            ('compositor
             (ewwm-secrets-compositor--type-string string surface-id))
            ('ydotool
             (ewwm-secrets-ydotool--type-string string)))
          ;; Cleanup
          (ewwm-secrets-autotype--cleanup)
          (when ewwm-secrets-autotype-clear-after
            (clear-string string))
          (run-hooks 'ewwm-secrets-autotype-complete-hook))
      (error
       (ewwm-secrets-autotype--cleanup)
       (let ((msg (error-message-string err)))
         (run-hook-with-args 'ewwm-secrets-autotype-error-hook msg)
         (error "ewwm-secrets-autotype: %s" msg))))))

(defun ewwm-secrets-autotype-credentials (username password &optional surface-id)
  "Type USERNAME + Tab + PASSWORD + Enter via auto-type.
SURFACE-ID is used by the compositor backend for targeted input.
Zeros PASSWORD from memory after typing if `ewwm-secrets-autotype-clear-after'."
  (let ((backend (ewwm-secrets-autotype--select-backend)))
    (unless backend
      (error "ewwm-secrets-autotype: no auto-type backend available"))
    (setq ewwm-secrets-autotype--active t
          ewwm-secrets-autotype--backend-name backend)
    (run-hooks 'ewwm-secrets-autotype-start-hook)
    (condition-case err
        (progn
          ;; Enter secure input mode
          (when (and ewwm-secrets-autotype-secure-input
                     (fboundp 'ewwm-vr-secure-input-enter))
            (ewwm-vr-secure-input-enter "autotype-creds" surface-id))
          ;; Start gaze-away monitoring
          (when (and ewwm-secrets-autotype-gaze-away
                     surface-id
                     (fboundp 'ewwm-secrets-gaze-away-start))
            (ewwm-secrets-gaze-away-start surface-id))
          ;; Dispatch to backend
          (pcase backend
            ('compositor
             (ewwm-secrets-compositor--type-credentials
              username password (or surface-id 0)))
            ('ydotool
             (ewwm-secrets-ydotool--type-credentials username password)))
          ;; Cleanup
          (ewwm-secrets-autotype--cleanup)
          (when ewwm-secrets-autotype-clear-after
            (clear-string password))
          (run-hooks 'ewwm-secrets-autotype-complete-hook))
      (error
       (ewwm-secrets-autotype--cleanup)
       (when ewwm-secrets-autotype-clear-after
         (clear-string password))
       (let ((msg (error-message-string err)))
         (run-hook-with-args 'ewwm-secrets-autotype-error-hook msg)
         (error "ewwm-secrets-autotype: %s" msg))))))

(defun ewwm-secrets-autotype-fields (fields &optional surface-id)
  "Type a list of FIELDS separated by Tab, ending with Enter.
SURFACE-ID is used by the compositor backend for targeted input."
  (let ((backend (ewwm-secrets-autotype--select-backend)))
    (unless backend
      (error "ewwm-secrets-autotype: no auto-type backend available"))
    (setq ewwm-secrets-autotype--active t
          ewwm-secrets-autotype--backend-name backend)
    (run-hooks 'ewwm-secrets-autotype-start-hook)
    (condition-case err
        (let ((combined (concat (mapconcat #'identity fields "\t") "\n")))
          ;; Enter secure input mode
          (when (and ewwm-secrets-autotype-secure-input
                     (fboundp 'ewwm-vr-secure-input-enter))
            (ewwm-vr-secure-input-enter "autotype-fields" surface-id))
          ;; Gaze-away monitoring
          (when (and ewwm-secrets-autotype-gaze-away
                     surface-id
                     (fboundp 'ewwm-secrets-gaze-away-start))
            (ewwm-secrets-gaze-away-start surface-id))
          ;; Dispatch
          (pcase backend
            ('compositor
             (ewwm-secrets-compositor--type-string combined surface-id))
            ('ydotool
             (ewwm-secrets-ydotool--type-with-tab-fields fields)))
          ;; Cleanup
          (ewwm-secrets-autotype--cleanup)
          (when ewwm-secrets-autotype-clear-after
            (dolist (f fields) (clear-string f))
            (clear-string combined))
          (run-hooks 'ewwm-secrets-autotype-complete-hook))
      (error
       (ewwm-secrets-autotype--cleanup)
       (let ((msg (error-message-string err)))
         (run-hook-with-args 'ewwm-secrets-autotype-error-hook msg)
         (error "ewwm-secrets-autotype: %s" msg))))))

(defun ewwm-secrets-autotype-abort ()
  "Abort the current auto-type operation."
  (when ewwm-secrets-autotype--active
    (pcase ewwm-secrets-autotype--backend-name
      ('compositor
       (when (fboundp 'ewwm-secrets-compositor--abort)
         (ewwm-secrets-compositor--abort))))
    (ewwm-secrets-autotype--cleanup)
    (message "ewwm-secrets-autotype: aborted")))

(defun ewwm-secrets-autotype--cleanup ()
  "Clean up after an auto-type operation.
Stops gaze-away monitoring, exits secure input mode, resets state."
  (when (and ewwm-secrets-autotype-gaze-away
             (fboundp 'ewwm-secrets-gaze-away-stop))
    (ewwm-secrets-gaze-away-stop))
  (when (and ewwm-secrets-autotype-secure-input
             (fboundp 'ewwm-vr-secure-input-exit))
    (ewwm-vr-secure-input-exit))
  (setq ewwm-secrets-autotype--active nil
        ewwm-secrets-autotype--backend-name nil))

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-secrets-autotype-login (&optional url)
  "Auto-type credentials for URL into the focused surface.
Queries Secret Service or KeePassXC for matching logins.
When called interactively, prompts for URL."
  (interactive (list (read-string "URL: ")))
  (let* ((logins (ewwm-keepassxc-get-logins url))
         (login (cond
                 ((null logins)
                  (error "No logins found for %s" url))
                 ((= (length logins) 1)
                  (car logins))
                 (t
                  (let* ((names (mapcar (lambda (l)
                                          (or (plist-get l :name)
                                              (plist-get l :login)
                                              "unknown"))
                                        logins))
                         (choice (completing-read "Select login: " names nil t)))
                    (cl-find choice logins
                             :test #'string=
                             :key (lambda (l)
                                    (or (plist-get l :name)
                                        (plist-get l :login)
                                        "unknown")))))))
         (username (plist-get login :login))
         (password (plist-get login :password)))
    (unless (and username password)
      (error "Incomplete credentials for %s" url))
    (ewwm-secrets-autotype-credentials username password)
    (message "ewwm-secrets-autotype: login complete for %s" url)))

(defun ewwm-secrets-autotype-status ()
  "Display auto-type dispatcher status."
  (interactive)
  (let ((backend (ewwm-secrets-autotype--select-backend)))
    (message (concat "ewwm-secrets-autotype status:\n"
                     "  backend-pref: %s\n"
                     "  selected:     %s\n"
                     "  active:       %s\n"
                     "  secure-input: %s\n"
                     "  gaze-away:    %s")
             ewwm-secrets-autotype-backend
             (or backend "none available")
             (if ewwm-secrets-autotype--active "yes" "no")
             (if ewwm-secrets-autotype-secure-input "enabled" "disabled")
             (if ewwm-secrets-autotype-gaze-away "enabled" "disabled"))))

;; ── Init / teardown ──────────────────────────────────────────

(defun ewwm-secrets-autotype-init ()
  "Initialize the auto-type dispatcher."
  (setq ewwm-secrets-autotype--active nil
        ewwm-secrets-autotype--backend-name nil)
  (message "ewwm-secrets-autotype: initialized (backend=%s)"
           (or (ewwm-secrets-autotype--select-backend) "none")))

(defun ewwm-secrets-autotype-teardown ()
  "Tear down the auto-type dispatcher."
  (when ewwm-secrets-autotype--active
    (ewwm-secrets-autotype-abort))
  (setq ewwm-secrets-autotype--active nil
        ewwm-secrets-autotype--backend-name nil))

(provide 'ewwm-secrets-autotype)
;;; ewwm-secrets-autotype.el ends here
