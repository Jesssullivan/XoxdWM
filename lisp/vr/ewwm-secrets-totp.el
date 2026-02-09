;;; ewwm-secrets-totp.el --- TOTP integration for EWWM secrets management  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; TOTP (Time-based One-Time Password) integration for the EWWM secrets
;; subsystem.  Retrieves TOTP codes from KeePassXC via the browser
;; protocol and types them into focused Wayland surfaces, supporting
;; full login flows with automatic 2FA handling.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")
(declare-function ewwm-keepassxc-get-totp "ewwm-keepassxc-browser")
(declare-function ewwm-keepassxc-get-logins "ewwm-keepassxc-browser")
(declare-function ewwm-secrets-compositor--type-string "ewwm-secrets-compositor")
(declare-function ewwm-secrets-ydotool--type-string "ewwm-secrets-ydotool")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-secrets-totp nil
  "TOTP integration for EWWM secrets management."
  :group 'ewwm)

(defcustom ewwm-secrets-totp-enable t
  "Non-nil to enable TOTP integration."
  :type 'boolean
  :group 'ewwm-secrets-totp)

(defcustom ewwm-secrets-totp-delay-ms 2000
  "Delay in milliseconds to wait for the 2FA page before typing TOTP.
After credentials are submitted, the remote site may need time to
load the TOTP input page."
  :type 'integer
  :group 'ewwm-secrets-totp)

(defcustom ewwm-secrets-totp-refresh-interval 30
  "TOTP window duration in seconds.
Standard TOTP uses a 30-second window."
  :type 'integer
  :group 'ewwm-secrets-totp)

(defcustom ewwm-secrets-totp-auto-copy nil
  "Non-nil to automatically copy the TOTP code to the kill ring."
  :type 'boolean
  :group 'ewwm-secrets-totp)

(defcustom ewwm-secrets-totp-mode-line t
  "Non-nil to show a countdown in the mode-line while a TOTP code is active."
  :type 'boolean
  :group 'ewwm-secrets-totp)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-secrets-totp-retrieved-hook nil
  "Hook run after a TOTP code is successfully retrieved.
Hook functions receive no arguments; the code is available in
`ewwm-secrets-totp--current-code'.")

(defvar ewwm-secrets-totp-typed-hook nil
  "Hook run after a TOTP code has been typed into a field.")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-secrets-totp--current-code nil
  "Current TOTP code string, or nil when inactive.")

(defvar ewwm-secrets-totp--expiry-time nil
  "Float-time when the current TOTP code expires, or nil.")

(defvar ewwm-secrets-totp--countdown-timer nil
  "Timer for mode-line countdown updates, or nil.")

(defvar ewwm-secrets-totp--entry-uuid nil
  "UUID of the KeePassXC entry currently being used for TOTP.")

(defvar ewwm-secrets-totp--active nil
  "Non-nil while a TOTP flow is in progress.")

;; ── Auto-type backend selection ──────────────────────────────

(defun ewwm-secrets-totp--type-string (string)
  "Type STRING into the focused surface via the preferred auto-type backend.
Tries the compositor backend first, then falls back to ydotool."
  (cond
   ((fboundp 'ewwm-secrets-compositor--type-string)
    (ewwm-secrets-compositor--type-string string))
   ((fboundp 'ewwm-secrets-ydotool--type-string)
    (ewwm-secrets-ydotool--type-string string))
   (t
    (error "ewwm-secrets-totp: no auto-type backend available"))))

;; ── Core functions ───────────────────────────────────────────

(defun ewwm-secrets-totp-get (entry-path-or-uuid)
  "Get a TOTP code from KeePassXC for ENTRY-PATH-OR-UUID.
Returns the 6-digit TOTP code as a string, or nil on failure.
Uses `ewwm-keepassxc-get-totp' via the browser protocol."
  (unless ewwm-secrets-totp-enable
    (error "ewwm-secrets-totp: TOTP integration is disabled"))
  (condition-case err
      (let ((code (ewwm-keepassxc-get-totp entry-path-or-uuid)))
        (when (and code (not (string-empty-p code)))
          (setq ewwm-secrets-totp--current-code code
                ewwm-secrets-totp--entry-uuid entry-path-or-uuid
                ewwm-secrets-totp--expiry-time
                (+ (float-time)
                   (ewwm-secrets-totp--time-remaining-from-epoch))
                ewwm-secrets-totp--active t)
          (when ewwm-secrets-totp-auto-copy
            (kill-new code))
          (when ewwm-secrets-totp-mode-line
            (ewwm-secrets-totp--start-countdown))
          (run-hooks 'ewwm-secrets-totp-retrieved-hook)
          code))
    (error
     (message "ewwm-secrets-totp: failed to get TOTP: %s"
              (error-message-string err))
     nil)))

(defun ewwm-secrets-totp-type (code &optional surface-id)
  "Type TOTP CODE into the focused field via the preferred auto-type backend.
Optional SURFACE-ID is currently unused but reserved for future
surface-targeted typing."
  (ignore surface-id)
  (unless ewwm-secrets-totp-enable
    (error "ewwm-secrets-totp: TOTP integration is disabled"))
  (ewwm-secrets-totp--type-string code)
  (run-hooks 'ewwm-secrets-totp-typed-hook))

(defun ewwm-secrets-totp--time-remaining-from-epoch ()
  "Return seconds remaining in the current TOTP window based on epoch time.
Uses `ewwm-secrets-totp-refresh-interval' as the window size."
  (let* ((now (float-time))
         (interval ewwm-secrets-totp-refresh-interval)
         (elapsed (mod (truncate now) interval)))
    (- interval elapsed)))

(defun ewwm-secrets-totp--time-remaining ()
  "Return seconds until the current TOTP code expires, or 0 if expired."
  (if ewwm-secrets-totp--expiry-time
      (max 0 (truncate (- ewwm-secrets-totp--expiry-time (float-time))))
    0))

;; ── Mode-line ────────────────────────────────────────────────

(defun ewwm-secrets-totp--start-countdown ()
  "Start the mode-line countdown timer, updating every second."
  (ewwm-secrets-totp--stop-countdown)
  (setq ewwm-secrets-totp--countdown-timer
        (run-at-time 0 1 #'ewwm-secrets-totp--countdown-tick)))

(defun ewwm-secrets-totp--countdown-tick ()
  "Timer callback: update mode-line or stop if expired."
  (let ((remaining (ewwm-secrets-totp--time-remaining)))
    (if (> remaining 0)
        (force-mode-line-update t)
      ;; Code expired
      (ewwm-secrets-totp--stop-countdown)
      (when ewwm-secrets-totp--current-code
        (clear-string ewwm-secrets-totp--current-code))
      (setq ewwm-secrets-totp--current-code nil
            ewwm-secrets-totp--expiry-time nil
            ewwm-secrets-totp--active nil)
      (force-mode-line-update t))))

(defun ewwm-secrets-totp--stop-countdown ()
  "Cancel the countdown timer and clear mode-line display."
  (when ewwm-secrets-totp--countdown-timer
    (cancel-timer ewwm-secrets-totp--countdown-timer)
    (setq ewwm-secrets-totp--countdown-timer nil))
  (force-mode-line-update t))

(defun ewwm-secrets-totp-mode-line-string ()
  "Return a mode-line string showing TOTP countdown, or nil when inactive.
Format: \" [TOTP:XXs]\" where XX is seconds remaining."
  (when (and ewwm-secrets-totp-mode-line
             ewwm-secrets-totp--active
             ewwm-secrets-totp--expiry-time)
    (let ((remaining (ewwm-secrets-totp--time-remaining)))
      (when (> remaining 0)
        (format " [TOTP:%ds]" remaining)))))

;; ── Full login flow ──────────────────────────────────────────

(defun ewwm-secrets-autotype-with-totp (&optional url)
  "Perform a full login flow: credentials + TOTP for URL.
1. Query KeePassXC for credentials matching URL.
2. Type username + Tab + password via auto-type backend.
3. Wait `ewwm-secrets-totp-delay-ms' for the 2FA page to load.
4. Get TOTP code from KeePassXC.
5. Type TOTP + Enter.
6. Show success message.

When called interactively, prompts for URL."
  (interactive (list (read-string "URL: ")))
  (unless ewwm-secrets-totp-enable
    (error "ewwm-secrets-totp: TOTP integration is disabled"))
  (unless url
    (error "ewwm-secrets-totp: URL is required"))
  ;; Step 1: Get credentials from KeePassXC
  (let* ((logins (ewwm-keepassxc-get-logins url))
         (login (cond
                 ((null logins)
                  (error "ewwm-secrets-totp: no logins found for %s" url))
                 ((= (length logins) 1)
                  (car logins))
                 (t
                  ;; Multiple matches — let user choose
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
         (password (plist-get login :password))
         (uuid (plist-get login :uuid)))
    (unless (and username password)
      (error "ewwm-secrets-totp: incomplete credentials for %s" url))
    ;; Step 2: Type username + Tab + password
    (ewwm-secrets-totp--type-string (concat username "\t" password "\n"))
    (message "ewwm-secrets-totp: credentials typed, waiting for 2FA page...")
    ;; Step 3: Wait for 2FA page, then type TOTP
    (run-at-time
     (/ ewwm-secrets-totp-delay-ms 1000.0) nil
     (lambda ()
       (condition-case err
           (let ((code (ewwm-secrets-totp-get (or uuid url))))
             (if code
                 (progn
                   ;; Step 5: Type TOTP + Enter
                   (ewwm-secrets-totp--type-string (concat code "\n"))
                   (run-hooks 'ewwm-secrets-totp-typed-hook)
                   ;; Step 6: Success
                   (message "ewwm-secrets-totp: login complete for %s" url))
               (message "ewwm-secrets-totp: failed to retrieve TOTP for %s" url)))
         (error
          (message "ewwm-secrets-totp: 2FA failed: %s"
                   (error-message-string err))))
       ;; Clear password from memory
       (when password
         (clear-string password))))))

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-secrets-totp (entry)
  "Retrieve and display a TOTP code for ENTRY.
When called interactively, prompts for the entry path or UUID.
Offers to auto-type the code or copy it to the kill ring."
  (interactive (list (read-string "Entry (path or UUID): ")))
  (unless ewwm-secrets-totp-enable
    (error "ewwm-secrets-totp: TOTP integration is disabled"))
  (let ((code (ewwm-secrets-totp-get entry)))
    (if code
        (let* ((remaining (ewwm-secrets-totp--time-remaining))
               (action (read-char-choice
                        (format "TOTP: %s (%ds remaining) — [t]ype [c]opy [q]uit: "
                                code remaining)
                       '(?t ?c ?q))))
          (pcase action
            (?t (ewwm-secrets-totp-type code)
                (message "ewwm-secrets-totp: code typed"))
            (?c (kill-new code)
                (message "ewwm-secrets-totp: code copied to kill ring"))
            (?q (message "ewwm-secrets-totp: cancelled"))))
      (message "ewwm-secrets-totp: failed to retrieve TOTP for %s" entry))))

(defun ewwm-secrets-totp-copy (entry)
  "Retrieve a TOTP code for ENTRY and copy it to the kill ring.
When called interactively, prompts for the entry path or UUID."
  (interactive (list (read-string "Entry (path or UUID): ")))
  (unless ewwm-secrets-totp-enable
    (error "ewwm-secrets-totp: TOTP integration is disabled"))
  (let ((code (ewwm-secrets-totp-get entry)))
    (if code
        (progn
          (kill-new code)
          (message "ewwm-secrets-totp: code copied (%ds remaining)"
                   (ewwm-secrets-totp--time-remaining)))
      (message "ewwm-secrets-totp: failed to retrieve TOTP for %s" entry))))

;; ── Init / teardown ──────────────────────────────────────────

(defun ewwm-secrets-totp-init ()
  "Initialize TOTP integration.
Currently a no-op; reserved for future setup."
  nil)

(defun ewwm-secrets-totp-teardown ()
  "Tear down TOTP integration.
Stops the countdown timer, clears the current code from memory,
and resets all internal state."
  (ewwm-secrets-totp--stop-countdown)
  (when ewwm-secrets-totp--current-code
    (clear-string ewwm-secrets-totp--current-code))
  (setq ewwm-secrets-totp--current-code nil
        ewwm-secrets-totp--expiry-time nil
        ewwm-secrets-totp--countdown-timer nil
        ewwm-secrets-totp--entry-uuid nil
        ewwm-secrets-totp--active nil))

(provide 'ewwm-secrets-totp)
;;; ewwm-secrets-totp.el ends here
