;;; ewwm-secrets-ydotool.el --- Ydotool auto-type backend for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Auto-type backend using ydotool for the EWWM secrets subsystem.
;; Types credentials into focused Wayland surfaces by driving ydotool
;; over its Unix domain socket.
;;
;; SECURITY: All secret strings are passed via stdin pipe to ydotool,
;; NEVER as command-line arguments (which would be visible in /proc).

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-secrets-ydotool nil
  "Ydotool auto-type backend for EWWM secrets."
  :group 'ewwm)

(defcustom ewwm-secrets-ydotool-enable t
  "Non-nil to enable the ydotool auto-type backend."
  :type 'boolean
  :group 'ewwm-secrets-ydotool)

(defcustom ewwm-secrets-ydotool-socket "/tmp/.ydotool_socket"
  "Path to the ydotoold Unix domain socket."
  :type 'string
  :group 'ewwm-secrets-ydotool)

(defcustom ewwm-secrets-ydotool-char-delay-ms 50
  "Delay in milliseconds between each typed character."
  :type 'integer
  :group 'ewwm-secrets-ydotool)

(defcustom ewwm-secrets-ydotool-executable "ydotool"
  "Path to the ydotool binary."
  :type 'string
  :group 'ewwm-secrets-ydotool)

(defcustom ewwm-secrets-ydotool-clear-after t
  "Non-nil to clear secret strings from memory after typing."
  :type 'boolean
  :group 'ewwm-secrets-ydotool)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-secrets-ydotool--available nil
  "Cached result of availability check.
Non-nil if ydotool binary and ydotoold socket are both present.")

(defvar ewwm-secrets-ydotool--typing-p nil
  "Non-nil while an auto-type operation is in progress.")

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-secrets-ydotool-type-start-hook nil
  "Hook run before a typing operation begins.")

(defvar ewwm-secrets-ydotool-type-complete-hook nil
  "Hook run after a typing operation completes.")

;; ── Availability checks ─────────────────────────────────────

(defun ewwm-secrets-ydotool-available-p ()
  "Return non-nil if ydotool is available for use.
Checks that the ydotoold socket exists and the ydotool binary is in PATH."
  (and ewwm-secrets-ydotool-enable
       (file-exists-p ewwm-secrets-ydotool-socket)
       (executable-find ewwm-secrets-ydotool-executable)
       t))

(defun ewwm-secrets-ydotool--verify-socket ()
  "Verify that ydotoold is running by checking its socket.
Returns non-nil if the socket exists and is accessible."
  (let ((socket ewwm-secrets-ydotool-socket))
    (and (file-exists-p socket)
         ;; Check it is a socket, not a stale regular file
         (let ((attrs (file-attributes socket)))
           (when attrs
             (eq (aref (file-attribute-modes attrs) 0) ?s))))))

;; ── Core typing functions ───────────────────────────────────

(defun ewwm-secrets-ydotool--type-string (string)
  "Type STRING via ydotool, passing it through stdin.
SECURITY: The string is written to the process stdin pipe and is
never placed on the command line.  Uses --clearmodifiers to avoid
interference from held modifier keys."
  (unless (ewwm-secrets-ydotool-available-p)
    (error "ewwm-secrets-ydotool: ydotool is not available"))
  (setq ewwm-secrets-ydotool--typing-p t)
  (run-hooks 'ewwm-secrets-ydotool-type-start-hook)
  (let* ((delay-arg (format "--key-delay=%d"
                            ewwm-secrets-ydotool-char-delay-ms))
         (proc (make-process
                :name "ewwm-ydotool-type"
                :command (list ewwm-secrets-ydotool-executable
                               "type" "--clearmodifiers"
                               delay-arg
                               "--file=-")
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (_proc event)
                  (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
                    (setq ewwm-secrets-ydotool--typing-p nil)
                    (run-hooks 'ewwm-secrets-ydotool-type-complete-hook))))))
    ;; Write string to stdin and close input
    (process-send-string proc string)
    (process-send-eof proc)
    proc))

(defun ewwm-secrets-ydotool--send-key (key)
  "Send a single KEY press via ydotool.
KEY is a key name such as \"Tab\", \"Return\", or \"Escape\"."
  (unless (ewwm-secrets-ydotool-available-p)
    (error "ewwm-secrets-ydotool: ydotool is not available"))
  (let ((proc (make-process
               :name "ewwm-ydotool-key"
               :command (list ewwm-secrets-ydotool-executable
                              "key" "--clearmodifiers" key)
               :connection-type 'pipe
               :noquery t)))
    proc))

(defun ewwm-secrets-ydotool--type-credentials (username password)
  "Type USERNAME, Tab, PASSWORD, Enter into the focused surface.
Clears PASSWORD from memory afterward if `ewwm-secrets-ydotool-clear-after'
is non-nil.  SECURITY: credentials are piped via stdin, never as arguments."
  (unless (ewwm-secrets-ydotool-available-p)
    (error "ewwm-secrets-ydotool: ydotool is not available"))
  (setq ewwm-secrets-ydotool--typing-p t)
  (run-hooks 'ewwm-secrets-ydotool-type-start-hook)
  ;; Build the composite string: username + Tab + password + Enter
  ;; Tab = \t, Enter = \n — ydotool type interprets these
  (let* ((composite (concat username "\t" password "\n"))
         (delay-arg (format "--key-delay=%d"
                            ewwm-secrets-ydotool-char-delay-ms))
         (proc (make-process
                :name "ewwm-ydotool-creds"
                :command (list ewwm-secrets-ydotool-executable
                               "type" "--clearmodifiers"
                               delay-arg
                               "--file=-")
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (_proc event)
                  (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
                    ;; Clear secrets from memory
                    (when ewwm-secrets-ydotool-clear-after
                      (clear-string password)
                      (clear-string composite))
                    (setq ewwm-secrets-ydotool--typing-p nil)
                    (run-hooks 'ewwm-secrets-ydotool-type-complete-hook))))))
    ;; Pipe credentials via stdin
    (process-send-string proc composite)
    (process-send-eof proc)
    proc))

(defun ewwm-secrets-ydotool--type-with-tab-fields (fields)
  "Type a list of FIELDS separated by Tab keystrokes.
Each element of FIELDS is a string.  The final field is followed by Enter.
SECURITY: all strings are piped via stdin."
  (unless (ewwm-secrets-ydotool-available-p)
    (error "ewwm-secrets-ydotool: ydotool is not available"))
  (when (null fields)
    (error "ewwm-secrets-ydotool: no fields to type"))
  (setq ewwm-secrets-ydotool--typing-p t)
  (run-hooks 'ewwm-secrets-ydotool-type-start-hook)
  ;; Join fields with Tab, append Enter
  (let* ((composite (concat (mapconcat #'identity fields "\t") "\n"))
         (delay-arg (format "--key-delay=%d"
                            ewwm-secrets-ydotool-char-delay-ms))
         (proc (make-process
                :name "ewwm-ydotool-fields"
                :command (list ewwm-secrets-ydotool-executable
                               "type" "--clearmodifiers"
                               delay-arg
                               "--file=-")
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (_proc event)
                  (when (string-match-p "\\(finished\\|exited\\|killed\\)" event)
                    ;; Clear field strings
                    (when ewwm-secrets-ydotool-clear-after
                      (dolist (f fields)
                        (clear-string f))
                      (clear-string composite))
                    (setq ewwm-secrets-ydotool--typing-p nil)
                    (run-hooks 'ewwm-secrets-ydotool-type-complete-hook))))))
    (process-send-string proc composite)
    (process-send-eof proc)
    proc))

;; ── Security functions ──────────────────────────────────────

(defun ewwm-secrets-ydotool--secure-type (string callback)
  "Type STRING securely via ydotool with cleanup.
After the ydotool process completes, clears STRING from memory
if `ewwm-secrets-ydotool-clear-after' is non-nil, then calls
CALLBACK with the process exit status symbol (`finished', `error')."
  (unless (ewwm-secrets-ydotool-available-p)
    (funcall callback 'error)
    (error "ewwm-secrets-ydotool: ydotool is not available"))
  (setq ewwm-secrets-ydotool--typing-p t)
  (run-hooks 'ewwm-secrets-ydotool-type-start-hook)
  (let* ((delay-arg (format "--key-delay=%d"
                            ewwm-secrets-ydotool-char-delay-ms))
         (proc (make-process
                :name "ewwm-ydotool-secure"
                :command (list ewwm-secrets-ydotool-executable
                               "type" "--clearmodifiers"
                               delay-arg
                               "--file=-")
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (_proc event)
                  (let ((result (cond
                                 ((string-match-p "finished" event) 'finished)
                                 (t 'error))))
                    ;; Clear the secret string
                    (when ewwm-secrets-ydotool-clear-after
                      (clear-string string))
                    (setq ewwm-secrets-ydotool--typing-p nil)
                    (run-hooks 'ewwm-secrets-ydotool-type-complete-hook)
                    (funcall callback result))))))
    (process-send-string proc string)
    (process-send-eof proc)
    proc))

;; ── Interactive commands ────────────────────────────────────

(defun ewwm-secrets-ydotool-test ()
  "Type the word \"test\" to verify ydotool is working."
  (interactive)
  (if (ewwm-secrets-ydotool-available-p)
      (progn
        (message "ewwm-secrets-ydotool: typing \"test\" in 1 second...")
        (run-at-time 1 nil
                     (lambda ()
                       (ewwm-secrets-ydotool--type-string "test"))))
    (message "ewwm-secrets-ydotool: not available")))

(defun ewwm-secrets-ydotool-status ()
  "Display ydotool availability status."
  (interactive)
  (let ((binary (executable-find ewwm-secrets-ydotool-executable))
        (socket-ok (ewwm-secrets-ydotool--verify-socket)))
    (message (concat "ewwm-secrets-ydotool status:\n"
                     "  enabled:    %s\n"
                     "  binary:     %s\n"
                     "  socket:     %s (%s)\n"
                     "  available:  %s\n"
                     "  typing:     %s")
             (if ewwm-secrets-ydotool-enable "yes" "no")
             (or binary "NOT FOUND")
             ewwm-secrets-ydotool-socket
             (if socket-ok "ok" "NOT FOUND")
             (if (ewwm-secrets-ydotool-available-p) "yes" "no")
             (if ewwm-secrets-ydotool--typing-p "yes" "no"))))

;; ── Init / teardown ─────────────────────────────────────────

(defun ewwm-secrets-ydotool-init ()
  "Initialize the ydotool auto-type backend.
Checks availability and caches the result."
  (setq ewwm-secrets-ydotool--available (ewwm-secrets-ydotool-available-p))
  (if ewwm-secrets-ydotool--available
      (message "ewwm-secrets-ydotool: initialized (available)")
    (message "ewwm-secrets-ydotool: initialized (not available)")))

(defun ewwm-secrets-ydotool-teardown ()
  "Tear down the ydotool auto-type backend.
Clears cached state."
  (setq ewwm-secrets-ydotool--available nil
        ewwm-secrets-ydotool--typing-p nil))

(provide 'ewwm-secrets-ydotool)
;;; ewwm-secrets-ydotool.el ends here
