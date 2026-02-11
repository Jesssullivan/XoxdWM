;;; ewwm-qutebrowser-ipc.el --- Qutebrowser IPC layer for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; IPC layer for qutebrowser communication.  Supports two methods:
;; FIFO (write commands to qutebrowser's command FIFO) and socket
;; (connect to qutebrowser's Unix domain socket using the JSON
;; protocol).  Dispatches via `ewwm-qutebrowser-ipc-method'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-current-surface
                  "ewwm-qutebrowser")

;; Forward-declare variables from ewwm-qutebrowser.el
(defvar ewwm-qutebrowser-ipc-method)
(defvar ewwm-qutebrowser-fifo-dir)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-ipc nil
  "Qutebrowser IPC settings for EWWM."
  :group 'ewwm-qutebrowser)

(defcustom ewwm-qutebrowser-ipc-timeout 5
  "Timeout in seconds for IPC operations."
  :type 'integer
  :group 'ewwm-qutebrowser-ipc)

(defcustom ewwm-qutebrowser-ipc-reconnect-delay 1.0
  "Delay in seconds before reconnecting to qutebrowser."
  :type 'number
  :group 'ewwm-qutebrowser-ipc)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-qutebrowser-ipc--socket-connection nil
  "Network process for qutebrowser socket connection.")

(defvar ewwm-qutebrowser-ipc--reconnect-timer nil
  "Timer for socket reconnection attempts.")

(defvar ewwm-qutebrowser-ipc--fifo-cache nil
  "Cached path to the qutebrowser FIFO.")

(defconst ewwm-qutebrowser-ipc--protocol-version 1
  "Qutebrowser IPC JSON protocol version.")

;; ── FIFO IPC ─────────────────────────────────────────────────

(defun ewwm-qutebrowser-ipc--fifo-path ()
  "Compute path to the qutebrowser command FIFO.
Looks for the FIFO under `ewwm-qutebrowser-fifo-dir' in
a subdirectory named qutebrowser_HASH where HASH is derived
from the username.  Returns nil if no FIFO is found."
  (or ewwm-qutebrowser-ipc--fifo-cache
      (let* ((runtime (if (boundp 'ewwm-qutebrowser-fifo-dir)
                          ewwm-qutebrowser-fifo-dir
                        (or (getenv "XDG_RUNTIME_DIR") "/tmp")))
             (pattern (expand-file-name
                       "qutebrowser/ipc-*" runtime))
             (candidates (file-expand-wildcards pattern t)))
        (when candidates
          (setq ewwm-qutebrowser-ipc--fifo-cache
                (car candidates))))))

(defun ewwm-qutebrowser-ipc--send-fifo (command)
  "Write COMMAND string to the qutebrowser FIFO.
COMMAND should be a JSON-formatted string."
  (let ((fifo (ewwm-qutebrowser-ipc--fifo-path)))
    (unless fifo
      (user-error "No qutebrowser FIFO found in %s"
                  (if (boundp 'ewwm-qutebrowser-fifo-dir)
                      ewwm-qutebrowser-fifo-dir
                    (or (getenv "XDG_RUNTIME_DIR") "/tmp"))))
    (unless (file-writable-p fifo)
      (user-error "Cannot write to qutebrowser FIFO: %s" fifo))
    (let ((coding-system-for-write 'utf-8-unix))
      (write-region (concat command "\n") nil fifo t 'quiet))))

;; ── JSON protocol ────────────────────────────────────────────

(defun ewwm-qutebrowser-ipc--format-json (command &rest args)
  "Format COMMAND with ARGS as qutebrowser JSON protocol.
Returns a JSON string of the form:
  {\"args\": [\":CMD ARGS\"], \"target_arg\": null,
   \"protocol_version\": 1}"
  (let* ((full-cmd (if args
                       (mapconcat #'identity
                                  (cons command args) " ")
                     command))
         (obj `(("args" . [,full-cmd])
                ("target_arg" . :json-null)
                ("protocol_version"
                 . ,ewwm-qutebrowser-ipc--protocol-version))))
    (json-encode obj)))

;; ── Socket IPC ───────────────────────────────────────────────

(defun ewwm-qutebrowser-ipc--connect-socket (path)
  "Connect to qutebrowser Unix domain socket at PATH.
Returns the network process on success, nil on failure."
  (condition-case err
      (let ((proc (make-network-process
                   :name "ewwm-qb-ipc"
                   :family 'local
                   :service path
                   :noquery t
                   :filter
                   #'ewwm-qutebrowser-ipc--socket-filter
                   :sentinel
                   #'ewwm-qutebrowser-ipc--socket-sentinel)))
        (setq ewwm-qutebrowser-ipc--socket-connection proc)
        proc)
    (error
     (message "ewwm-qutebrowser: socket connect failed: %s"
              (error-message-string err))
     nil)))

(defun ewwm-qutebrowser-ipc--send-socket (connection command)
  "Send COMMAND via socket CONNECTION.
COMMAND should be a JSON-formatted string."
  (unless (and connection (process-live-p connection))
    (user-error "Qutebrowser socket not connected"))
  (process-send-string connection (concat command "\n")))

(defun ewwm-qutebrowser-ipc--disconnect (connection)
  "Close qutebrowser socket CONNECTION."
  (when (and connection (process-live-p connection))
    (delete-process connection))
  (when (eq connection ewwm-qutebrowser-ipc--socket-connection)
    (setq ewwm-qutebrowser-ipc--socket-connection nil)))

(defun ewwm-qutebrowser-ipc--socket-filter (_proc output)
  "Handle OUTPUT from the qutebrowser socket _PROC."
  (condition-case nil
      (let ((data (json-read-from-string output)))
        (when (and (listp data)
                   (alist-get 'text data))
          (message "qutebrowser: %s" (alist-get 'text data))))
    (error nil)))

(defun ewwm-qutebrowser-ipc--socket-sentinel (proc event)
  "Handle sentinel EVENT for socket PROC."
  (let ((status (string-trim event)))
    (when (string-match-p
           "\\(connection broken\\|finished\\|deleted\\)"
           status)
      (when (eq proc ewwm-qutebrowser-ipc--socket-connection)
        (setq ewwm-qutebrowser-ipc--socket-connection nil))
      (message "ewwm-qutebrowser: socket disconnected"))))

;; ── Unified dispatch ─────────────────────────────────────────

(defun ewwm-qutebrowser-ipc-send (command &rest args)
  "Send COMMAND with ARGS to qutebrowser.
Dispatches via `ewwm-qutebrowser-ipc-method' (fifo or socket)."
  (let ((json-cmd (apply #'ewwm-qutebrowser-ipc--format-json
                         command args)))
    (pcase (if (boundp 'ewwm-qutebrowser-ipc-method)
               ewwm-qutebrowser-ipc-method
             'fifo)
      ('fifo
       (ewwm-qutebrowser-ipc--send-fifo json-cmd))
      ('socket
       (ewwm-qutebrowser-ipc--send-socket
        ewwm-qutebrowser-ipc--socket-connection json-cmd))
      (_
       (user-error "Unknown IPC method: %s"
                   ewwm-qutebrowser-ipc-method)))))

(defun ewwm-qutebrowser-ipc-connected-p ()
  "Return non-nil if qutebrowser IPC is available."
  (pcase (if (boundp 'ewwm-qutebrowser-ipc-method)
             ewwm-qutebrowser-ipc-method
           'fifo)
    ('fifo
     (let ((fifo (ewwm-qutebrowser-ipc--fifo-path)))
       (and fifo (file-writable-p fifo))))
    ('socket
     (and ewwm-qutebrowser-ipc--socket-connection
          (process-live-p
           ewwm-qutebrowser-ipc--socket-connection)))
    (_ nil)))

(provide 'ewwm-qutebrowser-ipc)
;;; ewwm-qutebrowser-ipc.el ends here
