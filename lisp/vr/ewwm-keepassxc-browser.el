;;; ewwm-keepassxc-browser.el --- KeePassXC Browser Protocol for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; KeePassXC Browser Protocol implementation for EWWM.
;; Communicates with KeePassXC via its browser extension protocol over
;; a Unix domain socket (keepassxc-proxy).  Provides credential lookup,
;; storage, TOTP retrieval, and database locking.
;;
;; The protocol uses NaCl X25519-XSalsa20-Poly1305 for encryption.
;; Crypto primitives are stubbed for external backing (libsodium FFI or
;; helper process); the message framing and protocol state machine are
;; fully implemented here.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ewwm-core)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-keepassxc nil
  "KeePassXC Browser Protocol integration."
  :group 'ewwm)

(defcustom ewwm-keepassxc-enable t
  "Master switch for KeePassXC integration."
  :type 'boolean
  :group 'ewwm-keepassxc)

(defcustom ewwm-keepassxc-proxy-socket nil
  "Path to the keepassxc-proxy Unix socket.
Nil means auto-detect at $XDG_RUNTIME_DIR/kpxc_server."
  :type '(choice (const :tag "Auto-detect" nil) string)
  :group 'ewwm-keepassxc)

(defcustom ewwm-keepassxc-client-id "ewwm-vr"
  "Client ID string sent during association."
  :type 'string
  :group 'ewwm-keepassxc)

(defcustom ewwm-keepassxc-association-file
  "~/.config/exwm-vr/keepassxc-association.json"
  "Path to persistent association key storage (JSON)."
  :type 'file
  :group 'ewwm-keepassxc)

(defcustom ewwm-keepassxc-timeout 5
  "Response timeout in seconds for protocol requests."
  :type 'integer
  :group 'ewwm-keepassxc)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-keepassxc-connect-hook nil
  "Hook run after successful connection to KeePassXC.")

(defvar ewwm-keepassxc-error-hook nil
  "Hook run on protocol errors.
Functions receive (ERROR-TYPE MESSAGE).")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-keepassxc--process nil
  "Network process for the keepassxc-proxy socket.")

(defvar ewwm-keepassxc--read-buffer ""
  "Accumulator for partial reads from the proxy socket.")

(defvar ewwm-keepassxc--client-public-key nil
  "Our X25519 public key (base64 string).")

(defvar ewwm-keepassxc--client-secret-key nil
  "Our X25519 secret key (base64 string).
Cleared on teardown with `clear-string'.")

(defvar ewwm-keepassxc--server-public-key nil
  "KeePassXC server public key (base64 string).")

(defvar ewwm-keepassxc--shared-key nil
  "Derived shared encryption key (base64 string).
Cleared on teardown with `clear-string'.")

(defvar ewwm-keepassxc--association-key nil
  "Stored association key from a successful associate handshake (base64).")

(defvar ewwm-keepassxc--association-name nil
  "Stored association name (identifier) from KeePassXC.")

(defvar ewwm-keepassxc--database-hash nil
  "Hash of the currently open KeePassXC database.")

(defvar ewwm-keepassxc--connected-p nil
  "Non-nil when connected and handshake is complete.")

(defvar ewwm-keepassxc--response nil
  "Temporary storage for synchronous response waiting.")

;; ── Crypto layer (stubs for NaCl X25519-XSalsa20-Poly1305) ──

(defun ewwm-keepassxc--generate-nonce ()
  "Generate a 24-byte random nonce, returned as a base64 string."
  (let ((bytes (make-string 24 0)))
    (dotimes (i 24)
      (aset bytes i (random 256)))
    (base64-encode-string bytes t)))

(defun ewwm-keepassxc--increment-nonce (nonce)
  "Increment NONCE (base64 string) by one, return new base64 string.
Used for response nonce validation per the KeePassXC protocol."
  (let* ((raw (base64-decode-string nonce))
         (bytes (string-to-vector raw))
         (carry 1)
         (i 0)
         (len (length bytes)))
    (while (and (< i len) (> carry 0))
      (let ((val (+ (aref bytes i) carry)))
        (aset bytes i (logand val #xff))
        (setq carry (ash val -8))
        (setq i (1+ i))))
    (base64-encode-string (concat bytes) t)))

(defun ewwm-keepassxc--generate-keypair ()
  "Generate an X25519 keypair for the browser protocol handshake.
Sets `ewwm-keepassxc--client-public-key' and
`ewwm-keepassxc--client-secret-key'.

NOTE: This is a stub.  A real implementation must call libsodium
crypto_box_keypair() via FFI module or external helper process.
The stub generates random bytes so the protocol framing can be tested."
  (let ((secret (make-string 32 0))
        (public (make-string 32 0)))
    ;; Fill with random bytes (placeholder for real X25519 keygen)
    (dotimes (i 32)
      (aset secret i (random 256))
      (aset public i (random 256)))
    (setq ewwm-keepassxc--client-secret-key (base64-encode-string secret t)
          ewwm-keepassxc--client-public-key (base64-encode-string public t))))

(defun ewwm-keepassxc--encrypt-message (msg nonce)
  "Encrypt JSON string MSG with NONCE using the shared key.
Returns the ciphertext as a base64 string.

NOTE: Stub -- returns MSG base64-encoded without real encryption.
A real implementation must call crypto_box_easy() with
`ewwm-keepassxc--shared-key' and NONCE."
  (ignore nonce) ; used in real implementation
  (base64-encode-string (encode-coding-string msg 'utf-8) t))

(defun ewwm-keepassxc--decrypt-message (encrypted nonce)
  "Decrypt base64 ENCRYPTED ciphertext with NONCE using the shared key.
Returns the plaintext JSON string.

NOTE: Stub -- returns base64-decoded ENCRYPTED without real decryption.
A real implementation must call crypto_box_open_easy() with
`ewwm-keepassxc--shared-key' and NONCE."
  (ignore nonce) ; used in real implementation
  (decode-coding-string (base64-decode-string encrypted) 'utf-8))

;; ── Socket path resolution ──────────────────────────────────

(defun ewwm-keepassxc--socket-path ()
  "Resolve the keepassxc-proxy Unix socket path.
Checks `ewwm-keepassxc-proxy-socket', then $XDG_RUNTIME_DIR/kpxc_server."
  (or ewwm-keepassxc-proxy-socket
      (let ((xdg (getenv "XDG_RUNTIME_DIR")))
        (when xdg
          (let ((path (expand-file-name "kpxc_server" xdg)))
            (when (file-exists-p path)
              path))))
      ;; Fallback: common macOS location
      (let ((tmpdir (or (getenv "TMPDIR") "/tmp")))
        (let ((path (expand-file-name "kpxc_server" tmpdir)))
          (when (file-exists-p path)
            path)))))

;; ── Connection management ───────────────────────────────────

(defun ewwm-keepassxc--connect ()
  "Open a Unix domain socket connection to keepassxc-proxy.
Returns the process on success, nil on failure."
  (ewwm-keepassxc--disconnect)
  (let ((path (ewwm-keepassxc--socket-path)))
    (unless path
      (run-hook-with-args 'ewwm-keepassxc-error-hook
                          'no-socket "keepassxc-proxy socket not found")
      (error "ewwm-keepassxc: proxy socket not found"))
    (condition-case err
        (progn
          (setq ewwm-keepassxc--process
                (make-network-process
                 :name "ewwm-keepassxc"
                 :family 'local
                 :service path
                 :coding 'utf-8
                 :filter #'ewwm-keepassxc--filter
                 :sentinel #'ewwm-keepassxc--sentinel
                 :noquery t))
          (setq ewwm-keepassxc--read-buffer "")
          (message "ewwm-keepassxc: connected to %s" path)
          ewwm-keepassxc--process)
      (error
       (run-hook-with-args 'ewwm-keepassxc-error-hook
                           'connect-failed (error-message-string err))
       (message "ewwm-keepassxc: connection failed: %s"
                (error-message-string err))
       nil))))

(defun ewwm-keepassxc--disconnect ()
  "Close the keepassxc-proxy connection and clear transport state."
  (when (and ewwm-keepassxc--process
             (process-live-p ewwm-keepassxc--process))
    (delete-process ewwm-keepassxc--process))
  (setq ewwm-keepassxc--process nil
        ewwm-keepassxc--read-buffer ""
        ewwm-keepassxc--connected-p nil))

(defun ewwm-keepassxc--connected-p ()
  "Return non-nil if the proxy socket is alive."
  (and ewwm-keepassxc--process
       (process-live-p ewwm-keepassxc--process)))

;; ── Process filter and sentinel ─────────────────────────────

(defun ewwm-keepassxc--filter (_proc data)
  "Process filter: accumulate DATA and extract complete JSON messages.
The KeePassXC proxy uses newline-delimited JSON."
  (setq ewwm-keepassxc--read-buffer
        (concat ewwm-keepassxc--read-buffer data))
  ;; KeePassXC proxy sends newline-delimited JSON
  (while (string-match "\n" ewwm-keepassxc--read-buffer)
    (let ((line (substring ewwm-keepassxc--read-buffer 0 (match-beginning 0))))
      (setq ewwm-keepassxc--read-buffer
            (substring ewwm-keepassxc--read-buffer (match-end 0)))
      (when (> (length line) 0)
        (condition-case err
            (let ((msg (json-read-from-string line)))
              (ewwm-keepassxc--handle-response msg))
          (error
           (message "ewwm-keepassxc: JSON parse error: %s"
                    (error-message-string err))))))))

(defun ewwm-keepassxc--sentinel (_proc event)
  "Process sentinel: handle connection state changes."
  (let ((event-str (string-trim event)))
    (cond
     ((string-match-p "connection broken\\|deleted" event-str)
      (setq ewwm-keepassxc--connected-p nil)
      (message "ewwm-keepassxc: connection lost"))
     (t
      (message "ewwm-keepassxc: %s" event-str)))))

;; ── Message send/receive ────────────────────────────────────

(defun ewwm-keepassxc--send-message (action &optional data)
  "Send a protocol message with ACTION and optional DATA fields.
ACTION is a string (e.g. \"change-public-keys\", \"get-logins\").
DATA is an alist of additional fields to merge into the request.
Returns the nonce used for this message."
  (unless (ewwm-keepassxc--connected-p)
    (error "ewwm-keepassxc: not connected"))
  (let* ((nonce (ewwm-keepassxc--generate-nonce))
         (msg-alist `((action . ,action)
                      (nonce . ,nonce)))
         json-str)
    ;; For change-public-keys, send unencrypted with our public key
    (if (string= action "change-public-keys")
        (progn
          (setq msg-alist (append msg-alist
                                  `((publicKey . ,ewwm-keepassxc--client-public-key)
                                    (clientID . ,ewwm-keepassxc-client-id))))
          (when data
            (setq msg-alist (append msg-alist data)))
          (setq json-str (json-encode msg-alist)))
      ;; All other messages: build inner message, encrypt, wrap
      (let* ((inner-alist (append `((action . ,action)
                                    (nonce . ,nonce))
                                  (or data nil)))
             (inner-json (json-encode inner-alist))
             (encrypted (ewwm-keepassxc--encrypt-message inner-json nonce)))
        (setq json-str (json-encode
                        `((action . ,action)
                          (message . ,encrypted)
                          (nonce . ,nonce)
                          (clientID . ,ewwm-keepassxc-client-id))))))
    (process-send-string ewwm-keepassxc--process
                         (concat json-str "\n"))
    nonce))

(defun ewwm-keepassxc--receive-response (&optional timeout)
  "Wait for a response from KeePassXC, with TIMEOUT seconds.
TIMEOUT defaults to `ewwm-keepassxc-timeout'.
Returns the decoded JSON response as an alist, or nil on timeout."
  (let ((deadline (+ (float-time) (or timeout ewwm-keepassxc-timeout))))
    (setq ewwm-keepassxc--response nil)
    (while (and (null ewwm-keepassxc--response)
                (< (float-time) deadline)
                (ewwm-keepassxc--connected-p))
      (accept-process-output ewwm-keepassxc--process 0.05))
    (prog1 ewwm-keepassxc--response
      (setq ewwm-keepassxc--response nil))))

(defun ewwm-keepassxc--handle-response (msg)
  "Handle a decoded JSON response MSG from KeePassXC.
For encrypted responses, decrypts the message field.
Stores result in `ewwm-keepassxc--response' for synchronous callers."
  (let ((action (alist-get 'action msg))
        (nonce (alist-get 'nonce msg))
        (encrypted (alist-get 'message msg))
        (error-code (alist-get 'errorCode msg))
        (error-msg (alist-get 'error msg)))
    ;; Check for protocol errors
    (when error-code
      (run-hook-with-args 'ewwm-keepassxc-error-hook
                          'protocol-error
                          (format "action=%s code=%s: %s"
                                  action error-code error-msg)))
    ;; Decrypt message field if present and we have a shared key
    (when (and encrypted ewwm-keepassxc--shared-key nonce)
      (condition-case err
          (let* ((plaintext (ewwm-keepassxc--decrypt-message encrypted nonce))
                 (inner (json-read-from-string plaintext)))
            ;; Merge decrypted fields into msg
            (setq msg (append inner msg)))
        (error
         (message "ewwm-keepassxc: decryption failed: %s"
                  (error-message-string err)))))
    (setq ewwm-keepassxc--response msg)))

;; ── Protocol handshake ──────────────────────────────────────

(defun ewwm-keepassxc--handshake ()
  "Perform the key exchange (change-public-keys action).
Sets `ewwm-keepassxc--server-public-key' on success.
Returns non-nil on success."
  (ewwm-keepassxc--generate-keypair)
  (ewwm-keepassxc--send-message "change-public-keys")
  (let ((resp (ewwm-keepassxc--receive-response)))
    (if (and resp
             (string= (alist-get 'action resp) "change-public-keys")
             (alist-get 'publicKey resp)
             (string= (alist-get 'success resp) "true"))
        (progn
          (setq ewwm-keepassxc--server-public-key (alist-get 'publicKey resp))
          ;; In a real implementation, derive the shared key here:
          ;; shared = crypto_box_beforenm(server_public, client_secret)
          (setq ewwm-keepassxc--shared-key
                (concat ewwm-keepassxc--client-secret-key)) ; placeholder
          (setq ewwm-keepassxc--connected-p t)
          (message "ewwm-keepassxc: key exchange complete")
          t)
      (run-hook-with-args 'ewwm-keepassxc-error-hook
                          'handshake-failed
                          (format "response: %S" resp))
      (message "ewwm-keepassxc: key exchange failed")
      nil)))

;; ── Association ─────────────────────────────────────────────

(defun ewwm-keepassxc--associate ()
  "Send associate action to KeePassXC.
Stores the association key and name on success.  Returns non-nil on success."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: handshake required before association"))
  (let ((id-key ewwm-keepassxc--client-public-key))
    (ewwm-keepassxc--send-message
     "associate"
     `((key . ,ewwm-keepassxc--client-public-key)
       (idKey . ,id-key)))
    (let ((resp (ewwm-keepassxc--receive-response)))
      (if (and resp
               (string= (alist-get 'success resp) "true")
               (alist-get 'id resp))
          (progn
            (setq ewwm-keepassxc--association-name (alist-get 'id resp)
                  ewwm-keepassxc--association-key id-key)
            (ewwm-keepassxc--save-association)
            (message "ewwm-keepassxc: associated as \"%s\""
                     ewwm-keepassxc--association-name)
            t)
        (run-hook-with-args 'ewwm-keepassxc-error-hook
                            'association-failed
                            (or (alist-get 'error resp) "unknown"))
        (message "ewwm-keepassxc: association failed: %s"
                 (or (alist-get 'error resp) "unknown"))
        nil))))

(defun ewwm-keepassxc--test-associate ()
  "Verify the existing association is still valid.
Returns non-nil if the association is recognized by KeePassXC."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (cond
   ((not (and ewwm-keepassxc--association-name
              ewwm-keepassxc--association-key))
    (message "ewwm-keepassxc: no stored association")
    nil)
   (t
    (ewwm-keepassxc--send-message
     "test-associate"
     `((id . ,ewwm-keepassxc--association-name)
       (key . ,ewwm-keepassxc--association-key)))
    (let ((resp (ewwm-keepassxc--receive-response)))
      (if (and resp (string= (alist-get 'success resp) "true"))
          (progn
            (setq ewwm-keepassxc--database-hash
                  (alist-get 'hash resp))
            (message "ewwm-keepassxc: association valid (db=%s)"
                     (or ewwm-keepassxc--database-hash "?"))
            t)
        (message "ewwm-keepassxc: association invalid or expired")
        nil)))))

;; ── Association file management ─────────────────────────────

(defun ewwm-keepassxc--save-association ()
  "Write association credentials to `ewwm-keepassxc-association-file'.
The file is written with restrictive permissions (#o600)."
  (let ((file (expand-file-name ewwm-keepassxc-association-file)))
    ;; Ensure directory exists
    (let ((dir (file-name-directory file)))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (let ((data (json-encode
                 `((clientID . ,ewwm-keepassxc-client-id)
                   (associationName . ,ewwm-keepassxc--association-name)
                   (associationKey . ,ewwm-keepassxc--association-key)
                   (databaseHash . ,ewwm-keepassxc--database-hash)))))
      (write-region data nil file nil 'silent)
      (set-file-modes file #o600)
      (message "ewwm-keepassxc: association saved to %s" file))))

(defun ewwm-keepassxc--load-association ()
  "Read association credentials from `ewwm-keepassxc-association-file'.
Returns non-nil if an association was loaded."
  (let ((file (expand-file-name ewwm-keepassxc-association-file)))
    (if (file-readable-p file)
        (condition-case err
            (let* ((content (with-temp-buffer
                              (insert-file-contents file)
                              (buffer-string)))
                   (data (json-read-from-string content)))
              (setq ewwm-keepassxc--association-name
                    (alist-get 'associationName data)
                    ewwm-keepassxc--association-key
                    (alist-get 'associationKey data)
                    ewwm-keepassxc--database-hash
                    (alist-get 'databaseHash data))
              (message "ewwm-keepassxc: loaded association \"%s\""
                       ewwm-keepassxc--association-name)
              t)
          (error
           (message "ewwm-keepassxc: failed to load association: %s"
                    (error-message-string err))
           nil))
      (message "ewwm-keepassxc: no association file at %s" file)
      nil)))

;; ── High-level API ──────────────────────────────────────────

(defun ewwm-keepassxc-get-logins (url)
  "Query KeePassXC for credentials matching URL.
Returns a list of plists: ((:login \"user\" :password \"pass\" :name \"Entry\") ...)."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (ewwm-keepassxc--send-message
   "get-logins"
   `((url . ,url)
     (keys . [((id . ,ewwm-keepassxc--association-name)
               (key . ,ewwm-keepassxc--association-key))])))
  (let ((resp (ewwm-keepassxc--receive-response)))
    (if (and resp (string= (alist-get 'success resp) "true"))
        (let ((entries (alist-get 'entries resp)))
          (mapcar (lambda (entry)
                    (list :login (alist-get 'login entry)
                          :password (alist-get 'password entry)
                          :name (alist-get 'name entry)
                          :uuid (alist-get 'uuid entry)))
                  (append entries nil))) ; coerce vector to list
      (message "ewwm-keepassxc: get-logins failed: %s"
               (or (alist-get 'error resp) "unknown"))
      nil)))

(defun ewwm-keepassxc-set-login (url login password &optional group)
  "Save a new credential to KeePassXC.
URL is the site URL, LOGIN the username, PASSWORD the password.
Optional GROUP specifies the KeePassXC group to store in."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (let ((data `((url . ,url)
                (login . ,login)
                (password . ,password)
                (id . ,ewwm-keepassxc--association-name)
                (key . ,ewwm-keepassxc--association-key))))
    (when group
      (setq data (append data `((group . ,group)))))
    (ewwm-keepassxc--send-message "set-login" data)
    (let ((resp (ewwm-keepassxc--receive-response)))
      (if (and resp (string= (alist-get 'success resp) "true"))
          (progn
            (message "ewwm-keepassxc: credential saved for %s" url)
            t)
        (message "ewwm-keepassxc: set-login failed: %s"
                 (or (alist-get 'error resp) "unknown"))
        nil))))

(defun ewwm-keepassxc-lock-database ()
  "Lock the KeePassXC database."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (ewwm-keepassxc--send-message "lock-database")
  (let ((resp (ewwm-keepassxc--receive-response)))
    (if (and resp (string= (alist-get 'action resp) "lock-database"))
        (progn
          (message "ewwm-keepassxc: database locked")
          t)
      (message "ewwm-keepassxc: lock-database failed")
      nil)))

(defun ewwm-keepassxc-get-database-hash ()
  "Get the identifier hash of the currently open KeePassXC database.
Returns the hash string, or nil on failure."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (ewwm-keepassxc--send-message
   "get-databasehash"
   `((id . ,ewwm-keepassxc--association-name)
     (key . ,ewwm-keepassxc--association-key)))
  (let ((resp (ewwm-keepassxc--receive-response)))
    (if (and resp (string= (alist-get 'success resp) "true"))
        (let ((hash (alist-get 'hash resp)))
          (setq ewwm-keepassxc--database-hash hash)
          hash)
      (message "ewwm-keepassxc: get-databasehash failed: %s"
               (or (alist-get 'error resp) "unknown"))
      nil)))

(defun ewwm-keepassxc-get-totp (uuid)
  "Get the current TOTP code for the entry with UUID.
Returns the TOTP string, or nil on failure."
  (unless ewwm-keepassxc--connected-p
    (error "ewwm-keepassxc: not connected"))
  (ewwm-keepassxc--send-message
   "get-totp"
   `((uuid . ,uuid)
     (id . ,ewwm-keepassxc--association-name)
     (key . ,ewwm-keepassxc--association-key)))
  (let ((resp (ewwm-keepassxc--receive-response)))
    (if (and resp (string= (alist-get 'success resp) "true"))
        (alist-get 'totp resp)
      (message "ewwm-keepassxc: get-totp failed: %s"
               (or (alist-get 'error resp) "unknown"))
      nil)))

;; ── Interactive commands ────────────────────────────────────

(defun ewwm-keepassxc-status ()
  "Display KeePassXC connection and association status."
  (interactive)
  (message (concat "ewwm-keepassxc: %s | assoc=%s | db=%s"
                   " | socket=%s")
           (if ewwm-keepassxc--connected-p "connected" "disconnected")
           (or ewwm-keepassxc--association-name "none")
           (or ewwm-keepassxc--database-hash "unknown")
           (or (ewwm-keepassxc--socket-path) "not found")))

(defun ewwm-keepassxc-associate ()
  "Interactive association wizard for KeePassXC.
Connects, performs key exchange, and associates with the database.
KeePassXC will prompt the user to confirm the association."
  (interactive)
  (unless ewwm-keepassxc-enable
    (user-error "ewwm-keepassxc: integration is disabled"))
  (message "ewwm-keepassxc: starting association wizard...")
  ;; Connect if needed
  (unless (ewwm-keepassxc--connected-p)
    (unless (ewwm-keepassxc--connect)
      (user-error "ewwm-keepassxc: could not connect to proxy")))
  ;; Key exchange
  (unless (ewwm-keepassxc--handshake)
    (user-error "ewwm-keepassxc: key exchange failed"))
  ;; Associate (KeePassXC will show a confirmation dialog)
  (message "ewwm-keepassxc: requesting association -- confirm in KeePassXC...")
  (if (ewwm-keepassxc--associate)
      (message "ewwm-keepassxc: association complete as \"%s\""
               ewwm-keepassxc--association-name)
    (user-error "ewwm-keepassxc: association was denied or failed")))

(defun ewwm-keepassxc-test-connection ()
  "Test that the KeePassXC protocol handshake works.
Connects, performs key exchange, and tests the stored association."
  (interactive)
  (unless ewwm-keepassxc-enable
    (user-error "ewwm-keepassxc: integration is disabled"))
  ;; Connect
  (unless (ewwm-keepassxc--connected-p)
    (unless (ewwm-keepassxc--connect)
      (user-error "ewwm-keepassxc: could not connect to proxy")))
  ;; Key exchange
  (unless ewwm-keepassxc--connected-p
    (unless (ewwm-keepassxc--handshake)
      (user-error "ewwm-keepassxc: key exchange failed")))
  ;; Test association
  (if (ewwm-keepassxc--test-associate)
      (message "ewwm-keepassxc: connection test passed (db=%s)"
               (or ewwm-keepassxc--database-hash "?"))
    (message "ewwm-keepassxc: connection OK but association invalid")))

;; ── Init / teardown ─────────────────────────────────────────

(defun ewwm-keepassxc-init ()
  "Initialize KeePassXC Browser Protocol integration.
Loads any stored association, connects to the proxy, and verifies."
  (when ewwm-keepassxc-enable
    ;; Load stored association
    (ewwm-keepassxc--load-association)
    ;; Connect to proxy
    (when (ewwm-keepassxc--connect)
      ;; Perform key exchange
      (when (ewwm-keepassxc--handshake)
        ;; Verify stored association if we have one
        (when ewwm-keepassxc--association-name
          (ewwm-keepassxc--test-associate))
        (run-hooks 'ewwm-keepassxc-connect-hook)))))

(defun ewwm-keepassxc-teardown ()
  "Shut down KeePassXC integration and clear sensitive state.
Uses `clear-string' to wipe key material from memory."
  (ewwm-keepassxc--disconnect)
  ;; Securely clear key material
  (when (and ewwm-keepassxc--client-secret-key
             (stringp ewwm-keepassxc--client-secret-key))
    (clear-string ewwm-keepassxc--client-secret-key))
  (when (and ewwm-keepassxc--shared-key
             (stringp ewwm-keepassxc--shared-key))
    (clear-string ewwm-keepassxc--shared-key))
  ;; Clear all protocol state
  (setq ewwm-keepassxc--client-public-key nil
        ewwm-keepassxc--client-secret-key nil
        ewwm-keepassxc--server-public-key nil
        ewwm-keepassxc--shared-key nil
        ewwm-keepassxc--association-key nil
        ewwm-keepassxc--association-name nil
        ewwm-keepassxc--database-hash nil
        ewwm-keepassxc--connected-p nil
        ewwm-keepassxc--response nil)
  (message "ewwm-keepassxc: teardown complete"))

(provide 'ewwm-keepassxc-browser)
;;; ewwm-keepassxc-browser.el ends here
