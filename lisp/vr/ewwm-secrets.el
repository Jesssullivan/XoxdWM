;;; ewwm-secrets.el --- D-Bus Secret Service backend for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; D-Bus Secret Service integration for the Emacs Wayland Window Manager.
;; Provides access to KeePassXC (or any Secret Service provider) via the
;; freedesktop.org Secret Service API, and registers an auth-source backend
;; so that `auth-source-search' transparently queries the password manager.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)
(require 'secrets)
(require 'auth-source)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-secrets nil
  "D-Bus Secret Service integration for EWWM."
  :group 'ewwm)

(defcustom ewwm-secrets-default-collection "KeePassXC"
  "Name of the Secret Service collection to use.
This should match the collection name exposed by your password manager
\(e.g. \"KeePassXC\" or \"Login\")."
  :type 'string
  :group 'ewwm-secrets)

(defcustom ewwm-secrets-enable t
  "Non-nil to enable Secret Service integration.
When nil, all ewwm-secrets functions become no-ops."
  :type 'boolean
  :group 'ewwm-secrets)

(defcustom ewwm-secrets-auto-unlock t
  "Non-nil to automatically prompt for unlock when the database is locked.
When the Secret Service collection is locked and a secret is requested,
prompt the user via `read-passwd' and attempt to unlock."
  :type 'boolean
  :group 'ewwm-secrets)

(defcustom ewwm-secrets-timeout 30
  "Timeout in seconds for D-Bus Secret Service calls."
  :type 'integer
  :group 'ewwm-secrets)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-secrets-unlock-hook nil
  "Hook run after a Secret Service collection is successfully unlocked.")

(defvar ewwm-secrets-error-hook nil
  "Hook run when a Secret Service error occurs.
Hook functions receive a single argument: the error message string.")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-secrets--connected nil
  "Non-nil when we have a live connection to the Secret Service.")

(defvar ewwm-secrets--last-collection nil
  "Cached name of the last-used Secret Service collection.")

(defvar ewwm-secrets--unlock-attempts 0
  "Number of unlock attempts for the current operation.
Used to prevent infinite unlock loops.")

(defconst ewwm-secrets--max-unlock-attempts 3
  "Maximum number of unlock attempts before giving up.")

;; ── Core functions ───────────────────────────────────────────

(defun ewwm-secrets--ensure-connected ()
  "Verify that the Secret Service is reachable and the default collection exists.
Signals an error with a helpful message if the collection cannot be found."
  (unless ewwm-secrets-enable
    (error "ewwm-secrets: Secret Service integration is disabled"))
  (condition-case err
      (let ((collections (secrets-list-collections)))
        (cond
         ((null collections)
          (setq ewwm-secrets--connected nil)
          (run-hook-with-args 'ewwm-secrets-error-hook
                              "No Secret Service collections found")
          (error "ewwm-secrets: no Secret Service collections found.  \
Is KeePassXC running with Secret Service integration enabled?"))
         ((not (member ewwm-secrets-default-collection collections))
          (setq ewwm-secrets--connected nil)
          (run-hook-with-args 'ewwm-secrets-error-hook
                              (format "Collection \"%s\" not found"
                                      ewwm-secrets-default-collection))
          (error "ewwm-secrets: collection \"%s\" not found.  \
Available collections: %s.  \
Check KeePassXC Settings > Secret Service Integration"
                 ewwm-secrets-default-collection
                 (mapconcat #'identity collections ", ")))
         (t
          (setq ewwm-secrets--connected t
                ewwm-secrets--last-collection ewwm-secrets-default-collection)
          t)))
    (error
     (setq ewwm-secrets--connected nil)
     (run-hook-with-args 'ewwm-secrets-error-hook (error-message-string err))
     (signal (car err) (cdr err)))))

(defun ewwm-secrets--try-unlock ()
  "Attempt to unlock the default collection.
Prompts for a password via `read-passwd' and calls `secrets-unlock-collection'.
Returns non-nil on success, nil on failure or if max attempts exceeded."
  (cond
   ((>= ewwm-secrets--unlock-attempts ewwm-secrets--max-unlock-attempts)
    (setq ewwm-secrets--unlock-attempts 0)
    (run-hook-with-args 'ewwm-secrets-error-hook
                        "Max unlock attempts exceeded")
    (message "ewwm-secrets: max unlock attempts exceeded")
    nil)
   ((not ewwm-secrets-auto-unlock)
    (run-hook-with-args 'ewwm-secrets-error-hook
                        "Collection locked and auto-unlock is disabled")
    nil)
   (t
    (cl-incf ewwm-secrets--unlock-attempts)
    (condition-case err
        (progn
          (secrets-unlock-collection ewwm-secrets-default-collection)
          ;; Verify the unlock succeeded by trying to list items
          (let ((items (secrets-list-items ewwm-secrets-default-collection)))
            (cond
             (items
              (setq ewwm-secrets--unlock-attempts 0)
              (run-hooks 'ewwm-secrets-unlock-hook)
              t)
             (t
              ;; Unlock may have been cancelled or failed
              nil))))
      (error
       (run-hook-with-args 'ewwm-secrets-error-hook
                           (error-message-string err))
       nil)))))

(defun ewwm-secrets-get (entry-path)
  "Retrieve the secret for ENTRY-PATH from the default collection.
If the database is locked and `ewwm-secrets-auto-unlock' is non-nil,
prompt the user and attempt to unlock.  Returns the secret string or nil."
  (ewwm-secrets--ensure-connected)
  (setq ewwm-secrets--unlock-attempts 0)
  (let ((secret nil)
        (done nil))
    (while (not done)
      (condition-case _err
          (let ((result (secrets-get-secret
                         ewwm-secrets-default-collection entry-path)))
            (cond
             ;; Got a valid secret
             ((and result (not (string-empty-p result)))
              (setq secret result
                    done t))
             ;; Nil or empty — database might be locked
             ((ewwm-secrets--try-unlock)
              ;; Unlocked, retry on next iteration
              nil)
             ;; Unlock failed or not attempted
             (t
              (setq done t))))
        (error
         (cond
          ((ewwm-secrets--try-unlock)
           nil)  ; retry
          (t
           (setq done t))))))
    secret))

(defun ewwm-secrets-get-attributes (entry-path)
  "Retrieve the attributes alist for ENTRY-PATH in the default collection."
  (ewwm-secrets--ensure-connected)
  (condition-case err
      (secrets-get-attributes ewwm-secrets-default-collection entry-path)
    (error
     (run-hook-with-args 'ewwm-secrets-error-hook (error-message-string err))
     nil)))

(defun ewwm-secrets-search (attributes)
  "Search for entries matching ATTRIBUTES in the default collection.
ATTRIBUTES is an alist of (KEY . VALUE) pairs for the Secret Service lookup.
Returns a list of matching item names."
  (ewwm-secrets--ensure-connected)
  (condition-case err
      (secrets-search-items ewwm-secrets-default-collection attributes)
    (error
     (run-hook-with-args 'ewwm-secrets-error-hook (error-message-string err))
     nil)))

(defun ewwm-secrets-list-items ()
  "List all item names in the default collection."
  (ewwm-secrets--ensure-connected)
  (condition-case err
      (secrets-list-items ewwm-secrets-default-collection)
    (error
     (run-hook-with-args 'ewwm-secrets-error-hook (error-message-string err))
     nil)))

;; ── auth-source backend ──────────────────────────────────────

;; In Emacs 30+ auth-source-backend is an EIEIO class (defclass),
;; so we construct instances directly rather than cl-defstruct :include.

(defun ewwm-secrets-auth-source-search (backend &rest spec)
  "Search the Secret Service for credentials matching SPEC.
BACKEND is the `auth-source-backend' instance.
Recognized keys: :host, :port, :user.  Maps :host to the URL attribute
and :user to the UserName attribute in the Secret Service.
Returns a list of auth-source entry plists."
  (ignore backend)
  (when ewwm-secrets-enable
    (condition-case err
        (let* ((host (plist-get spec :host))
               (user (plist-get spec :user))
               (port (plist-get spec :port))
               (attrs nil)
               (results nil))
          ;; Build Secret Service attribute query
          (when host
            (push (cons "URL" host) attrs))
          (when user
            (push (cons "UserName" user) attrs))
          ;; Search items
          (let ((items (if attrs
                           (ewwm-secrets-search attrs)
                         (ewwm-secrets-list-items))))
            (dolist (item items)
              (let* ((item-attrs (ewwm-secrets-get-attributes item))
                     (item-host (or (cdr (assoc "URL" item-attrs)) host))
                     (item-user (or (cdr (assoc "UserName" item-attrs)) user))
                     (item-port (or port
                                    (cdr (assoc "Port" item-attrs))))
                     ;; Standard auth-source pattern: :secret is a closure
                     (entry (list :host item-host
                                  :port item-port
                                  :user item-user
                                  :secret (let ((item-name item))
                                            (lambda ()
                                              (ewwm-secrets-get item-name))))))
                (push entry results))))
          (nreverse results))
      (error
       (run-hook-with-args 'ewwm-secrets-error-hook (error-message-string err))
       nil))))

(defun ewwm-secrets-auth-source-backend-parse (entry)
  "Create an ewwm-secrets auth-source backend from ENTRY.
ENTRY should be the symbol `ewwm-secrets'."
  (when (eq entry 'ewwm-secrets)
    (auth-source-backend
     :type 'ewwm-secrets
     :source "D-Bus Secret Service"
     :search-function #'ewwm-secrets-auth-source-search)))

;; Register with auth-source
(advice-add 'auth-source-backend-parse :before-until
            #'ewwm-secrets-auth-source-backend-parse)

;; ── Interactive commands ─────────────────────────────────────

(defun ewwm-secrets-status ()
  "Display the Secret Service collection status in the minibuffer."
  (interactive)
  (cond
   ((not ewwm-secrets-enable)
    (message "ewwm-secrets: disabled"))
   (t
    (condition-case err
        (let* ((collections (secrets-list-collections))
               (found (member ewwm-secrets-default-collection collections))
               (item-count (when found
                             (length (secrets-list-items
                                      ewwm-secrets-default-collection)))))
          (cond
           ((not collections)
            (message "ewwm-secrets: no Secret Service provider found"))
           ((not found)
            (message "ewwm-secrets: collection \"%s\" not found (available: %s)"
                     ewwm-secrets-default-collection
                     (mapconcat #'identity collections ", ")))
           (t
            (message "ewwm-secrets: collection \"%s\" — %d items, connected: %s"
                     ewwm-secrets-default-collection
                     (or item-count 0)
                     (if ewwm-secrets--connected "yes" "no")))))
      (error
       (message "ewwm-secrets: error — %s" (error-message-string err)))))))

(defun ewwm-secrets-list ()
  "Show all items in the default Secret Service collection in a temp buffer."
  (interactive)
  (ewwm-secrets--ensure-connected)
  (let ((items (ewwm-secrets-list-items)))
    (with-current-buffer (get-buffer-create "*ewwm-secrets*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Secret Service collection: %s\n"
                        ewwm-secrets-default-collection))
        (insert (format "Items: %d\n" (length items)))
        (insert (make-string 50 ?─) "\n\n")
        (if (null items)
            (insert "(no items)\n")
          (dolist (item items)
            (insert (format "  %s\n" item))
            (let ((attrs (ewwm-secrets-get-attributes item)))
              (dolist (attr attrs)
                (insert (format "    %s: %s\n" (car attr) (cdr attr))))
              (insert "\n")))))
      (goto-char (point-min))
      (special-mode))
    (display-buffer "*ewwm-secrets*")))

;; ── Init / teardown ──────────────────────────────────────────

(defun ewwm-secrets-init ()
  "Initialize Secret Service integration.
Verifies that the Secret Service is reachable and registers the
auth-source backend."
  (when ewwm-secrets-enable
    (ewwm-secrets--ensure-connected)
    (message "ewwm-secrets: initialized (collection: %s)"
             ewwm-secrets-default-collection)))

(defun ewwm-secrets-teardown ()
  "Tear down Secret Service integration and clear cached state."
  (setq ewwm-secrets--connected nil
        ewwm-secrets--last-collection nil
        ewwm-secrets--unlock-attempts 0))

(provide 'ewwm-secrets)
;;; ewwm-secrets.el ends here
