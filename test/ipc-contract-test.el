;;; ipc-contract-test.el --- Static IPC contract checks -*- lexical-binding: t; -*-

;;; Commentary:
;; These tests keep the Rust dispatcher and Lisp IPC clients from drifting
;; silently while the WM authority boundary moves into native XoxdWM code.

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst ipc-contract-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst ipc-contract-test--known-elisp-without-rust-handler
  '("anchor-create"
    "anchor-goto"
    "anchor-list"
    "anchor-remove"
    "anchor-restore"
    "anchor-status"
    "bci-attention-calibrate"
    "bci-attention-toggle"
    "bci-dnd-disable"
    "bci-dnd-enable"
    "bci-hardware-check"
    "bci-mi-calibrate"
    "bci-mi-toggle"
    "bci-nfb-start"
    "bci-nfb-stop"
    "bci-p300-cancel"
    "bci-ssvep-configure"
    "command"
    "focus-routing-configure"
    "focus-routing-set-dwell"
    "focus-routing-set-mode"
    "focus-routing-status"
    "focus-surface"
    "follow-configure"
    "follow-recenter"
    "follow-set-policy"
    "follow-status"
    "gaze-zone-set-layout"
    "hand-tracking-configure"
    "hand-tracking-toggle"
    "input-latency-probe"
    "multimodal-disable"
    "multimodal-enable"
    "multimodal-set-dwell"
    "multimodal-three-factor-start"
    "overlay-create"
    "overlay-link-surface"
    "overlay-list"
    "overlay-remove"
    "overlay-set-alpha"
    "overlay-set-visible"
    "overlay-status"
    "passkey-response"
    "passthrough-disable"
    "passthrough-enable"
    "passthrough-set-blend-mode"
    "passthrough-set-opacity"
    "passthrough-status"
    "surface-move-interactive"
    "surface-resize-interactive"
    "transient-configure"
    "transient-list"
    "transient-set-offset"
    "transient-set-placement"
    "transient-status")
  "Current known Lisp request types that do not have Rust dispatch handlers.")

(defconst ipc-contract-test--known-payload-debt
  '("surface-resize geometry payload")
  "Known IPC schema debt not represented by command-name parity alone.")

(defun ipc-contract-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative ipc-contract-test--root))
    (buffer-string)))

(defun ipc-contract-test--rust-dispatch-commands ()
  "Return top-level Rust IPC dispatch command strings."
  (let ((content (ipc-contract-test--read-file "compositor/src/ipc/dispatch.rs"))
        (start nil)
        (end nil)
        (commands nil))
    (setq start (string-match "match msg_type\\.as_deref()" content))
    (should start)
    (setq end (string-match "^[[:space:]]*other =>" content start))
    (should end)
    (with-temp-buffer
      (insert (substring content start end))
      (goto-char (point-min))
      (while (re-search-forward "Some(\"\\([^\"]+\\)\")" nil t)
        (push (match-string 1) commands)))
    (sort (delete-dups commands) #'string<)))

(defun ipc-contract-test--elisp-request-types ()
  "Return Lisp request `:type' keywords found under lisp/."
  (let ((commands nil))
    (dolist (dir '("lisp/core" "lisp/vr" "lisp/ext"))
      (let ((abs-dir (expand-file-name dir ipc-contract-test--root)))
        (when (file-directory-p abs-dir)
          (dolist (file (directory-files-recursively abs-dir "\\.el\\'"))
            (with-temp-buffer
              (insert-file-contents file)
              (goto-char (point-min))
              (while (re-search-forward ":type[[:space:]\n]+:\\([A-Za-z0-9_-]+\\)" nil t)
                (unless (string= (match-string 1) "event")
                  (push (match-string 1) commands))))))))
    (sort (delete-dups commands) #'string<)))

(ert-deftest ipc-contract/elisp-requests-have-rust-handler-or-known-debt ()
  "Every Lisp request type should either dispatch in Rust or be listed debt."
  (let* ((rust (ipc-contract-test--rust-dispatch-commands))
         (elisp (ipc-contract-test--elisp-request-types))
         (missing (cl-set-difference elisp rust :test #'string=))
         (unexpected (cl-set-difference
                      missing
                      ipc-contract-test--known-elisp-without-rust-handler
                      :test #'string=))
         (retired-debt (cl-set-difference
                        ipc-contract-test--known-elisp-without-rust-handler
                        missing
                        :test #'string=)))
    (ert-info ((format "unexpected missing Rust handlers: %S" unexpected))
      (should-not unexpected))
    (ert-info ((format "known IPC debt no longer observed; update the debt list: %S"
                       retired-debt))
      (should-not retired-debt))))

(ert-deftest ipc-contract/known-schema-debt-is-explicit ()
  "Schema mismatches that command-name parity cannot see should stay explicit."
  (dolist (item '("surface-resize geometry payload"))
    (should (member item ipc-contract-test--known-payload-debt))))

(ert-deftest ipc-contract/docs-do-not-make-emacs-the-wm-brain ()
  "IPC docs should describe native authority, not Emacs WM authority."
  (let ((docs (ipc-contract-test--read-file "docs/ipc-protocol.md")))
    (should-not (string-match-p "Emacs is the window management brain" docs))
    (should-not (string-match-p "All layout policy decisions flow from Emacs" docs))
    (should (string-match-p "Rust compositor is the native WM/DE authority" docs))
    (should (string-match-p "Emacs/eGreg clients" docs))
    (should (string-match-p "not the WM authority" docs))))

(ert-deftest ipc-contract/focus-event-legacy-alias-is-client-only ()
  "Rust emits surface-focused; Lisp keeps focus-changed compatibility."
  (let ((seat (ipc-contract-test--read-file "compositor/src/handlers/seat.rs"))
        (lisp (ipc-contract-test--read-file "lisp/vr/ewwm-ipc.el")))
    (should (string-match-p "\"surface-focused\"" seat))
    (should (string-match-p "\"previous-id\"" seat))
    (should-not (string-match-p "\"focus-changed\"" seat))
    (should (string-match-p ":focus-changed" lisp))
    (should (string-match-p "ewwm-ipc--on-focus-changed-compat" lisp))))

(ert-deftest ipc-contract/gaze-focus-event-name-is-canonical ()
  "Rust and Lisp agree on gaze-focus-request; Lisp accepts retired spelling."
  (let ((rust (ipc-contract-test--read-file "compositor/src/vr/gaze_focus.rs"))
        (lisp (ipc-contract-test--read-file "lisp/vr/ewwm-vr-eye.el")))
    (should (string-match-p ":gaze-focus-request" rust))
    (should-not (string-match-p ":gaze-focus-requested" rust))
    (should (string-match-p ":gaze-focus-request[[:space:]]+\\. ewwm-vr-eye--on-gaze-focus-request" lisp))
    (should (string-match-p ":gaze-focus-requested[[:space:]]+\\. ewwm-vr-eye--on-gaze-focus-request" lisp))))

(ert-deftest ipc-contract/native-policy-aliases-are-documented ()
  "Native aliases and client-only legacy events are explicitly documented."
  (let ((docs (ipc-contract-test--read-file "docs/ipc-protocol.md")))
    (dolist (token '(":launch-app" ":reload-config"
                    ":focus-changed" ":gaze-focus-requested"))
      (should (string-match-p (regexp-quote token) docs)))
    (should (string-match-p "temporary command aliases" docs))
    (should (string-match-p "client-only compatibility alias" docs))))

;;; ipc-contract-test.el ends here
