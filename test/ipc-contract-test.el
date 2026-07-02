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
  nil
  "Current known Lisp request types that do not have Rust dispatch handlers.")

(defconst ipc-contract-test--known-payload-debt
  nil
  "Known IPC schema debt not represented by command-name parity alone.")

(defconst ipc-contract-test--known-rust-events-without-elisp-handler
  '("activation-requested"
    "autotype-char"
    "double-left-wink"
    "double-right-wink"
    "dpms-changed"
    "extended-closure"
    "fatigue-alert-critical"
    "fatigue-alert-mild"
    "fatigue-alert-significant"
    "fatigue-level-changed"
    "gaze-cooldown-ended"
    "gaze-cooldown-started"
    "gaze-dwell-started"
    "gaze-focus-cancelled"
    "gaze-reading-entered"
    "gaze-reading-exited"
    "gaze-saccade-detected"
    "left-wink"
    "natural-blink"
    "native-key-action"
    "pointer-locked"
    "pointer-unlocked"
    "right-wink"
    "surface-updated")
  "Rust-emitted events not consumed by the current Emacs app layer.

These are either server-side lifecycle/telemetry events or explicit native
event names whose old Lisp-side app clients have not been promoted yet.")

(defconst ipc-contract-test--known-elisp-events-without-rust-emitter
  '("autotype-error"
    "bci-attention"
    "bci-connected"
    "bci-disconnected"
    "bci-error"
    "bci-frame"
    "bci-mi"
    "bci-mi-calibration"
    "bci-multimodal"
    "bci-nfb-frame"
    "bci-p300"
    "bci-quality"
    "bci-ssvep"
    "beyond-connected"
    "beyond-disconnected"
    "beyond-status"
    "blink"
    "capture-status"
    "fatigue-alert"
    "fatigue-metrics"
    "focus-routing-dwell-progress"
    "follow-following"
    "follow-status"
    "gaze-calibration-drift"
    "gaze-cooldown"
    "gaze-data"
    "gaze-dwell"
    "gaze-fixation"
    "gaze-focus"
    "gaze-focus-request"
    "gaze-reading-mode"
    "gaze-saccade"
    "gaze-saccade-state"
    "gaze-target-changed"
    "gaze-tracking-lost"
    "gaze-update"
    "gesture-ended"
    "gesture-started"
    "gesture-swipe"
    "gpu-power-state-changed"
    "hand-confidence"
    "hand-tracking-lost"
    "hand-tracking-started"
    "keyboard-layout-changed"
    "keyboard-special-key"
    "keyboard-text-input"
    "keyboard-visibility"
    "output-list-response"
    "overlay-created"
    "overlay-list"
    "overlay-removed"
    "passthrough-state-changed"
    "radial-confirmed"
    "radial-state"
    "surface-title-changed"
    "transient-added"
    "transient-list"
    "transient-removed"
    "vr-click"
    "vr-display-hmd-selected"
    "vr-display-hotplug"
    "vr-display-mode-changed"
    "vr-frame-stats"
    "vr-grab-ended"
    "vr-grab-started"
    "vr-pointer"
    "vr-scene-layout-changed"
    "vr-scene-surface-added"
    "vr-scene-surface-removed"
    "wink"
    "wink-calibration-result")
  "Emacs app-layer event handlers without a native Rust emitter today.

This is explicit design/prototype debt, not native compositor proof.")

(defun ipc-contract-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative ipc-contract-test--root))
    (buffer-string)))

(defun ipc-contract-test--project-files (dirs regexp)
  "Return project files below DIRS whose names match REGEXP."
  (let ((files nil))
    (dolist (dir dirs)
      (let ((abs-dir (expand-file-name dir ipc-contract-test--root)))
        (when (file-directory-p abs-dir)
          (setq files
                (append files
                        (directory-files-recursively abs-dir regexp))))))
    files))

(defun ipc-contract-test--collect-regexp-matches (content regexp)
  "Return first capture group matches for REGEXP in CONTENT."
  (let ((matches nil)
        (start 0))
    (while (string-match regexp content start)
      (push (match-string 1 content) matches)
      (setq start (match-end 0)))
    matches))

(defun ipc-contract-test--rust-event-types ()
  "Return Rust compositor event keywords emitted through IPC."
  (let ((events nil))
    (dolist (file (ipc-contract-test--project-files '("compositor/src") "\\.rs\\'"))
      (let ((content (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
        (setq events
              (append
               (ipc-contract-test--collect-regexp-matches
                content "format_event\\s-*(\\s-*\"\\([A-Za-z0-9_-]+\\)\"")
               (ipc-contract-test--collect-regexp-matches
                content "broadcast_anchor_event\\s-*(\\s-*[^,]+,\\s-*\"\\([A-Za-z0-9_-]+\\)\"")
               (ipc-contract-test--collect-regexp-matches
                content ":event\\s-+:event\\s-+:\\([A-Za-z0-9_-]+\\)")
               events))))
    (setq events (cl-remove-if (lambda (event)
                                 (member event '("event" "test")))
                               events))
    (sort (delete-dups events) #'string<)))

(defun ipc-contract-test--elisp-event-types ()
  "Return Lisp IPC event handler keywords under the app-layer Lisp tree."
  (let ((events nil))
    (dolist (file (ipc-contract-test--project-files '("lisp/vr" "lisp/ext") "\\.el\\'"))
      (let ((content (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
        (when (or (string-match-p "ewwm-ipc-register-events" content)
                  (string-match-p "ewwm-ipc--event-handlers" content))
          (setq events
                (append
                 (ipc-contract-test--collect-regexp-matches
                  content "(\\s-*:\\([A-Za-z0-9_-]+\\)\\s-*\\.")
                 events)))))
    (sort (delete-dups events) #'string<)))

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
      (while (re-search-forward "^[[:space:]]*Some(\"\\([^\"]+\\)\")[[:space:]]*=>" nil t)
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
                (let ((command (match-string 1)))
                  (unless (member command '("event" "response" "error"))
                    (push command commands)))))))))
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
  (should-not ipc-contract-test--known-payload-debt))

(ert-deftest ipc-contract/rust-events-have-elisp-handler-or-known-boundary ()
  "Rust compositor events should be consumed by Lisp or listed as explicit debt."
  (let* ((rust (ipc-contract-test--rust-event-types))
         (elisp (ipc-contract-test--elisp-event-types))
         (missing (cl-set-difference rust elisp :test #'string=))
         (unexpected (cl-set-difference
                      missing
                      ipc-contract-test--known-rust-events-without-elisp-handler
                      :test #'string=))
         (retired-debt (cl-set-difference
                        ipc-contract-test--known-rust-events-without-elisp-handler
                        missing
                        :test #'string=)))
    (ert-info ((format "unexpected Rust events without Lisp handlers: %S" unexpected))
      (should-not unexpected))
    (ert-info ((format "known Rust event debt no longer observed; update the debt list: %S"
                       retired-debt))
      (should-not retired-debt))))

(ert-deftest ipc-contract/elisp-event-handlers-have-rust-emitter-or-known-app-layer-source ()
  "Lisp event handlers should map to Rust emitters or explicit app-layer debt."
  (let* ((rust (ipc-contract-test--rust-event-types))
         (elisp (ipc-contract-test--elisp-event-types))
         (missing (cl-set-difference elisp rust :test #'string=))
         (unexpected (cl-set-difference
                      missing
                      ipc-contract-test--known-elisp-events-without-rust-emitter
                      :test #'string=))
         (retired-debt (cl-set-difference
                        ipc-contract-test--known-elisp-events-without-rust-emitter
                        missing
                        :test #'string=)))
    (ert-info ((format "unexpected Lisp event handlers without Rust emitters: %S"
                       unexpected))
      (should-not unexpected))
    (ert-info ((format "known Lisp app-layer event debt no longer observed; update the debt list: %S"
                       retired-debt))
      (should-not retired-debt))))

;;; ipc-contract-test.el ends here
