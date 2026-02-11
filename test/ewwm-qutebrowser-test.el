;;; ewwm-qutebrowser-test.el --- Tests for qutebrowser core modules  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'ewwm-core)
(require 'ewwm-qutebrowser)
(require 'ewwm-qutebrowser-ipc)
(require 'ewwm-qutebrowser-tabs)

;; Forward-declare functions from modules under test
(declare-function ewwm-qutebrowser--surface-p "ewwm-qutebrowser")
(declare-function ewwm-qutebrowser-ipc--format-json
                  "ewwm-qutebrowser-ipc")
(declare-function ewwm-qutebrowser-ipc--fifo-path
                  "ewwm-qutebrowser-ipc")
(declare-function ewwm-qutebrowser-tab--create
                  "ewwm-qutebrowser-tabs")
(declare-function ewwm-qutebrowser-tab--find
                  "ewwm-qutebrowser-tabs")
(declare-function ewwm-qutebrowser-tab--update
                  "ewwm-qutebrowser-tabs")
(declare-function ewwm-qutebrowser-tab--kill
                  "ewwm-qutebrowser-tabs")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-qutebrowser-tab--buffers)
(defvar ewwm-qutebrowser-tab-buffer-prefix)
(defvar ewwm-qutebrowser-ipc--fifo-cache)
(defvar ewwm-qutebrowser-fifo-dir)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-qb/core-provides-feature ()
  "ewwm-qutebrowser provides its feature."
  (should (featurep 'ewwm-qutebrowser)))

(ert-deftest ewwm-qb/ipc-provides-feature ()
  "ewwm-qutebrowser-ipc provides its feature."
  (should (featurep 'ewwm-qutebrowser-ipc)))

(ert-deftest ewwm-qb/tabs-provides-feature ()
  "ewwm-qutebrowser-tabs provides its feature."
  (should (featurep 'ewwm-qutebrowser-tabs)))

;; ── Groups exist ─────────────────────────────────────────────

(ert-deftest ewwm-qb/core-group-exists ()
  "ewwm-qutebrowser customization group exists."
  (should (get 'ewwm-qutebrowser 'custom-group)))

(ert-deftest ewwm-qb/ipc-group-exists ()
  "ewwm-qutebrowser-ipc customization group exists."
  (should (get 'ewwm-qutebrowser-ipc 'custom-group)))

(ert-deftest ewwm-qb/tabs-group-exists ()
  "ewwm-qutebrowser-tabs customization group exists."
  (should (get 'ewwm-qutebrowser-tabs 'custom-group)))

;; ── Defcustom defaults ──────────────────────────────────────

(ert-deftest ewwm-qb/app-id-defcustom ()
  "Default app-id is org.qutebrowser.qutebrowser."
  (should (equal (default-value 'ewwm-qutebrowser-app-id)
                 "org.qutebrowser.qutebrowser")))

(ert-deftest ewwm-qb/command-defcustom ()
  "Default command is qutebrowser."
  (should (equal (default-value 'ewwm-qutebrowser-command)
                 "qutebrowser")))

(ert-deftest ewwm-qb/default-url-defcustom ()
  "Default URL is about:blank."
  (should (equal (default-value 'ewwm-qutebrowser-default-url)
                 "about:blank")))

(ert-deftest ewwm-qb/ipc-method-defcustom ()
  "Default IPC method is fifo."
  (should (eq (default-value 'ewwm-qutebrowser-ipc-method) 'fifo)))

(ert-deftest ewwm-qb/ipc-timeout-defcustom ()
  "Default IPC timeout is 5."
  (should (= (default-value 'ewwm-qutebrowser-ipc-timeout) 5)))

(ert-deftest ewwm-qb/tab-buffer-prefix-defcustom ()
  "Default tab buffer prefix is *qb:."
  (should (equal (default-value 'ewwm-qutebrowser-tab-buffer-prefix)
                 "*qb:")))

(ert-deftest ewwm-qb/tab-close-on-kill-defcustom ()
  "Default tab-close-on-kill is t."
  (should (eq (default-value 'ewwm-qutebrowser-tab-close-on-kill) t)))

(ert-deftest ewwm-qb/tab-sync-on-switch-defcustom ()
  "Default tab-sync-on-switch is t."
  (should (eq (default-value 'ewwm-qutebrowser-tab-sync-on-switch) t)))

;; ── Function existence ──────────────────────────────────────

(ert-deftest ewwm-qb/surface-p-exists ()
  "ewwm-qutebrowser--surface-p is defined."
  (should (fboundp 'ewwm-qutebrowser--surface-p)))

(ert-deftest ewwm-qb/surfaces-exists ()
  "ewwm-qutebrowser--surfaces is defined."
  (should (fboundp 'ewwm-qutebrowser--surfaces)))

(ert-deftest ewwm-qb/current-surface-exists ()
  "ewwm-qutebrowser-current-surface is defined."
  (should (fboundp 'ewwm-qutebrowser-current-surface)))

(ert-deftest ewwm-qb/open-url-exists ()
  "ewwm-qutebrowser-open-url is defined."
  (should (fboundp 'ewwm-qutebrowser-open-url)))

(ert-deftest ewwm-qb/launch-exists ()
  "ewwm-qutebrowser-launch is defined."
  (should (fboundp 'ewwm-qutebrowser-launch)))

(ert-deftest ewwm-qb/ipc-send-exists ()
  "ewwm-qutebrowser-ipc-send is defined."
  (should (fboundp 'ewwm-qutebrowser-ipc-send)))

(ert-deftest ewwm-qb/ipc-connected-p-exists ()
  "ewwm-qutebrowser-ipc-connected-p is defined."
  (should (fboundp 'ewwm-qutebrowser-ipc-connected-p)))

(ert-deftest ewwm-qb/ipc-format-json-exists ()
  "ewwm-qutebrowser-ipc--format-json is defined."
  (should (fboundp 'ewwm-qutebrowser-ipc--format-json)))

(ert-deftest ewwm-qb/tab-create-exists ()
  "ewwm-qutebrowser-tab--create is defined."
  (should (fboundp 'ewwm-qutebrowser-tab--create)))

(ert-deftest ewwm-qb/tab-find-exists ()
  "ewwm-qutebrowser-tab--find is defined."
  (should (fboundp 'ewwm-qutebrowser-tab--find)))

;; ── Surface matching ────────────────────────────────────────

(ert-deftest ewwm-qb/surface-p-plist-match ()
  "surface-p matches plist with correct app-id."
  (should (ewwm-qutebrowser--surface-p
           '(:app-id "org.qutebrowser.qutebrowser" :title "Test"))))

(ert-deftest ewwm-qb/surface-p-plist-no-match ()
  "surface-p rejects plist with wrong app-id."
  (should-not (ewwm-qutebrowser--surface-p
               '(:app-id "org.mozilla.firefox" :title "Test"))))

(ert-deftest ewwm-qb/surface-p-buffer-match ()
  "surface-p matches buffer with correct app-id."
  (let ((buf (generate-new-buffer " *test-qb*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq-local ewwm-app-id "org.qutebrowser.qutebrowser"))
          (should (ewwm-qutebrowser--surface-p buf)))
      (kill-buffer buf))))

;; ── IPC JSON format ─────────────────────────────────────────

(ert-deftest ewwm-qb/format-json-simple-command ()
  "format-json produces valid JSON for a simple command."
  (let* ((json-str (ewwm-qutebrowser-ipc--format-json ":open"))
         (parsed (json-read-from-string json-str)))
    (should (equal (alist-get 'args parsed) [":open"]))
    (should (equal (alist-get 'protocol_version parsed) 1))))

(ert-deftest ewwm-qb/format-json-with-args ()
  "format-json joins command and args."
  (let* ((json-str (ewwm-qutebrowser-ipc--format-json
                    ":open" "-t" "https://example.com"))
         (parsed (json-read-from-string json-str)))
    (should (equal (alist-get 'args parsed)
                   [":open -t https://example.com"]))))

(ert-deftest ewwm-qb/format-json-has-target-arg ()
  "format-json includes target_arg key."
  (let* ((json-str (ewwm-qutebrowser-ipc--format-json ":back"))
         (parsed (json-read-from-string json-str)))
    ;; target_arg is present in the output
    (should (assoc 'target_arg parsed))))

;; ── Tab buffer creation ─────────────────────────────────────

(ert-deftest ewwm-qb/tab-create-returns-buffer ()
  "tab--create returns a live buffer."
  (let ((ewwm-qutebrowser-tab--buffers nil)
        (ewwm-qutebrowser-tab-buffer-prefix "*qb:"))
    (let ((buf (ewwm-qutebrowser-tab--create
                0 "https://example.com" "Example")))
      (unwind-protect
          (progn
            (should (bufferp buf))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (= ewwm-qutebrowser-tab-index 0))
              (should (equal ewwm-qutebrowser-tab-url
                             "https://example.com"))
              (should (equal ewwm-qutebrowser-tab-title "Example"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ewwm-qb/tab-find-returns-buffer ()
  "tab--find returns the correct buffer."
  (let ((ewwm-qutebrowser-tab--buffers nil)
        (ewwm-qutebrowser-tab-buffer-prefix "*qb:"))
    (let ((buf (ewwm-qutebrowser-tab--create
                0 "https://example.com" "Example")))
      (unwind-protect
          (should (eq (ewwm-qutebrowser-tab--find 0) buf))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ewwm-qb/tab-update-changes-title ()
  "tab--update changes the tab title."
  (let ((ewwm-qutebrowser-tab--buffers nil)
        (ewwm-qutebrowser-tab-buffer-prefix "*qb:"))
    (let ((buf (ewwm-qutebrowser-tab--create
                0 "https://example.com" "Old Title")))
      (unwind-protect
          (progn
            (ewwm-qutebrowser-tab--update 0 :title "New Title")
            (with-current-buffer buf
              (should (equal ewwm-qutebrowser-tab-title "New Title"))))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest ewwm-qb/tab-kill-removes-buffer ()
  "tab--kill kills the buffer and removes from alist."
  (let ((ewwm-qutebrowser-tab--buffers nil)
        (ewwm-qutebrowser-tab-buffer-prefix "*qb:"))
    (let ((buf (ewwm-qutebrowser-tab--create
                0 "https://example.com" "Test")))
      (should (ewwm-qutebrowser-tab--kill 0))
      (should-not (buffer-live-p buf))
      (should-not (ewwm-qutebrowser-tab--find 0)))))

;; ── FIFO path computation ───────────────────────────────────

(ert-deftest ewwm-qb/fifo-path-returns-nil-when-empty ()
  "fifo-path returns nil when no FIFO exists."
  (let ((ewwm-qutebrowser-ipc--fifo-cache nil)
        (ewwm-qutebrowser-fifo-dir "/nonexistent-dir-xyz"))
    (should-not (ewwm-qutebrowser-ipc--fifo-path))))

(ert-deftest ewwm-qb/fifo-path-uses-cache ()
  "fifo-path returns cached value."
  (let ((ewwm-qutebrowser-ipc--fifo-cache "/tmp/test-fifo"))
    (should (equal (ewwm-qutebrowser-ipc--fifo-path) "/tmp/test-fifo"))
    ;; Reset cache
    (setq ewwm-qutebrowser-ipc--fifo-cache nil)))

;; ── Interactive command checks ──────────────────────────────

(ert-deftest ewwm-qb/open-url-interactive ()
  "ewwm-qutebrowser-open-url is interactive."
  (should (commandp 'ewwm-qutebrowser-open-url)))

(ert-deftest ewwm-qb/back-interactive ()
  "ewwm-qutebrowser-back is interactive."
  (should (commandp 'ewwm-qutebrowser-back)))

(ert-deftest ewwm-qb/launch-interactive ()
  "ewwm-qutebrowser-launch is interactive."
  (should (commandp 'ewwm-qutebrowser-launch)))

;;; ewwm-qutebrowser-test.el ends here
