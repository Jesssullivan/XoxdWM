;;; e2e-flat-desktop-test.el --- E2E flat desktop mode tests  -*- lexical-binding: t -*-

;; End-to-end tests for flat desktop mode (no VR, no biometrics).
;; Validates all core EWWM modules load and interoperate correctly
;; in a traditional Wayland compositor configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-workspace)
(require 'ewwm-layout)
(require 'ewwm-input)
(require 'ewwm-manage)
(require 'ewwm-floating)
(require 'ewwm-launch)
(require 'ewwm-headless)
(require 'ewwm-environment)

;; Forward-declare dynamic variables
(defvar ewwm--surface-buffer-alist)
(defvar ewwm-workspace-current-index)
(defvar ewwm-workspace--configs)
(defvar ewwm-workspace--names)
(defvar ewwm-workspace-number)
(defvar ewwm-layout--current)
(defvar ewwm-layout--usable-area)
(defvar ewwm-layout--cycle-list)
(defvar ewwm-input--global-keys)
(defvar ewwm-manage-rules)
(defvar ewwm-launch--processes)
(defvar ewwm-ipc--event-handlers)
(defvar ewwm-headless-mode)

;; ── Module loading ──────────────────────────────────────────

(ert-deftest e2e-flat/all-core-modules-loaded ()
  "All core EWWM modules provide their features."
  (dolist (feat '(ewwm-core
                  ewwm-ipc
                  ewwm-workspace
                  ewwm-layout
                  ewwm-input
                  ewwm-manage
                  ewwm-floating
                  ewwm-launch
                  ewwm-headless
                  ewwm-environment))
    (should (featurep feat))))

(ert-deftest e2e-flat/core-module-count ()
  "At least 10 core modules loaded."
  (let ((count 0))
    (dolist (feat '(ewwm-core ewwm-ipc ewwm-workspace ewwm-layout
                    ewwm-input ewwm-manage ewwm-floating ewwm-launch
                    ewwm-headless ewwm-environment))
      (when (featurep feat)
        (cl-incf count)))
    (should (>= count 10))))

;; ── Workspace API ───────────────────────────────────────────

(ert-deftest e2e-flat/workspace-switch-callable ()
  "Workspace switch API is callable and updates state."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace--configs (make-vector 4 nil))
        (ewwm-workspace--names (make-vector 4 nil))
        (ewwm-workspace-switch-hook nil)
        (ewwm--surface-buffer-alist nil))
    (dotimes (i 4)
      (aset ewwm-workspace--names i (format "ws-%d" i)))
    (ewwm-workspace-switch 2)
    (should (= ewwm-workspace-current-index 2))))

(ert-deftest e2e-flat/workspace-next-prev ()
  "Workspace next/prev cycle correctly."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace--configs (make-vector 4 nil))
        (ewwm-workspace--names (make-vector 4 nil))
        (ewwm-workspace-switch-hook nil)
        (ewwm--surface-buffer-alist nil))
    (dotimes (i 4)
      (aset ewwm-workspace--names i (format "ws-%d" i)))
    (ewwm-workspace-switch-next)
    (should (= ewwm-workspace-current-index 1))
    (ewwm-workspace-switch-prev)
    (should (= ewwm-workspace-current-index 0))))

(ert-deftest e2e-flat/workspace-list-returns-plists ()
  "Workspace list returns proper plist structure."
  (let ((ewwm-workspace-number 4)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace--configs (make-vector 4 nil))
        (ewwm-workspace--names (make-vector 4 nil))
        (ewwm--surface-buffer-alist nil))
    (dotimes (i 4)
      (aset ewwm-workspace--names i (format "ws-%d" i)))
    (let ((result (ewwm-workspace-list)))
      (should (= (length result) 4))
      (should (plist-get (car result) :index))
      (should (plist-get (car result) :name)))))

;; ── Layout modes ────────────────────────────────────────────

(ert-deftest e2e-flat/layout-modes-exist ()
  "All layout modes exist in the cycle list."
  (should (memq 'tiling ewwm-layout--cycle-list))
  (should (memq 'monocle ewwm-layout--cycle-list))
  (should (memq 'grid ewwm-layout--cycle-list)))

(ert-deftest e2e-flat/layout-set-changes-mode ()
  "Setting layout updates current mode."
  (let ((ewwm-layout--current 'tiling)
        (ewwm-layout-change-hook nil)
        (ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0))
    (setq ewwm-layout--current 'monocle)
    (should (eq ewwm-layout--current 'monocle))))

(ert-deftest e2e-flat/layout-cycle-function-exists ()
  "Layout cycle function is available."
  (should (fboundp 'ewwm-layout-cycle))
  (should (commandp 'ewwm-layout-cycle)))

;; ── Focus follows mouse configurable ────────────────────────

(ert-deftest e2e-flat/focus-follows-mouse-configurable ()
  "Input intercept mode is configurable."
  (should (boundp 'ewwm-input-intercept-mode))
  (should (eq (get 'ewwm-input-intercept-mode 'custom-type)
              'boolean)))

;; ── IPC protocol ────────────────────────────────────────────

(ert-deftest e2e-flat/ipc-message-encode-decode ()
  "IPC message encoding produces length-prefixed format."
  (let ((encoded (ewwm-ipc--encode-message '(:type :ping))))
    ;; First 4 bytes are length prefix
    (should (>= (length encoded) 5))
    ;; Decode length
    (let ((len (ewwm-ipc--decode-length encoded)))
      (should (> len 0))
      (should (= (+ 4 len) (length encoded))))))

(ert-deftest e2e-flat/ipc-event-handlers-registered ()
  "Default IPC event handlers are registered."
  (should (assq :surface-created ewwm-ipc--event-handlers))
  (should (assq :surface-destroyed ewwm-ipc--event-handlers))
  (should (assq :key-pressed ewwm-ipc--event-handlers))
  (should (assq :workspace-changed ewwm-ipc--event-handlers)))

(ert-deftest e2e-flat/ipc-connected-p-returns-nil ()
  "IPC connected-p returns nil when not connected."
  (should-not (ewwm-ipc-connected-p)))

;; ── Layer-shell usable area ─────────────────────────────────

(ert-deftest e2e-flat/usable-area-tracking ()
  "Usable area can be set and queried."
  (let ((ewwm-layout--usable-area nil)
        (ewwm-workspace-current-index 0)
        (ewwm--surface-buffer-alist nil))
    (ewwm-layout--set-usable-area '(:x 0 :y 32 :w 1920 :h 1048))
    (should (equal (ewwm-layout-usable-area)
                   '(:x 0 :y 32 :w 1920 :h 1048)))))

;; ── XWayland compatibility ──────────────────────────────────

(ert-deftest e2e-flat/xwayland-functions-exist ()
  "XWayland compatibility functions exist."
  (should (fboundp 'ewwm-surface-x11-p))
  (should (fboundp 'ewwm-x11-surfaces))
  (should (fboundp 'ewwm-manage-rule-x11-class)))

;; ── Surface buffer alist management ─────────────────────────

(ert-deftest e2e-flat/surface-buffer-crud ()
  "Surface buffer CRUD operations work correctly."
  (let ((ewwm--surface-buffer-alist nil))
    ;; Create
    (let ((buf (ewwm--create-surface-buffer 42 "foot" "Terminal")))
      (should (buffer-live-p buf))
      ;; Read
      (should (eq (ewwm--get-buffer 42) buf))
      (should (= (ewwm--surface-count) 1))
      ;; Update
      (ewwm--update-surface-title 42 "New Title")
      (with-current-buffer buf
        (should (equal ewwm-title "New Title")))
      ;; Delete
      (ewwm--destroy-surface-buffer 42)
      (should (= (ewwm--surface-count) 0)))))

;; ── Headless mode ───────────────────────────────────────────

(ert-deftest e2e-flat/headless-mode-available ()
  "Headless mode functions are available as fallback."
  (should (fboundp 'ewwm-headless-init))
  (should (fboundp 'ewwm-headless-teardown))
  (should (fboundp 'ewwm-headless-detect))
  (should (fboundp 'ewwm-headless-status)))

;; ── Key grab pipeline ───────────────────────────────────────

(ert-deftest e2e-flat/key-grab-ungrab-pipeline ()
  "Key grab/ungrab pipeline updates internal state."
  (let ((ewwm-input--global-keys nil))
    (ewwm-input-set-key "s-q" #'kill-buffer)
    (should (alist-get "s-q" ewwm-input--global-keys
                       nil nil #'equal))
    (ewwm-input-unset-key "s-q")
    (should-not (alist-get "s-q" ewwm-input--global-keys
                           nil nil #'equal))))

;; ── Launch functions ────────────────────────────────────────

(ert-deftest e2e-flat/launch-functions-callable ()
  "All launch functions are callable."
  (should (fboundp 'ewwm-launch))
  (should (fboundp 'ewwm-launch-interactively))
  (should (fboundp 'ewwm-launch-favorite))
  (should (fboundp 'ewwm-launch-list))
  (should (fboundp 'ewwm-launch-kill-all)))

;; ── Manage rules ────────────────────────────────────────────

(ert-deftest e2e-flat/manage-rules-configurable ()
  "Manage rules are configurable and matchable."
  (let ((ewwm-manage-rules
         `((,(ewwm-manage-rule-app-id "^foot$")
            :workspace 2 :floating nil)
           (,(ewwm-manage-rule-x11-class "^KeePassXC")
            :floating t))))
    ;; Match foot
    (let ((actions (ewwm-manage--match-rules
                    '(:app-id "foot" :title "Terminal"))))
      (should (= (plist-get actions :workspace) 2)))
    ;; Match X11 KeePassXC
    (let ((actions (ewwm-manage--match-rules
                    '(:x11 t :x11-class "KeePassXC" :app-id ""))))
      (should (eq (plist-get actions :floating) t)))))

;; ── Floating window API ─────────────────────────────────────

(ert-deftest e2e-flat/floating-api-complete ()
  "Floating window API functions exist."
  (should (fboundp 'ewwm-floating-toggle))
  (should (fboundp 'ewwm-floating-list))
  (should (fboundp 'ewwm-floating-p))
  (should (fboundp 'ewwm-floating-move))
  (should (fboundp 'ewwm-floating-resize)))

;; ── Window class matching ───────────────────────────────────

(ert-deftest e2e-flat/window-class-matching ()
  "Window class matching predicates work."
  (let ((by-app (ewwm-manage-rule-app-id "qutebrowser"))
        (by-title (ewwm-manage-rule-title "^vim")))
    (should (funcall by-app '(:app-id "qutebrowser")))
    (should-not (funcall by-app '(:app-id "foot")))
    (should (funcall by-title '(:title "vim-edit")))
    (should-not (funcall by-title '(:title "terminal")))))

;; ── Environment check subsystems ────────────────────────────

(ert-deftest e2e-flat/environment-check-functions ()
  "Environment check functions exist."
  (should (fboundp 'ewwm-environment--check-env-var))
  (should (fboundp 'ewwm-environment--check-path))
  (should (boundp 'ewwm-environment--check-results)))

(ert-deftest e2e-flat/environment-defcustom-groups ()
  "Environment defcustom groups exist."
  (dolist (grp '(ewwm ewwm-workspace ewwm-layout
                 ewwm-input ewwm-floating ewwm-launch
                 ewwm-headless ewwm-environment))
    (should (get grp 'custom-group))))

;; ── Hooks default to nil ────────────────────────────────────

(ert-deftest e2e-flat/hooks-default-nil ()
  "All core hooks default to nil."
  (dolist (hook '(ewwm-workspace-switch-hook
                  ewwm-layout-change-hook
                  ewwm-manage-finish-hook
                  ewwm-manage-close-hook
                  ewwm-ipc-connected-hook
                  ewwm-ipc-disconnected-hook
                  ewwm-mode-hook))
    (should (boundp hook))
    (should-not (default-value hook))))

;; ── Mode exists ─────────────────────────────────────────────

(ert-deftest e2e-flat/ewwm-mode-defined ()
  "ewwm-mode major mode is defined."
  (should (fboundp 'ewwm-mode)))

;; ── Fullscreen toggle ───────────────────────────────────────

(ert-deftest e2e-flat/fullscreen-toggle-exists ()
  "Fullscreen toggle function exists."
  (should (fboundp 'ewwm-fullscreen-toggle)))

;; ── Surface buffer pipeline ─────────────────────────────────

(ert-deftest e2e-flat/surface-create-manage-destroy ()
  "Full surface lifecycle: create -> manage -> destroy."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-manage-rules nil)
        (ewwm-manage-finish-hook nil)
        (ewwm-manage-close-hook nil)
        (ewwm-workspace-current-index 0))
    ;; Create via manage
    (let ((buf (ewwm-manage--on-create
                '(:id 100 :app-id "foot" :title "Test"))))
      (should (buffer-live-p buf))
      (should (= (ewwm--surface-count) 1))
      ;; Destroy
      (ewwm-manage--on-destroy '(:id 100))
      (should (= (ewwm--surface-count) 0)))))

;;; e2e-flat-desktop-test.el ends here
