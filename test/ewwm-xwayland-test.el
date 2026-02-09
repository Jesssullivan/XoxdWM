;;; ewwm-xwayland-test.el --- Week 6 XWayland tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-manage)
(require 'ewwm-layout)
(require 'ewwm-floating)
(require 'ewwm-ipc)

;; ── XWayland surface creation ──────────────────────────────

(ert-deftest ewwm-xwayland/x11-surface-sets-x11-flag ()
  "XWayland surfaces have ewwm-x11-p set to t."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "xterm" :title "xterm"
                  :x11 t :x11-class "XTerm" :x11-instance "xterm"))))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should (buffer-local-value 'ewwm-x11-p buf))
            (should (string= (buffer-local-value 'ewwm-x11-class buf) "XTerm"))
            (should (string= (buffer-local-value 'ewwm-x11-instance buf) "xterm")))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-xwayland/wayland-surface-not-x11 ()
  "Native Wayland surfaces have ewwm-x11-p nil."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "foot" :title "foot"))))
      (unwind-protect
          (progn
            (should (buffer-live-p buf))
            (should-not (buffer-local-value 'ewwm-x11-p buf)))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-xwayland/x11-class-populates-from-msg ()
  "X11 class comes from :x11-class field in IPC message."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "firefox" :title "Firefox"
                  :x11 t :x11-class "Firefox-esr" :x11-instance "Navigator"))))
      (unwind-protect
          (progn
            (should (string= (buffer-local-value 'ewwm-x11-class buf) "Firefox-esr"))
            (should (string= (buffer-local-value 'ewwm-x11-instance buf) "Navigator")))
        (ewwm--destroy-surface-buffer 1)))))

;; ── Transient window auto-float ────────────────────────────

(ert-deftest ewwm-xwayland/transient-x11-auto-floats ()
  "X11 transient windows (dialogs) are auto-floated."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "dialog" :title "Save As"
                  :x11 t :x11-class "Dialog" :x11-instance "dialog"
                  :transient t))))
      (unwind-protect
          (should (eq (buffer-local-value 'ewwm-surface-state buf) 'floating))
        (ewwm--destroy-surface-buffer 1)))))

(ert-deftest ewwm-xwayland/non-transient-not-auto-float ()
  "Non-transient X11 windows are not auto-floated."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "xterm" :title "xterm"
                  :x11 t :x11-class "XTerm" :x11-instance "xterm"))))
      (unwind-protect
          (should (eq (buffer-local-value 'ewwm-surface-state buf) 'managed))
        (ewwm--destroy-surface-buffer 1)))))

;; ── ewwm-surface-x11-p predicate ──────────────────────────

(ert-deftest ewwm-xwayland/surface-x11-p-predicate ()
  "ewwm-surface-x11-p returns non-nil for X11 surfaces."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((x11-buf (ewwm-manage--on-create
                    '(:id 1 :app-id "xterm" :title "xterm" :x11 t)))
          (wl-buf (ewwm-manage--on-create
                   '(:id 2 :app-id "foot" :title "foot"))))
      (unwind-protect
          (progn
            (should (ewwm-surface-x11-p x11-buf))
            (should-not (ewwm-surface-x11-p wl-buf)))
        (ewwm--destroy-surface-buffer 1)
        (ewwm--destroy-surface-buffer 2)))))

(ert-deftest ewwm-xwayland/x11-surfaces-list ()
  "ewwm-x11-surfaces returns only X11 surface buffers."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (ewwm-manage--on-create '(:id 1 :app-id "xterm" :title "xterm" :x11 t))
    (ewwm-manage--on-create '(:id 2 :app-id "foot" :title "foot"))
    (ewwm-manage--on-create '(:id 3 :app-id "xclock" :title "xclock" :x11 t))
    (unwind-protect
        (progn
          (should (= (length (ewwm-x11-surfaces)) 2))
          (should (cl-every #'ewwm-surface-x11-p (ewwm-x11-surfaces))))
      (ewwm--destroy-surface-buffer 1)
      (ewwm--destroy-surface-buffer 2)
      (ewwm--destroy-surface-buffer 3))))

;; ── Manage rule helpers ────────────────────────────────────

(ert-deftest ewwm-xwayland/manage-rule-x11-class ()
  "ewwm-manage-rule-x11-class creates a working predicate."
  (let ((pred (ewwm-manage-rule-x11-class "^Firefox")))
    (should (funcall pred '(:x11 t :x11-class "Firefox-esr")))
    (should-not (funcall pred '(:x11 t :x11-class "Chromium")))
    (should-not (funcall pred '(:app-id "firefox")))))

(ert-deftest ewwm-xwayland/manage-rule-app-id ()
  "ewwm-manage-rule-app-id creates a working predicate."
  (let ((pred (ewwm-manage-rule-app-id "^org\\.qutebrowser")))
    (should (funcall pred '(:app-id "org.qutebrowser.qutebrowser")))
    (should-not (funcall pred '(:app-id "foot")))))

(ert-deftest ewwm-xwayland/manage-rule-title ()
  "ewwm-manage-rule-title creates a working predicate."
  (let ((pred (ewwm-manage-rule-title "Save As")))
    (should (funcall pred '(:title "Save As - foo.txt")))
    (should-not (funcall pred '(:title "Terminal")))))

;; ── Manage rules integration ───────────────────────────────

(ert-deftest ewwm-xwayland/manage-rules-applied ()
  "Manage rules correctly classify X11 surfaces."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules
         `((,(ewwm-manage-rule-x11-class "^KeePassXC") . (:floating t))
           (,(ewwm-manage-rule-app-id "^org\\.qutebrowser") . (:workspace 2)))))
    (let ((keepass-buf (ewwm-manage--on-create
                        '(:id 1 :app-id "keepassxc" :title "KeePassXC"
                          :x11 t :x11-class "KeePassXC")))
          (qute-buf (ewwm-manage--on-create
                     '(:id 2 :app-id "org.qutebrowser.qutebrowser"
                       :title "qutebrowser"))))
      (unwind-protect
          (progn
            (should (eq (buffer-local-value 'ewwm-surface-state keepass-buf) 'floating))
            (should (= (buffer-local-value 'ewwm-workspace qute-buf) 2)))
        (ewwm--destroy-surface-buffer 1)
        (ewwm--destroy-surface-buffer 2)))))

;; ── Layer-shell usable area ────────────────────────────────

(ert-deftest ewwm-xwayland/usable-area-handler ()
  "Usable area handler updates layout state."
  (let ((ewwm-layout--usable-area nil))
    (ewwm-layout--on-usable-area-changed
     '(:x 0 :y 30 :w 1920 :h 1050))
    (should ewwm-layout--usable-area)
    (should (= (plist-get ewwm-layout--usable-area :y) 30))
    (should (= (plist-get ewwm-layout--usable-area :h) 1050))))

(ert-deftest ewwm-xwayland/usable-area-accessor ()
  "ewwm-layout-usable-area returns the current usable area."
  (let ((ewwm-layout--usable-area '(:x 10 :y 20 :w 1900 :h 1060)))
    (should (equal (ewwm-layout-usable-area)
                   '(:x 10 :y 20 :w 1900 :h 1060)))))

(ert-deftest ewwm-xwayland/usable-area-nil-initially ()
  "Usable area is nil before any layer-shell event."
  (let ((ewwm-layout--usable-area nil))
    (should-not (ewwm-layout-usable-area))))

;; ── IPC event handler registration ─────────────────────────

(ert-deftest ewwm-xwayland/ipc-event-handlers-exist ()
  "All IPC event handler functions exist."
  (should (fboundp 'ewwm-ipc--on-surface-created))
  (should (fboundp 'ewwm-ipc--on-surface-destroyed))
  (should (fboundp 'ewwm-ipc--on-key-pressed))
  (should (fboundp 'ewwm-ipc--on-output-usable-area-changed)))

(ert-deftest ewwm-xwayland/ipc-event-handlers-alist ()
  "Event handler alist includes output-usable-area-changed."
  (should (assq :output-usable-area-changed ewwm-ipc--event-handlers)))

;; ── Mode-line shows X11 indicator ──────────────────────────

(ert-deftest ewwm-xwayland/mode-line-x11-indicator ()
  "Mode-line includes X11 indicator for XWayland surfaces."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (let ((buf (ewwm-manage--on-create
                '(:id 1 :app-id "xterm" :title "xterm"
                  :x11 t :x11-class "XTerm"))))
      (unwind-protect
          (with-current-buffer buf
            ;; The mode-line-format contains an :eval for X11 indicator
            (let ((flat (flatten-tree mode-line-format)))
              (should (member 'ewwm-x11-p flat))))
        (ewwm--destroy-surface-buffer 1)))))

;; ── Mixed Wayland/X11 workspace ────────────────────────────

(ert-deftest ewwm-xwayland/mixed-surfaces-on-workspace ()
  "Workspace can contain both Wayland and X11 surfaces."
  (let ((ewwm--surface-buffer-alist nil)
        (ewwm-workspace-current-index 0)
        (ewwm-manage-rules nil))
    (ewwm-manage--on-create '(:id 1 :app-id "foot" :title "Terminal"))
    (ewwm-manage--on-create '(:id 2 :app-id "xterm" :title "xterm" :x11 t))
    (ewwm-manage--on-create '(:id 3 :app-id "firefox" :title "Firefox" :x11 t))
    (unwind-protect
        (let ((ws-bufs (ewwm--buffers-on-workspace 0)))
          (should (= (length ws-bufs) 3))
          ;; Mix of X11 and Wayland
          (should (= (length (cl-remove-if-not
                              #'ewwm-surface-x11-p ws-bufs))
                     2)))
      (ewwm--destroy-surface-buffer 1)
      (ewwm--destroy-surface-buffer 2)
      (ewwm--destroy-surface-buffer 3))))

;;; ewwm-xwayland-test.el ends here
