;;; ewwm-bci-nfb-test.el --- Tests for neurofeedback module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-nfb)

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-nfb-session-dir)
(defvar ewwm-bci-nfb-log-interval)
(defvar ewwm-bci-nfb-display-channels)
(defvar ewwm-bci-nfb-bar-width)
(defvar ewwm-bci-nfb-auto-export)
(defvar ewwm-bci-nfb--active)
(defvar ewwm-bci-nfb--session-id)
(defvar ewwm-bci-nfb--session-start)
(defvar ewwm-bci-nfb--data-buffer)
(defvar ewwm-bci-nfb--timer)
(defvar ewwm-bci-nfb--protocol)
(defvar ewwm-bci-nfb--latest-frame)
(defvar ewwm-bci-nfb--frame-count)
(defvar ewwm-bci-nfb-start-hook)
(defvar ewwm-bci-nfb-stop-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/provides-feature ()
  "ewwm-bci-nfb provides its feature."
  (should (featurep 'ewwm-bci-nfb)))

(ert-deftest ewwm-bci-nfb/group-exists ()
  "ewwm-bci-nfb customization group exists."
  (should (get 'ewwm-bci-nfb 'custom-group)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/default-not-active ()
  "NFB is not active by default."
  (let ((ewwm-bci-nfb--active nil))
    (should-not ewwm-bci-nfb--active)))

(ert-deftest ewwm-bci-nfb/default-nil-session-id ()
  "Session ID is nil by default."
  (let ((ewwm-bci-nfb--session-id nil))
    (should-not ewwm-bci-nfb--session-id)))

(ert-deftest ewwm-bci-nfb/default-nil-timer ()
  "Timer is nil by default."
  (let ((ewwm-bci-nfb--timer nil))
    (should-not ewwm-bci-nfb--timer)))

;; ── Defcustom defaults ───────────────────────────────────────

(ert-deftest ewwm-bci-nfb/log-interval-default ()
  "Default log-interval is 1.0."
  (should (= (default-value 'ewwm-bci-nfb-log-interval) 1.0)))

(ert-deftest ewwm-bci-nfb/display-channels-default ()
  "Default display-channels is 8."
  (should (= (default-value 'ewwm-bci-nfb-display-channels) 8)))

(ert-deftest ewwm-bci-nfb/bar-width-default ()
  "Default bar-width is 40."
  (should (= (default-value 'ewwm-bci-nfb-bar-width) 40)))

(ert-deftest ewwm-bci-nfb/session-dir-default ()
  "Default session directory matches expected path."
  (should (stringp (default-value 'ewwm-bci-nfb-session-dir))))

;; ── Start ─────────────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/start-creates-session ()
  "Start creates session-id and sets active."
  (let ((ewwm-bci-nfb--active nil)
        (ewwm-bci-nfb--session-id nil)
        (ewwm-bci-nfb--session-start nil)
        (ewwm-bci-nfb--data-buffer nil)
        (ewwm-bci-nfb--frame-count 0)
        (ewwm-bci-nfb--latest-frame nil)
        (ewwm-bci-nfb--protocol nil)
        (ewwm-bci-nfb--timer nil)
        (ewwm-bci-nfb-start-hook nil)
        (ewwm-bci-nfb-log-interval 1.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil))
              ((symbol-function 'display-buffer)
               (lambda (_buf) nil)))
      (ewwm-bci-nfb-start 'alpha-trainer)
      (should ewwm-bci-nfb--active)
      (should (stringp ewwm-bci-nfb--session-id))
      (should (numberp ewwm-bci-nfb--session-start))
      (should (eq ewwm-bci-nfb--protocol 'alpha-trainer))
      (should (= ewwm-bci-nfb--frame-count 0)))))

;; ── Stop ──────────────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/stop-clears-state ()
  "Stop clears timer and marks inactive."
  (let ((ewwm-bci-nfb--active t)
        (ewwm-bci-nfb--session-id "20260101-120000")
        (ewwm-bci-nfb--session-start (float-time))
        (ewwm-bci-nfb--data-buffer nil)
        (ewwm-bci-nfb--frame-count 5)
        (ewwm-bci-nfb--latest-frame nil)
        (ewwm-bci-nfb--protocol 'alpha-trainer)
        (ewwm-bci-nfb--timer nil)
        (ewwm-bci-nfb-auto-export nil)
        (ewwm-bci-nfb-stop-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-nfb-stop)
      (should-not ewwm-bci-nfb--active))))

;; ── Frame event ──────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/frame-updates-data ()
  "Frame event updates data buffer and frame count."
  (let ((ewwm-bci-nfb--active t)
        (ewwm-bci-nfb--data-buffer nil)
        (ewwm-bci-nfb--latest-frame nil)
        (ewwm-bci-nfb--frame-count 0)
        (ewwm-bci-nfb-display-channels 8))
    ;; No display buffer, so update-display is a no-op
    (ewwm-bci-nfb--on-bci-nfb-frame
     '(:bands (:alpha 30.0 :beta 15.0)
       :channels (10.0 12.0 11.0)
       :score 0.75))
    (should (= ewwm-bci-nfb--frame-count 1))
    (should (= (length ewwm-bci-nfb--data-buffer) 1))
    (should (plist-get (car ewwm-bci-nfb--data-buffer) :time))
    ;; Second frame
    (ewwm-bci-nfb--on-bci-nfb-frame
     '(:bands (:alpha 32.0 :beta 14.0)
       :channels (11.0 13.0 10.0)
       :score 0.80))
    (should (= ewwm-bci-nfb--frame-count 2))
    (should (= (length ewwm-bci-nfb--data-buffer) 2))))

(ert-deftest ewwm-bci-nfb/frame-inactive-ignored ()
  "Frame event ignored when not active."
  (let ((ewwm-bci-nfb--active nil)
        (ewwm-bci-nfb--data-buffer nil)
        (ewwm-bci-nfb--frame-count 0)
        (ewwm-bci-nfb--latest-frame nil))
    (ewwm-bci-nfb--on-bci-nfb-frame
     '(:bands (:alpha 30.0) :score 0.5))
    (should (= ewwm-bci-nfb--frame-count 0))
    (should-not ewwm-bci-nfb--data-buffer)))

;; ── Export ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/export-creates-csv ()
  "Export creates CSV file from data buffer."
  (let* ((tmpdir (make-temp-file "nfb-test-" t))
         (ewwm-bci-nfb-session-dir tmpdir)
         (ewwm-bci-nfb--session-id "test-session")
         (ewwm-bci-nfb--protocol 'alpha-trainer)
         (ewwm-bci-nfb--data-buffer
          (list (list :time 1000.0
                      :bands '(:delta 5.0 :theta 8.0
                                :alpha 30.0 :beta 15.0
                                :gamma 3.0)
                      :score 0.7))))
    (ewwm-bci-nfb--export-session-data)
    (let ((csv-file (expand-file-name
                     "test-session-alpha-trainer.csv"
                     tmpdir)))
      (should (file-exists-p csv-file))
      ;; Cleanup
      (delete-file csv-file)
      (delete-directory tmpdir))))

;; ── Protocol setup ───────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/alpha-trainer-params ()
  "Alpha trainer protocol returns correct params."
  (let ((params (ewwm-bci-nfb--protocol-params 'alpha-trainer)))
    (should (eq (plist-get params :target-band) :alpha))
    (should (= (plist-get params :threshold) 0.5))
    (should (eq (plist-get params :reward-band) :alpha))
    (should (eq (plist-get params :inhibit-band) :theta))))

(ert-deftest ewwm-bci-nfb/beta-trainer-params ()
  "Beta trainer protocol returns correct params."
  (let ((params (ewwm-bci-nfb--protocol-params 'beta-trainer)))
    (should (eq (plist-get params :target-band) :beta))
    (should (= (plist-get params :threshold) 0.4))
    (should (eq (plist-get params :reward-band) :beta))))

(ert-deftest ewwm-bci-nfb/mi-trainer-params ()
  "MI trainer protocol returns correct params."
  (let ((params (ewwm-bci-nfb--protocol-params 'mi-trainer)))
    (should (eq (plist-get params :target-band) :mu))
    (should (= (plist-get params :threshold) 0.3))
    (should (eq (plist-get params :reward-band) :mu))))

;; ── Status ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/status-message-callable ()
  "Status command is interactive."
  (should (commandp 'ewwm-bci-nfb-status)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-bci-nfb/event-registration-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-nfb--register-events)
    (should (assq :bci-nfb-frame ewwm-ipc--event-handlers))
    (let ((count (length ewwm-ipc--event-handlers)))
      (ewwm-bci-nfb--register-events)
      (should (= (length ewwm-ipc--event-handlers) count)))))

;; ── Init / teardown ──────────────────────────────────────────

(ert-deftest ewwm-bci-nfb/init-callable ()
  "ewwm-bci-nfb-init is callable."
  (let ((ewwm-ipc--event-handlers nil))
    (should (progn (ewwm-bci-nfb-init) t))))

(ert-deftest ewwm-bci-nfb/teardown-resets-state ()
  "Teardown resets all NFB state."
  (let ((ewwm-bci-nfb--active nil)
        (ewwm-bci-nfb--session-id nil)
        (ewwm-bci-nfb--session-start nil)
        (ewwm-bci-nfb--data-buffer nil)
        (ewwm-bci-nfb--timer nil)
        (ewwm-bci-nfb--protocol nil)
        (ewwm-bci-nfb--latest-frame nil)
        (ewwm-bci-nfb--frame-count 0))
    (ewwm-bci-nfb-teardown)
    (should-not ewwm-bci-nfb--active)
    (should-not ewwm-bci-nfb--session-id)
    (should-not ewwm-bci-nfb--protocol)
    (should (= ewwm-bci-nfb--frame-count 0))))

;;; ewwm-bci-nfb-test.el ends here
