;;; ewwm-bci-core-test.el --- Tests for BCI core module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-core)

;; Forward-declare IPC functions
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-board-id)
(defvar ewwm-bci-serial-port)
(defvar ewwm-bci-sample-rate)
(defvar ewwm-bci-notch-frequency)
(defvar ewwm-bci-artifact-rejection)
(defvar ewwm-bci-data-retention-days)
(defvar ewwm-bci-daemon-path)
(defvar ewwm-bci-daemon-args)
(defvar ewwm-bci-auto-reconnect)
(defvar ewwm-bci-reconnect-delay)
(defvar ewwm-bci--connection-state)
(defvar ewwm-bci--streaming)
(defvar ewwm-bci--channel-quality)
(defvar ewwm-bci--frames-received)
(defvar ewwm-bci--daemon-process)
(defvar ewwm-bci--last-error)
(defvar ewwm-bci--session-start)
(defvar ewwm-bci--reconnect-timer)
(defvar ewwm-bci-connected-hook)
(defvar ewwm-bci-disconnected-hook)
(defvar ewwm-bci-quality-change-hook)
(defvar ewwm-bci-error-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Feature / group ────────────────────────────────────────────

(ert-deftest ewwm-bci-core/provides-feature ()
  "ewwm-bci-core provides its feature."
  (should (featurep 'ewwm-bci-core)))

(ert-deftest ewwm-bci-core/group-exists ()
  "ewwm-bci customization group exists."
  (should (get 'ewwm-bci 'custom-group)))

;; ── Defcustom defaults ────────────────────────────────────────

(ert-deftest ewwm-bci-core/board-id-default ()
  "Default board-id is 0."
  (should (= (default-value 'ewwm-bci-board-id) 0)))

(ert-deftest ewwm-bci-core/serial-port-default ()
  "Default serial-port is /dev/openbci."
  (should (equal (default-value 'ewwm-bci-serial-port)
                 "/dev/openbci")))

(ert-deftest ewwm-bci-core/sample-rate-default ()
  "Default sample-rate is 250."
  (should (= (default-value 'ewwm-bci-sample-rate) 250)))

(ert-deftest ewwm-bci-core/notch-frequency-default ()
  "Default notch-frequency is 60."
  (should (= (default-value 'ewwm-bci-notch-frequency) 60)))

(ert-deftest ewwm-bci-core/artifact-rejection-default ()
  "Default artifact-rejection is t."
  (should (eq (default-value 'ewwm-bci-artifact-rejection) t)))

(ert-deftest ewwm-bci-core/data-retention-default ()
  "Default data-retention-days is 90."
  (should (= (default-value 'ewwm-bci-data-retention-days) 90)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-core/default-connection-state ()
  "Default connection state is disconnected."
  (let ((ewwm-bci--connection-state 'disconnected))
    (should (eq ewwm-bci--connection-state 'disconnected))))

(ert-deftest ewwm-bci-core/default-not-streaming ()
  "Default streaming is nil."
  (let ((ewwm-bci--streaming nil))
    (should-not ewwm-bci--streaming)))

(ert-deftest ewwm-bci-core/default-zero-frames ()
  "Default frames-received is 0."
  (let ((ewwm-bci--frames-received 0))
    (should (= ewwm-bci--frames-received 0))))

(ert-deftest ewwm-bci-core/daemon-process-nil ()
  "Daemon process is nil by default."
  (let ((ewwm-bci--daemon-process nil))
    (should-not ewwm-bci--daemon-process)))

;; ── Connected event ───────────────────────────────────────────

(ert-deftest ewwm-bci-core/connected-event-updates-state ()
  "Connected event sets state to connected and streaming."
  (let ((ewwm-bci--connection-state 'connecting)
        (ewwm-bci--streaming nil)
        (ewwm-bci--session-start nil)
        (ewwm-bci--last-error "old")
        (ewwm-bci--reconnect-timer nil)
        (ewwm-bci-board-id 0)
        (ewwm-bci-sample-rate 250)
        (ewwm-bci-connected-hook nil))
    (ewwm-bci--on-bci-connected
     '(:board-id 1 :sample-rate 500))
    (should (eq ewwm-bci--connection-state 'connected))
    (should ewwm-bci--streaming)
    (should ewwm-bci--session-start)
    (should-not ewwm-bci--last-error)))

(ert-deftest ewwm-bci-core/connected-hook-fires ()
  "Connected hook fires on connection event."
  (let ((ewwm-bci--connection-state 'disconnected)
        (ewwm-bci--streaming nil)
        (ewwm-bci--session-start nil)
        (ewwm-bci--last-error nil)
        (ewwm-bci--reconnect-timer nil)
        (ewwm-bci-board-id 0)
        (ewwm-bci-sample-rate 250)
        (hook-fired nil))
    (let ((ewwm-bci-connected-hook
           (list (lambda () (setq hook-fired t)))))
      (ewwm-bci--on-bci-connected '(:board-id 1))
      (should hook-fired))))

;; ── Disconnected event ────────────────────────────────────────

(ert-deftest ewwm-bci-core/disconnected-event-updates-state ()
  "Disconnected event sets state to disconnected."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (ewwm-bci-auto-reconnect nil)
        (ewwm-bci--reconnect-timer nil)
        (ewwm-bci-disconnected-hook nil))
    (ewwm-bci--on-bci-disconnected '(:reason "test"))
    (should (eq ewwm-bci--connection-state 'disconnected))
    (should-not ewwm-bci--streaming)))

(ert-deftest ewwm-bci-core/disconnected-hook-fires ()
  "Disconnected hook fires with reason."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (ewwm-bci-auto-reconnect nil)
        (ewwm-bci--reconnect-timer nil)
        (hook-reason nil))
    (let ((ewwm-bci-disconnected-hook
           (list (lambda (reason) (setq hook-reason reason)))))
      (ewwm-bci--on-bci-disconnected '(:reason "cable"))
      (should (equal hook-reason "cable")))))

;; ── Quality event ─────────────────────────────────────────────

(ert-deftest ewwm-bci-core/quality-event-updates-channels ()
  "Quality event updates channel quality."
  (let ((ewwm-bci--channel-quality nil)
        (ewwm-bci-quality-change-hook nil))
    (ewwm-bci--on-bci-quality
     '(:channels (1 good 2 fair 3 poor)))
    (should (equal ewwm-bci--channel-quality
                   '(1 good 2 fair 3 poor)))))

(ert-deftest ewwm-bci-core/quality-hook-fires ()
  "Quality change hook fires with channel data."
  (let ((ewwm-bci--channel-quality nil)
        (hook-data nil))
    (let ((ewwm-bci-quality-change-hook
           (list (lambda (q) (setq hook-data q)))))
      (ewwm-bci--on-bci-quality '(:channels (1 good)))
      (should (equal hook-data '(1 good))))))

;; ── Error event ───────────────────────────────────────────────

(ert-deftest ewwm-bci-core/error-event-records-error ()
  "Error event records message and sets state."
  (let ((ewwm-bci--last-error nil)
        (ewwm-bci--connection-state 'connected)
        (ewwm-bci-error-hook nil))
    (ewwm-bci--on-bci-error '(:message "board timeout"))
    (should (equal ewwm-bci--last-error "board timeout"))
    (should (eq ewwm-bci--connection-state 'error))))

(ert-deftest ewwm-bci-core/error-hook-fires ()
  "Error hook fires with error message."
  (let ((ewwm-bci--last-error nil)
        (ewwm-bci--connection-state 'connected)
        (hook-msg nil))
    (let ((ewwm-bci-error-hook
           (list (lambda (msg) (setq hook-msg msg)))))
      (ewwm-bci--on-bci-error '(:message "overflow"))
      (should (equal hook-msg "overflow")))))

;; ── Frame counter ─────────────────────────────────────────────

(ert-deftest ewwm-bci-core/frame-event-increments ()
  "Frame event increments frames-received."
  (let ((ewwm-bci--frames-received 10))
    (ewwm-bci--on-bci-frame nil)
    (should (= ewwm-bci--frames-received 11))))

;; ── Start sends IPC ───────────────────────────────────────────

(ert-deftest ewwm-bci-core/start-sends-ipc ()
  "Start sends bci-start IPC when connected."
  (let ((ewwm-bci--daemon-process nil)
        (ewwm-bci--connection-state 'disconnected)
        (ewwm-bci-board-id 1)
        (ewwm-bci-serial-port "/dev/ttyUSB0")
        (ewwm-bci-sample-rate 250)
        (ewwm-bci-notch-frequency 60)
        (ewwm-bci-artifact-rejection t)
        (ewwm-bci-daemon-path "nonexistent-daemon")
        (ewwm-bci-daemon-args nil)
        (sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs)))
              ((symbol-function 'start-process)
               (lambda (&rest _args) nil)))
      (ewwm-bci-start)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-start)))))

;; ── Stop sends IPC ────────────────────────────────────────────

(ert-deftest ewwm-bci-core/stop-sends-ipc ()
  "Stop sends bci-stop IPC when connected."
  (let ((ewwm-bci--daemon-process nil)
        (ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs))))
      (ewwm-bci-stop)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-stop)))))

;; ── Restart calls stop then start ─────────────────────────────

(ert-deftest ewwm-bci-core/restart-calls-stop ()
  "Restart sends stop IPC."
  (let ((ewwm-bci--daemon-process nil)
        (ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs)))
              ((symbol-function 'run-with-timer)
               (lambda (_secs _repeat _fn) nil)))
      (ewwm-bci-restart)
      ;; Stop should send bci-stop
      (should (cl-some
               (lambda (m) (eq (plist-get m :type) :bci-stop))
               sent-msgs)))))

;; ── Status message ────────────────────────────────────────────

(ert-deftest ewwm-bci-core/status-message-format ()
  "Status displays formatted state."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (ewwm-bci--frames-received 42)
        (ewwm-bci--session-start nil)
        (ewwm-bci--last-error nil))
    (should (stringp (with-output-to-string
                       (ewwm-bci-status))))))

;; ── Signal quality display ────────────────────────────────────

(ert-deftest ewwm-bci-core/signal-quality-no-data ()
  "Signal quality with nil channels says no data."
  (let ((ewwm-bci--channel-quality nil))
    (ewwm-bci-signal-quality)
    ;; Should not error
    (should t)))

(ert-deftest ewwm-bci-core/signal-quality-with-data ()
  "Signal quality with data formats channels."
  (let ((ewwm-bci--channel-quality '(1 good 2 fair)))
    (ewwm-bci-signal-quality)
    (should t)))

;; ── Hardware check ────────────────────────────────────────────

(ert-deftest ewwm-bci-core/hardware-check-sends-ipc ()
  "Hardware check sends IPC when connected."
  (let ((sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs))))
      (ewwm-bci-hardware-check)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-hardware-check)))))

;; ── Event registration ────────────────────────────────────────

(ert-deftest ewwm-bci-core/register-events-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci--register-events)
    (let ((count1 (length ewwm-ipc--event-handlers)))
      (ewwm-bci--register-events)
      (should (= (length ewwm-ipc--event-handlers) count1)))))

;; ── Init ──────────────────────────────────────────────────────

(ert-deftest ewwm-bci-core/init-registers-events ()
  "Init registers IPC events."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-init)
    (should (assq :bci-connected ewwm-ipc--event-handlers))
    (should (assq :bci-disconnected ewwm-ipc--event-handlers))
    (should (assq :bci-quality ewwm-ipc--event-handlers))
    (should (assq :bci-error ewwm-ipc--event-handlers))
    (should (assq :bci-frame ewwm-ipc--event-handlers))))

;; ── Teardown ──────────────────────────────────────────────────

(ert-deftest ewwm-bci-core/teardown-clears-state ()
  "Teardown resets all state to defaults."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t)
        (ewwm-bci--channel-quality '(1 good))
        (ewwm-bci--frames-received 100)
        (ewwm-bci--last-error "fail")
        (ewwm-bci--session-start 12345.0)
        (ewwm-bci--daemon-process nil)
        (ewwm-bci--reconnect-timer nil))
    (ewwm-bci-teardown)
    (should (eq ewwm-bci--connection-state 'disconnected))
    (should-not ewwm-bci--streaming)
    (should-not ewwm-bci--channel-quality)
    (should (= ewwm-bci--frames-received 0))
    (should-not ewwm-bci--last-error)
    (should-not ewwm-bci--session-start)))

;; ── Connection state transitions ──────────────────────────────

(ert-deftest ewwm-bci-core/error-state-on-error-event ()
  "Error event transitions to error state."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--last-error nil)
        (ewwm-bci-error-hook nil))
    (ewwm-bci--on-bci-error '(:message "fail"))
    (should (eq ewwm-bci--connection-state 'error))))

;; ── Channel quality list format ───────────────────────────────

(ert-deftest ewwm-bci-core/channel-quality-plist ()
  "Channel quality is stored as a plist."
  (let ((ewwm-bci--channel-quality nil)
        (ewwm-bci-quality-change-hook nil))
    (ewwm-bci--on-bci-quality
     '(:channels (1 good 2 poor 3 disconnected)))
    (should (eq (plist-get ewwm-bci--channel-quality 1) 'good))
    (should (eq (plist-get ewwm-bci--channel-quality 2) 'poor))
    (should (eq (plist-get ewwm-bci--channel-quality 3)
                'disconnected))))

;; ── Mode-line ─────────────────────────────────────────────────

(ert-deftest ewwm-bci-core/mode-line-connected ()
  "Mode-line shows ON when connected and streaming."
  (let ((ewwm-bci--connection-state 'connected)
        (ewwm-bci--streaming t))
    (should (equal (ewwm-bci-mode-line-string) " [BCI:ON]"))))

(ert-deftest ewwm-bci-core/mode-line-disconnected ()
  "Mode-line returns nil when disconnected."
  (let ((ewwm-bci--connection-state 'disconnected)
        (ewwm-bci--streaming nil))
    (should-not (ewwm-bci-mode-line-string))))

;;; ewwm-bci-core-test.el ends here
