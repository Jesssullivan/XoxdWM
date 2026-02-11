;;; ewwm-bci-p300-test.el --- Tests for BCI P300 module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-p300)

;; Forward-declare IPC functions
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-p300-enabled)
(defvar ewwm-bci-p300-repetitions)
(defvar ewwm-bci-p300-min-confidence)
(defvar ewwm-bci-p300-soa-ms)
(defvar ewwm-bci-p300-flash-duration-ms)
(defvar ewwm-bci-p300-timeout-ms)
(defvar ewwm-bci-p300--active)
(defvar ewwm-bci-p300--last-result)
(defvar ewwm-bci-p300--callback)
(defvar ewwm-bci-p300--targets)
(defvar ewwm-bci-p300--timeout-timer)
(defvar ewwm-bci-p300--trial-count)
(defvar ewwm-bci-p300-detect-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Feature / group ────────────────────────────────────────────

(ert-deftest ewwm-bci-p300/provides-feature ()
  "ewwm-bci-p300 provides its feature."
  (should (featurep 'ewwm-bci-p300)))

(ert-deftest ewwm-bci-p300/group-exists ()
  "ewwm-bci-p300 customization group exists."
  (should (get 'ewwm-bci-p300 'custom-group)))

;; ── Defcustom defaults ────────────────────────────────────────

(ert-deftest ewwm-bci-p300/enabled-default ()
  "Default enabled is nil."
  (should-not (default-value 'ewwm-bci-p300-enabled)))

(ert-deftest ewwm-bci-p300/repetitions-default ()
  "Default repetitions is 5."
  (should (= (default-value 'ewwm-bci-p300-repetitions) 5)))

(ert-deftest ewwm-bci-p300/min-confidence-default ()
  "Default min-confidence is 0.7."
  (should (= (default-value 'ewwm-bci-p300-min-confidence) 0.7)))

(ert-deftest ewwm-bci-p300/soa-ms-default ()
  "Default soa-ms is 200."
  (should (= (default-value 'ewwm-bci-p300-soa-ms) 200)))

(ert-deftest ewwm-bci-p300/flash-duration-default ()
  "Default flash-duration-ms is 100."
  (should (= (default-value 'ewwm-bci-p300-flash-duration-ms)
             100)))

(ert-deftest ewwm-bci-p300/timeout-ms-default ()
  "Default timeout-ms is 30000."
  (should (= (default-value 'ewwm-bci-p300-timeout-ms) 30000)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-p300/default-not-active ()
  "Default active is nil."
  (let ((ewwm-bci-p300--active nil))
    (should-not ewwm-bci-p300--active)))

(ert-deftest ewwm-bci-p300/default-nil-last-result ()
  "Default last-result is nil."
  (let ((ewwm-bci-p300--last-result nil))
    (should-not ewwm-bci-p300--last-result)))

(ert-deftest ewwm-bci-p300/default-nil-callback ()
  "Default callback is nil."
  (let ((ewwm-bci-p300--callback nil))
    (should-not ewwm-bci-p300--callback)))

;; ── P300 event invokes callback ───────────────────────────────

(ert-deftest ewwm-bci-p300/event-invokes-callback ()
  "P300 detected event invokes callback with target."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300-detect-hook nil)
        (cb-args nil))
    (let ((ewwm-bci-p300--callback
           (lambda (target conf) (setq cb-args (list target conf)))))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "ws2" :confidence 0.85
         :latency-ms 320 :status detected))
      (should cb-args)
      (should (equal (nth 0 cb-args) "ws2"))
      (should (= (nth 1 cb-args) 0.85)))))

;; ── Confidence below threshold rejects ────────────────────────

(ert-deftest ewwm-bci-p300/low-confidence-rejects ()
  "P300 event below threshold does not invoke callback."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300-detect-hook nil)
        (cb-called nil))
    (let ((ewwm-bci-p300--callback
           (lambda (_t _c) (setq cb-called t))))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "ws1" :confidence 0.3
         :latency-ms 280 :status rejected))
      (should-not cb-called))))

;; ── Confirm sets up callback and sends IPC ────────────────────

(ert-deftest ewwm-bci-p300/confirm-sets-callback ()
  "Confirm sets callback, targets, and active flag."
  (let ((ewwm-bci-p300--active nil)
        (ewwm-bci-p300--callback nil)
        (ewwm-bci-p300--targets nil)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-timeout-ms 30000)
        (ewwm-bci-p300-repetitions 5)
        (ewwm-bci-p300-soa-ms 200)
        (ewwm-bci-p300-flash-duration-ms 100)
        (sent-msgs nil)
        (my-cb (lambda (_t _c) nil)))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs)))
              ((symbol-function 'run-with-timer)
               (lambda (_secs _repeat _fn) 'fake-timer)))
      (ewwm-bci-p300-confirm "Pick one"
                              '("a" "b" "c") my-cb)
      (should ewwm-bci-p300--active)
      (should (eq ewwm-bci-p300--callback my-cb))
      (should (equal ewwm-bci-p300--targets '("a" "b" "c")))
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-p300-start)))))

;; ── Stop clears state ─────────────────────────────────────────

(ert-deftest ewwm-bci-p300/stop-clears-state ()
  "Stop clears active and callback."
  (let ((ewwm-bci-p300--active t)
        (ewwm-bci-p300--timeout-timer nil)
        (cb-args nil))
    (let ((ewwm-bci-p300--callback
           (lambda (target conf)
             (setq cb-args (list target conf)))))
      (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
                 (lambda () nil)))
        (ewwm-bci-p300-stop)
        (should-not ewwm-bci-p300--active)
        (should-not ewwm-bci-p300--callback)
        ;; Callback should have been called with nil
        (should cb-args)
        (should-not (nth 0 cb-args))))))

;; ── Timeout fires ─────────────────────────────────────────────

(ert-deftest ewwm-bci-p300/timeout-invokes-callback-nil ()
  "Timeout invokes callback with nil."
  (let ((ewwm-bci-p300--active t)
        (ewwm-bci-p300--timeout-timer nil)
        (cb-args nil))
    (let ((ewwm-bci-p300--callback
           (lambda (target conf)
             (setq cb-args (list target conf)))))
      (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
                 (lambda () nil)))
        (ewwm-bci-p300--on-timeout)
        (should cb-args)
        (should-not (nth 0 cb-args))
        (should-not (nth 1 cb-args))
        (should-not ewwm-bci-p300--active)))))

;; ── Detect hook fires ─────────────────────────────────────────

(ert-deftest ewwm-bci-p300/detect-hook-fires ()
  "Detect hook fires on successful detection."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300--callback nil)
        (hook-args nil))
    (let ((ewwm-bci-p300-detect-hook
           (list (lambda (target conf)
                   (setq hook-args (list target conf))))))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "ok" :confidence 0.9
         :latency-ms 310 :status detected))
      (should hook-args)
      (should (equal (nth 0 hook-args) "ok"))
      (should (= (nth 1 hook-args) 0.9)))))

;; ── Status message ────────────────────────────────────────────

(ert-deftest ewwm-bci-p300/status-message-format ()
  "Status displays formatted state."
  (let ((ewwm-bci-p300--active nil)
        (ewwm-bci-p300--trial-count 3)
        (ewwm-bci-p300--last-result
         '(:target-id "ws1" :confidence 0.88)))
    (should (stringp (with-output-to-string
                       (ewwm-bci-p300-status))))))

;; ── Disabled skips processing ─────────────────────────────────

(ert-deftest ewwm-bci-p300/disabled-skips ()
  "Disabled P300 does not process events."
  (let ((ewwm-bci-p300-enabled nil)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300--callback nil)
        (ewwm-bci-p300-detect-hook nil))
    (ewwm-bci-p300--on-bci-p300
     '(:target-id "x" :confidence 0.99
       :latency-ms 300 :status detected))
    (should (= ewwm-bci-p300--trial-count 0))))

;; ── Callback cleared after invocation ─────────────────────────

(ert-deftest ewwm-bci-p300/callback-cleared-after-invoke ()
  "Callback is cleared after successful invocation."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300-detect-hook nil))
    (let ((ewwm-bci-p300--callback
           (lambda (_t _c) nil)))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "y" :confidence 0.8
         :latency-ms 300 :status detected))
      (should-not ewwm-bci-p300--callback))))

;; ── Targets tracking ──────────────────────────────────────────

(ert-deftest ewwm-bci-p300/targets-stored ()
  "Confirm stores targets list."
  (let ((ewwm-bci-p300--active nil)
        (ewwm-bci-p300--callback nil)
        (ewwm-bci-p300--targets nil)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-timeout-ms 30000)
        (ewwm-bci-p300-repetitions 5)
        (ewwm-bci-p300-soa-ms 200)
        (ewwm-bci-p300-flash-duration-ms 100))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil))
              ((symbol-function 'run-with-timer)
               (lambda (_s _r _fn) 'fake)))
      (ewwm-bci-p300-confirm "test"
                              '("t1" "t2" "t3" "t4")
                              (lambda (_t _c) nil))
      (should (equal ewwm-bci-p300--targets
                     '("t1" "t2" "t3" "t4"))))))

;; ── Event registration idempotent ─────────────────────────────

(ert-deftest ewwm-bci-p300/register-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-p300--register-events)
    (let ((count1 (length ewwm-ipc--event-handlers)))
      (ewwm-bci-p300--register-events)
      (should (= (length ewwm-ipc--event-handlers) count1)))))

;; ── Trial count increments ────────────────────────────────────

(ert-deftest ewwm-bci-p300/trial-count-increments ()
  "Trial count increments on successful detection."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--trial-count 5)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.7)
        (ewwm-bci-p300--callback nil)
        (ewwm-bci-p300-detect-hook nil))
    (ewwm-bci-p300--on-bci-p300
     '(:target-id "z" :confidence 0.85
       :latency-ms 300 :status detected))
    (should (= ewwm-bci-p300--trial-count 6))))

;; ── Teardown ──────────────────────────────────────────────────

(ert-deftest ewwm-bci-p300/teardown-clears-state ()
  "Teardown resets all state."
  (let ((ewwm-bci-p300--active nil)
        (ewwm-bci-p300--last-result '(:target-id "x"))
        (ewwm-bci-p300--callback nil)
        (ewwm-bci-p300--targets '("a" "b"))
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300--trial-count 10))
    (ewwm-bci-p300-teardown)
    (should-not ewwm-bci-p300--active)
    (should-not ewwm-bci-p300--last-result)
    (should-not ewwm-bci-p300--callback)
    (should-not ewwm-bci-p300--targets)
    (should (= ewwm-bci-p300--trial-count 0))))

;;; ewwm-bci-p300-test.el ends here
