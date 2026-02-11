;;; ewwm-bci-mi-test.el --- Tests for motor imagery module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-mi)

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-mi-enabled)
(defvar ewwm-bci-mi-left-action)
(defvar ewwm-bci-mi-right-action)
(defvar ewwm-bci-mi-foot-action)
(defvar ewwm-bci-mi-min-confidence)
(defvar ewwm-bci-mi-cooldown-ms)
(defvar ewwm-bci-mi-feedback)
(defvar ewwm-bci-mi--calibrated)
(defvar ewwm-bci-mi--last-result)
(defvar ewwm-bci-mi--classifications)
(defvar ewwm-bci-mi--last-dispatch-time)
(defvar ewwm-bci-mi--calibration-progress)
(defvar ewwm-bci-mi-classify-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-bci-mi/provides-feature ()
  "ewwm-bci-mi provides its feature."
  (should (featurep 'ewwm-bci-mi)))

(ert-deftest ewwm-bci-mi/group-exists ()
  "ewwm-bci-mi customization group exists."
  (should (get 'ewwm-bci-mi 'custom-group)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-mi/default-not-calibrated ()
  "MI is not calibrated by default."
  (let ((ewwm-bci-mi--calibrated nil))
    (should-not ewwm-bci-mi--calibrated)))

(ert-deftest ewwm-bci-mi/default-nil-last-result ()
  "Last result is nil by default."
  (let ((ewwm-bci-mi--last-result nil))
    (should-not ewwm-bci-mi--last-result)))

(ert-deftest ewwm-bci-mi/default-zero-classifications ()
  "Classifications counter starts at zero."
  (should (= (default-value 'ewwm-bci-mi--classifications) 0)))

;; ── Defcustom defaults ───────────────────────────────────────

(ert-deftest ewwm-bci-mi/min-confidence-default ()
  "Default min-confidence is 0.6."
  (should (= (default-value 'ewwm-bci-mi-min-confidence) 0.6)))

(ert-deftest ewwm-bci-mi/cooldown-ms-default ()
  "Default cooldown-ms is 1500."
  (should (= (default-value 'ewwm-bci-mi-cooldown-ms) 1500)))

(ert-deftest ewwm-bci-mi/feedback-default ()
  "Default feedback is t."
  (should (eq (default-value 'ewwm-bci-mi-feedback) t)))

(ert-deftest ewwm-bci-mi/enabled-default ()
  "Default enabled is nil."
  (should-not (default-value 'ewwm-bci-mi-enabled)))

;; ── MI event dispatch ─────────────────────────────────────────

(ert-deftest ewwm-bci-mi/left-dispatches-previous-buffer ()
  "Left MI classification dispatches configured action."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (dispatched nil))
    (let ((ewwm-bci-mi-left-action
           (lambda () (setq dispatched 'left))))
      (ewwm-bci-mi--on-bci-mi
       '(:class left :confidence 0.8 :band-power 42.0))
      (should (eq dispatched 'left))
      (should (= ewwm-bci-mi--classifications 1)))))

(ert-deftest ewwm-bci-mi/right-dispatches-next-buffer ()
  "Right MI classification dispatches configured action."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (dispatched nil))
    (let ((ewwm-bci-mi-right-action
           (lambda () (setq dispatched 'right))))
      (ewwm-bci-mi--on-bci-mi
       '(:class right :confidence 0.9 :band-power 50.0))
      (should (eq dispatched 'right)))))

(ert-deftest ewwm-bci-mi/foot-dispatches-workspace-cycle ()
  "Foot MI classification dispatches configured action."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (dispatched nil))
    (let ((ewwm-bci-mi-foot-action
           (lambda () (setq dispatched 'foot))))
      (ewwm-bci-mi--on-bci-mi
       '(:class foot :confidence 0.7 :band-power 38.0))
      (should (eq dispatched 'foot)))))

(ert-deftest ewwm-bci-mi/below-threshold-rejects ()
  "Confidence below threshold rejects classification."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.6)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (dispatched nil))
    (let ((ewwm-bci-mi-left-action
           (lambda () (setq dispatched t))))
      (ewwm-bci-mi--on-bci-mi
       '(:class left :confidence 0.3 :band-power 20.0))
      (should-not dispatched)
      (should (= ewwm-bci-mi--classifications 0))
      ;; last-result still updated
      (should (eq (plist-get ewwm-bci-mi--last-result :class)
                  'left)))))

(ert-deftest ewwm-bci-mi/cooldown-prevents-rapid-dispatch ()
  "Cooldown prevents rapid consecutive dispatches."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 10000)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time (float-time))
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (dispatched nil))
    (let ((ewwm-bci-mi-left-action
           (lambda () (setq dispatched t))))
      (ewwm-bci-mi--on-bci-mi
       '(:class left :confidence 0.9 :band-power 50.0))
      (should-not dispatched)
      (should (= ewwm-bci-mi--classifications 0)))))

;; ── Calibration ──────────────────────────────────────────────

(ert-deftest ewwm-bci-mi/calibrate-sends-ipc ()
  "Calibrate sends IPC command."
  (let ((sent nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (setq sent msg))))
      (let ((ewwm-bci-mi--calibrated nil)
            (ewwm-bci-mi--calibration-progress nil))
        (ewwm-bci-mi-calibrate)
        (should (eq (plist-get sent :type) :bci-mi-calibrate))
        (should (= ewwm-bci-mi--calibration-progress 0))))))

(ert-deftest ewwm-bci-mi/calibration-progress-tracking ()
  "Calibration progress tracked from event."
  (let ((ewwm-bci-mi--calibrated nil)
        (ewwm-bci-mi--calibration-progress nil))
    (ewwm-bci-mi--on-bci-mi-calibration
     '(:status progress :progress 50))
    (should (= ewwm-bci-mi--calibration-progress 50))
    (ewwm-bci-mi--on-bci-mi-calibration
     '(:status complete :progress 100))
    (should ewwm-bci-mi--calibrated)
    (should-not ewwm-bci-mi--calibration-progress)))

;; ── Hook ─────────────────────────────────────────────────────

(ert-deftest ewwm-bci-mi/classify-hook-fires ()
  "mi-classify-hook fires on successful classification."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback nil)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (hook-result nil))
    (let ((ewwm-bci-mi-classify-hook nil)
          (ewwm-bci-mi-left-action #'ignore))
      (add-hook 'ewwm-bci-mi-classify-hook
                (lambda (cls conf)
                  (setq hook-result (list cls conf))))
      (ewwm-bci-mi--on-bci-mi
       '(:class left :confidence 0.8 :band-power 42.0))
      (should (eq (car hook-result) 'left))
      (should (= (cadr hook-result) 0.8)))))

;; ── Toggle ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-mi/toggle-flips-enabled ()
  "Toggle flips the enabled flag."
  (let ((ewwm-bci-mi-enabled nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-mi-toggle)
      (should ewwm-bci-mi-enabled)
      (ewwm-bci-mi-toggle)
      (should-not ewwm-bci-mi-enabled))))

;; ── Status ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-mi/status-message-format ()
  "Status message includes expected fields."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi--calibrated t)
        (ewwm-bci-mi--classifications 5)
        (ewwm-bci-mi--last-result '(:class left :confidence 0.8))
        (msg-text nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg-text
                       (apply #'format fmt args)))))
      (ewwm-bci-mi-status)
      (should (string-match-p "enabled=yes" msg-text))
      (should (string-match-p "cal=yes" msg-text)))))

;; ── Disabled MI skips processing ─────────────────────────────

(ert-deftest ewwm-bci-mi/disabled-skips-processing ()
  "Disabled MI skips event processing entirely."
  (let ((ewwm-bci-mi-enabled nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi-classify-hook nil))
    (let ((ewwm-bci-mi-left-action #'ignore))
      (ewwm-bci-mi--on-bci-mi
       '(:class left :confidence 0.9 :band-power 50.0))
      (should-not ewwm-bci-mi--last-result)
      (should (= ewwm-bci-mi--classifications 0)))))

;; ── Feedback message ─────────────────────────────────────────

(ert-deftest ewwm-bci-mi/feedback-message-on-dispatch ()
  "Feedback message shown when ewwm-bci-mi-feedback is t."
  (let ((ewwm-bci-mi-enabled t)
        (ewwm-bci-mi-min-confidence 0.5)
        (ewwm-bci-mi-cooldown-ms 0)
        (ewwm-bci-mi-feedback t)
        (ewwm-bci-mi--last-dispatch-time nil)
        (ewwm-bci-mi--last-result nil)
        (ewwm-bci-mi--classifications 0)
        (ewwm-bci-mi-classify-hook nil)
        (msg-text nil))
    (let ((ewwm-bci-mi-left-action #'ignore))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg-text
                         (apply #'format fmt args)))))
        (ewwm-bci-mi--on-bci-mi
         '(:class left :confidence 0.75 :band-power 42.0))
        (should (string-match-p "left" msg-text))
        (should (string-match-p "75%" msg-text))))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-bci-mi/event-registration-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-mi--register-events)
    (should (assq :bci-mi ewwm-ipc--event-handlers))
    (should (assq :bci-mi-calibration
                  ewwm-ipc--event-handlers))
    (let ((count (length ewwm-ipc--event-handlers)))
      (ewwm-bci-mi--register-events)
      (should (= (length ewwm-ipc--event-handlers) count)))))

;; ── Init / teardown ──────────────────────────────────────────

(ert-deftest ewwm-bci-mi/init-callable ()
  "ewwm-bci-mi-init is callable."
  (let ((ewwm-ipc--event-handlers nil))
    (should (progn (ewwm-bci-mi-init) t))))

(ert-deftest ewwm-bci-mi/teardown-resets-state ()
  "Teardown resets all MI state."
  (let ((ewwm-bci-mi--calibrated t)
        (ewwm-bci-mi--last-result '(:class left))
        (ewwm-bci-mi--classifications 10)
        (ewwm-bci-mi--last-dispatch-time 12345.0)
        (ewwm-bci-mi--calibration-progress 50))
    (ewwm-bci-mi-teardown)
    (should-not ewwm-bci-mi--calibrated)
    (should-not ewwm-bci-mi--last-result)
    (should (= ewwm-bci-mi--classifications 0))
    (should-not ewwm-bci-mi--last-dispatch-time)
    (should-not ewwm-bci-mi--calibration-progress)))

;;; ewwm-bci-mi-test.el ends here
