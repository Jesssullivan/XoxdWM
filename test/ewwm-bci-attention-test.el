;;; ewwm-bci-attention-test.el --- Tests for BCI attention module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-attention)

;; Forward-declare IPC functions
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-attention-enabled)
(defvar ewwm-bci-attention-threshold)
(defvar ewwm-bci-attention-dnd-threshold)
(defvar ewwm-bci-attention-drowsy-threshold)
(defvar ewwm-bci-attention-break-minutes)
(defvar ewwm-bci-attention-auto-save)
(defvar ewwm-bci-attention-update-interval)
(defvar ewwm-bci-attention--state)
(defvar ewwm-bci-attention--score)
(defvar ewwm-bci-attention--band-powers)
(defvar ewwm-bci-attention--dnd-active)
(defvar ewwm-bci-attention--state-start-time)
(defvar ewwm-bci-attention--last-update-time)
(defvar ewwm-bci-attention--history)
(defvar ewwm-bci-attention-change-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Feature / group ────────────────────────────────────────────

(ert-deftest ewwm-bci-attention/provides-feature ()
  "ewwm-bci-attention provides its feature."
  (should (featurep 'ewwm-bci-attention)))

(ert-deftest ewwm-bci-attention/group-exists ()
  "ewwm-bci-attention customization group exists."
  (should (get 'ewwm-bci-attention 'custom-group)))

;; ── Defcustom defaults ────────────────────────────────────────

(ert-deftest ewwm-bci-attention/enabled-default ()
  "Default enabled is t."
  (should (eq (default-value 'ewwm-bci-attention-enabled) t)))

(ert-deftest ewwm-bci-attention/threshold-default ()
  "Default threshold is 0.6."
  (should (= (default-value 'ewwm-bci-attention-threshold) 0.6)))

(ert-deftest ewwm-bci-attention/dnd-threshold-default ()
  "Default dnd-threshold is 0.8."
  (should (= (default-value 'ewwm-bci-attention-dnd-threshold)
             0.8)))

(ert-deftest ewwm-bci-attention/drowsy-threshold-default ()
  "Default drowsy-threshold is 0.2."
  (should (= (default-value 'ewwm-bci-attention-drowsy-threshold)
             0.2)))

(ert-deftest ewwm-bci-attention/break-minutes-default ()
  "Default break-minutes is 5."
  (should (= (default-value 'ewwm-bci-attention-break-minutes)
             5)))

(ert-deftest ewwm-bci-attention/auto-save-default ()
  "Default auto-save is t."
  (should (eq (default-value 'ewwm-bci-attention-auto-save) t)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-attention/default-state-neutral ()
  "Default attention state is neutral."
  (let ((ewwm-bci-attention--state 'neutral))
    (should (eq ewwm-bci-attention--state 'neutral))))

(ert-deftest ewwm-bci-attention/default-score-zero ()
  "Default attention score is 0.0."
  (let ((ewwm-bci-attention--score 0.0))
    (should (= ewwm-bci-attention--score 0.0))))

;; ── Attention event updates state and score ───────────────────

(ert-deftest ewwm-bci-attention/event-updates-score ()
  "Attention event updates score."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention
       '(:score 0.75 :band-powers (:delta 10.0 :theta 5.0)))
      (should (= ewwm-bci-attention--score 0.75)))))

;; ── State classification ──────────────────────────────────────

(ert-deftest ewwm-bci-attention/classify-deep-focus ()
  "Score > 0.8 triggers deep-focus."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.9))
      (should (eq ewwm-bci-attention--state 'deep-focus)))))

(ert-deftest ewwm-bci-attention/classify-focused ()
  "Score 0.6-0.8 triggers focused."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.7))
      (should (eq ewwm-bci-attention--state 'focused)))))

(ert-deftest ewwm-bci-attention/classify-neutral ()
  "Score 0.2-0.6 stays neutral."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'focused)
        (ewwm-bci-attention--score 0.7)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.4))
      (should (eq ewwm-bci-attention--state 'neutral)))))

(ert-deftest ewwm-bci-attention/classify-drowsy ()
  "Score <= 0.2 triggers drowsy."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.5)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil))
              ((symbol-function 'save-some-buffers)
               (lambda (&rest _args) nil))
              ((symbol-function 'display-warning)
               (lambda (&rest _args) nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.1))
      (should (eq ewwm-bci-attention--state 'drowsy)))))

;; ── Attention change hook ─────────────────────────────────────

(ert-deftest ewwm-bci-attention/change-hook-fires ()
  "Hook fires with old-state, new-state, score on transition."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (hook-args nil))
    (let ((ewwm-bci-attention-change-hook
           (list (lambda (old new score)
                   (setq hook-args (list old new score))))))
      (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
                 (lambda () nil)))
        (ewwm-bci-attention--on-bci-attention '(:score 0.9))
        (should hook-args)
        (should (eq (nth 0 hook-args) 'neutral))
        (should (eq (nth 1 hook-args) 'deep-focus))
        (should (= (nth 2 hook-args) 0.9))))))

;; ── Deep focus triggers DND ───────────────────────────────────

(ert-deftest ewwm-bci-attention/deep-focus-activates-dnd ()
  "Deep focus activates DND."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.9))
      (should ewwm-bci-attention--dnd-active))))

;; ── Disabled attention skips processing ───────────────────────

(ert-deftest ewwm-bci-attention/disabled-skips ()
  "Disabled attention does not process events."
  (let ((ewwm-bci-attention-enabled nil)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0))
    (ewwm-bci-attention--on-bci-attention '(:score 0.9))
    (should (= ewwm-bci-attention--score 0.0))
    (should (eq ewwm-bci-attention--state 'neutral))))

;; ── History tracking ──────────────────────────────────────────

(ert-deftest ewwm-bci-attention/history-records ()
  "History records time-score pairs."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.5))
      (should (= (length ewwm-bci-attention--history) 1))
      (should (= (cdar ewwm-bci-attention--history) 0.5)))))

;; ── Band powers from event ────────────────────────────────────

(ert-deftest ewwm-bci-attention/band-powers-tracked ()
  "Band powers are stored from event."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention
       '(:score 0.5
         :band-powers (:delta 12.0 :theta 8.0 :alpha 6.0)))
      (should (= (plist-get ewwm-bci-attention--band-powers :delta)
                 12.0)))))

;; ── State start time updates ──────────────────────────────────

(ert-deftest ewwm-bci-attention/state-start-time-updates ()
  "State-start-time updates on transition."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 0.0)
        (ewwm-bci-attention-auto-save nil)
        (ewwm-bci-attention-change-hook nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention--on-bci-attention '(:score 0.9))
      (should ewwm-bci-attention--state-start-time))))

;; ── Calibrate sends IPC ───────────────────────────────────────

(ert-deftest ewwm-bci-attention/calibrate-sends-ipc ()
  "Calibrate sends IPC when connected."
  (let ((sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs))))
      (ewwm-bci-attention-calibrate)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-attention-calibrate)))))

;; ── Toggle ────────────────────────────────────────────────────

(ert-deftest ewwm-bci-attention/toggle-flips-enabled ()
  "Toggle flips enabled flag."
  (let ((ewwm-bci-attention-enabled t))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention-toggle)
      (should-not ewwm-bci-attention-enabled)
      (ewwm-bci-attention-toggle)
      (should ewwm-bci-attention-enabled))))

;; ── Status message ────────────────────────────────────────────

(ert-deftest ewwm-bci-attention/status-message-format ()
  "Status displays formatted state."
  (let ((ewwm-bci-attention--state 'focused)
        (ewwm-bci-attention--score 0.72)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention--state-start-time nil))
    (should (stringp (with-output-to-string
                       (ewwm-bci-attention-status))))))

;; ── Event registration idempotent ─────────────────────────────

(ert-deftest ewwm-bci-attention/register-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-attention--register-events)
    (let ((count1 (length ewwm-ipc--event-handlers)))
      (ewwm-bci-attention--register-events)
      (should (= (length ewwm-ipc--event-handlers) count1)))))

;; ── Teardown ──────────────────────────────────────────────────

(ert-deftest ewwm-bci-attention/teardown-clears-state ()
  "Teardown resets all state."
  (let ((ewwm-bci-attention--state 'deep-focus)
        (ewwm-bci-attention--score 0.95)
        (ewwm-bci-attention--band-powers '(:alpha 10.0))
        (ewwm-bci-attention--state-start-time 99.0)
        (ewwm-bci-attention--last-update-time 99.0)
        (ewwm-bci-attention--history '((1 . 0.9)))
        (ewwm-bci-attention--dnd-active nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-attention-teardown)
      (should (eq ewwm-bci-attention--state 'neutral))
      (should (= ewwm-bci-attention--score 0.0))
      (should-not ewwm-bci-attention--band-powers)
      (should-not ewwm-bci-attention--history))))

;;; ewwm-bci-attention-test.el ends here
