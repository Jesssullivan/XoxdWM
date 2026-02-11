;;; ewwm-bci-multimodal-test.el --- Tests for multimodal fusion  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-multimodal)

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-multimodal-enabled)
(defvar ewwm-bci-multimodal-adaptive-dwell)
(defvar ewwm-bci-multimodal-dwell-focused-ms)
(defvar ewwm-bci-multimodal-dwell-relaxed-ms)
(defvar ewwm-bci-multimodal-dwell-default-ms)
(defvar ewwm-bci-multimodal-focused-threshold)
(defvar ewwm-bci-multimodal-relaxed-threshold)
(defvar ewwm-bci-multimodal-two-factor)
(defvar ewwm-bci-multimodal-two-factor-timeout-ms)
(defvar ewwm-bci-multimodal-three-factor-security)
(defvar ewwm-bci-multimodal--active)
(defvar ewwm-bci-multimodal--dwell-override)
(defvar ewwm-bci-multimodal--pending-confirm)
(defvar ewwm-bci-multimodal--confirm-timer)
(defvar ewwm-bci-multimodal--three-factor-state)
(defvar ewwm-bci-multimodal--three-factor-callback)
(defvar ewwm-bci-multimodal--last-attention-score)
(defvar ewwm-bci-multimodal-fusion-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/provides-feature ()
  "ewwm-bci-multimodal provides its feature."
  (should (featurep 'ewwm-bci-multimodal)))

(ert-deftest ewwm-bci-multimodal/group-exists ()
  "ewwm-bci-multimodal customization group exists."
  (should (get 'ewwm-bci-multimodal 'custom-group)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/default-not-active ()
  "Multimodal is not active by default."
  (let ((ewwm-bci-multimodal--active nil))
    (should-not ewwm-bci-multimodal--active)))

(ert-deftest ewwm-bci-multimodal/default-nil-dwell-override ()
  "Dwell override is nil by default."
  (let ((ewwm-bci-multimodal--dwell-override nil))
    (should-not ewwm-bci-multimodal--dwell-override)))

(ert-deftest ewwm-bci-multimodal/default-nil-pending ()
  "Pending confirm is nil by default."
  (let ((ewwm-bci-multimodal--pending-confirm nil))
    (should-not ewwm-bci-multimodal--pending-confirm)))

;; ── Defcustom defaults ───────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/dwell-focused-default ()
  "Default dwell-focused-ms is 150."
  (should (= (default-value
              'ewwm-bci-multimodal-dwell-focused-ms) 150)))

(ert-deftest ewwm-bci-multimodal/dwell-relaxed-default ()
  "Default dwell-relaxed-ms is 400."
  (should (= (default-value
              'ewwm-bci-multimodal-dwell-relaxed-ms) 400)))

(ert-deftest ewwm-bci-multimodal/dwell-default-ms ()
  "Default dwell-default-ms is 250."
  (should (= (default-value
              'ewwm-bci-multimodal-dwell-default-ms) 250)))

(ert-deftest ewwm-bci-multimodal/two-factor-default ()
  "Default two-factor is nil."
  (should-not (default-value 'ewwm-bci-multimodal-two-factor)))

(ert-deftest ewwm-bci-multimodal/three-factor-default ()
  "Default three-factor-security is nil."
  (should-not (default-value
               'ewwm-bci-multimodal-three-factor-security)))

(ert-deftest ewwm-bci-multimodal/focused-threshold-default ()
  "Default focused-threshold is 0.7."
  (should (= (default-value
              'ewwm-bci-multimodal-focused-threshold) 0.7)))

(ert-deftest ewwm-bci-multimodal/relaxed-threshold-default ()
  "Default relaxed-threshold is 0.4."
  (should (= (default-value
              'ewwm-bci-multimodal-relaxed-threshold) 0.4)))

;; ── Adaptive dwell ──────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/adaptive-dwell-focused ()
  "Score >= 0.7 sets focused dwell time (150ms)."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 150)
        (ewwm-bci-multimodal-dwell-relaxed-ms 400)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal--handle-attention
       '(:score 0.85))
      (should (= ewwm-bci-multimodal--dwell-override 150)))))

(ert-deftest ewwm-bci-multimodal/adaptive-dwell-relaxed ()
  "Score <= 0.4 sets relaxed dwell time (400ms)."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 150)
        (ewwm-bci-multimodal-dwell-relaxed-ms 400)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal--handle-attention
       '(:score 0.2))
      (should (= ewwm-bci-multimodal--dwell-override 400)))))

(ert-deftest ewwm-bci-multimodal/adaptive-dwell-default ()
  "Score between thresholds sets default dwell time (250ms)."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 150)
        (ewwm-bci-multimodal-dwell-relaxed-ms 400)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal--handle-attention
       '(:score 0.55))
      (should (= ewwm-bci-multimodal--dwell-override 250)))))

;; ── Two-factor: gaze + MI ───────────────────────────────────

(ert-deftest ewwm-bci-multimodal/two-factor-gaze-queues ()
  "Gaze select queues for MI confirmation in two-factor mode."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-two-factor t)
        (ewwm-bci-multimodal-two-factor-timeout-ms 5000)
        (ewwm-bci-multimodal--pending-confirm nil)
        (ewwm-bci-multimodal--confirm-timer nil)
        (ewwm-bci-multimodal-fusion-hook nil))
    (ewwm-bci-multimodal--handle-gaze-select
     '(:target "window-1" :action ignore))
    (should ewwm-bci-multimodal--pending-confirm)
    (should (equal (plist-get
                    ewwm-bci-multimodal--pending-confirm
                    :target)
                   "window-1"))
    ;; Cleanup timer
    (when ewwm-bci-multimodal--confirm-timer
      (cancel-timer ewwm-bci-multimodal--confirm-timer))))

(ert-deftest ewwm-bci-multimodal/two-factor-mi-confirms ()
  "MI confirm dispatches pending two-factor action."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-two-factor t)
        (ewwm-bci-multimodal-two-factor-timeout-ms 5000)
        (ewwm-bci-multimodal--confirm-timer nil)
        (ewwm-bci-multimodal-fusion-hook nil)
        (dispatched nil))
    ;; Set up pending confirm
    (let ((ewwm-bci-multimodal--pending-confirm
           (list :target "window-1"
                 :action (lambda (tgt) (setq dispatched tgt))
                 :time (float-time)
                 :modality 'gaze)))
      (ewwm-bci-multimodal--handle-mi-confirm
       '(:class left :confidence 0.8))
      (should (equal dispatched "window-1")))))

(ert-deftest ewwm-bci-multimodal/two-factor-timeout-clears ()
  "Timeout clears pending confirmation."
  (let ((ewwm-bci-multimodal--pending-confirm
         (list :target "window-1"
               :action #'ignore
               :time (float-time)
               :modality 'gaze))
        (ewwm-bci-multimodal--confirm-timer nil))
    (ewwm-bci-multimodal--confirm-timeout)
    (should-not ewwm-bci-multimodal--pending-confirm)))

;; ── Three-factor security ───────────────────────────────────

(ert-deftest ewwm-bci-multimodal/three-factor-disabled-passthrough ()
  "Three-factor disabled passes through immediately."
  (let ((ewwm-bci-multimodal-three-factor-security nil)
        (ewwm-bci-multimodal--three-factor-state nil)
        (ewwm-bci-multimodal--three-factor-callback nil)
        (result nil))
    (ewwm-bci-multimodal-three-factor-verify
     "test-action"
     (lambda (action success)
       (setq result (list action success))))
    (should (equal result '("test-action" t)))))

;; ── Fusion hook ──────────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/fusion-hook-fires ()
  "Fusion hook fires with action and modalities."
  (let ((ewwm-bci-multimodal-fusion-hook nil)
        (hook-result nil))
    (add-hook 'ewwm-bci-multimodal-fusion-hook
              (lambda (action mods)
                (setq hook-result (list action mods))))
    (ewwm-bci-multimodal--dispatch-action
     "window-1" #'ignore '(gaze mi))
    (should (equal (cadr hook-result) '(gaze mi)))))

;; ── Toggle ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/toggle-enables ()
  "Toggle enables multimodal fusion."
  (let ((ewwm-bci-multimodal-enabled nil)
        (ewwm-bci-multimodal--active nil)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--pending-confirm nil)
        (ewwm-bci-multimodal--confirm-timer nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal-toggle)
      (should ewwm-bci-multimodal-enabled)
      (should ewwm-bci-multimodal--active))))

(ert-deftest ewwm-bci-multimodal/toggle-disables ()
  "Toggle disables multimodal fusion."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal--dwell-override 150)
        (ewwm-bci-multimodal--pending-confirm nil)
        (ewwm-bci-multimodal--confirm-timer nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal-toggle)
      (should-not ewwm-bci-multimodal-enabled)
      (should-not ewwm-bci-multimodal--active)
      (should-not ewwm-bci-multimodal--dwell-override))))

;; ── Status ───────────────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/status-callable ()
  "Status command is interactive."
  (should (commandp 'ewwm-bci-multimodal-status)))

(ert-deftest ewwm-bci-multimodal/status-format ()
  "Status produces a message with expected fields."
  (let ((ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal--dwell-override 150)
        (ewwm-bci-multimodal--last-attention-score 0.75)
        (ewwm-bci-multimodal-two-factor t)
        (ewwm-bci-multimodal-three-factor-security nil)
        (msg-text nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (setq msg-text
                       (apply #'format fmt args)))))
      (ewwm-bci-multimodal-status)
      (should (string-match-p "active=yes" msg-text))
      (should (string-match-p "dwell=150ms" msg-text))
      (should (string-match-p "2fa=on" msg-text)))))

;; ── Cancel-pending ──────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/cancel-pending-clears ()
  "Cancel-pending clears timer and pending state."
  (let ((ewwm-bci-multimodal--pending-confirm
         '(:target "w1" :action ignore))
        (ewwm-bci-multimodal--confirm-timer nil))
    (ewwm-bci-multimodal--cancel-pending)
    (should-not ewwm-bci-multimodal--pending-confirm)
    (should-not ewwm-bci-multimodal--confirm-timer)))

;; ── Disabled multimodal skips ───────────────────────────────

(ert-deftest ewwm-bci-multimodal/disabled-skips-processing ()
  "Disabled multimodal skips event processing."
  (let ((ewwm-bci-multimodal-enabled nil)
        (ewwm-bci-multimodal--active nil)
        (ewwm-bci-multimodal--dwell-override nil))
    (ewwm-bci-multimodal--on-bci-multimodal
     '(:subtype attention-update :score 0.9))
    (should-not ewwm-bci-multimodal--dwell-override)))

;; ── Dispatch action ─────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/dispatch-calls-action ()
  "Dispatch calls function and runs hook."
  (let ((ewwm-bci-multimodal-fusion-hook nil)
        (dispatched nil)
        (hook-ran nil))
    (add-hook 'ewwm-bci-multimodal-fusion-hook
              (lambda (_action _mods)
                (setq hook-ran t)))
    (ewwm-bci-multimodal--dispatch-action
     "target"
     (lambda (tgt) (setq dispatched tgt))
     '(gaze))
    (should (equal dispatched "target"))
    (should hook-ran)))

;; ── Attention update adjusts dwell ──────────────────────────

(ert-deftest ewwm-bci-multimodal/attention-update-adjusts ()
  "Attention update via on-bci-multimodal adjusts dwell."
  (let ((ewwm-bci-multimodal-enabled t)
        (ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal-adaptive-dwell t)
        (ewwm-bci-multimodal-dwell-focused-ms 150)
        (ewwm-bci-multimodal-dwell-relaxed-ms 400)
        (ewwm-bci-multimodal-dwell-default-ms 250)
        (ewwm-bci-multimodal-focused-threshold 0.7)
        (ewwm-bci-multimodal-relaxed-threshold 0.4)
        (ewwm-bci-multimodal--dwell-override nil)
        (ewwm-bci-multimodal--last-attention-score 0.0))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-multimodal--on-bci-multimodal
       '(:subtype attention-update :score 0.8))
      (should (= ewwm-bci-multimodal--dwell-override 150)))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/event-registration-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-multimodal--register-events)
    (should (assq :bci-multimodal ewwm-ipc--event-handlers))
    (let ((count (length ewwm-ipc--event-handlers)))
      (ewwm-bci-multimodal--register-events)
      (should (= (length ewwm-ipc--event-handlers) count)))))

;; ── Init / teardown ──────────────────────────────────────────

(ert-deftest ewwm-bci-multimodal/init-callable ()
  "ewwm-bci-multimodal-init is callable."
  (let ((ewwm-ipc--event-handlers nil))
    (should (progn (ewwm-bci-multimodal-init) t))))

(ert-deftest ewwm-bci-multimodal/teardown-resets-state ()
  "Teardown resets all multimodal state."
  (let ((ewwm-bci-multimodal--active t)
        (ewwm-bci-multimodal--dwell-override 150)
        (ewwm-bci-multimodal--pending-confirm nil)
        (ewwm-bci-multimodal--confirm-timer nil)
        (ewwm-bci-multimodal--three-factor-state nil)
        (ewwm-bci-multimodal--three-factor-callback nil)
        (ewwm-bci-multimodal--last-attention-score 0.8))
    (ewwm-bci-multimodal-teardown)
    (should-not ewwm-bci-multimodal--active)
    (should-not ewwm-bci-multimodal--dwell-override)
    (should (= ewwm-bci-multimodal--last-attention-score 0.0))))

;;; ewwm-bci-multimodal-test.el ends here
