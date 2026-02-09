;;; ewwm-vr-input-test.el --- Week 10 VR input tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-input)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Defcustoms ──────────────────────────────────────────────

(ert-deftest ewwm-vr-input/gaze-ray-offset-defcustom ()
  "Default gaze ray offset is right-handed."
  (should (equal (default-value 'ewwm-vr-gaze-ray-offset) '(0.15 -0.10 -0.05))))

(ert-deftest ewwm-vr-input/head-scroll-enable-defcustom ()
  "Head scroll enabled by default."
  (should (eq (default-value 'ewwm-vr-head-scroll-enable) t)))

(ert-deftest ewwm-vr-input/head-scroll-deadzone-defcustom ()
  "Head scroll deadzone defaults to 5."
  (should (= (default-value 'ewwm-vr-head-scroll-deadzone) 5.0)))

(ert-deftest ewwm-vr-input/head-scroll-speed-defcustom ()
  "Head scroll speed defaults to 3."
  (should (= (default-value 'ewwm-vr-head-scroll-speed) 3.0)))

(ert-deftest ewwm-vr-input/depth-step-defcustom ()
  "Depth step defaults to 0.2."
  (should (= (default-value 'ewwm-vr-depth-step) 0.2)))

(ert-deftest ewwm-vr-input/cursor-scale-defcustom ()
  "Cursor scale defaults to 3."
  (should (= (default-value 'ewwm-vr-cursor-scale) 3.0)))

(ert-deftest ewwm-vr-input/show-ray-defcustom ()
  "Show ray defaults to t."
  (should (eq (default-value 'ewwm-vr-show-ray) t)))

(ert-deftest ewwm-vr-input/show-hit-point-defcustom ()
  "Show hit point defaults to t."
  (should (eq (default-value 'ewwm-vr-show-hit-point) t)))

(ert-deftest ewwm-vr-input/follow-mode-defcustom ()
  "Default follow mode is lazy."
  (should (eq (default-value 'ewwm-vr-emacs-follow-mode) 'lazy)))

(ert-deftest ewwm-vr-input/snap-grid-defcustom ()
  "Snap grid defaults to nil."
  (should (null (default-value 'ewwm-vr-snap-grid))))

;; ── Variables ───────────────────────────────────────────────

(ert-deftest ewwm-vr-input/pointer-surface-exists ()
  "ewwm-vr-pointer-surface variable exists."
  (should (boundp 'ewwm-vr-pointer-surface)))

(ert-deftest ewwm-vr-input/pointer-position-exists ()
  "ewwm-vr-pointer-position variable exists."
  (should (boundp 'ewwm-vr-pointer-position)))

(ert-deftest ewwm-vr-input/pointer-distance-exists ()
  "ewwm-vr-pointer-distance variable exists."
  (should (boundp 'ewwm-vr-pointer-distance)))

(ert-deftest ewwm-vr-input/ray-state-exists ()
  "ewwm-vr-ray-state variable exists."
  (should (boundp 'ewwm-vr-ray-state)))

(ert-deftest ewwm-vr-input/grab-active-exists ()
  "ewwm-vr-grab-active variable exists."
  (should (boundp 'ewwm-vr-grab-active)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-input/click-interactive ()
  "ewwm-vr-click is interactive."
  (should (commandp 'ewwm-vr-click)))

(ert-deftest ewwm-vr-input/right-click-interactive ()
  "ewwm-vr-right-click is interactive."
  (should (commandp 'ewwm-vr-right-click)))

(ert-deftest ewwm-vr-input/grab-interactive ()
  "ewwm-vr-grab is interactive."
  (should (commandp 'ewwm-vr-grab)))

(ert-deftest ewwm-vr-input/grab-release-interactive ()
  "ewwm-vr-grab-release is interactive."
  (should (commandp 'ewwm-vr-grab-release)))

(ert-deftest ewwm-vr-input/pull-closer-interactive ()
  "ewwm-vr-pull-closer is interactive."
  (should (commandp 'ewwm-vr-pull-closer)))

(ert-deftest ewwm-vr-input/push-away-interactive ()
  "ewwm-vr-push-away is interactive."
  (should (commandp 'ewwm-vr-push-away)))

(ert-deftest ewwm-vr-input/set-follow-interactive ()
  "ewwm-vr-set-follow is interactive."
  (should (commandp 'ewwm-vr-set-follow)))

(ert-deftest ewwm-vr-input/set-gaze-offset-interactive ()
  "ewwm-vr-set-gaze-offset is interactive."
  (should (commandp 'ewwm-vr-set-gaze-offset)))

(ert-deftest ewwm-vr-input/calibrate-gaze-interactive ()
  "ewwm-vr-calibrate-gaze is interactive."
  (should (commandp 'ewwm-vr-calibrate-gaze)))

(ert-deftest ewwm-vr-input/calibrate-confirm-interactive ()
  "ewwm-vr-calibrate-confirm is interactive."
  (should (commandp 'ewwm-vr-calibrate-confirm)))

(ert-deftest ewwm-vr-input/pointer-state-interactive ()
  "ewwm-vr-pointer-state is interactive."
  (should (commandp 'ewwm-vr-pointer-state)))

;; ── Set follow validation ───────────────────────────────────

(ert-deftest ewwm-vr-input/set-follow-invalid ()
  "Setting invalid follow mode signals error."
  (should-error (ewwm-vr-set-follow 1 'invalid)))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-input/pointer-event ()
  "Pointer event updates state."
  (let ((ewwm-vr-pointer-surface nil)
        (ewwm-vr-pointer-position nil)
        (ewwm-vr-pointer-distance nil)
        (ewwm-vr-ray-state nil))
    (ewwm-vr-input--on-pointer '(:surface-id 42 :x 100 :y 200 :distance 1.8 :ray :hovering))
    (should (= ewwm-vr-pointer-surface 42))
    (should (equal ewwm-vr-pointer-position '(100 . 200)))
    (should (= ewwm-vr-pointer-distance 1.8))
    (should (eq ewwm-vr-ray-state :hovering))))

(ert-deftest ewwm-vr-input/focus-changed-event ()
  "Focus changed event updates state and runs hook."
  (let ((ewwm-vr-pointer-surface nil)
        (hook-args nil))
    (let ((ewwm-vr-focus-change-hook
           (list (lambda (sid prev) (setq hook-args (list sid prev))))))
      (ewwm-vr-input--on-focus-changed '(:surface-id 42 :prev-surface-id 37))
      (should (= ewwm-vr-pointer-surface 42))
      (should (equal hook-args '(42 37))))))

(ert-deftest ewwm-vr-input/click-event-hook ()
  "Click event runs hook."
  (let ((hook-args nil))
    (let ((ewwm-vr-click-hook
           (list (lambda (btn sid x y) (setq hook-args (list btn sid x y))))))
      (ewwm-vr-input--on-click '(:button :left :surface-id 42 :x 100 :y 200))
      (should (equal hook-args '(:left 42 100 200))))))

(ert-deftest ewwm-vr-input/grab-started-event ()
  "Grab started event sets state."
  (let ((ewwm-vr-grab-active nil))
    (let ((ewwm-vr-grab-start-hook nil))
      (ewwm-vr-input--on-grab-started '(:surface-id 42))
      (should ewwm-vr-grab-active))))

(ert-deftest ewwm-vr-input/grab-ended-event ()
  "Grab ended event clears state."
  (let ((ewwm-vr-grab-active t))
    (let ((ewwm-vr-grab-end-hook nil))
      (ewwm-vr-input--on-grab-ended '(:surface-id 42 :position (:x 1.0 :y 1.5 :z -1.8)))
      (should-not ewwm-vr-grab-active))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-input/register-events ()
  "ewwm-vr-input--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-input--register-events)
    (should (assq :vr-pointer ewwm-ipc--event-handlers))
    (should (assq :vr-focus-changed ewwm-ipc--event-handlers))
    (should (assq :vr-click ewwm-ipc--event-handlers))
    (should (assq :vr-grab-started ewwm-ipc--event-handlers))
    (should (assq :vr-grab-ended ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-input/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-input--register-events)
    (ewwm-vr-input--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :vr-pointer))
                        ewwm-ipc--event-handlers))
               1))))

;; ── Minor mode ──────────────────────────────────────────────

(ert-deftest ewwm-vr-input/minor-mode-exists ()
  "ewwm-vr-input-mode minor mode exists."
  (should (fboundp 'ewwm-vr-input-mode)))

;; ── Init / teardown ─────────────────────────────────────────

(ert-deftest ewwm-vr-input/teardown-clears-state ()
  "ewwm-vr-input-teardown clears all state."
  (let ((ewwm-vr-pointer-surface 42)
        (ewwm-vr-pointer-position '(100 . 200))
        (ewwm-vr-pointer-distance 1.8)
        (ewwm-vr-ray-state 'hovering)
        (ewwm-vr-grab-active t))
    (ewwm-vr-input-teardown)
    (should-not ewwm-vr-pointer-surface)
    (should-not ewwm-vr-pointer-position)
    (should-not ewwm-vr-pointer-distance)
    (should-not ewwm-vr-ray-state)
    (should-not ewwm-vr-grab-active)))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-input/focus-change-hook-exists ()
  "Focus change hook exists."
  (should (boundp 'ewwm-vr-focus-change-hook)))

(ert-deftest ewwm-vr-input/click-hook-exists ()
  "Click hook exists."
  (should (boundp 'ewwm-vr-click-hook)))

(ert-deftest ewwm-vr-input/grab-start-hook-exists ()
  "Grab start hook exists."
  (should (boundp 'ewwm-vr-grab-start-hook)))

(ert-deftest ewwm-vr-input/grab-end-hook-exists ()
  "Grab end hook exists."
  (should (boundp 'ewwm-vr-grab-end-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-input/provides-feature ()
  "ewwm-vr-input provides its feature."
  (should (featurep 'ewwm-vr-input)))

;;; ewwm-vr-input-test.el ends here
