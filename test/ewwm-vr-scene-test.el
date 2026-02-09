;;; ewwm-vr-scene-test.el --- Week 8 VR scene tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-vr-scene)

;; Forward-declare so `let' creates dynamic binding (needed for `boundp')
(defvar ewwm-ipc--event-handlers)

;; ── Variables and defcustoms ────────────────────────────────

(ert-deftest ewwm-vr-scene/layout-variable-exists ()
  "ewwm-vr-scene-layout variable exists."
  (should (boundp 'ewwm-vr-scene-layout)))

(ert-deftest ewwm-vr-scene/ppu-variable-exists ()
  "ewwm-vr-scene-ppu variable exists."
  (should (boundp 'ewwm-vr-scene-ppu)))

(ert-deftest ewwm-vr-scene/background-variable-exists ()
  "ewwm-vr-scene-background variable exists."
  (should (boundp 'ewwm-vr-scene-background)))

(ert-deftest ewwm-vr-scene/surfaces-variable-exists ()
  "ewwm-vr-scene-surfaces variable exists."
  (should (boundp 'ewwm-vr-scene-surfaces)))

(ert-deftest ewwm-vr-scene/default-layout-defcustom ()
  "Default layout is arc."
  (should (eq (default-value 'ewwm-vr-scene-default-layout) 'arc)))

(ert-deftest ewwm-vr-scene/default-ppu-defcustom ()
  "Default PPU is 1000."
  (should (= (default-value 'ewwm-vr-scene-default-ppu) 1000)))

(ert-deftest ewwm-vr-scene/default-background-defcustom ()
  "Default background is dark."
  (should (eq (default-value 'ewwm-vr-scene-default-background) 'dark)))

(ert-deftest ewwm-vr-scene/grid-columns-defcustom ()
  "Grid columns defaults to 2."
  (should (= (default-value 'ewwm-vr-scene-grid-columns) 2)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-vr-scene/status-interactive ()
  "ewwm-vr-scene-status is interactive."
  (should (commandp 'ewwm-vr-scene-status)))

(ert-deftest ewwm-vr-scene/set-layout-interactive ()
  "ewwm-vr-scene-set-layout is interactive."
  (should (commandp 'ewwm-vr-scene-set-layout)))

(ert-deftest ewwm-vr-scene/set-ppu-interactive ()
  "ewwm-vr-scene-set-ppu is interactive."
  (should (commandp 'ewwm-vr-scene-set-ppu)))

(ert-deftest ewwm-vr-scene/set-background-interactive ()
  "ewwm-vr-scene-set-background is interactive."
  (should (commandp 'ewwm-vr-scene-set-background)))

(ert-deftest ewwm-vr-scene/set-projection-interactive ()
  "ewwm-vr-scene-set-projection is interactive."
  (should (commandp 'ewwm-vr-scene-set-projection)))

(ert-deftest ewwm-vr-scene/focus-interactive ()
  "ewwm-vr-scene-focus is interactive."
  (should (commandp 'ewwm-vr-scene-focus)))

(ert-deftest ewwm-vr-scene/move-interactive ()
  "ewwm-vr-scene-move is interactive."
  (should (commandp 'ewwm-vr-scene-move)))

;; ── Set layout validation ───────────────────────────────────

(ert-deftest ewwm-vr-scene/set-layout-arc ()
  "Setting layout to arc succeeds."
  (let ((ewwm-vr-scene-layout nil))
    (ewwm-vr-scene-set-layout 'arc)
    (should (eq ewwm-vr-scene-layout 'arc))))

(ert-deftest ewwm-vr-scene/set-layout-grid ()
  "Setting layout to grid succeeds."
  (let ((ewwm-vr-scene-layout nil))
    (ewwm-vr-scene-set-layout 'grid)
    (should (eq ewwm-vr-scene-layout 'grid))))

(ert-deftest ewwm-vr-scene/set-layout-stack ()
  "Setting layout to stack succeeds."
  (let ((ewwm-vr-scene-layout nil))
    (ewwm-vr-scene-set-layout 'stack)
    (should (eq ewwm-vr-scene-layout 'stack))))

(ert-deftest ewwm-vr-scene/set-layout-freeform ()
  "Setting layout to freeform succeeds."
  (let ((ewwm-vr-scene-layout nil))
    (ewwm-vr-scene-set-layout 'freeform)
    (should (eq ewwm-vr-scene-layout 'freeform))))

(ert-deftest ewwm-vr-scene/set-layout-invalid ()
  "Setting invalid layout signals error."
  (should-error (ewwm-vr-scene-set-layout 'invalid)))

;; ── Set background validation ───────────────────────────────

(ert-deftest ewwm-vr-scene/set-background-dark ()
  "Setting background to dark succeeds."
  (let ((ewwm-vr-scene-background nil))
    (ewwm-vr-scene-set-background 'dark)
    (should (eq ewwm-vr-scene-background 'dark))))

(ert-deftest ewwm-vr-scene/set-background-gradient ()
  "Setting background to gradient succeeds."
  (let ((ewwm-vr-scene-background nil))
    (ewwm-vr-scene-set-background 'gradient)
    (should (eq ewwm-vr-scene-background 'gradient))))

(ert-deftest ewwm-vr-scene/set-background-passthrough ()
  "Setting background to passthrough succeeds."
  (let ((ewwm-vr-scene-background nil))
    (ewwm-vr-scene-set-background 'passthrough)
    (should (eq ewwm-vr-scene-background 'passthrough))))

(ert-deftest ewwm-vr-scene/set-background-invalid ()
  "Setting invalid background signals error."
  (should-error (ewwm-vr-scene-set-background 'invalid)))

;; ── Set projection validation ───────────────────────────────

(ert-deftest ewwm-vr-scene/set-projection-invalid ()
  "Setting invalid projection signals error."
  (should-error (ewwm-vr-scene-set-projection 1 'sphere)))

;; ── PPU ─────────────────────────────────────────────────────

(ert-deftest ewwm-vr-scene/set-ppu-global ()
  "Setting global PPU updates state."
  (let ((ewwm-vr-scene-ppu nil))
    (ewwm-vr-scene-set-ppu 2000)
    (should (= ewwm-vr-scene-ppu 2000))))

(ert-deftest ewwm-vr-scene/set-ppu-per-surface ()
  "Setting per-surface PPU does not update global state."
  (let ((ewwm-vr-scene-ppu 1000))
    (ewwm-vr-scene-set-ppu 2000 42)
    (should (= ewwm-vr-scene-ppu 1000))))

;; ── Init and teardown ───────────────────────────────────────

(ert-deftest ewwm-vr-scene/init-sets-defaults ()
  "ewwm-vr-scene-init sets layout, ppu, background from defcustoms."
  (let ((ewwm-vr-scene-layout nil)
        (ewwm-vr-scene-ppu nil)
        (ewwm-vr-scene-background nil))
    (ewwm-vr-scene-init)
    (should (eq ewwm-vr-scene-layout 'arc))
    (should (= ewwm-vr-scene-ppu 1000))
    (should (eq ewwm-vr-scene-background 'dark))))

(ert-deftest ewwm-vr-scene/teardown-clears-state ()
  "ewwm-vr-scene-teardown clears all state."
  (let ((ewwm-vr-scene-layout 'arc)
        (ewwm-vr-scene-ppu 1000)
        (ewwm-vr-scene-background 'dark)
        (ewwm-vr-scene-surfaces '((1 . (:ppu 1000)))))
    (ewwm-vr-scene-teardown)
    (should-not ewwm-vr-scene-layout)
    (should-not ewwm-vr-scene-ppu)
    (should-not ewwm-vr-scene-background)
    (should-not ewwm-vr-scene-surfaces)))

;; ── Event registration ──────────────────────────────────────

(ert-deftest ewwm-vr-scene/register-events ()
  "ewwm-vr-scene--register-events adds handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-scene--register-events)
    (should (assq :vr-scene-layout-changed ewwm-ipc--event-handlers))
    (should (assq :vr-scene-surface-added ewwm-ipc--event-handlers))
    (should (assq :vr-scene-surface-removed ewwm-ipc--event-handlers))))

(ert-deftest ewwm-vr-scene/register-events-idempotent ()
  "Calling register twice doesn't duplicate."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-vr-scene--register-events)
    (ewwm-vr-scene--register-events)
    (should (= (length (cl-remove-if-not
                        (lambda (pair) (eq (car pair) :vr-scene-layout-changed))
                        ewwm-ipc--event-handlers))
               1))))

;; ── IPC event handlers ──────────────────────────────────────

(ert-deftest ewwm-vr-scene/layout-changed-event ()
  "Layout changed event updates local state."
  (let ((ewwm-vr-scene-layout nil)
        (ewwm-vr-scene-layout-hook nil))
    (ewwm-vr-scene--on-layout-changed '(:layout :grid))
    (should (eq ewwm-vr-scene-layout :grid))))

(ert-deftest ewwm-vr-scene/layout-changed-hook-runs ()
  "Layout changed event runs hook."
  (let ((ewwm-vr-scene-layout nil)
        (hook-ran nil))
    (let ((ewwm-vr-scene-layout-hook (list (lambda () (setq hook-ran t)))))
      (ewwm-vr-scene--on-layout-changed '(:layout :arc))
      (should hook-ran))))

;; ── Hooks exist ─────────────────────────────────────────────

(ert-deftest ewwm-vr-scene/layout-hook-exists ()
  "Layout hook variable exists."
  (should (boundp 'ewwm-vr-scene-layout-hook)))

(ert-deftest ewwm-vr-scene/surface-hook-exists ()
  "Surface hook variable exists."
  (should (boundp 'ewwm-vr-scene-surface-hook)))

;; ── Provides ────────────────────────────────────────────────

(ert-deftest ewwm-vr-scene/provides-feature ()
  "ewwm-vr-scene provides its feature."
  (should (featurep 'ewwm-vr-scene)))

;;; ewwm-vr-scene-test.el ends here
