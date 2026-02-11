;;; ewwm-bci-ssvep-test.el --- Tests for BCI SSVEP module  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-bci-ssvep)

;; Forward-declare IPC functions
(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")
(declare-function ewwm-workspace-switch "ewwm-workspace")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-bci-ssvep-enabled)
(defvar ewwm-bci-ssvep-frequencies)
(defvar ewwm-bci-ssvep-window-seconds)
(defvar ewwm-bci-ssvep-min-confidence)
(defvar ewwm-bci-ssvep-cooldown-ms)
(defvar ewwm-bci-ssvep--active)
(defvar ewwm-bci-ssvep--last-result)
(defvar ewwm-bci-ssvep--classifications)
(defvar ewwm-bci-ssvep--last-switch-time)
(defvar ewwm-bci-ssvep-select-hook)
(defvar ewwm-ipc--event-handlers)

;; ── Feature / group ────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/provides-feature ()
  "ewwm-bci-ssvep provides its feature."
  (should (featurep 'ewwm-bci-ssvep)))

(ert-deftest ewwm-bci-ssvep/group-exists ()
  "ewwm-bci-ssvep customization group exists."
  (should (get 'ewwm-bci-ssvep 'custom-group)))

;; ── Defcustom defaults ────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/enabled-default ()
  "Default enabled is nil (photosensitivity)."
  (should-not (default-value 'ewwm-bci-ssvep-enabled)))

(ert-deftest ewwm-bci-ssvep/frequencies-default ()
  "Default frequencies are 4 workspace-frequency pairs."
  (let ((freqs (default-value 'ewwm-bci-ssvep-frequencies)))
    (should (= (length freqs) 4))
    (should (= (cdr (assq 1 freqs)) 12.0))
    (should (= (cdr (assq 2 freqs)) 15.0))
    (should (= (cdr (assq 3 freqs)) 20.0))
    (should (= (cdr (assq 4 freqs)) 24.0))))

(ert-deftest ewwm-bci-ssvep/window-seconds-default ()
  "Default window-seconds is 3.0."
  (should (= (default-value 'ewwm-bci-ssvep-window-seconds)
             3.0)))

(ert-deftest ewwm-bci-ssvep/min-confidence-default ()
  "Default min-confidence is 0.7."
  (should (= (default-value 'ewwm-bci-ssvep-min-confidence)
             0.7)))

(ert-deftest ewwm-bci-ssvep/cooldown-ms-default ()
  "Default cooldown-ms is 2000."
  (should (= (default-value 'ewwm-bci-ssvep-cooldown-ms) 2000)))

;; ── Default state ─────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/default-not-active ()
  "Default active is nil."
  (let ((ewwm-bci-ssvep--active nil))
    (should-not ewwm-bci-ssvep--active)))

(ert-deftest ewwm-bci-ssvep/default-nil-last-result ()
  "Default last-result is nil."
  (let ((ewwm-bci-ssvep--last-result nil))
    (should-not ewwm-bci-ssvep--last-result)))

(ert-deftest ewwm-bci-ssvep/default-zero-classifications ()
  "Default classifications is 0."
  (let ((ewwm-bci-ssvep--classifications 0))
    (should (= ewwm-bci-ssvep--classifications 0))))

;; ── SSVEP event dispatches workspace switch ───────────────────

(ert-deftest ewwm-bci-ssvep/event-switches-workspace ()
  "SSVEP event with high confidence switches workspace."
  (let ((ewwm-bci-ssvep-enabled t)
        (ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result nil)
        (ewwm-bci-ssvep--classifications 0)
        (ewwm-bci-ssvep--last-switch-time nil)
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 2000)
        (ewwm-bci-ssvep-select-hook nil)
        (switched-to nil))
    (cl-letf (((symbol-function 'ewwm-workspace-switch)
               (lambda (ws) (setq switched-to ws))))
      (ewwm-bci-ssvep--on-bci-ssvep
       '(:workspace 2 :frequency 15.0 :confidence 0.85))
      (should (= switched-to 2))
      (should (= ewwm-bci-ssvep--classifications 1)))))

;; ── Confidence below threshold rejected ───────────────────────

(ert-deftest ewwm-bci-ssvep/low-confidence-rejected ()
  "SSVEP event below threshold does not switch."
  (let ((ewwm-bci-ssvep-enabled t)
        (ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result nil)
        (ewwm-bci-ssvep--classifications 0)
        (ewwm-bci-ssvep--last-switch-time nil)
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 2000)
        (ewwm-bci-ssvep-select-hook nil)
        (switched nil))
    (cl-letf (((symbol-function 'ewwm-workspace-switch)
               (lambda (_ws) (setq switched t))))
      (ewwm-bci-ssvep--on-bci-ssvep
       '(:workspace 3 :frequency 20.0 :confidence 0.4))
      (should-not switched)
      (should (= ewwm-bci-ssvep--classifications 0)))))

;; ── Select hook fires ─────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/select-hook-fires ()
  "SSVEP select hook fires on classification."
  (let ((ewwm-bci-ssvep-enabled t)
        (ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result nil)
        (ewwm-bci-ssvep--classifications 0)
        (ewwm-bci-ssvep--last-switch-time nil)
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 2000)
        (hook-args nil))
    (let ((ewwm-bci-ssvep-select-hook
           (list (lambda (ws conf)
                   (setq hook-args (list ws conf))))))
      (cl-letf (((symbol-function 'ewwm-workspace-switch)
                 (lambda (_ws) nil)))
        (ewwm-bci-ssvep--on-bci-ssvep
         '(:workspace 1 :frequency 12.0 :confidence 0.9))
        (should hook-args)
        (should (= (nth 0 hook-args) 1))
        (should (= (nth 1 hook-args) 0.9))))))

;; ── Cooldown prevents rapid switching ─────────────────────────

(ert-deftest ewwm-bci-ssvep/cooldown-blocks-switch ()
  "Cooldown prevents rapid workspace switches."
  (let ((ewwm-bci-ssvep-enabled t)
        (ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result nil)
        (ewwm-bci-ssvep--classifications 0)
        (ewwm-bci-ssvep--last-switch-time (float-time))
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 60000)
        (ewwm-bci-ssvep-select-hook nil)
        (switched nil))
    (cl-letf (((symbol-function 'ewwm-workspace-switch)
               (lambda (_ws) (setq switched t))))
      (ewwm-bci-ssvep--on-bci-ssvep
       '(:workspace 2 :frequency 15.0 :confidence 0.95))
      ;; last-result should be updated but no switch
      (should-not switched)
      (should (= ewwm-bci-ssvep--classifications 0)))))

;; ── Mode toggle ───────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/mode-enables ()
  "SSVEP mode enables active flag."
  (let ((ewwm-bci-ssvep--active nil)
        (ewwm-bci-ssvep-frequencies '((1 . 12.0)))
        (ewwm-bci-ssvep-window-seconds 3.0)
        (ewwm-bci-ssvep-min-confidence 0.7))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () nil)))
      (ewwm-bci-ssvep-mode 1)
      (should ewwm-bci-ssvep--active)
      (ewwm-bci-ssvep-mode -1)
      (should-not ewwm-bci-ssvep--active))))

;; ── Status message ────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/status-message-format ()
  "Status displays formatted state."
  (let ((ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--classifications 5)
        (ewwm-bci-ssvep--last-result
         '(:workspace 3 :confidence 0.82)))
    (should (stringp (with-output-to-string
                       (ewwm-bci-ssvep-status))))))

;; ── Configure sends IPC ───────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/configure-sends-ipc ()
  "Configure sends IPC with frequencies."
  (let ((ewwm-bci-ssvep-frequencies '((1 . 12.0) (2 . 15.0)))
        (ewwm-bci-ssvep-window-seconds 3.0)
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 2000)
        (sent-msgs nil))
    (cl-letf (((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t))
              ((symbol-function 'ewwm-ipc-send)
               (lambda (msg) (push msg sent-msgs))))
      (ewwm-bci-ssvep-configure)
      (should (= (length sent-msgs) 1))
      (should (eq (plist-get (car sent-msgs) :type)
                  :bci-ssvep-configure)))))

;; ── Disabled skips processing ─────────────────────────────────

(ert-deftest ewwm-bci-ssvep/disabled-skips ()
  "Disabled SSVEP does not process events."
  (let ((ewwm-bci-ssvep-enabled nil)
        (ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--classifications 0)
        (ewwm-bci-ssvep--last-result nil)
        (ewwm-bci-ssvep--last-switch-time nil)
        (ewwm-bci-ssvep-min-confidence 0.7)
        (ewwm-bci-ssvep-cooldown-ms 2000)
        (ewwm-bci-ssvep-select-hook nil))
    (ewwm-bci-ssvep--on-bci-ssvep
     '(:workspace 1 :frequency 12.0 :confidence 0.99))
    (should (= ewwm-bci-ssvep--classifications 0))))

;; ── Event registration idempotent ─────────────────────────────

(ert-deftest ewwm-bci-ssvep/register-idempotent ()
  "Event registration is idempotent."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-ssvep--register-events)
    (let ((count1 (length ewwm-ipc--event-handlers)))
      (ewwm-bci-ssvep--register-events)
      (should (= (length ewwm-ipc--event-handlers) count1)))))

;; ── Mode-line ─────────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/mode-line-active ()
  "Mode-line shows SSVEP when active with result."
  (let ((ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result '(:workspace 2)))
    (should (equal (ewwm-bci-ssvep-mode-line-string)
                   " [SSVEP:ws2]"))))

(ert-deftest ewwm-bci-ssvep/mode-line-no-result ()
  "Mode-line shows -- when active with no result."
  (let ((ewwm-bci-ssvep--active t)
        (ewwm-bci-ssvep--last-result nil))
    (should (equal (ewwm-bci-ssvep-mode-line-string)
                   " [SSVEP:--]"))))

(ert-deftest ewwm-bci-ssvep/mode-line-inactive ()
  "Mode-line returns nil when not active."
  (let ((ewwm-bci-ssvep--active nil))
    (should-not (ewwm-bci-ssvep-mode-line-string))))

;; ── Teardown ──────────────────────────────────────────────────

(ert-deftest ewwm-bci-ssvep/teardown-clears-state ()
  "Teardown resets all state."
  (let ((ewwm-bci-ssvep--active nil)
        (ewwm-bci-ssvep--last-result '(:workspace 1))
        (ewwm-bci-ssvep--classifications 10)
        (ewwm-bci-ssvep--last-switch-time 99.0))
    (ewwm-bci-ssvep-teardown)
    (should-not ewwm-bci-ssvep--active)
    (should-not ewwm-bci-ssvep--last-result)
    (should (= ewwm-bci-ssvep--classifications 0))
    (should-not ewwm-bci-ssvep--last-switch-time)))

;;; ewwm-bci-ssvep-test.el ends here
