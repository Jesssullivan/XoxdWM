;;; e2e-bci-mode-test.el --- E2E BCI mode tests  -*- lexical-binding: t -*-

;; End-to-end tests for BCI mode: attention, SSVEP, P300, motor imagery,
;; neurofeedback, multimodal fusion, signal quality, and secure input.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-ipc)
(require 'ewwm-bci-core)
(require 'ewwm-bci-attention)
(require 'ewwm-bci-ssvep)
(require 'ewwm-bci-p300)
(require 'ewwm-bci-mi)
(require 'ewwm-bci-nfb)
(require 'ewwm-bci-multimodal)
(require 'ewwm-vr-secure-input)

;; Forward-declare dynamic variables
(defvar ewwm-ipc--event-handlers)
(defvar ewwm-bci-multimodal-enabled)
(defvar ewwm-bci-multimodal--active)
(defvar ewwm-bci-multimodal-adaptive-dwell)
(defvar ewwm-bci-multimodal-dwell-focused-ms)
(defvar ewwm-bci-multimodal-dwell-relaxed-ms)
(defvar ewwm-bci-multimodal-dwell-default-ms)
(defvar ewwm-bci-multimodal-focused-threshold)
(defvar ewwm-bci-multimodal-relaxed-threshold)
(defvar ewwm-bci-multimodal--dwell-override)
(defvar ewwm-bci-multimodal--last-attention-score)
(defvar ewwm-bci-multimodal--pending-confirm)
(defvar ewwm-bci-multimodal--confirm-timer)
(defvar ewwm-bci-multimodal-fusion-hook)
(defvar ewwm-bci-attention-enabled)
(defvar ewwm-bci-attention--score)
(defvar ewwm-bci-attention--state)
(defvar ewwm-bci-attention--band-powers)
(defvar ewwm-bci-attention--dnd-active)
(defvar ewwm-bci-attention--state-start-time)
(defvar ewwm-bci-attention--last-update-time)
(defvar ewwm-bci-attention--history)
(defvar ewwm-bci-attention-change-hook)
(defvar ewwm-bci-attention-dnd-threshold)
(defvar ewwm-bci-attention-threshold)
(defvar ewwm-bci-attention-drowsy-threshold)
(defvar ewwm-bci-attention-update-interval)
(defvar ewwm-bci-attention-auto-save)
(defvar ewwm-bci-p300-enabled)
(defvar ewwm-bci-p300--active)
(defvar ewwm-bci-p300--callback)
(defvar ewwm-bci-p300--targets)
(defvar ewwm-bci-p300--timeout-timer)
(defvar ewwm-bci-p300--trial-count)
(defvar ewwm-bci-p300--last-result)
(defvar ewwm-bci-p300-detect-hook)
(defvar ewwm-bci-p300-min-confidence)
(defvar ewwm-vr-secure-input--active)
(defvar ewwm-vr-secure-input-pause-eeg)

;; ── Module loading ──────────────────────────────────────────

(ert-deftest e2e-bci/all-modules-loaded ()
  "All 7 BCI modules provide their features."
  (dolist (feat '(ewwm-bci-core
                  ewwm-bci-attention
                  ewwm-bci-ssvep
                  ewwm-bci-p300
                  ewwm-bci-mi
                  ewwm-bci-nfb
                  ewwm-bci-multimodal))
    (should (featurep feat))))

;; ── Attention score pipeline ────────────────────────────────

(ert-deftest e2e-bci/attention-score-pipeline ()
  "Attention event updates score and state."
  (let ((ewwm-bci-attention-enabled t)
        (ewwm-bci-attention--score 0.0)
        (ewwm-bci-attention--state 'neutral)
        (ewwm-bci-attention--band-powers nil)
        (ewwm-bci-attention--dnd-active nil)
        (ewwm-bci-attention--state-start-time nil)
        (ewwm-bci-attention--last-update-time nil)
        (ewwm-bci-attention--history nil)
        (ewwm-bci-attention-change-hook nil)
        (ewwm-bci-attention-threshold 0.6)
        (ewwm-bci-attention-dnd-threshold 0.8)
        (ewwm-bci-attention-drowsy-threshold 0.2)
        (ewwm-bci-attention-update-interval 2.0)
        (ewwm-bci-attention-auto-save nil))
    (ewwm-bci-attention--on-bci-attention
     '(:score 0.75
       :state focused
       :band-powers (:alpha 0.4 :beta 0.6 :theta 0.2)))
    (should (= ewwm-bci-attention--score 0.75))
    (should (eq ewwm-bci-attention--state 'focused))))

;; ── SSVEP workspace selection ───────────────────────────────

(ert-deftest e2e-bci/ssvep-functions ()
  "SSVEP workspace selection functions exist."
  (should (fboundp 'ewwm-bci-ssvep-status))
  (should (fboundp 'ewwm-bci-ssvep-configure))
  (should (fboundp 'ewwm-bci-ssvep-mode-line-string)))

(ert-deftest e2e-bci/ssvep-init-teardown ()
  "SSVEP init and teardown are callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-ssvep-init)
    (ewwm-bci-ssvep-teardown)))

;; ── P300 confirmation ───────────────────────────────────────

(ert-deftest e2e-bci/p300-confirmation-mechanism ()
  "P300 callback mechanism dispatches on detection."
  (let ((ewwm-bci-p300-enabled t)
        (ewwm-bci-p300--active t)
        (ewwm-bci-p300--trial-count 0)
        (ewwm-bci-p300--last-result nil)
        (ewwm-bci-p300--timeout-timer nil)
        (ewwm-bci-p300-min-confidence 0.6)
        (ewwm-bci-p300-detect-hook nil)
        (cb-result nil))
    (let ((ewwm-bci-p300--callback
           (lambda (target confidence)
             (setq cb-result (list target confidence)))))
      (ewwm-bci-p300--on-bci-p300
       '(:target-id "yes"
         :confidence 0.85
         :latency-ms 300
         :status detected))
      (should (equal (car cb-result) "yes"))
      (should (= (cadr cb-result) 0.85)))))

;; ── Motor imagery ───────────────────────────────────────────

(ert-deftest e2e-bci/motor-imagery-functions ()
  "Motor imagery classification functions exist."
  (should (fboundp 'ewwm-bci-mi-calibrate))
  (should (fboundp 'ewwm-bci-mi-status))
  (should (fboundp 'ewwm-bci-mi-toggle))
  (should (commandp 'ewwm-bci-mi-calibrate)))

(ert-deftest e2e-bci/motor-imagery-init-teardown ()
  "Motor imagery init and teardown callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-mi-init)
    (ewwm-bci-mi-teardown)))

;; ── EEG fatigue monitoring ──────────────────────────────────

(ert-deftest e2e-bci/eeg-fatigue-rust-file ()
  "EEG fatigue Rust module exists."
  (let ((root (locate-dominating-file default-directory ".git")))
    (should (file-exists-p
             (expand-file-name
              "compositor/src/vr/fatigue_eeg.rs" root)))))

;; ── Neurofeedback ───────────────────────────────────────────

(ert-deftest e2e-bci/neurofeedback-protocols ()
  "Neurofeedback training protocol params exist."
  (should (fboundp 'ewwm-bci-nfb--protocol-params))
  (should (fboundp 'ewwm-bci-nfb-start))
  (should (fboundp 'ewwm-bci-nfb-stop))
  (should (fboundp 'ewwm-bci-nfb-status))
  (should (fboundp 'ewwm-bci-nfb-export-session)))

(ert-deftest e2e-bci/neurofeedback-init-teardown ()
  "Neurofeedback init and teardown callable."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci-nfb-init)
    (ewwm-bci-nfb-teardown)))

;; ── Multimodal fusion ───────────────────────────────────────

(ert-deftest e2e-bci/multimodal-adaptive-dwell ()
  "Multimodal attention -> adaptive dwell pipeline."
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
      ;; High attention -> focused dwell
      (ewwm-bci-multimodal--handle-attention '(:score 0.85))
      (should (= ewwm-bci-multimodal--dwell-override 150))
      ;; Low attention -> relaxed dwell
      (ewwm-bci-multimodal--handle-attention '(:score 0.3))
      (should (= ewwm-bci-multimodal--dwell-override 400)))))

;; ── BCI + secure input ──────────────────────────────────────

(ert-deftest e2e-bci/secure-input-pauses-eeg ()
  "Secure input mode config includes EEG pause."
  (should (boundp 'ewwm-vr-secure-input-pause-eeg))
  (should (eq ewwm-vr-secure-input-pause-eeg t)))

;; ── Synthetic board ─────────────────────────────────────────

(ert-deftest e2e-bci/synthetic-board-injection ()
  "Synthetic board ID is available in defcustoms."
  (should (boundp 'ewwm-bci-board-id))
  (should (integerp ewwm-bci-board-id)))

;; ── Hardware check ──────────────────────────────────────────

(ert-deftest e2e-bci/hardware-check-function ()
  "BCI hardware check is an interactive command."
  (should (fboundp 'ewwm-bci-hardware-check))
  (should (commandp 'ewwm-bci-hardware-check)))

;; ── Signal quality ──────────────────────────────────────────

(ert-deftest e2e-bci/signal-quality-monitoring ()
  "Signal quality monitoring functions exist."
  (should (fboundp 'ewwm-bci-signal-quality))
  (should (commandp 'ewwm-bci-signal-quality)))

;; ── Data retention ──────────────────────────────────────────

(ert-deftest e2e-bci/data-retention-management ()
  "Data retention defcustom exists."
  (should (boundp 'ewwm-bci-data-retention-days))
  (should (integerp ewwm-bci-data-retention-days)))

;; ── IPC handlers ────────────────────────────────────────────

(ert-deftest e2e-bci/all-ipc-handlers ()
  "All BCI IPC event handlers are functions."
  (dolist (fn '(ewwm-bci--on-bci-connected
                ewwm-bci--on-bci-disconnected
                ewwm-bci--on-bci-quality
                ewwm-bci--on-bci-error
                ewwm-bci--on-bci-frame
                ewwm-bci-attention--on-bci-attention
                ewwm-bci-ssvep--on-bci-ssvep
                ewwm-bci-p300--on-bci-p300
                ewwm-bci-mi--on-bci-mi
                ewwm-bci-nfb--on-bci-nfb-frame
                ewwm-bci-multimodal--on-bci-multimodal))
    (should (fboundp fn))))

;; ── Event registration ──────────────────────────────────────

(ert-deftest e2e-bci/all-events-registered ()
  "All BCI modules register their event handlers."
  (let ((ewwm-ipc--event-handlers nil))
    (ewwm-bci--register-events)
    (ewwm-bci-attention--register-events)
    (ewwm-bci-ssvep--register-events)
    (ewwm-bci-p300--register-events)
    (ewwm-bci-mi--register-events)
    (ewwm-bci-nfb--register-events)
    (ewwm-bci-multimodal--register-events)
    (dolist (key '(:bci-connected
                   :bci-disconnected
                   :bci-quality
                   :bci-error
                   :bci-frame
                   :bci-attention
                   :bci-ssvep
                   :bci-p300
                   :bci-mi
                   :bci-nfb-frame
                   :bci-multimodal))
      (should (assq key ewwm-ipc--event-handlers)))))

;; ── Defcustom groups ────────────────────────────────────────

(ert-deftest e2e-bci/defcustom-groups-exist ()
  "All BCI defcustom groups exist."
  (dolist (grp '(ewwm-bci
                 ewwm-bci-attention
                 ewwm-bci-ssvep
                 ewwm-bci-p300
                 ewwm-bci-mi
                 ewwm-bci-nfb
                 ewwm-bci-multimodal))
    (should (get grp 'custom-group))))

;; ── Init/teardown pairs ─────────────────────────────────────

(ert-deftest e2e-bci/all-init-teardown-pairs ()
  "All BCI modules have init/teardown pairs."
  (let ((ewwm-ipc--event-handlers nil))
    (dolist (pair '((ewwm-bci-init . ewwm-bci-teardown)
                    (ewwm-bci-attention-init . ewwm-bci-attention-teardown)
                    (ewwm-bci-ssvep-init . ewwm-bci-ssvep-teardown)
                    (ewwm-bci-p300-init . ewwm-bci-p300-teardown)
                    (ewwm-bci-mi-init . ewwm-bci-mi-teardown)
                    (ewwm-bci-nfb-init . ewwm-bci-nfb-teardown)
                    (ewwm-bci-multimodal-init . ewwm-bci-multimodal-teardown)))
      (should (fboundp (car pair)))
      (should (fboundp (cdr pair)))
      (funcall (car pair))
      (funcall (cdr pair)))))

;;; e2e-bci-mode-test.el ends here
