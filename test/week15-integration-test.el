;;; week15-integration-test.el --- Week 15 integration tests  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-core)
(require 'ewwm-environment)

;; ── Module Loading ──────────────────────────────────────────

(ert-deftest week15-integration/environment-module-loads ()
  "ewwm-environment module loads without error."
  (should (featurep 'ewwm-environment)))

(ert-deftest week15-integration/environment-provides-feature ()
  "ewwm-environment provides its feature."
  (should (featurep 'ewwm-environment)))

;; ── Defcustom Group ─────────────────────────────────────────

(ert-deftest week15-integration/environment-group-exists ()
  "ewwm-environment customization group exists."
  (should (get 'ewwm-environment 'custom-group)))

;; ── Function Availability ───────────────────────────────────

(ert-deftest week15-integration/check-functions-exist ()
  "All public environment check functions exist."
  (should (functionp 'ewwm-check-environment))
  (should (functionp 'ewwm-environment-ok-p))
  (should (functionp 'ewwm-environment-check-all)))

(ert-deftest week15-integration/internal-check-functions-exist ()
  "All internal check functions exist."
  (should (functionp 'ewwm-environment--check-env-var))
  (should (functionp 'ewwm-environment--check-path))
  (should (functionp 'ewwm-environment--check-executable))
  (should (functionp 'ewwm-environment--check-wayland))
  (should (functionp 'ewwm-environment--check-vr))
  (should (functionp 'ewwm-environment--check-compositor))
  (should (functionp 'ewwm-environment--check-dbus))
  (should (functionp 'ewwm-environment--check-bci))
  (should (functionp 'ewwm-environment--check-eye-tracking))
  (should (functionp 'ewwm-environment--check-secrets)))

;; ── End-to-End Simulation ───────────────────────────────────

(ert-deftest week15-integration/full-check-under-simulated-env ()
  "Full environment check under simulated Wayland environment."
  (let ((process-environment
         (append '("WAYLAND_DISPLAY=wayland-1"
                   "XDG_RUNTIME_DIR=/run/user/1000"
                   "XDG_CURRENT_DESKTOP=EXWM-VR"
                   "XDG_SESSION_TYPE=wayland"
                   "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus"
                   "XR_RUNTIME_JSON=/etc/xdg/openxr/1/active_runtime.json"
                   "WLR_RENDERER=gles2"
                   "BRAINFLOW_BOARD_ID=0")
                 process-environment)))
    (let ((results (ewwm-environment-check-all)))
      ;; Should have all categories
      (should (= (length results) 7))
      ;; Wayland env vars should pass
      (let ((wayland-results (cdr (assoc "Wayland Session" results))))
        (let ((wl-display (cl-find "WAYLAND_DISPLAY" wayland-results
                                   :key #'car :test #'equal)))
          (should wl-display)
          (should (eq (nth 1 wl-display) 'ok)))))))

(ert-deftest week15-integration/format-produces-readable-output ()
  "Formatted output is human-readable."
  (let ((results (ewwm-environment-check-all)))
    (let ((formatted (ewwm-environment--format-results results)))
      (should (> (length formatted) 100))
      (should (string-match-p "==" formatted))
      (should (string-match-p "Summary" formatted)))))

(ert-deftest week15-integration/check-all-results-structure ()
  "Each check result has 3-element structure."
  (let ((results (ewwm-environment-check-all)))
    (dolist (category results)
      (dolist (check (cdr category))
        (should (= (length check) 3))
        (should (stringp (nth 0 check)))
        (should (memq (nth 1 check) '(ok warn error)))
        (should (stringp (nth 2 check)))))))

(ert-deftest week15-integration/brainflow-board-id-lookup ()
  "BrainFlow board ID lookup works for all defined devices."
  (dolist (entry ewwm-environment-brainflow-board-ids)
    (should (stringp (car entry)))
    (should (integerp (cdr entry)))))

;;; week15-integration-test.el ends here
