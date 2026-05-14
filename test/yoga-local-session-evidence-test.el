;;; yoga-local-session-evidence-test.el --- yoga evidence checker tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst yoga-local-session-evidence-test--script
  (expand-file-name
   "packaging/scripts/xoxdwm-yoga-local-session-evidence-check"
   (expand-file-name ".." (file-name-directory load-file-name))))

(defconst yoga-local-session-evidence-test--pass-packet
  (concat
   "date: 2026-05-13T12:00:00-04:00\n"
   "host: yoga\n"
   "kernel: 6.12.0\n"
   "rpm_versions: exwm-vr-0.5.4-1.el10 exwm-vr-compositor-0.5.4-1.el10\n"
   "display_manager: sddm display-manager.service active\n"
   "session_entry: /usr/share/wayland-sessions/xoxdwm.desktop\n"
   "login_method: fresh greeter login\n"
   "autologin_used: no\n"
   "loginctl_class: user\n"
   "loginctl_type: wayland\n"
   "loginctl_seat: seat0\n"
   "loginctl_state: active\n"
   "loginctl_remote: no\n"
   "user_units: exwm-vr.target active; exwm-vr-compositor.service active; exwm-vr-emacs.service active\n"
   "runtime_sockets: /run/user/1000/wayland-0 /run/user/1000/ewwm-ipc.sock\n"
   "journal_markers: ewwm-compositor started drm backend; packaged session bootstrap /usr/share/exwm-vr/exwm-vr-session; ewwm: initialized\n"
   "rollback_tested: yes\n"
   "vr_openxr_claims: no\n"
   "promotion_decision: pass\n"
   "remaining_gaps: none for local session lane\n"))

(defun yoga-local-session-evidence-test--run (packet &rest args)
  "Run the yoga local-session checker against PACKET with ARGS."
  (let ((file (make-temp-file "xoxdwm-yoga-local-session-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert packet))
          (with-temp-buffer
            (let ((status (apply #'call-process
                                 yoga-local-session-evidence-test--script
                                 nil t nil
                                 (append args (list file)))))
              (list status (buffer-string)))))
      (delete-file file))))

(ert-deftest yoga-local-session-evidence/allows-non-promotion-packet ()
  "Draft notes without a pass claim should stay valid but not become proof."
  (pcase-let ((`(,status ,output)
               (yoga-local-session-evidence-test--run
                "host: yoga\nremaining_gaps: still needs manual/fresh-login packet\n")))
    (should (= 0 status))
    (should (string-match-p
             "yoga_local_session_evidence_check=no_promotion_claim"
             output))))

(ert-deftest yoga-local-session-evidence/requires-promotion-in-strict-mode ()
  "Strict mode should not convert an inventory note into proof."
  (pcase-let ((`(,status ,output)
               (yoga-local-session-evidence-test--run
                "host: yoga\nlogin_method: fresh greeter login\n"
                "--require-pass")))
    (should (= 64 status))
    (should (string-match-p
             "strict yoga local-session proof requires a pass promotion_decision/result"
             output))))

(ert-deftest yoga-local-session-evidence/accepts-complete-manual-fresh-login-packet ()
  "A complete local Wayland packet should satisfy the pass checker."
  (pcase-let ((`(,status ,output)
               (yoga-local-session-evidence-test--run
                yoga-local-session-evidence-test--pass-packet
                "--require-pass")))
    (should (= 0 status))
    (should (string-match-p
             "yoga_local_session_evidence_check=passed"
             output))))

(ert-deftest yoga-local-session-evidence/rejects-autologin-promotion ()
  "Autologin evidence should not promote the local session lane."
  (let ((packet (replace-regexp-in-string
                 "login_method: fresh greeter login\nautologin_used: no"
                 "login_method: sddm-autologin\nautologin_used: yes"
                 yoga-local-session-evidence-test--pass-packet)))
    (pcase-let ((`(,status ,output)
                 (yoga-local-session-evidence-test--run packet "--require-pass")))
      (should (= 64 status))
      (should (string-match-p
               "manual/fresh-login evidence cannot use autologin"
               output)))))

(ert-deftest yoga-local-session-evidence/rejects-vr-openxr-claim ()
  "The yoga local-session packet should not carry Honey/VR promotion claims."
  (let ((packet (replace-regexp-in-string
                 "vr_openxr_claims: no"
                 "vr_openxr_claims: yes, P4 visual first frame observed"
                 yoga-local-session-evidence-test--pass-packet)))
    (pcase-let ((`(,status ,output)
                 (yoga-local-session-evidence-test--run packet "--require-pass")))
      (should (= 64 status))
      (should (string-match-p
               "yoga local-session proof cannot include VR/OpenXR promotion claims"
               output)))))

(ert-deftest yoga-local-session-evidence/rejects-visual-claim-despite-vr-no-field ()
  "Explicit visual-observed fields should not be hidden by vr_openxr_claims=no."
  (let ((packet (concat yoga-local-session-evidence-test--pass-packet
                        "visual_observed: yes\n")))
    (pcase-let ((`(,status ,output)
                 (yoga-local-session-evidence-test--run packet "--require-pass")))
      (should (= 64 status))
      (should (string-match-p
               "yoga local-session proof cannot include VR/OpenXR promotion claims"
               output)))))

(ert-deftest yoga-local-session-evidence/rejects-missing-session-entry ()
  "The packaged session has to be proven by session entry or loginctl Desktop."
  (let ((packet (replace-regexp-in-string
                 "session_entry: /usr/share/wayland-sessions/xoxdwm.desktop"
                 "session_entry: unknown"
                 yoga-local-session-evidence-test--pass-packet)))
    (pcase-let ((`(,status ,output)
                 (yoga-local-session-evidence-test--run packet "--require-pass")))
      (should (= 64 status))
      (should (string-match-p
               "yoga local-session proof requires packaged XoxdWM/EXWM-VR session entry"
               output)))))

(ert-deftest yoga-local-session-evidence/rejects-missing-runtime-socket ()
  "Both Wayland and IPC sockets are required for the local session pass."
  (let ((packet (replace-regexp-in-string
                 "/run/user/1000/wayland-0 /run/user/1000/ewwm-ipc.sock"
                 "/run/user/1000/wayland-0"
                 yoga-local-session-evidence-test--pass-packet)))
    (pcase-let ((`(,status ,output)
                 (yoga-local-session-evidence-test--run packet "--require-pass")))
      (should (= 64 status))
      (should (string-match-p
               "yoga local-session proof requires ewwm-ipc.sock runtime socket"
               output)))))

(provide 'yoga-local-session-evidence-test)
;;; yoga-local-session-evidence-test.el ends here
