;;; honey-p4-gate-test.el --- Honey P4 visual claim guardrails -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep `honey` visual first-frame claims separate from P3 OpenXR session smoke.

;;; Code:

(require 'ert)

(defconst honey-p4-gate-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defun honey-p4-gate-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative honey-p4-gate-test--root))
    (buffer-string)))

(ert-deftest honey-p4-gate/template-requires-human-observation ()
  "The P4 template should require explicit human-visible headset evidence."
  (let ((template (honey-p4-gate-test--read-file
                   "docs/honey-p4-visual-first-frame-template.md")))
    (should (string-match-p "visible non-black headset output" template))
    (should (string-match-p "Human observed visible non-black frame" template))
    (should (string-match-p "visual_observed=yes" template))
    (should (string-match-p "rke2-server` is observed, not disturbed" template))
    (should (string-match-p "P3 pass / P4 fail" template))
    (should (string-match-p "Do not promote P4 from logs alone" template))))

(ert-deftest honey-p4-gate/template-records-renderer-diagnostics ()
  "The P4 template should capture renderer counters for black headset output."
  (let ((template (honey-p4-gate-test--read-file
                   "docs/honey-p4-visual-first-frame-template.md")))
    (should (string-match-p "Renderer Diagnostics" template))
    (should (string-match-p "vr-diagnostics" template))
    (should (string-match-p "frame_wait_count" template))
    (should (string-match-p "frame_begin_count" template))
    (should (string-match-p "frame_end_count" template))
    (should (string-match-p "last_readback_hash" template))))

(ert-deftest honey-p4-gate/support-matrix-links-template ()
  "The support matrix should point P4 claims at the narrow evidence packet."
  (let ((matrix (honey-p4-gate-test--read-file "docs/support-matrix.md")))
    (should (string-match-p "honey-p4-visual-first-frame-template\\.md" matrix))
    (should (string-match-p "Current `honey` classification: P3 pass / P4 fail" matrix))))

(ert-deftest honey-p4-gate/openxr-wrapper-keeps-p3-and-p4-separate ()
  "The OpenXR wrapper should distinguish P3 session smoke from P4 observation."
  (let ((script (honey-p4-gate-test--read-file
                 "packaging/scripts/exwm-vr-openxr-smoke")))
    (should (string-match-p "proof_ladder=P3_OPENXR_SESSION" script))
    (should (string-match-p "visual_first_frame=P4_OBSERVED" script))
    (should (string-match-p "visual_first_frame=P4_UNOBSERVED" script))
    (should (string-match-p "EXWM_VR_VISUAL_OBSERVED=yes|no" script))
    (should-not (string-match-p "first frame confirmed" script))))

(provide 'honey-p4-gate-test)
;;; honey-p4-gate-test.el ends here
