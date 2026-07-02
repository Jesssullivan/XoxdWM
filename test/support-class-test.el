;;; support-class-test.el --- Support classification guardrails -*- lexical-binding: t; -*-

;;; Commentary:
;; Claim-gating tests for subsystem inventory versus named-host support.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst support-class-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst support-class-test--allowed-classes
  '("Product" "Smoke" "Prototype" "Synthetic" "Design")
  "Normalized support classes accepted by the truth surface.")

(defconst support-class-test--expected-multimodal-rows
  '(("Eye tracking" . "Prototype")
    ("Hand tracking / gestures" . "Prototype")
    ("BCI / BrainFlow hardware acquisition" . "Design")
    ("BCI synthetic pipeline" . "Synthetic")
    ("Mouth / voice input" . "Design"))
  "Expected support classes for biomodal and synthetic input rows.")

(defun support-class-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative support-class-test--root))
    (buffer-string)))

(defun support-class-test--matrix-rows ()
  "Return support-matrix rows as (NAME STATUS NOTES)."
  (let (rows)
    (dolist (line (split-string
                   (support-class-test--read-file "docs/support-matrix.md")
                   "\n"))
      (when (string-prefix-p "| " line)
        (let ((cells (mapcar #'string-trim
                             (split-string (substring line 1 -1) "|"))))
          (when (and (>= (length cells) 3)
                     (not (member (car cells)
                                  '("Target" "Surface" "Area" "Workflow Area"
                                    "Level")))
                     (not (string-match-p "\\`P[0-9]+\\'" (car cells)))
                     (not (string-match-p "\\`[- ]+\\'" (cadr cells))))
            (push (list (car cells) (cadr cells) (caddr cells)) rows)))))
    (nreverse rows)))

(defun support-class-test--matrix-status (row-name)
  "Return support-matrix status for ROW-NAME."
  (cadr (assoc row-name (support-class-test--matrix-rows))))

(ert-deftest support-class/feature-matrix-is-inventory-not-support ()
  "The feature inventory must not read as the support promise."
  (let ((feature-matrix (support-class-test--read-file "docs/feature-matrix.md"))
        (support-matrix (support-class-test--read-file "docs/support-matrix.md")))
    (should (string-match-p "inventory, not a support promise" feature-matrix))
    (should (string-match-p "support surface" support-matrix))
    (should (string-match-p "listed here as `Product` or `Smoke`" support-matrix))
    (should (string-match-p "treat it as `Prototype`, `Synthetic`, or `Design`"
                            support-matrix))))

(ert-deftest support-class/support-matrix-uses-normalized-classes ()
  "Support rows should use only the normalized support class vocabulary."
  (let ((support-matrix (support-class-test--read-file "docs/support-matrix.md")))
    (dolist (support-class support-class-test--allowed-classes)
      (should (string-match-p
               (format "- `%s`:" (regexp-quote support-class))
               support-matrix)))
    (should-not (string-match-p "`Proven`" support-matrix))
    (dolist (row (support-class-test--matrix-rows))
      (should (member (cadr row) support-class-test--allowed-classes)))))

(ert-deftest support-class/multimodal-claims-stay-below-product-support ()
  "Biomodal rows should not be promoted by subsystem inventory alone."
  (dolist (expected support-class-test--expected-multimodal-rows)
    (should (equal (support-class-test--matrix-status (car expected))
                   (cdr expected)))))

(ert-deftest support-class/truth-lint-script-passes ()
  "The dedicated support-class truth-lint script should pass."
  (let* ((script (expand-file-name "scripts/support-class-truth-lint"
                                   support-class-test--root))
         (buffer (get-buffer-create "*support-class-truth-lint*"))
         (exit-code (call-process "python3" nil buffer nil script)))
    (when (not (zerop exit-code))
      (message "%s" (with-current-buffer buffer (buffer-string))))
    (should (= 0 exit-code))))

(ert-deftest support-class/status-denies-unproven-biomodal-product-claims ()
  "The status page should explicitly deny unproven named-host biomodal support."
  (let ((status (support-class-test--read-file "docs/status.md")))
    (should (string-match-p
             "proven eye tracking, hand tracking, or BCI support"
             status))
    (should (string-match-p "not yet in-goggles first-frame" status))))

(ert-deftest support-class/subsystem-guides-link-truth-surface ()
  "Subsystem guides should defer named-host support claims to truth docs."
  (dolist (relative '("docs/bci-guide.md"
                      "docs/eye-tracking-guide.md"
                      "docs/vr-guide.md"))
    (let ((content (support-class-test--read-file relative)))
      (should (string-match-p "support-matrix\\.md" content))
      (should (string-match-p "status\\.md" content)))))

(provide 'support-class-test)
;;; support-class-test.el ends here
