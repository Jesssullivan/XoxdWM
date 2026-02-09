;;; week1-integration-test.el --- Week 1 integration tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Verify Week 1 deliverables compose correctly:
;; - All EXWM modules exist and have valid structure
;; - Test harness itself works
;; - Project scaffold files are present

;;; Code:

(require 'ert)

(defvar week1-test--project-root
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name buffer-file-name))))
  "Project root directory.")

;; ── scaffold tests ──────────────────────────────────────

(ert-deftest week1-integration/flake-exists ()
  "flake.nix exists in project root."
  (should (file-exists-p
           (expand-file-name "flake.nix" week1-test--project-root))))

(ert-deftest week1-integration/justfile-exists ()
  "justfile exists in project root."
  (should (file-exists-p
           (expand-file-name "justfile" week1-test--project-root))))

(ert-deftest week1-integration/cliff-toml-exists ()
  "cliff.toml exists in project root."
  (should (file-exists-p
           (expand-file-name "cliff.toml" week1-test--project-root))))

(ert-deftest week1-integration/editorconfig-exists ()
  ".editorconfig exists in project root."
  (should (file-exists-p
           (expand-file-name ".editorconfig" week1-test--project-root))))

(ert-deftest week1-integration/ci-pipeline-exists ()
  "GitHub Actions CI workflow exists."
  (should (file-exists-p
           (expand-file-name ".github/workflows/ci.yml"
                             week1-test--project-root))))

(ert-deftest week1-integration/pre-commit-hook-exists ()
  "Pre-commit hook exists and is executable."
  (let ((hook (expand-file-name ".githooks/pre-commit"
                                week1-test--project-root)))
    (should (file-exists-p hook))
    (should (file-executable-p hook))))

(ert-deftest week1-integration/commit-msg-hook-exists ()
  "Commit-msg hook exists and is executable."
  (let ((hook (expand-file-name ".githooks/commit-msg"
                                week1-test--project-root)))
    (should (file-exists-p hook))
    (should (file-executable-p hook))))

;; ── module completeness tests ───────────────────────────

(ert-deftest week1-integration/all-exwm-modules-exist ()
  "All 12 EXWM modules exist."
  (let ((modules '("exwm-core.el" "exwm-workspace.el" "exwm-input.el"
                    "exwm.el" "exwm-manage.el" "exwm-floating.el"
                    "exwm-layout.el" "exwm-xim.el" "exwm-systemtray.el"
                    "exwm-randr.el" "exwm-xsettings.el" "exwm-background.el")))
    (dolist (mod modules)
      (should (file-exists-p
               (expand-file-name mod week1-test--project-root))))))

(ert-deftest week1-integration/all-modules-lexical-binding ()
  "All EXWM modules use lexical binding."
  (let ((modules '("exwm-core.el" "exwm-workspace.el" "exwm-input.el"
                    "exwm.el" "exwm-manage.el" "exwm-floating.el"
                    "exwm-layout.el" "exwm-xim.el" "exwm-systemtray.el"
                    "exwm-randr.el" "exwm-xsettings.el" "exwm-background.el")))
    (dolist (mod modules)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name mod week1-test--project-root) nil 0 200)
        (should (string-match-p "lexical-binding: t" (buffer-string)))))))

(ert-deftest week1-integration/all-modules-provide-feature ()
  "All EXWM modules provide their expected feature."
  (let ((modules '("exwm-core" "exwm-workspace" "exwm-input"
                    "exwm" "exwm-manage" "exwm-floating"
                    "exwm-layout" "exwm-xim" "exwm-systemtray"
                    "exwm-randr" "exwm-xsettings" "exwm-background")))
    (dolist (mod modules)
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name (concat mod ".el") week1-test--project-root))
        (should (string-match-p
                 (format "(provide '%s)" mod)
                 (buffer-string)))))))

;; ── test harness meta-tests ─────────────────────────────

(ert-deftest week1-integration/ert-available ()
  "ERT framework is available."
  (should (featurep 'ert)))

(ert-deftest week1-integration/test-dir-exists ()
  "test/ directory exists."
  (should (file-directory-p
           (expand-file-name "test" week1-test--project-root))))

(ert-deftest week1-integration/run-tests-exists ()
  "test/run-tests.el exists."
  (should (file-exists-p
           (expand-file-name "test/run-tests.el" week1-test--project-root))))

;;; week1-integration-test.el ends here
