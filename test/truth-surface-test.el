;;; truth-surface-test.el --- Reality-driven documentation tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Cheap checks that keep the repo's truth surface from drifting silently.

;;; Code:

(require 'ert)

(defconst truth-surface-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(defconst truth-surface-test--reference-docs
  '("docs/developer-guide.md"
    "docs/api-reference.md"
    "docs/user-guide.md"
    "docs/eye-tracking-guide.md"
    "docs/bci-guide.md"
    "docs/vr-guide.md")
  "Reference docs that should not carry the old v0.1.0 framing.")

(defun truth-surface-test--read-file (relative)
  "Return project file RELATIVE as a string."
  (with-temp-buffer
    (insert-file-contents (expand-file-name relative truth-surface-test--root))
    (buffer-string)))

(defun truth-surface-test--dispatch-command-count ()
  "Count explicit IPC commands in the dispatcher."
  (let ((count 0))
    (dolist (line (split-string
                   (truth-surface-test--read-file "compositor/src/ipc/dispatch.rs")
                   "\n"))
      (when (string-match-p
             "^[[:space:]]*Some(\"[^\"]+\")[[:space:]]*=>"
             line)
        (setq count (1+ count))))
    count))

(defun truth-surface-test--feature-matrix-command-count ()
  "Extract the documented IPC command count from the feature matrix."
  (let ((content (truth-surface-test--read-file "docs/feature-matrix.md")))
    (when (string-match "\\*\\*\\([0-9]+\\) commands\\*\\*" content)
      (string-to-number (match-string 1 content)))))

(defun truth-surface-test--markdown-section (relative heading)
  "Return markdown section HEADING from RELATIVE, or nil if missing."
  (let* ((content (truth-surface-test--read-file relative))
         (pattern (format "^### %s\n" (regexp-quote heading))))
    (when (string-match pattern content)
      (let ((start (match-end 0)))
        (if (string-match "^### " content start)
            (substring content start (match-beginning 0))
          (substring content start))))))

(ert-deftest truth-surface/reference-docs-drop-v010-headers ()
  "Active reference docs should not carry the old v0.1.0 banner."
  (dolist (relative truth-surface-test--reference-docs)
    (should-not
     (string-match-p "Version 0\\.1\\.0"
                     (truth-surface-test--read-file relative)))))

(ert-deftest truth-surface/developer-guide-does-not-reference-removed-plan ()
  "The developer guide should not reference the removed PLAN.md file."
  (should-not
   (string-match-p "\\bPLAN\\.md\\b"
                   (truth-surface-test--read-file "docs/developer-guide.md"))))

(ert-deftest truth-surface/feature-matrix-command-count-matches-dispatcher ()
  "Feature-matrix IPC count should track the current dispatcher."
  (should
   (= (truth-surface-test--dispatch-command-count)
      (truth-surface-test--feature-matrix-command-count))))

(ert-deftest truth-surface/ci-triggers-on-docs-and-readme ()
  "CI should notice doc and README drift."
  (let ((workflow (truth-surface-test--read-file ".github/workflows/ci.yml")))
    (should (string-match-p "README\\.md" workflow))
    (should (string-match-p "docs/\\*\\*" workflow))))

(ert-deftest truth-surface/readme-links-remote-build-authority ()
  "README should link the explicit remote-build authority doc."
  (let ((readme (truth-surface-test--read-file "README.md")))
    (should (string-match-p "docs/remote-build-authority\\.md" readme))
    (should (file-exists-p
             (expand-file-name "docs/remote-build-authority.md"
                               truth-surface-test--root)))))

(ert-deftest truth-surface/readme-links-remote-proof-lanes ()
  "README should link the explicit remote proof-lane doc."
  (let ((readme (truth-surface-test--read-file "README.md")))
    (should (string-match-p "docs/remote-proof-lanes\\.md" readme))
    (should (file-exists-p
             (expand-file-name "docs/remote-proof-lanes.md"
                               truth-surface-test--root)))))

(ert-deftest truth-surface/public-guides-link-remote-authority-surface ()
  "Public guides should route readers to the remote authority docs."
  (dolist (relative '("docs/installation-quickstart.md"
                      "docs/user-guide.md"
                      "docs/bci-quickstart.md"))
    (let ((content (truth-surface-test--read-file relative)))
      (should (string-match-p "remote-build-authority\\.md" content)))))

(ert-deftest truth-surface/secondary-linux-guides-link-remote-authority-surface ()
  "Secondary Linux-oriented guides should still route readers to authority docs."
  (dolist (relative '("docs/rocky10-nix-deployment.md"
                      "docs/qemu-testing.md"))
    (let ((content (truth-surface-test--read-file relative)))
      (should (string-match-p "remote-build-authority\\.md" content))
      (should (string-match-p "remote-proof-lanes\\.md" content)))))

(ert-deftest truth-surface/remote-proof-doc-tracks-live-workflows-and-recipes ()
  "Remote proof doc should track the named workflow and operator surface."
  (let ((doc (truth-surface-test--read-file "docs/remote-proof-lanes.md"))
        (justfile (truth-surface-test--read-file "justfile")))
    (dolist (workflow '("runner-health.yml"
                        "self-hosted-fast.yml"
                        "nix-cache.yml"
                        "cache-warm.yml"
                        "rocky-test.yml"
                        "packaging.yml"
                        "vr-hardware.yml"))
      (should (string-match-p (regexp-quote workflow) doc))
      (should (file-exists-p
               (expand-file-name (concat ".github/workflows/" workflow)
                                 truth-surface-test--root))))
    (dolist (recipe '("remote-proof-surface"
                      "remote-proof-runs"
                      "remote-runner-health"
                      "remote-cache-warm"
                      "remote-vr-smoke"
                      "remote-package"))
      (should (string-match-p
               (format "^%s\\b" (regexp-quote recipe))
               justfile)))))

(ert-deftest truth-surface/developer-guide-keeps-neo-checks-cheap ()
  "The `neo` section should stay on cheap control-plane checks."
  (let ((section (truth-surface-test--markdown-section
                  "docs/developer-guide.md"
                  "Local Control-Plane Sanity On `neo`")))
    (should section)
    (should (string-match-p "just truth-lint" section))
    (should (string-match-p "just test" section))
    (should-not (string-match-p "cargo build --manifest-path" section))
    (should-not (string-match-p "just build" section))))

(ert-deftest truth-surface/user-guide-tracks-packaged-rocky-session-surface ()
  "The user guide should describe the packaged Rocky session honestly."
  (let ((guide (truth-surface-test--read-file "docs/user-guide.md")))
    (should (string-match-p "exwm-vr-compositor\\.service" guide))
    (should (string-match-p "exwm-vr\\.target" guide))
    (should (string-match-p "Select \"EXWM-VR\"" guide))
    (should (string-match-p "\\$XDG_RUNTIME_DIR/wayland-0" guide))
    (should (string-match-p "\\$XDG_RUNTIME_DIR/ewwm-ipc\\.sock" guide))))

(provide 'truth-surface-test)
;;; truth-surface-test.el ends here
