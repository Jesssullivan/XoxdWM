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

(ert-deftest truth-surface/emacs-egreg-app-layer-contract-is-explicit ()
  "Public docs should keep Emacs/eGreg app-layer status separate from WM authority."
  (let ((readme (truth-surface-test--read-file "README.md"))
        (guide (truth-surface-test--read-file "docs/developer-guide.md"))
        (app-doc (truth-surface-test--read-file "docs/emacs-egreg-app-layer.md"))
        (compat (truth-surface-test--read-file "docs/app-compatibility.md"))
        (status (truth-surface-test--read-file "docs/status.md")))
    (should (string-match-p "docs/emacs-egreg-app-layer\\.md" readme))
    (dolist (content (list readme guide))
      (should-not (string-match-p "Emacs window-management layer" content))
      (should-not (string-match-p "Emacs is the window management brain" content))
      (should-not (string-match-p "Emacs as policy" content)))
    (should (string-match-p "native Wayland WM/DE authority" readme))
    (should (string-match-p "Rust compositor.*owns" guide))
    (should (string-match-p "Emacs/eGreg runs as an[ \n]+optional" guide))
    (should (string-match-p "xoxdwm-emacs\\.service" app-doc))
    (should (string-match-p "emacsclient -c" app-doc))
    (should (string-match-p "Wayland native" app-doc))
    (should (string-match-p "XWayland" app-doc))
    (should (string-match-p "compatibility path is explicit" app-doc))
    (should (string-match-p "control/diagnostic/app integration only" app-doc))
    (should (string-match-p "not the WM authority" app-doc))
    (should (string-match-p "debug/editor-only" app-doc))
    (should (string-match-p "Emacs pgtk / eGreg" compat))
    (should (string-match-p "GitHub #47.*closed" status))
    (should-not (string-match-p "Remaining authority-adjacent product gates.*#47"
                                status))))

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

(ert-deftest truth-surface/readme-links-hygiene-minisprint ()
  "README should link the current hygiene mini-sprint plan."
  (let ((readme (truth-surface-test--read-file "README.md"))
        (doc (truth-surface-test--read-file
              "docs/hygiene-minisprint-2026-04-25.md")))
    (should (string-match-p "docs/hygiene-minisprint-2026-04-25\\.md" readme))
    (should (string-match-p "GloriousFlywheel" doc))
    (should (string-match-p "rockies" doc))
    (should (string-match-p "repo-shaped runner taxonomy is debt" doc))
    (should (string-match-p "Do not stop `rke2`" doc))
    (should-not
     (string-match-p "monado-beyond` is not yet installed"
                     readme))))

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
        (authority (truth-surface-test--read-file "docs/remote-build-authority.md"))
        (justfile (truth-surface-test--read-file "justfile")))
    (dolist (workflow '("runner-health.yml"
                        "self-hosted-fast.yml"
                        "nix-cache.yml"
                        "cache-warm.yml"
                        "rocky-test.yml"
                        "packaging.yml"
                        "monado-companion.yml"
                        "openxr-smoke-client.yml"
                        "vr-hardware.yml"))
      (should (string-match-p (regexp-quote workflow) doc))
      (should (file-exists-p
               (expand-file-name (concat ".github/workflows/" workflow)
                                truth-surface-test--root))))
    (dolist (recipe '("remote-proof-surface"
                      "remote-proof-runs"
                      "remote-runner-health"
                      "remote-cache-warm"
                      "remote-monado-package"
                      "remote-vr-smoke"
                      "remote-package"
                      "remote-openxr-smoke-client-package"
                      "honey-shell"
                      "honey-devshell"
                      "honey-run"
                      "honey-proof-env"
                      "honey-openxr-status"
                      "honey-openxr-smoke"))
      (should (string-match-p
               (format "^%s\\b" (regexp-quote recipe))
               justfile)))
    (should (string-match-p "direnv" authority))
    (should (string-match-p "use flake" authority))
    (should (string-match-p "just honey-devshell" authority))
    (should (string-match-p "just honey-run honey -- <command" authority))
    (should (string-match-p "rockies" authority))
    (should (string-match-p "Bazel" authority))
    (should (string-match-p "neo.*honey" doc))
    (should (string-match-p "just honey-devshell" doc))
    (should (string-match-p "just honey-proof-env" doc))))

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
    (should (string-match-p "xoxdwm-compositor\\.service" guide))
    (should (string-match-p "xoxdwm\\.target" guide))
    (should (string-match-p "exwm-vr-compositor\\.service" guide))
    (should (string-match-p "exwm-vr\\.target" guide))
    (should (string-match-p "Select \"XoxdWM\"" guide))
    (should (string-match-p "\\$XDG_RUNTIME_DIR/wayland-0" guide))
    (should (string-match-p "\\$XDG_RUNTIME_DIR/ewwm-ipc\\.sock" guide))))

(ert-deftest truth-surface/rocky-guide-tracks-xoxdwm-unit-aliases ()
  "The Rocky deployment guide should use primary xoxdwm aliases."
  (let ((guide (truth-surface-test--read-file "docs/rocky10-nix-deployment.md")))
    (should (string-match-p "xoxdwm-compositor\\.service" guide))
    (should (string-match-p "xoxdwm-emacs\\.service" guide))
    (should (string-match-p "xoxdwm\\.target" guide))
    (should (string-match-p "exwm-vr-compositor\\.service" guide))
    (should (string-match-p "exwm-vr-emacs\\.service" guide))
    (should (string-match-p "exwm-vr\\.target" guide))
    (should (string-match-p "compatibility user units" guide))))

(ert-deftest truth-surface/packaging-workflow-copies-rocky-session-bootstrap ()
  "The RPM packaging workflow should include the dedicated session bootstrap."
  (let ((workflow (truth-surface-test--read-file ".github/workflows/packaging.yml")))
    (should (string-match-p "packaging/emacs/exwm-vr-session-init\\.el" workflow))
    (should (file-exists-p
             (expand-file-name "packaging/emacs/exwm-vr-session-init.el"
                               truth-surface-test--root)))))

(ert-deftest truth-surface/core-docs-link-yoga-session-run-note ()
  "Core truth docs should link the named-host `yoga` session note."
  (should (file-exists-p
           (expand-file-name "docs/yoga-session-proof-2026-04-22.md"
                             truth-surface-test--root)))
  (dolist (relative '("README.md"
                      "docs/status.md"
                      "docs/support-matrix.md"
                      "docs/grounded-milestone-plan-2026-q2.md"))
    (should
     (string-match-p "yoga-session-proof-2026-04-22\\.md"
                     (truth-surface-test--read-file relative)))))

(ert-deftest truth-surface/core-docs-link-honey-substrate-note ()
  "Core truth docs should link the named-host `honey` substrate note."
  (should (file-exists-p
           (expand-file-name "docs/honey-substrate-proof-2026-04-22.md"
                             truth-surface-test--root)))
  (dolist (relative '("README.md"
                      "docs/status.md"
                      "docs/support-matrix.md"
                      "docs/grounded-milestone-plan-2026-q2.md"))
    (should
     (string-match-p "honey-substrate-proof-2026-04-22\\.md"
                     (truth-surface-test--read-file relative)))))

(ert-deftest truth-surface/yoga-run-note-keeps-the-installed-proof-boundary-explicit ()
  "The `yoga` run note should record staged and installed proof boundaries."
  (let ((doc (truth-surface-test--read-file
              "docs/yoga-session-proof-2026-04-22.md")))
    (should (string-match-p "24768509226" doc))
    (should (string-match-p "0\\.5\\.4-1\\.el10" doc))
    (should (string-match-p "0\\.5\\.3-1\\.el10" doc))
    (should (string-match-p "staged" doc))
    (should (string-match-p "installed" doc))
    (should (string-match-p "exwm-vr\\.target" doc))
    (should (string-match-p "failed" doc))))

(ert-deftest truth-surface/honey-run-note-keeps-proof-vs-product-boundary-explicit ()
  "The `honey` note should record the direct proof without overstating stability."
  (let ((doc (truth-surface-test--read-file
              "docs/honey-substrate-proof-2026-04-22.md")))
    (should (string-match-p "XRT_COMPOSITOR_FORCE_WAYLAND=1" doc))
    (should (string-match-p "Wayland window fallback" doc))
    (should (string-match-p "XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1" doc))
    (should (string-match-p "granting DRM lease request" doc))
    (should (string-match-p "READY" doc))
    (should (string-match-p "installed package surface" doc))
    (should (string-match-p "XDG_RUNTIME_DIR=/run/user/1000" doc))
    (should (string-match-p "24804821792" doc))
    (should (string-match-p "24807084915" doc))
    (should (string-match-p "monado-beyond" doc))
    (should (string-match-p "libhidapi-libusb\\.so\\.0" doc))
    (should (string-match-p "/usr/bin/monado-service" doc))
    (should (string-match-p "/usr/share/openxr/1/openxr_monado\\.json" doc))))

(ert-deftest truth-surface/honey-proof-ladder-keeps-p3-and-p4-separate ()
  "Support docs and templates should not equate focused CLI smoke with first frame."
  (let ((matrix (truth-surface-test--read-file "docs/support-matrix.md"))
        (runbook (truth-surface-test--read-file "docs/honey-fresh-boot-runbook-2026-04-26.md"))
        (template (truth-surface-test--read-file "docs/honey-fresh-boot-evidence-template.md"))
        (smoke (truth-surface-test--read-file "packaging/scripts/exwm-vr-openxr-smoke")))
    (dolist (level '("P0 Inventory"
                     "P1 Host Substrate"
                     "P2 Lease/Runtime"
                     "P3 OpenXR Session"
                     "P4 Visual First Frame"
                     "P5 Fresh-Boot Repeatability"
                     "P6 Operator Stability"))
      (let* ((parts (split-string level " " t))
             (code (car parts))
             (name (mapconcat #'identity (cdr parts) " ")))
        (should (string-match-p
                 (format "%s\\(.\\|\n\\)*%s"
                         (regexp-quote code)
                         (regexp-quote name))
                 matrix))))
    (should (string-match-p "P3 pass / P4 fail" matrix))
    (should (string-match-p "visual_observed=no" matrix))
    (should (string-match-p "visual_observed" runbook))
    (should (string-match-p "visual_observed" template))
    (should (string-match-p "proof_ladder=P3_OPENXR_SESSION" smoke))
    (should (string-match-p "openxr_smoke=p3_session_after_ready_timeout" smoke))
    (should-not (string-match-p "passed_after_ready_timeout" smoke))
    (should-not (string-match-p "first frame confirmed" smoke))))

(provide 'truth-surface-test)
;;; truth-surface-test.el ends here
