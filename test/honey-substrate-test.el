;;; honey-substrate-test.el --- Honey substrate helper tests -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defconst honey-substrate--setup-script
  (expand-file-name "packaging/scripts/exwm-vr-setup"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--monado-spec
  (expand-file-name "packaging/rpm/monado-beyond.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--monado-workflow
  (expand-file-name ".github/workflows/monado-companion.yml"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--justfile
  (expand-file-name "justfile"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--remote-authority-doc
  (expand-file-name "docs/remote-build-authority.md"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(ert-deftest honey-substrate/beyond-first-frame-sets-runtime-dir ()
  "beyond-first-frame uses a real runtime dir for remote OpenXR client tools."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p "export XDG_RUNTIME_DIR=.*run/user/\\$[(]id -u[)]" script))
      (should (string-match-p "XDG_RUNTIME_DIR=\"\\$XDG_RUNTIME_DIR\"" script)))))

(ert-deftest honey-substrate/beyond-first-frame-allows-connected-dp-fallback ()
  "beyond-first-frame does not hard-fail when DP is connected but non_desktop is absent."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p "No non_desktop=1 connector found; continuing because" script))
      (should (string-match-p "no connected DisplayPort fallback matched" script)))))

(ert-deftest honey-substrate/monado-env-docs-point-to-supported-config-surface ()
  "The setup helper should install direct-mode env files under ~/.config/exwm-vr."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p "\\.config/exwm-vr/compositor\\.env" script))
      (should (string-match-p "\\.config/exwm-vr/monado\\.env" script))
      (should (string-match-p "# MONADO_SERVICE_BIN=/usr/local/bin/monado-service" script))
      (should (string-match-p "systemctl --user start exwm-vr-monado.service" script))
      (should (string-match-p "XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1" script))
      (should (string-match-p "EWWM_DRM_LEASE_CONNECTORS=DP-2" script)))))

(ert-deftest honey-substrate/monado-launch-cleans-stale-ipc-and-prefers-packaged-runtime ()
  "The launcher should clear dead IPC sockets and prefer the packaged runtime."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "packaging/scripts/exwm-vr-monado-launch"
                       (expand-file-name ".." (file-name-directory load-file-name))))
    (let ((script (buffer-string)))
      (should (string-match-p "monado_comp_ipc" script))
      (should (string-match-p "pgrep -x monado-service" script))
      (should (string-match-p "rm -f \"\\${socket_path}\"" script))
      (should (string-match-p "/usr/bin/monado-service" script))
      (should (string-match-p "/usr/local/bin/monado-service" script)))))

(ert-deftest honey-substrate/monado-companion-lane-keeps-rocky-deps-honest ()
  "The Monado companion RPM lane should not depend on unavailable Rocky libuvc packaging."
  (with-temp-buffer
    (insert-file-contents honey-substrate--monado-spec)
    (let ((spec (buffer-string)))
      (should-not (string-match-p "libuvc-devel" spec))
      (should (string-match-p (regexp-quote "XRT_HAVE_LIBUVC=OFF") spec))
      (should (string-match-p (regexp-quote "XRT_BUILD_DRIVER_UVC=OFF") spec))
      (should (string-match-p (regexp-quote "XRT_BUILD_DRIVER_RIFT_SENSOR=OFF") spec))
      (should-not (string-match-p (regexp-quote "%{_libdir}/pkgconfig/monado*.pc") spec))
      (should-not (string-match-p (regexp-quote "%{_libdir}/cmake/monado*/") spec))
      (should (string-match-p (regexp-quote "%{_bindir}/monado-ctl") spec))
      (should (string-match-p (regexp-quote "%{_includedir}/monado/monado.h") spec))
      (should (string-match-p (regexp-quote "%{_datadir}/steamvr-monado/") spec))
      (should (string-match-p (regexp-quote "* Tue Mar 10 2026 EXWM-VR") spec))))
  (with-temp-buffer
    (insert-file-contents honey-substrate--monado-workflow)
    (let ((workflow (buffer-string)))
      (should (string-match-p "packaging/rpm/monado-beyond\\.spec" workflow))
      (should (string-match-p "epel-release-latest-10\\.noarch\\.rpm" workflow))
      (should-not (string-match-p "libuvc-devel" workflow)))))

(ert-deftest honey-substrate/justfile-exposes-remote-honey-dev-lane ()
  "The task runner should expose thin remote operator helpers for `neo` -> `honey`."
  (with-temp-buffer
    (insert-file-contents honey-substrate--justfile)
    (let ((justfile (buffer-string)))
      (should (string-match-p "^honey-shell host=\"honey\"" justfile))
      (should (string-match-p "^honey-devshell host=\"honey\"" justfile))
      (should (string-match-p "^honey-run host=\"honey\"" justfile))
      (should (string-match-p "^honey-proof-env host=\"honey\"" justfile))
      (should (string-match-p "remote_repo_path := \"/home/jess/XoxdWM\"" justfile))
      (should (string-match-p "cd {{remote_repo_path}}" justfile))
      (should (string-match-p "nix develop --command" justfile))
      (should (string-match-p "XDG_RUNTIME_DIR=.*run/user/" justfile)))))

(ert-deftest honey-substrate/remote-authority-doc-separates-direnv-honey-and-bazel ()
  "Remote authority docs should keep local direnv, `honey` devshells, and `rockies` Bazel distinct."
  (with-temp-buffer
    (insert-file-contents honey-substrate--remote-authority-doc)
    (let ((doc (buffer-string)))
      (should (string-match-p "use flake" doc))
      (should (string-match-p "direnv" doc))
      (should (string-match-p "just honey-devshell" doc))
      (should (string-match-p "just honey-run honey -- <command" doc))
      (should (string-match-p "rockies" doc))
      (should (string-match-p "Bazel" doc)))))

;;; honey-substrate-test.el ends here
