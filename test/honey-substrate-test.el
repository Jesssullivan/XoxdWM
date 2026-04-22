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
      (should (string-match-p "MONADO_SERVICE_BIN=/usr/local/bin/monado-service" script))
      (should (string-match-p "XRT_COMPOSITOR_FORCE_WAYLAND_DIRECT=1" script))
      (should (string-match-p "EWWM_DRM_LEASE_CONNECTORS=DP-2" script)))))

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
      (should (string-match-p (regexp-quote "%{_datadir}/steamvr-monado/") spec))
      (should (string-match-p (regexp-quote "* Tue Mar 10 2026 EXWM-VR") spec))))
  (with-temp-buffer
    (insert-file-contents honey-substrate--monado-workflow)
    (let ((workflow (buffer-string)))
      (should (string-match-p "packaging/rpm/monado-beyond\\.spec" workflow))
      (should (string-match-p "epel-release-latest-10\\.noarch\\.rpm" workflow))
      (should-not (string-match-p "libuvc-devel" workflow)))))

;;; honey-substrate-test.el ends here
