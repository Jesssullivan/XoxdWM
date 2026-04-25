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

(defconst honey-substrate--native-deps-workflow
  (expand-file-name ".github/workflows/native-deps.yml"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--wlroots-spec
  (expand-file-name "packaging/rpm/wlroots-beyond.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--sway-spec
  (expand-file-name "packaging/rpm/sway-beyond.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--justfile
  (expand-file-name "justfile"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--monado-launch-script
  (expand-file-name "packaging/scripts/exwm-vr-monado-launch"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--openxr-smoke-script
  (expand-file-name "packaging/scripts/exwm-vr-openxr-smoke"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--exwm-vr-spec
  (expand-file-name "packaging/rpm/exwm-vr.spec"
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

(ert-deftest honey-substrate/rocky-host-setup-uses-real-gles-provider ()
  "The host setup helper should use Rocky's actual GLES provider package."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p (regexp-quote "libglvnd-devel") script))
      (should-not (string-match-p (regexp-quote "mesa-libGLES-devel") script)))))

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
    (insert-file-contents honey-substrate--monado-launch-script)
    (let ((script (buffer-string)))
      (should (string-match-p (regexp-quote "monado_comp_ipc") script))
      (should (string-match-p (regexp-quote "pgrep -x monado-service") script))
      (should (string-match-p (regexp-quote "rm -f \"${socket_path}\"") script))
      (should (string-match-p (regexp-quote "/usr/bin/monado-service") script))
      (should (string-match-p (regexp-quote "/usr/local/bin/monado-service") script)))))

(ert-deftest honey-substrate/openxr-smoke-wrapper-keeps-client-proof-bounded ()
  "The OpenXR smoke wrapper should expose status-only and bounded client runs."
  (with-temp-buffer
    (insert-file-contents honey-substrate--openxr-smoke-script)
    (let ((script (buffer-string)))
      (should (string-match-p (regexp-quote "--status-only") script))
      (should (string-match-p (regexp-quote "EXWM_VR_OPENXR_CLIENT") script))
      (should (string-match-p (regexp-quote "EXWM_VR_OPENXR_TIMEOUT") script))
      (should (string-match-p (regexp-quote "XDG_RUNTIME_DIR") script))
      (should (string-match-p (regexp-quote "XR_RUNTIME_JSON") script))
      (should (string-match-p (regexp-quote "/usr/local/bin/hello_xr") script))
      (should (string-match-p (regexp-quote "timeout \"$timeout_seconds\"") script))
      (should (string-match-p (regexp-quote "monado_comp_ipc") script))
      (should (string-match-p (regexp-quote "openxr_smoke=passed_after_ready_timeout") script)))))

(ert-deftest honey-substrate/exwm-vr-monado-package-installs-openxr-smoke-wrapper ()
  "The opt-in Monado subpackage should carry the repo-owned OpenXR smoke wrapper."
  (with-temp-buffer
    (insert-file-contents honey-substrate--exwm-vr-spec)
    (let ((spec (buffer-string)))
      (should (string-match-p (regexp-quote "Requires:       bash") spec))
      (should (string-match-p (regexp-quote "Requires:       coreutils") spec))
      (should (string-match-p (regexp-quote "packaging/scripts/exwm-vr-openxr-smoke") spec))
      (should (string-match-p (regexp-quote "%{_libexecdir}/%{project_name}/openxr-smoke") spec)))))

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
      (should (string-match-p (regexp-quote "monado-beyond-[0-9]*.rpm") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debuginfo-*.rpm'") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debugsource-*.rpm'") workflow))
      (should-not (string-match-p "libuvc-devel" workflow)))))

(ert-deftest honey-substrate/wlroots-rpm-lane-keeps-version-macro-non-recursive ()
  "wlroots RPM spec should not redefine the same version macro recursively."
  (with-temp-buffer
    (insert-file-contents honey-substrate--wlroots-spec)
    (let ((spec (buffer-string)))
      (should (string-match-p (regexp-quote "%global wlroots_default_version 0.18.2") spec))
      (should (string-match-p
               (regexp-quote "Version:        %{?wlroots_version}%{!?wlroots_version:%{wlroots_default_version}}")
               spec))
      (should-not
       (string-match-p
        (regexp-quote "%define wlroots_version %{?wlroots_version}%{!?wlroots_version:0.18.2}")
        spec))
      (should (string-match-p
               (regexp-quote "BuildRequires:  xorg-x11-server-Xwayland-devel")
               spec))
      (should-not (string-match-p (regexp-quote "BuildRequires:  xwayland") spec))
      (should (string-match-p (regexp-quote "%{_libdir}/libwlroots-*.so") spec))
      (should-not (string-match-p (regexp-quote "%{_libdir}/libwlroots-*.so.*") spec))
      (should (string-match-p (regexp-quote "%{_includedir}/wlroots-0.18/") spec))
      (should-not (string-match-p (regexp-quote "%{_includedir}/wlr/") spec))
      (should (string-match-p (regexp-quote "* Tue Mar 10 2026 EXWM-VR") spec)))))

(ert-deftest honey-substrate/sway-rpm-lane-keeps-version-and-xwayland-surface-honest ()
  "sway RPM spec should use a non-recursive version macro and Rocky Xwayland names."
  (with-temp-buffer
    (insert-file-contents honey-substrate--sway-spec)
    (let ((spec (buffer-string)))
      (should (string-match-p (regexp-quote "%global sway_default_version 1.10") spec))
      (should (string-match-p
               (regexp-quote "Version:        %{?sway_version}%{!?sway_version:%{sway_default_version}}")
               spec))
      (should-not
       (string-match-p
        (regexp-quote "%define sway_version %{?sway_version}%{!?sway_version:1.10}")
        spec))
      (should (string-match-p
               (regexp-quote "Requires:       xorg-x11-server-Xwayland")
               spec))
      (should-not (string-match-p
                   (regexp-quote "-Dxwayland=enabled")
                   spec))
      (should (string-match-p
               (regexp-quote "%config(noreplace) %{_sysconfdir}/sway/config")
               spec))
      (should (string-match-p
               (regexp-quote "%{_datadir}/backgrounds/sway/*.png")
               spec))
      (should-not (string-match-p (regexp-quote "Requires:       xwayland") spec))
      (should (string-match-p (regexp-quote "* Tue Mar 10 2026 EXWM-VR") spec)))))

(ert-deftest honey-substrate/native-rpm-specs-use-portable-meson-invocations ()
  "wlroots/sway RPM specs should not depend on distro-specific %%meson macros."
  (dolist (spec-path (list honey-substrate--wlroots-spec
                           honey-substrate--sway-spec))
    (with-temp-buffer
      (insert-file-contents spec-path)
      (let ((spec (buffer-string)))
        (should (string-match-p (regexp-quote "meson setup build") spec))
        (should (string-match-p (regexp-quote "ninja -C build") spec))
        (should (string-match-p (regexp-quote "DESTDIR=%{buildroot} ninja -C build install") spec))
        (should-not (string-match-p (regexp-quote "%meson") spec))
        (should-not (string-match-p (regexp-quote "%meson_build") spec))
        (should-not (string-match-p (regexp-quote "%meson_install") spec))))))

(ert-deftest honey-substrate/native-deps-workflow-builds-rpm-lane-in-rocky ()
  "The native RPM lane should run in a Rocky container with Rocky-native deps."
  (with-temp-buffer
    (insert-file-contents honey-substrate--native-deps-workflow)
    (let ((workflow (buffer-string)))
      (should (string-match-p (regexp-quote "container:") workflow))
      (should (string-match-p (regexp-quote "image: rockylinux/rockylinux:10") workflow))
      (should (string-match-p (regexp-quote "epel-release-latest-10.noarch.rpm") workflow))
      (should (string-match-p (regexp-quote "dnf config-manager --set-enabled crb || dnf config-manager setopt crb.enabled=1") workflow))
      (should (string-match-p (regexp-quote "libglvnd-devel") workflow))
      (should-not (string-match-p (regexp-quote "mesa-libGLES-devel") workflow))
      (should (string-match-p (regexp-quote "xorg-x11-server-Xwayland-devel") workflow))
      (should-not (string-match-p (regexp-quote "apt-get") workflow))
      (should-not (string-match-p (regexp-quote "python3 -m pip install --break-system-packages 'meson>=1.5.0'") workflow)))))

(ert-deftest honey-substrate/native-deps-workflow-selects-main-wlroots-rpms ()
  "The native RPM lane should not accidentally pick wlroots debug RPMs."
  (with-temp-buffer
    (insert-file-contents honey-substrate--native-deps-workflow)
    (let ((workflow (buffer-string)))
      (should (string-match-p (regexp-quote "wlroots-beyond-[0-9]*.rpm") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debuginfo-*.rpm'") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debugsource-*.rpm'") workflow))
      (should (string-match-p (regexp-quote "wlroots-beyond-devel-*.rpm") workflow)))))

(ert-deftest honey-substrate/justfile-exposes-remote-honey-dev-lane ()
  "The task runner should expose thin remote operator helpers for `neo` -> `honey`."
  (with-temp-buffer
    (insert-file-contents honey-substrate--justfile)
    (let ((justfile (buffer-string)))
      (should (string-match-p "^honey-shell host=\"honey\"" justfile))
      (should (string-match-p "^honey-devshell host=\"honey\"" justfile))
      (should (string-match-p "^honey-run host=\"honey\"" justfile))
      (should (string-match-p "^honey-proof-env host=\"honey\"" justfile))
      (should (string-match-p "^honey-openxr-status host=\"honey\"" justfile))
      (should (string-match-p "^honey-openxr-smoke host=\"honey\"" justfile))
      (should-not (string-match-p (regexp-quote "extra=\"${extra#--}\"") justfile))
      (should (string-match-p "remote_repo_path := \"/home/jess/XoxdWM\"" justfile))
      (should (string-match-p "cd {{remote_repo_path}}" justfile))
      (should (string-match-p "nix develop --command" justfile))
      (should (string-match-p "XDG_RUNTIME_DIR=.*run/user/" justfile))
      (should (string-match-p (regexp-quote "cmd_b64=\"$(printf '%s' \"${cmd}\" | base64 | tr -d '\\n')\"") justfile))
      (should (string-match-p (regexp-quote "ssh jess@{{host}} bash -s -- \"${cmd_b64}\" <<'REMOTE'") justfile))
      (should (string-match-p (regexp-quote "cmd=\"$(printf '%s' \"$1\" | base64 --decode)\"") justfile))
      (should (string-match-p (regexp-quote "exec nix develop --command bash -lc \"$cmd\"") justfile))
      (should-not (string-match-p (regexp-quote "cmd='{{args}}'") justfile))
      (should-not (string-match-p (regexp-quote "exec nix develop --command \"$@\"") justfile)))))

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
