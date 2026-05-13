;;; honey-substrate-test.el --- Honey substrate helper tests -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst honey-substrate--setup-script
  (expand-file-name "packaging/scripts/exwm-vr-setup"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--monado-spec
  (expand-file-name "packaging/rpm/monado-beyond.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--monado-workflow
  (expand-file-name ".github/workflows/monado-companion.yml"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--openxr-smoke-client-spec
  (expand-file-name "packaging/rpm/exwm-vr-openxr-smoke-client.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--openxr-smoke-client-workflow
  (expand-file-name ".github/workflows/openxr-smoke-client.yml"
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

(defconst honey-substrate--hmd-connector-script
  (expand-file-name "packaging/scripts/exwm-vr-hmd-connector"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--kernel-dsc-truth-script
  (expand-file-name "packaging/scripts/exwm-vr-kernel-dsc-truth"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--beyond-power-script
  (expand-file-name "packaging/scripts/beyond-power-on"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--amdgpu-dsc-pps-debugfs-patch
  (expand-file-name "patches/amdgpu-dsc-pps-debugfs.patch"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--sway-config
  (expand-file-name "packaging/sway/config"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--sway-status
  (expand-file-name "packaging/sway/status.sh"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--exwm-vr-spec
  (expand-file-name "packaging/rpm/exwm-vr.spec"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--remote-authority-doc
  (expand-file-name "docs/remote-build-authority.md"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defconst honey-substrate--pps-runbook
  (expand-file-name "docs/honey-pps-diagnostic-runbook-2026-05-12.md"
                    (expand-file-name ".." (file-name-directory load-file-name))))

(defun honey-substrate--read-file (path)
  "Return PATH contents as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun honey-substrate--section (text start end)
  "Return TEXT section from START regexp to END regexp."
  (let ((start-pos (string-match start text)))
    (when start-pos
      (let ((end-pos (string-match end text start-pos)))
        (substring text start-pos end-pos)))))

(defun honey-substrate--write-edid (path product)
  "Write a minimal binary EDID at PATH for BIG PRODUCT."
  (let ((bytes (make-string 128 0)))
    (aset bytes 8 #x09)   ; BIG manufacturer, byte 8
    (aset bytes 9 #x27)   ; BIG manufacturer, byte 9
    (pcase product
      (#x1234
       (aset bytes 10 #x34)
       (aset bytes 11 #x12))
      (#x5095
       (aset bytes 10 #x95)
       (aset bytes 11 #x50))
      (_ (error "unsupported product")))
    (let ((coding-system-for-write 'binary))
      (write-region (string-make-unibyte bytes) nil path nil 'silent))))

(defun honey-substrate--make-connector (root name status &optional non-desktop product)
  "Create fake DRM connector NAME under ROOT."
  (let ((dir (expand-file-name name root)))
    (make-directory dir t)
    (write-region status nil (expand-file-name "status" dir) nil 'silent)
    (when non-desktop
      (write-region non-desktop nil (expand-file-name "non_desktop" dir) nil 'silent))
    (when product
      (honey-substrate--write-edid (expand-file-name "edid" dir) product))
    dir))

(defun honey-substrate--resolve-fixture (root &rest args)
  "Run the connector resolver against fake DRM ROOT with ARGS."
  (with-temp-buffer
    (let ((rc (apply #'call-process honey-substrate--hmd-connector-script
                     nil t nil "--sysfs-root" root args)))
      (cons rc (string-trim (buffer-string))))))

(defun honey-substrate--write-fake-openxr-runtime (path)
  "Write a minimal fake OpenXR runtime JSON to PATH."
  (write-region
   (concat "{\n"
           "  \"file_format_version\": \"1.0.0\",\n"
           "  \"runtime\": {\n"
           "    \"name\": \"Fake Monado\",\n"
           "    \"library_path\": \"/tmp/libfake_openxr.so\"\n"
           "  }\n"
           "}\n")
   nil path nil 'silent))

(defun honey-substrate--write-fake-openxr-client (path)
  "Write a minimal fake OpenXR client executable to PATH."
  (write-region
   (concat "#!/usr/bin/env bash\n"
           "set -euo pipefail\n"
           "echo READY\n"
           "echo xrGetSystem\n"
           "echo state=FOCUSED\n")
   nil path nil 'silent)
  (set-file-modes path #o755))

(defun honey-substrate--run-openxr-smoke (env &rest args)
  "Run the OpenXR smoke wrapper with ENV and ARGS, returning (RC . OUTPUT)."
  (with-temp-buffer
    (let ((process-environment (append env process-environment)))
      (let ((rc (apply #'call-process honey-substrate--openxr-smoke-script
                       nil (list t t) nil args)))
        (cons rc (buffer-string))))))

(ert-deftest honey-substrate/beyond-first-frame-sets-runtime-dir ()
  "beyond-first-frame uses a real runtime dir for remote OpenXR client tools."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p "export XDG_RUNTIME_DIR=.*run/user/\\$[(]id -u[)]" script))
      (should (string-match-p "EXWM_VR_OPENXR_ACCEPT_TIMEOUT_AFTER_READY=1" script))
      (should (string-match-p "exwm-vr-openxr-smoke" script)))))

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
      (should (string-match-p "exwm-vr-hmd-connector" script))
      (should-not (string-match-p "EWWM_DRM_LEASE_CONNECTORS=DP-2" script)))))

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
      (should (string-match-p (regexp-quote "runtime_name=") script))
      (should (string-match-p (regexp-quote "runtime_library_path=") script))
      (should (string-match-p (regexp-quote "exwm_vr_compositor_service") script))
      (should (string-match-p (regexp-quote "exwm_vr_monado_service") script))
      (should (string-match-p (regexp-quote "/usr/libexec/exwm-vr/hello_xr") script))
      (should (string-match-p (regexp-quote "exwm-vr-hello-xr") script))
      (should (string-match-p (regexp-quote "/usr/local/bin/hello_xr") script))
      (should (string-match-p (regexp-quote "timeout \"$timeout_seconds\"") script))
      (should (string-match-p (regexp-quote "monado_comp_ipc") script))
      (should (string-match-p (regexp-quote "openxr_smoke=p3_session_after_ready_timeout") script))
      (should (string-match-p (regexp-quote "proof_ladder=P3_OPENXR_SESSION") script))
      (should (string-match-p (regexp-quote "visual_observed=") script))
      (should (string-match-p (regexp-quote "EXWM_VR_VISUAL_OBSERVED=yes|no|not_observed") script))
      (should (string-match-p (regexp-quote "EXWM_VR_VISUAL_OBSERVER") script))
      (should (string-match-p (regexp-quote "EXWM_VR_VISUAL_CONFIRMATION=VISIBLE_NON_BLACK") script))
      (should (string-match-p (regexp-quote "EXWM_VR_VISUAL_OBSERVED=yes requires EXWM_VR_VISUAL_OBSERVER") script))
      (should (string-match-p (regexp-quote "visual_first_frame=P4_OBSERVED") script))
      (should (string-match-p (regexp-quote "visual_first_frame=P4_UNOBSERVED") script))
      (should-not (string-match-p (regexp-quote "first frame confirmed") script)))))

(ert-deftest honey-substrate/openxr-smoke-requires-human-confirmation-for-p4 ()
  "P4 observed output should require an explicit human confirmation token."
  (let* ((root (make-temp-file "honey-openxr-smoke-" t))
         (runtime-json (expand-file-name "active_runtime.json" root))
         (runtime-dir (expand-file-name "runtime" root))
         (client (expand-file-name "fake-openxr-client" root)))
    (unwind-protect
        (progn
          (make-directory runtime-dir t)
          (honey-substrate--write-fake-openxr-runtime runtime-json)
          (honey-substrate--write-fake-openxr-client client)
          (let ((missing-confirmation
                 (honey-substrate--run-openxr-smoke
                  (list (concat "XR_RUNTIME_JSON=" runtime-json)
                        (concat "XDG_RUNTIME_DIR=" runtime-dir)
                        (concat "EXWM_VR_OPENXR_CLIENT=" client)
                        "EXWM_VR_VISUAL_OBSERVED=yes")
                  "--timeout" "0")))
            (should (= (car missing-confirmation) 64))
            (should (string-match-p
                     "EXWM_VR_VISUAL_OBSERVED=yes requires"
                     (cdr missing-confirmation))))
          (let ((observed
                 (honey-substrate--run-openxr-smoke
                  (list (concat "XR_RUNTIME_JSON=" runtime-json)
                        (concat "XDG_RUNTIME_DIR=" runtime-dir)
                        (concat "EXWM_VR_OPENXR_CLIENT=" client)
                        "EXWM_VR_VISUAL_OBSERVED=yes"
                        "EXWM_VR_VISUAL_OBSERVER=lab-observer"
                        "EXWM_VR_VISUAL_CONFIRMATION=VISIBLE_NON_BLACK")
                  "--timeout" "0")))
            (should (= (car observed) 0))
            (should (string-match-p "openxr_smoke=passed" (cdr observed)))
            (should (string-match-p "visual_observer=lab-observer" (cdr observed)))
            (should (string-match-p "visual_confirmation=VISIBLE_NON_BLACK" (cdr observed)))
            (should (string-match-p "visual_first_frame=P4_OBSERVED" (cdr observed)))))
      (delete-directory root t))))

(ert-deftest honey-substrate/connector-resolver-prefers-nondesktop-dp ()
  "The HMD resolver prefers a connected DP connector with non_desktop=1."
  (let* ((root (make-temp-file "honey-drm-" t))
         result)
    (unwind-protect
        (progn
          (honey-substrate--make-connector root "card0-DP-1" "connected" "0" #x1234)
          (honey-substrate--make-connector root "card0-DP-2" "connected" "1" nil)
          (setq result (honey-substrate--resolve-fixture root))
          (should (= (car result) 0))
          (should (string= (cdr result) "DP-2")))
      (delete-directory root t))))

(ert-deftest honey-substrate/connector-resolver-accepts-big-edid-products ()
  "The HMD resolver recognizes BIG EDID products 0x1234 and 0x5095."
  (dolist (case '(("card0-DP-1" #x1234 "DP-1")
                  ("card0-DP-2" #x5095 "DP-2")))
    (let* ((root (make-temp-file "honey-drm-" t))
           result)
      (unwind-protect
          (progn
            (honey-substrate--make-connector root (nth 0 case) "connected" "0" (nth 1 case))
            (setq result (honey-substrate--resolve-fixture root))
            (should (= (car result) 0))
            (should (string= (cdr result) (nth 2 case))))
        (delete-directory root t)))))

(ert-deftest honey-substrate/connector-resolver-handles-missing-nondesktop-and-disconnected-dp ()
  "BIG EDID works without non_desktop, but disconnected DP is rejected."
  (let* ((root (make-temp-file "honey-drm-" t))
         result)
    (unwind-protect
        (progn
          (honey-substrate--make-connector root "card0-DP-1" "connected" nil #x1234)
          (setq result (honey-substrate--resolve-fixture root))
          (should (= (car result) 0))
          (should (string= (cdr result) "DP-1")))
      (delete-directory root t)))
  (let* ((root (make-temp-file "honey-drm-" t))
         result)
    (unwind-protect
        (progn
          (honey-substrate--make-connector root "card0-DP-1" "disconnected" "1" #x1234)
          (honey-substrate--make-connector root "card0-HDMI-A-1" "connected" "0" nil)
          (setq result (honey-substrate--resolve-fixture root))
          (should-not (= (car result) 0)))
      (delete-directory root t))))

(ert-deftest honey-substrate/connector-resolver-falls-back-to-explicit-override ()
  "An HDMI-only management display does not become the HMD; explicit DP override is last resort."
  (let* ((root (make-temp-file "honey-drm-" t))
         result)
    (unwind-protect
        (progn
          (honey-substrate--make-connector root "card0-HDMI-A-1" "connected" "0" nil)
          (setq result (honey-substrate--resolve-fixture root "--override" "DP-9"))
          (should (= (car result) 0))
          (should (string= (cdr result) "DP-9")))
      (delete-directory root t))))

(ert-deftest honey-substrate/safe-vr-paths-do-not-carry-stale-honey-defaults ()
  "Read-only/status/P3 smoke paths should not bake in stale Honey connector paths."
  (let* ((setup (honey-substrate--read-file honey-substrate--setup-script))
         (justfile (honey-substrate--read-file honey-substrate--justfile))
         (sway-config (honey-substrate--read-file honey-substrate--sway-config))
         (sway-status (honey-substrate--read-file honey-substrate--sway-status))
         (first-frame (honey-substrate--section
                       setup "^cmd_beyond_first_frame()"
                       "^cmd_openxr_build()")))
    (dolist (content (list setup justfile sway-config sway-status))
      (should-not (string-match-p "/sys/kernel/debug/dri/1/" content))
      (should-not (string-match-p "hidraw3" content)))
    (should first-frame)
    (should-not (string-match-p "pkill -f monado-service" first-frame))
    (should-not (string-match-p "first frame confirmed" first-frame))
    (should (string-match-p "P3" first-frame))
    (should (string-match-p "P4" first-frame))))

(ert-deftest honey-substrate/beyond-power-on-uses-correct-hid-command-slot ()
  "The packaged Beyond helper should put SetWorkState at byte[1]."
  (with-temp-buffer
    (insert-file-contents honey-substrate--beyond-power-script)
    (let ((script (buffer-string)))
      (should (string-match-p (regexp-quote "POWER_ON_REPORT_ID = 0x00") script))
      (should (string-match-p (regexp-quote "pkt[0] = POWER_ON_REPORT_ID") script))
      (should (string-match-p (regexp-quote "pkt[1] = 0x22") script))
      (should (string-match-p (regexp-quote "pkt[2] = phase") script))
      (should-not (string-match-p (regexp-quote "pkt[2] = 0x22") script)))))

(ert-deftest honey-substrate/openxr-smoke-client-rpm-lane-builds-packaged-client ()
  "The OpenXR smoke client RPM lane should package a repo-managed client path."
  (with-temp-buffer
    (insert-file-contents honey-substrate--openxr-smoke-client-spec)
    (let ((spec (buffer-string)))
      (should (string-match-p (regexp-quote "Name:           exwm-vr-openxr-smoke-client") spec))
      (should (string-match-p (regexp-quote "KhronosGroup/OpenXR-SDK-Source") spec))
      (should (string-match-p (regexp-quote "PRESENTATION_BACKEND=xlib") spec))
      (should (string-match-p (regexp-quote "-DCMAKE_SKIP_RPATH=ON") spec))
      (should (string-match-p (regexp-quote "../libexec/%{project_name}/hello_xr") spec))
      (should (string-match-p (regexp-quote "%{_libexecdir}/%{project_name}/hello_xr") spec))
      (should (string-match-p (regexp-quote "%{_bindir}/exwm-vr-hello-xr") spec))
      (should (string-match-p (regexp-quote "openxr-libs") spec))
      (should (string-match-p (regexp-quote "vulkan-loader") spec))))
  (with-temp-buffer
    (insert-file-contents honey-substrate--openxr-smoke-client-workflow)
    (let ((workflow (buffer-string)))
      (should (string-match-p (regexp-quote "container: rockylinux/rockylinux:10") workflow))
      (should (string-match-p (regexp-quote "packaging/rpm/exwm-vr-openxr-smoke-client.spec") workflow))
      (should (string-match-p (regexp-quote "OpenXR-SDK-Source-${SDK_COMMIT}.tar.gz") workflow))
      (should (string-match-p (regexp-quote "exwm-vr-openxr-smoke-client-[0-9]*.rpm") workflow))
      (should (string-match-p (regexp-quote "/usr/bin/exwm-vr-hello-xr") workflow))
      (should (string-match-p (regexp-quote "/usr/bin/exwm-vr-hello-xr -> ../libexec/exwm-vr/hello_xr") workflow))
      (should (string-match-p (regexp-quote "/usr/libexec/exwm-vr/hello_xr") workflow))
      (should (string-match-p (regexp-quote "RPATH|RUNPATH") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debuginfo-*.rpm'") workflow))
      (should (string-match-p (regexp-quote "! -name '*-debugsource-*.rpm'") workflow)))))

(ert-deftest honey-substrate/exwm-vr-monado-package-installs-openxr-smoke-wrapper ()
  "The opt-in Monado subpackage should carry the repo-owned OpenXR smoke wrapper."
  (with-temp-buffer
    (insert-file-contents honey-substrate--exwm-vr-spec)
    (let ((spec (buffer-string)))
      (should (string-match-p (regexp-quote "Requires:       bash") spec))
      (should (string-match-p (regexp-quote "Requires:       coreutils") spec))
      (should (string-match-p (regexp-quote "packaging/scripts/exwm-vr-openxr-smoke") spec))
      (should (string-match-p (regexp-quote "packaging/scripts/exwm-vr-hmd-connector") spec))
      (should (string-match-p (regexp-quote "packaging/scripts/exwm-vr-kernel-dsc-truth") spec))
      (should (string-match-p (regexp-quote "%{_libexecdir}/%{project_name}/openxr-smoke") spec))
      (should (string-match-p (regexp-quote "%{_libexecdir}/%{project_name}/hmd-connector") spec))
      (should (string-match-p (regexp-quote "%{_libexecdir}/%{project_name}/kernel-dsc-truth") spec)))))

(ert-deftest honey-substrate/kernel-dsc-truth-helper-is-read-only ()
  "The kernel DSC truth helper should verify patches without mutating the display."
  (with-temp-buffer
    (insert-file-contents honey-substrate--kernel-dsc-truth-script)
    (let ((script (buffer-string)))
      (should (string-match-p (regexp-quote "qp_table_444_8bpc_max") script))
      (should (string-match-p (regexp-quote "qp_table_444_8bpc_min") script))
      (should (string-match-p (regexp-quote "drm_parse_vesa_specific_block") script))
      (should (string-match-p (regexp-quote "dsc_bpp_x16") script))
      (should (string-match-p (regexp-quote "connected_dp_edids") script))
      (should (string-match-p (regexp-quote "default=\"auto\"") script))
      (should (string-match-p (regexp-quote "resolve_connector") script))
      (should (string-match-p (regexp-quote "dsc_pic_parameter_set") script))
      (should (string-match-p (regexp-quote "PPS_RC_RANGE_OFFSET = 58") script))
      (should (string-match-p (regexp-quote "decode_rc_range") script))
      (should (string-match-p (regexp-quote "min_qp") script))
      (should (string-match-p (regexp-quote "bpg_offset") script))
      (should (string-match-p (regexp-quote "pps_rc_ranges_bpp8_444_patched") script))
      (should (string-match-p (regexp-quote "dpcd_registers") script))
      (should-not (string-match-p (regexp-quote "link_settings\" >") script))
      (should-not (string-match-p (regexp-quote "dsc_bits_per_pixel\" >") script))
      (should-not (string-match-p (regexp-quote "dsc_disable_passthrough\" >") script))
      (should-not (string-match-p (regexp-quote "systemctl") script))
      (should-not (string-match-p (regexp-quote "pkill") script))
      (should-not (string-match-p (regexp-quote "detach_kernel_driver") script)))))

(ert-deftest honey-substrate/kernel-dsc-truth-decodes-pps-debugfs-hex ()
  "The DSC truth helper should decode the PPS hex emitted by the debugfs patch."
  (skip-unless (executable-find "python3"))
  (let ((code
         (concat
          "from importlib.machinery import SourceFileLoader\n"
          "import sys\n"
          "mod = SourceFileLoader('kernel_dsc_truth', sys.argv[1]).load_module()\n"
          "pps = bytearray(128)\n"
          "pps[0] = 0x12\n"
          "pps[3] = 0x88\n"
          "pps[4] = 0x20\n"
          "pps[5] = 0x80\n"
          "for index, (min_qp, max_qp, bpg) in enumerate(zip(\n"
          "    mod.QP_MIN_BPP8_PATCHED,\n"
          "    mod.QP_MAX_BPP8_PATCHED,\n"
          "    mod.RC_BPG_OFFSET_BPP8_444_PATCHED,\n"
          "    strict=True,\n"
          ")):\n"
          "    word = mod.encode_rc_range(min_qp, max_qp, bpg)\n"
          "    offset = mod.PPS_RC_RANGE_OFFSET + index * 2\n"
          "    pps[offset] = word >> 8\n"
          "    pps[offset + 1] = word & 0xff\n"
          "raw_text = ' '.join(f'{byte:02x}' for byte in pps).encode() + b'\\n'\n"
          "parsed = mod.pps_bytes(raw_text)\n"
          "assert parsed == bytes(pps)\n"
          "decoded = mod.decode_pps(parsed)\n"
          "assert decoded['bits_per_component'] == 8\n"
          "assert decoded['bits_per_pixel_x16'] == 128\n"
          "assert decoded['rc_ranges'] == mod.expected_rc_ranges(\n"
          "    mod.QP_MAX_BPP8_PATCHED,\n"
          "    mod.QP_MIN_BPP8_PATCHED,\n"
          "    mod.RC_BPG_OFFSET_BPP8_444_PATCHED,\n"
          ")\n")))
    (with-temp-buffer
      (should (= 0 (call-process "python3" nil t nil
                                 "-c" code honey-substrate--kernel-dsc-truth-script))))))

(ert-deftest honey-substrate/kernel-dsc-truth-uses-sops-sudo-wrapper ()
  "Remote PPS/kernel truth targets should use the Honey SOPS sudo wrapper."
  (with-temp-buffer
    (insert-file-contents honey-substrate--justfile)
    (let* ((justfile (buffer-string))
           (pps-target (honey-substrate--section
                        justfile
                        "^beyond-kernel-pps"
                        "^# Run the read-only kernel/driver DSC truth helper"))
           (truth-target (honey-substrate--section
                          justfile
                          "^honey-kernel-dsc-truth"
                          "^# ── dev")))
      (should (string-match-p (regexp-quote "kernel_dsc_truth_script") justfile))
      (should (string-match-p (regexp-quote "just honey-kernel-dsc-truth") pps-target))
      (should (string-match-p (regexp-quote "exwm-vr-kernel-dsc-truth") truth-target))
      (should (string-match-p (regexp-quote "just honey-sudo-run {{host}}") truth-target))
      (should-not (string-match-p (regexp-quote "sudo -S") pps-target))
      (should-not (string-match-p (regexp-quote "BECOME_PASSWORD_FILE") pps-target)))))

(ert-deftest honey-substrate/amdgpu-pps-debugfs-patch-is-read-only ()
  "The optional kernel PPS patch should expose cached PPS bytes without mutation."
  (with-temp-buffer
    (insert-file-contents honey-substrate--amdgpu-dsc-pps-debugfs-patch)
    (let ((patch (buffer-string)))
      (should (string-match-p (regexp-quote "dsc_pic_parameter_set") patch))
      (should (string-match-p (regexp-quote "stream->dsc_packed_pps") patch))
      (should (string-match-p (regexp-quote "simple_read_from_buffer") patch))
      (should (string-match-p (regexp-quote "dp_dsc_pic_parameter_set_read") patch))
      (should-not (string-match-p (regexp-quote ".write = dp_dsc_pic_parameter_set") patch))
      (should-not (string-match-p (regexp-quote "link_settings") patch))
      (should-not (string-match-p (regexp-quote "dc_link_set_preferred_training_settings") patch)))))

(ert-deftest honey-substrate/pps-runbook-pins-diagnostic-artifact-payload ()
  "The Honey PPS runbook should pin the inspected diagnostic RPM artifact."
  (with-temp-buffer
    (insert-file-contents honey-substrate--pps-runbook)
    (let ((runbook (buffer-string)))
      (should (string-match-p (regexp-quote "Artifact ID: `6937251018`") runbook))
      (should (string-match-p (regexp-quote "Artifact name: `kernel-xr-rpms-generic`") runbook))
      (should (string-match-p (regexp-quote "Realized RPM release: `6.19.5-12.xr.el10`") runbook))
      (should (string-match-p (regexp-quote "kernel-xr-6.19.5-12.xr.el10.x86_64.rpm") runbook))
      (should (string-match-p (regexp-quote "57f469a4b2ddebd5097c22606e1671eaa54a7860ff40feeca1e31ee09acfc2cb") runbook))
      (should (string-match-p (regexp-quote "53f4b02bb89bbb905b96c50a274aa800f7529ad452b63b1dc22ccd8f8fbe93f0") runbook))
      (should (string-match-p (regexp-quote "489c1406734856eb8edab3ec78d0e198265a9eede732612eab34bc11a694f16d") runbook))
      (should (string-match-p (regexp-quote "Do not install a kernel") runbook))
      (should (string-match-p (regexp-quote "without explicit attended operator approval") runbook)))))

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
      (should (string-match-p "^honey-sudo-run host=\"honey\"" justfile))
      (should (string-match-p "^honey-sudo-check host=\"honey\"" justfile))
      (should (string-match-p "^honey-proof-env host=\"honey\"" justfile))
      (should (string-match-p "^honey-openxr-status host=\"honey\"" justfile))
      (should (string-match-p "^honey-openxr-smoke host=\"honey\"" justfile))
      (should (string-match-p (regexp-quote "BECOME_PASSWORD_FILE") justfile))
      (should (string-match-p (regexp-quote ".config/sops-nix/secrets/become/password") justfile))
      (should-not (string-match-p (regexp-quote "echo 'tinyland' | sudo -S") justfile))
      (should-not (string-match-p (regexp-quote "sudo /tmp/exwm-vr-setup") justfile))
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

(ert-deftest honey-substrate/honey-openxr-status-uses-plain-read-only-ssh ()
  "The status-only OpenXR lane should not enter the full remote dev shell."
  (with-temp-buffer
    (insert-file-contents honey-substrate--justfile)
    (let* ((justfile (buffer-string))
           (start (string-match "^honey-openxr-status host=\"honey\":" justfile))
           (end (and start (string-match "^\\[group" justfile (1+ start))))
           (stanza (and start end (substring justfile start end))))
      (should stanza)
      (should (string-match-p (regexp-quote "ssh jess@{{host}} bash -s") stanza))
      (should (string-match-p (regexp-quote "./packaging/scripts/exwm-vr-openxr-smoke --status-only") stanza))
      (should-not (string-match-p (regexp-quote "just honey-run") stanza))
      (should-not (string-match-p (regexp-quote "nix develop") stanza)))))

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
