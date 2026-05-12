;;; emacs-egreg-app-layer-test.el --- Emacs/eGreg app-layer guardrails -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst emacs-egreg-app-layer-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for Emacs/eGreg app-layer tests.")

(defun emacs-egreg-app-layer-test--read-file (relative)
  "Return the contents of RELATIVE from the repository root."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name relative emacs-egreg-app-layer-test--root))
    (buffer-string)))

(ert-deftest emacs-egreg-app-layer/systemd-unit-is-optional-client ()
  "The packaged Emacs/eGreg unit should be an optional app-layer client."
  (let ((service
         (emacs-egreg-app-layer-test--read-file
          "packaging/systemd/exwm-vr-emacs.service"))
        (native-target
         (emacs-egreg-app-layer-test--read-file
          "packaging/systemd/xoxdwm.target"))
        (legacy-target
         (emacs-egreg-app-layer-test--read-file
          "packaging/systemd/exwm-vr.target")))
    (should (string-match-p "Description=XoxdWM Emacs App-Layer Client"
                            service))
    (should (string-match-p "Alias=xoxdwm-emacs\\.service" service))
    (should (string-match-p "WantedBy=exwm-vr\\.target" service))
    (should-not (string-match-p "WantedBy=.*xoxdwm\\.target" service))
    (should-not (string-match-p "PartOf=.*xoxdwm\\.target" service))
    (should (string-match-p "Wants=exwm-vr-compositor\\.service"
                            native-target))
    (should-not (string-match-p "exwm-vr-emacs\\.service" native-target))
    (should (string-match-p
             "Wants=exwm-vr-compositor\\.service exwm-vr-emacs\\.service"
             legacy-target))))

(ert-deftest emacs-egreg-app-layer/service-uses-wayland-app-profile ()
  "The packaged app-layer service should launch Emacs as a Wayland app."
  (let ((service
         (emacs-egreg-app-layer-test--read-file
          "packaging/systemd/exwm-vr-emacs.service"))
        (compat
         (emacs-egreg-app-layer-test--read-file
          "docs/app-compatibility.md")))
    (should (string-match-p
             "ExecStart=/usr/bin/emacs --fg-daemon=exwm-vr --quick --load /usr/share/exwm-vr/exwm-vr-session-init\\.el"
             service))
    (dolist (needle '("Environment=WAYLAND_DISPLAY=wayland-0"
                      "Environment=QT_QPA_PLATFORM=wayland"
                      "Environment=MOZ_ENABLE_WAYLAND=1"
                      "Environment=GDK_BACKEND=wayland,x11"))
      (should (string-match-p needle service)))
    (should (string-match-p "Emacs pgtk / eGreg" compat))
    (should (string-match-p "Preferred app profile: Emacs pgtk on the native Wayland socket"
                            compat))
    (should (string-match-p "Non-pgtk Emacs builds are compatibility clients through XWayland"
                            compat))))

(ert-deftest emacs-egreg-app-layer/session-init-connects-to-native-compositor ()
  "The app-layer bootstrap should connect to an existing native compositor."
  (let ((init
         (emacs-egreg-app-layer-test--read-file
          "packaging/emacs/exwm-vr-session-init.el"))
        (entry
         (emacs-egreg-app-layer-test--read-file "lisp/vr/ewwm.el")))
    (should (string-match-p "(require 'ewwm)" init))
    (should (string-match-p "(ewwm-global-mode 1)" init))
    (should (string-match-p "(ewwm-init t)" entry))
    (should (string-match-p "When SKIP-COMPOSITOR is non-nil" entry))
    (should (string-match-p "skip starting the compositor" entry))
    (should-not (string-match-p "(ewwm--start-compositor)" init))))

(ert-deftest emacs-egreg-app-layer/nix-surfaces-keep-native-session-separate ()
  "Nix module surfaces should keep xoxdwm native and Emacs/eGreg optional."
  (let ((nixos
         (emacs-egreg-app-layer-test--read-file
          "nix/modules/exwm-vr.nix"))
        (home
         (emacs-egreg-app-layer-test--read-file
          "nix/home-manager/exwm-vr.nix")))
    (dolist (surface (list nixos home))
      (should (string-match-p "XoxdWM Emacs App-Layer Client" surface))
      (should (string-match-p "xoxdwm-emacs\\.service" surface))
      (should (string-match-p "ewwm-emacs\\.service" surface))
      (should (string-match-p "systemd\\.user\\.targets\\.\"xoxdwm\"" surface))
      (should (string-match-p "exwm-vr-compositor\\.service" surface)))
    (should (string-match-p "requires = \\[ \"exwm-vr-compositor\\.service\" \\]"
                            nixos))
    (should (string-match-p "Wants = \\[ \"exwm-vr-compositor\\.service\" \\]"
                            home))))

(ert-deftest emacs-egreg-app-layer/proof-command-is-advertised ()
  "The focused proof lane should be easy to run and documented."
  (let ((justfile (emacs-egreg-app-layer-test--read-file "justfile"))
        (contract
         (emacs-egreg-app-layer-test--read-file
          "docs/emacs-egreg-app-layer.md"))
        (status
         (emacs-egreg-app-layer-test--read-file "docs/status.md")))
    (should (string-match-p "^emacs-egreg-app-layer-test:" justfile))
    (should (string-match-p "test/emacs-egreg-app-layer-test\\.el" justfile))
    (should (string-match-p "just emacs-egreg-app-layer-test" contract))
    (should (string-match-p "GitHub #48 is covered by the Emacs/eGreg app-layer contract"
                            status))
    (should-not (string-match-p "remaining authority-adjacent product gate.*#48"
                                status))))

(provide 'emacs-egreg-app-layer-test)
;;; emacs-egreg-app-layer-test.el ends here
