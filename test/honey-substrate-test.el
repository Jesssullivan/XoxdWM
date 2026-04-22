;;; honey-substrate-test.el --- Honey substrate helper tests -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(defconst honey-substrate--setup-script
  (expand-file-name "packaging/scripts/exwm-vr-setup"
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

(ert-deftest honey-substrate/monado-env-docs-mark-wayland-fallback-honestly ()
  "The setup helper should describe forced Wayland as a fallback, not lease mode."
  (with-temp-buffer
    (insert-file-contents honey-substrate--setup-script)
    (let ((script (buffer-string)))
      (should (string-match-p "Wayland window fallback" script))
      (should-not (string-match-p "Force Wayland DRM lease target" script)))))

;;; honey-substrate-test.el ends here
