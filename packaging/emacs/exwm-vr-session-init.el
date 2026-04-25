;;; exwm-vr-session-init.el --- Dedicated EXWM-VR session bootstrap  -*- lexical-binding: t; -*-

;; This file is loaded by the packaged Rocky session so Emacs starts the
;; window-manager surface directly instead of inheriting an unrelated user
;; init.el.  Optional host/user config is loaded from EXWM_VR_CONFIG_FILE or
;; ~/.config/exwm-vr/config.el before enabling ewwm.

;;; Code:

(let* ((session-file (or load-file-name buffer-file-name))
       (session-dir (and session-file (file-name-directory session-file)))
       (packaged-load-paths
        (when session-dir
          (mapcar
           (lambda (subdir)
             (expand-file-name
              (format "../emacs/site-lisp/exwm-vr/%s" subdir)
              session-dir))
           '("core" "vr" "ext")))))
  (dolist (path packaged-load-paths)
    (when (file-directory-p path)
      (add-to-list 'load-path path))))

(let* ((config-file
        (or (getenv "EXWM_VR_CONFIG_FILE")
            (expand-file-name
             "config.el"
             (or (getenv "EXWM_VR_CONFIG_DIR")
                 (expand-file-name "~/.config/exwm-vr"))))))
  (when (file-readable-p config-file)
    (load config-file nil 'nomessage)))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(require 'ewwm)

;; The compositor is managed by systemd, but allow extra time for its IPC
;; socket to appear before ewwm tries to connect on real Rocky hosts.
(setq ewwm-compositor-startup-timeout
      (max ewwm-compositor-startup-timeout 30))

(unless (bound-and-true-p ewwm-global-mode)
  (ewwm-global-mode 1))

(provide 'exwm-vr-session-init)
;;; exwm-vr-session-init.el ends here
