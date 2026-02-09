;;; exwm-workspace-test.el --- Tests for exwm-workspace  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest exwm-workspace-test/file-exists ()
  "exwm-workspace.el exists in the project."
  (let ((file (expand-file-name "exwm-workspace.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (should (file-exists-p file))))

(ert-deftest exwm-workspace-test/defines-workspace-number ()
  "exwm-workspace.el defines exwm-workspace-number defcustom."
  (let ((file (expand-file-name "exwm-workspace.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defcustom exwm-workspace-number" (buffer-string))))))

(ert-deftest exwm-workspace-test/defines-workspace-list ()
  "exwm-workspace.el defines exwm-workspace--list."
  (let ((file (expand-file-name "exwm-workspace.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defvar exwm-workspace--list" (buffer-string))))))

(ert-deftest exwm-workspace-test/defines-switch ()
  "exwm-workspace.el defines exwm-workspace-switch."
  (let ((file (expand-file-name "exwm-workspace.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defun exwm-workspace-switch " (buffer-string))))))

;;; exwm-workspace-test.el ends here
