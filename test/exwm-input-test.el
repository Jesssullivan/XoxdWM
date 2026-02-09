;;; exwm-input-test.el --- Tests for exwm-input  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)

(ert-deftest exwm-input-test/file-exists ()
  "exwm-input.el exists in the project."
  (let ((file (expand-file-name "exwm-input.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (should (file-exists-p file))))

(ert-deftest exwm-input-test/defines-global-keys ()
  "exwm-input.el defines exwm-input--global-keys."
  (let ((file (expand-file-name "exwm-input.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defvar exwm-input--global-keys" (buffer-string))))))

(ert-deftest exwm-input-test/defines-set-key ()
  "exwm-input.el defines exwm-input-set-key as interactive."
  (let ((file (expand-file-name "exwm-input.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defun exwm-input-set-key" (buffer-string))))))

(ert-deftest exwm-input-test/defines-prefix-keys ()
  "exwm-input.el defines exwm-input-prefix-keys defcustom."
  (let ((file (expand-file-name "exwm-input.el"
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory
                                   (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "defcustom exwm-input-prefix-keys" (buffer-string))))))

;;; exwm-input-test.el ends here
