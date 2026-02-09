;;; exwm-core-test.el --- Tests for exwm-core  -*- lexical-binding: t -*-

;;; Commentary:
;; Unit tests for EXWM core definitions.
;; These tests verify the module loads and core data structures are correct
;; without requiring an X connection.

;;; Code:

(require 'ert)

;; We can't load exwm-core directly since it requires xcb (XELB).
;; Instead, test what we can about module structure and definitions
;; that are available after a controlled load attempt.

(ert-deftest exwm-core-test/file-exists ()
  "exwm-core.el exists in the project."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (should (file-exists-p core-file))))

(ert-deftest exwm-core-test/file-has-lexical-binding ()
  "exwm-core.el uses lexical binding."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file nil 0 200)
      (should (string-match-p "lexical-binding: t" (buffer-string))))))

(ert-deftest exwm-core-test/provides-feature ()
  "exwm-core.el provides the exwm-core feature."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "(provide 'exwm-core)" (buffer-string))))))

(ert-deftest exwm-core-test/defines-id-buffer-alist ()
  "exwm-core.el defines exwm--id-buffer-alist."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar exwm--id-buffer-alist" (buffer-string))))))

(ert-deftest exwm-core-test/defines-exwm-mode ()
  "exwm-core.el defines exwm-mode as a derived mode."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "define-derived-mode exwm-mode" (buffer-string))))))

(ert-deftest exwm-core-test/defines-connection-var ()
  "exwm-core.el defines exwm--connection variable."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar exwm--connection" (buffer-string))))))

(ert-deftest exwm-core-test/defines-root-var ()
  "exwm-core.el defines exwm--root variable."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar exwm--root" (buffer-string))))))

(ert-deftest exwm-core-test/defines-class-name ()
  "exwm-core.el defines exwm-class-name buffer-local."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar-local exwm-class-name" (buffer-string))))))

(ert-deftest exwm-core-test/defines-title ()
  "exwm-core.el defines exwm-title buffer-local."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar-local exwm-title" (buffer-string))))))

(ert-deftest exwm-core-test/defines-mode-map ()
  "exwm-core.el defines exwm-mode-map keymap."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "defvar-keymap exwm-mode-map" (buffer-string))))))

(ert-deftest exwm-core-test/requires-xcb ()
  "exwm-core.el requires xcb (XELB)."
  (let ((core-file (expand-file-name "exwm-core.el"
                                     (file-name-directory
                                      (directory-file-name
                                       (file-name-directory
                                        (or load-file-name buffer-file-name)))))))
    (with-temp-buffer
      (insert-file-contents core-file)
      (should (string-match-p "(require 'xcb)" (buffer-string))))))

;;; exwm-core-test.el ends here
