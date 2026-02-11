;;; ewwm-qutebrowser-consult.el --- Consult sources for qutebrowser  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Bookmark, quickmark, and history integration for qutebrowser
;; using the consult narrowing framework.  Provides consult sources
;; that read qutebrowser data files and SQLite history database.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

(declare-function ewwm-qutebrowser-ipc-send
                  "ewwm-qutebrowser-ipc")
(declare-function consult--multi "consult")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-qutebrowser-consult nil
  "Consult sources for qutebrowser bookmarks and history."
  :group 'ewwm-qutebrowser
  :prefix "ewwm-qutebrowser-consult-")

(defcustom ewwm-qutebrowser-consult-history-limit 1000
  "Maximum number of history entries to load."
  :type 'integer
  :group 'ewwm-qutebrowser-consult)

(defcustom ewwm-qutebrowser-consult-bookmark-file
  "~/.local/share/qutebrowser/bookmarks/urls"
  "Path to the qutebrowser bookmarks file."
  :type 'string
  :group 'ewwm-qutebrowser-consult)

(defcustom ewwm-qutebrowser-consult-quickmark-file
  "~/.config/qutebrowser/quickmarks"
  "Path to the qutebrowser quickmarks file."
  :type 'string
  :group 'ewwm-qutebrowser-consult)

(defcustom ewwm-qutebrowser-consult-history-db
  "~/.local/share/qutebrowser/history.sqlite"
  "Path to the qutebrowser history SQLite database."
  :type 'string
  :group 'ewwm-qutebrowser-consult)

;; ── Internal readers ─────────────────────────────────────────

(defun ewwm-qutebrowser-consult--read-bookmarks ()
  "Parse the qutebrowser bookmarks file.
Each line has the format: URL TITLE.
Returns a list of propertized strings."
  (let ((file (expand-file-name
               ewwm-qutebrowser-consult-bookmark-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (result)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (when (string-match
                     "\\`\\(https?://[^ ]+\\)\
\\(?: \\(.*\\)\\)?\\'"
                     line)
                (let* ((url (match-string 1 line))
                       (title (or (match-string 2 line) url))
                       (cand (format "%s  %s" title url)))
                  (push (propertize cand 'url url)
                        result))))
            (forward-line 1))
          (nreverse result))))))

(defun ewwm-qutebrowser-consult--read-quickmarks ()
  "Parse the qutebrowser quickmarks file.
Each line has the format: NAME URL.
Returns a list of propertized strings."
  (let ((file (expand-file-name
               ewwm-qutebrowser-consult-quickmark-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (let (result)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (when (string-match
                     "\\`\\([^ ]+\\) \\(https?://.*\\)\\'"
                     line)
                (let* ((name (match-string 1 line))
                       (url (match-string 2 line))
                       (cand (format "[%s]  %s" name url)))
                  (push (propertize cand 'url url)
                        result))))
            (forward-line 1))
          (nreverse result))))))

(defun ewwm-qutebrowser-consult--read-history ()
  "Query qutebrowser SQLite history database.
Returns a list of propertized strings, limited by
`ewwm-qutebrowser-consult-history-limit'.
Requires the sqlite3 command-line tool."
  (let ((db (expand-file-name
             ewwm-qutebrowser-consult-history-db)))
    (when (and (file-readable-p db)
               (executable-find "sqlite3"))
      (let* ((query
              (format
               "SELECT url,title FROM History \
ORDER BY atime DESC LIMIT %d;"
               ewwm-qutebrowser-consult-history-limit))
             (output
              (with-temp-buffer
                (call-process "sqlite3" nil t nil
                              "-separator" "|" db query)
                (buffer-string))))
        (when (> (length output) 0)
          (let (result)
            (dolist (line (split-string output "\n" t))
              (when (string-match
                     "\\`\\([^|]+\\)|\\(.*\\)\\'" line)
                (let* ((url (match-string 1 line))
                       (title (match-string 2 line))
                       (title (if (string-empty-p title)
                                  url title))
                       (cand (format "%s  %s" title url)))
                  (push (propertize cand 'url url)
                        result))))
            (nreverse result)))))))

;; ── Consult sources ──────────────────────────────────────────

(defvar ewwm-qutebrowser-consult--source-bookmarks
  `(:name "Bookmarks"
    :narrow ?b
    :category url
    :items
    ,#'ewwm-qutebrowser-consult--read-bookmarks)
  "Consult source for qutebrowser bookmarks.")

(defvar ewwm-qutebrowser-consult--source-quickmarks
  `(:name "Quickmarks"
    :narrow ?q
    :category url
    :items
    ,#'ewwm-qutebrowser-consult--read-quickmarks)
  "Consult source for qutebrowser quickmarks.")

(defvar ewwm-qutebrowser-consult--source-history
  `(:name "History"
    :narrow ?h
    :category url
    :items
    ,#'ewwm-qutebrowser-consult--read-history)
  "Consult source for qutebrowser history.")

;; ── Interactive command ──────────────────────────────────────

(defun ewwm-qutebrowser-consult-open ()
  "Browse qutebrowser bookmarks, quickmarks, and history.
Uses consult multi-source selection and opens the selected
URL in qutebrowser via IPC."
  (interactive)
  (unless (fboundp 'consult--multi)
    (user-error
     "Package `consult' is required for this command"))
  (let ((result
         (consult--multi
          (list
           ewwm-qutebrowser-consult--source-bookmarks
           ewwm-qutebrowser-consult--source-quickmarks
           ewwm-qutebrowser-consult--source-history))))
    (when result
      (let ((selected (car result)))
        (when selected
          (let ((url (get-text-property 0 'url selected)))
            (when (and url
                       (fboundp 'ewwm-qutebrowser-ipc-send))
              (ewwm-qutebrowser-ipc-send
               (format ":open %s" url)))))))))

(provide 'ewwm-qutebrowser-consult)
;;; ewwm-qutebrowser-consult.el ends here
