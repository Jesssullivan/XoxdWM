;;; ewwm-qutebrowser-feature-test.el --- Tests for qutebrowser feature modules  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-qutebrowser)
(require 'ewwm-qutebrowser-ipc)
(require 'ewwm-qutebrowser-theme)
(require 'ewwm-qutebrowser-consult)
(require 'ewwm-qutebrowser-downloads)
(require 'ewwm-qutebrowser-reader)
(require 'ewwm-qutebrowser-adblock)
(require 'ewwm-qutebrowser-userscript)
(require 'ewwm-qutebrowser-gaze)

;; Forward-declare functions from feature modules
(declare-function ewwm-qutebrowser-theme--extract-colors
                  "ewwm-qutebrowser-theme")
(declare-function ewwm-qutebrowser-theme--generate-python
                  "ewwm-qutebrowser-theme")
(declare-function ewwm-qutebrowser-consult--read-bookmarks
                  "ewwm-qutebrowser-consult")
(declare-function ewwm-qutebrowser-consult--read-quickmarks
                  "ewwm-qutebrowser-consult")
(declare-function ewwm-qutebrowser-downloads--update-mode-line
                  "ewwm-qutebrowser-downloads")
(declare-function ewwm-qutebrowser-reader--generate-css
                  "ewwm-qutebrowser-reader")
(declare-function ewwm-qutebrowser-gaze-follow-toggle
                  "ewwm-qutebrowser-gaze")

;; Forward-declare dynamic variables for `let' bindings
(defvar ewwm-qutebrowser-reader-font-size)
(defvar ewwm-qutebrowser-reader-dark-mode)
(defvar ewwm-qutebrowser-reader-line-spacing)
(defvar ewwm-qutebrowser-reader-max-width)
(defvar ewwm-qutebrowser-downloads--active)
(defvar ewwm-qutebrowser-downloads--mode-line-string)
(defvar ewwm-qutebrowser-download-mode-line)
(defvar ewwm-qutebrowser-gaze-follow-enable)
(defvar ewwm-qutebrowser-consult-bookmark-file)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest ewwm-qb-feat/theme-provides-feature ()
  "ewwm-qutebrowser-theme provides its feature."
  (should (featurep 'ewwm-qutebrowser-theme)))

(ert-deftest ewwm-qb-feat/consult-provides-feature ()
  "ewwm-qutebrowser-consult provides its feature."
  (should (featurep 'ewwm-qutebrowser-consult)))

(ert-deftest ewwm-qb-feat/downloads-provides-feature ()
  "ewwm-qutebrowser-downloads provides its feature."
  (should (featurep 'ewwm-qutebrowser-downloads)))

(ert-deftest ewwm-qb-feat/reader-provides-feature ()
  "ewwm-qutebrowser-reader provides its feature."
  (should (featurep 'ewwm-qutebrowser-reader)))

(ert-deftest ewwm-qb-feat/adblock-provides-feature ()
  "ewwm-qutebrowser-adblock provides its feature."
  (should (featurep 'ewwm-qutebrowser-adblock)))

(ert-deftest ewwm-qb-feat/userscript-provides-feature ()
  "ewwm-qutebrowser-userscript provides its feature."
  (should (featurep 'ewwm-qutebrowser-userscript)))

(ert-deftest ewwm-qb-feat/gaze-provides-feature ()
  "ewwm-qutebrowser-gaze provides its feature."
  (should (featurep 'ewwm-qutebrowser-gaze)))

;; ── Theme extraction ────────────────────────────────────────

(ert-deftest ewwm-qb-feat/theme-extract-returns-alist ()
  "extract-colors returns an alist."
  (let ((colors (ewwm-qutebrowser-theme--extract-colors)))
    (should (listp colors))
    (should (> (length colors) 0))
    (should (assoc "bg" colors))))

(ert-deftest ewwm-qb-feat/theme-extract-has-expected-keys ()
  "extract-colors includes bg, fg, modeline-bg, keyword."
  (let ((colors (ewwm-qutebrowser-theme--extract-colors)))
    (should (assoc "bg" colors))
    (should (assoc "fg" colors))
    (should (assoc "modeline-bg" colors))
    (should (assoc "keyword" colors))))

(ert-deftest ewwm-qb-feat/theme-generate-python-string ()
  "generate-python returns a string with config lines."
  (let ((colors (ewwm-qutebrowser-theme--extract-colors)))
    (let ((python (ewwm-qutebrowser-theme--generate-python colors)))
      (should (stringp python))
      (should (string-match-p "c\\.colors\\." python))
      (should (string-match-p "Auto-generated" python)))))

;; ── Consult sources ─────────────────────────────────────────

(ert-deftest ewwm-qb-feat/consult-bookmark-reader-exists ()
  "Bookmark reader function exists."
  (should (fboundp 'ewwm-qutebrowser-consult--read-bookmarks)))

(ert-deftest ewwm-qb-feat/consult-quickmark-reader-exists ()
  "Quickmark reader function exists."
  (should (fboundp 'ewwm-qutebrowser-consult--read-quickmarks)))

(ert-deftest ewwm-qb-feat/consult-bookmark-returns-nil-no-file ()
  "Bookmark reader returns nil when file does not exist."
  (let ((ewwm-qutebrowser-consult-bookmark-file "/nonexistent-xyz"))
    (should-not (ewwm-qutebrowser-consult--read-bookmarks))))

;; ── Download tracking ───────────────────────────────────────

(ert-deftest ewwm-qb-feat/downloads-mode-line-empty-when-no-active ()
  "Mode-line string is empty when no downloads active."
  (let ((ewwm-qutebrowser-downloads--active nil)
        (ewwm-qutebrowser-downloads--mode-line-string "")
        (ewwm-qutebrowser-download-mode-line t))
    (ewwm-qutebrowser-downloads--update-mode-line)
    (should (equal ewwm-qutebrowser-downloads--mode-line-string ""))))

(ert-deftest ewwm-qb-feat/downloads-mode-line-shows-count ()
  "Mode-line string shows DL count when downloads active."
  (let ((ewwm-qutebrowser-downloads--active
         (list (cons 1 (list :url "http://x" :state 'active))
               (cons 2 (list :url "http://y" :state 'active))))
        (ewwm-qutebrowser-downloads--mode-line-string "")
        (ewwm-qutebrowser-download-mode-line t))
    (ewwm-qutebrowser-downloads--update-mode-line)
    (should (string-match-p "DL:2"
                            ewwm-qutebrowser-downloads--mode-line-string))))

(ert-deftest ewwm-qb-feat/downloads-list-interactive ()
  "ewwm-qutebrowser-downloads-list is interactive."
  (should (commandp 'ewwm-qutebrowser-downloads-list)))

;; ── Reader CSS generation ───────────────────────────────────

(ert-deftest ewwm-qb-feat/reader-css-contains-font-size ()
  "Generated CSS contains the font-size value."
  (let ((ewwm-qutebrowser-reader-font-size 22)
        (ewwm-qutebrowser-reader-dark-mode t)
        (ewwm-qutebrowser-reader-line-spacing 1.6)
        (ewwm-qutebrowser-reader-max-width "60ch"))
    (let ((css (ewwm-qutebrowser-reader--generate-css)))
      (should (string-match-p "font-size:22px" css)))))

(ert-deftest ewwm-qb-feat/reader-css-dark-mode ()
  "Generated CSS uses dark background when dark-mode is t."
  (let ((ewwm-qutebrowser-reader-font-size 18)
        (ewwm-qutebrowser-reader-dark-mode t)
        (ewwm-qutebrowser-reader-line-spacing 1.8)
        (ewwm-qutebrowser-reader-max-width "60ch"))
    (let ((css (ewwm-qutebrowser-reader--generate-css)))
      (should (string-match-p "#1a1a2e" css)))))

;; ── Adblock config ──────────────────────────────────────────

(ert-deftest ewwm-qb-feat/adblock-method-defcustom ()
  "Default adblock method is both."
  (should (eq (default-value 'ewwm-qutebrowser-adblock-method) 'both)))

(ert-deftest ewwm-qb-feat/adblock-lists-defcustom ()
  "Default adblock lists include easylist."
  (let ((lists (default-value 'ewwm-qutebrowser-adblock-lists)))
    (should (listp lists))
    (should (cl-some (lambda (url) (string-match-p "easylist" url))
                     lists))))

;; ── Userscript management ───────────────────────────────────

(ert-deftest ewwm-qb-feat/userscript-list-installed-exists ()
  "ewwm-qutebrowser-userscript--list-installed is defined."
  (should (fboundp 'ewwm-qutebrowser-userscript--list-installed)))

(ert-deftest ewwm-qb-feat/userscript-ensure-dir-exists ()
  "ewwm-qutebrowser-userscript--ensure-dir is defined."
  (should (fboundp 'ewwm-qutebrowser-userscript--ensure-dir)))

;; ── Gaze config ─────────────────────────────────────────────

(ert-deftest ewwm-qb-feat/gaze-scroll-speed-defcustom ()
  "Default scroll speed is 2.0."
  (should (= (default-value 'ewwm-qutebrowser-gaze-scroll-speed) 2.0)))

(ert-deftest ewwm-qb-feat/gaze-follow-dwell-defcustom ()
  "Default follow dwell is 600."
  (should (= (default-value 'ewwm-qutebrowser-gaze-follow-dwell-ms) 600)))

(ert-deftest ewwm-qb-feat/gaze-follow-enable-defcustom ()
  "Default follow enable is t."
  (should (eq (default-value 'ewwm-qutebrowser-gaze-follow-enable) t)))

(ert-deftest ewwm-qb-feat/gaze-follow-toggle-toggles ()
  "follow-toggle flips the enable flag."
  (let ((ewwm-qutebrowser-gaze-follow-enable t))
    (ewwm-qutebrowser-gaze-follow-toggle)
    (should-not ewwm-qutebrowser-gaze-follow-enable)))

;; ── Interactive commands ────────────────────────────────────

(ert-deftest ewwm-qb-feat/theme-sync-interactive ()
  "ewwm-qutebrowser-theme-sync is interactive."
  (should (commandp 'ewwm-qutebrowser-theme-sync)))

(ert-deftest ewwm-qb-feat/reader-mode-interactive ()
  "ewwm-qutebrowser-reader-mode is interactive."
  (should (commandp 'ewwm-qutebrowser-reader-mode)))

(ert-deftest ewwm-qb-feat/adblock-update-interactive ()
  "ewwm-qutebrowser-adblock-update is interactive."
  (should (commandp 'ewwm-qutebrowser-adblock-update)))

(ert-deftest ewwm-qb-feat/gaze-scroll-toggle-interactive ()
  "ewwm-qutebrowser-gaze-scroll-toggle is interactive."
  (should (commandp 'ewwm-qutebrowser-gaze-scroll-toggle)))

;;; ewwm-qutebrowser-feature-test.el ends here
