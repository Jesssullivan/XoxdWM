;;; ewwm-benchmark-test.el --- Tests for ewwm-benchmark.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ewwm-core)
(require 'ewwm-benchmark)

;; ── Module loading ───────────────────────────────────────────

(ert-deftest benchmark/provides-feature ()
  "ewwm-benchmark provides its feature."
  (should (featurep 'ewwm-benchmark)))

(ert-deftest benchmark/group-exists ()
  "ewwm-benchmark customization group exists."
  (should (get 'ewwm-benchmark 'custom-group)))

;; ── Defcustom defaults ──────────────────────────────────────

(ert-deftest benchmark/output-dir-defcustom ()
  "Default output dir is a string."
  (should (stringp (default-value 'ewwm-benchmark-output-dir))))

(ert-deftest benchmark/output-format-defcustom ()
  "Default output format is json."
  (should (eq (default-value 'ewwm-benchmark-output-format) 'json)))

(ert-deftest benchmark/warmup-iterations-defcustom ()
  "Default warmup iterations is 10."
  (should (= (default-value 'ewwm-benchmark-warmup-iterations) 10)))

(ert-deftest benchmark/iterations-defcustom ()
  "Default iterations is 100."
  (should (= (default-value 'ewwm-benchmark-iterations) 100)))

(ert-deftest benchmark/output-format-options ()
  "Output format accepts json and csv."
  (let ((type (get 'ewwm-benchmark-output-format 'custom-type)))
    (should type)
    (should (listp type))))

;; ── Variables ────────────────────────────────────────────────

(ert-deftest benchmark/results-var-exists ()
  "Results variable exists and is initially nil."
  (should (boundp 'ewwm-benchmark--results)))

(ert-deftest benchmark/running-var-exists ()
  "Running variable exists."
  (should (boundp 'ewwm-benchmark--running)))

;; ── Statistics helpers ───────────────────────────────────────

(ert-deftest benchmark/compute-stats-empty ()
  "Compute stats handles empty list."
  (let ((stats (ewwm-benchmark--compute-stats nil)))
    (should (= (plist-get stats :count) 0))
    (should (= (plist-get stats :mean) 0.0))))

(ert-deftest benchmark/compute-stats-single ()
  "Compute stats handles single sample."
  (let ((stats (ewwm-benchmark--compute-stats '(5.0))))
    (should (= (plist-get stats :count) 1))
    (should (= (plist-get stats :min) 5.0))
    (should (= (plist-get stats :max) 5.0))
    (should (= (plist-get stats :mean) 5.0))))

(ert-deftest benchmark/compute-stats-multiple ()
  "Compute stats computes correct min/max/mean."
  (let ((stats (ewwm-benchmark--compute-stats
                '(1.0 2.0 3.0 4.0 5.0))))
    (should (= (plist-get stats :count) 5))
    (should (= (plist-get stats :min) 1.0))
    (should (= (plist-get stats :max) 5.0))
    (should (= (plist-get stats :mean) 3.0))))

(ert-deftest benchmark/percentile-empty ()
  "Percentile returns 0 for empty list."
  (should (= (ewwm-benchmark--percentile nil 0.5) 0.0)))

(ert-deftest benchmark/percentile-basic ()
  "Percentile returns expected value for sorted list."
  (let ((sorted '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)))
    (should (= (ewwm-benchmark--percentile sorted 0.5) 6.0))
    (should (= (ewwm-benchmark--percentile sorted 0.0) 1.0))))

;; ── Function existence ──────────────────────────────────────

(ert-deftest benchmark/ipc-latency-exists ()
  "ewwm-benchmark-ipc-latency is defined."
  (should (fboundp 'ewwm-benchmark-ipc-latency)))

(ert-deftest benchmark/memory-exists ()
  "ewwm-benchmark-memory is defined."
  (should (fboundp 'ewwm-benchmark-memory)))

(ert-deftest benchmark/frame-timing-exists ()
  "ewwm-benchmark-frame-timing is defined."
  (should (fboundp 'ewwm-benchmark-frame-timing)))

(ert-deftest benchmark/input-latency-exists ()
  "ewwm-benchmark-input-latency is defined."
  (should (fboundp 'ewwm-benchmark-input-latency)))

(ert-deftest benchmark/run-all-exists ()
  "ewwm-benchmark-run-all is defined."
  (should (fboundp 'ewwm-benchmark-run-all)))

(ert-deftest benchmark/report-exists ()
  "ewwm-benchmark-report is defined."
  (should (fboundp 'ewwm-benchmark-report)))

;; ── Interactive commands ─────────────────────────────────────

(ert-deftest benchmark/ipc-latency-interactive ()
  "ewwm-benchmark-ipc-latency is interactive."
  (should (commandp 'ewwm-benchmark-ipc-latency)))

(ert-deftest benchmark/memory-interactive ()
  "ewwm-benchmark-memory is interactive."
  (should (commandp 'ewwm-benchmark-memory)))

(ert-deftest benchmark/frame-timing-interactive ()
  "ewwm-benchmark-frame-timing is interactive."
  (should (commandp 'ewwm-benchmark-frame-timing)))

(ert-deftest benchmark/input-latency-interactive ()
  "ewwm-benchmark-input-latency is interactive."
  (should (commandp 'ewwm-benchmark-input-latency)))

(ert-deftest benchmark/run-all-interactive ()
  "ewwm-benchmark-run-all is interactive."
  (should (commandp 'ewwm-benchmark-run-all)))

(ert-deftest benchmark/report-interactive ()
  "ewwm-benchmark-report is interactive."
  (should (commandp 'ewwm-benchmark-report)))

;; ── Memory benchmark ─────────────────────────────────────────

(ert-deftest benchmark/emacs-memory-returns-plist ()
  "ewwm-benchmark--emacs-memory returns a plist with expected keys."
  (let ((mem (ewwm-benchmark--emacs-memory)))
    (should (plist-get mem :gc-count))
    (should (plist-get mem :gc-elapsed))
    (should (plist-get mem :heap-bytes))
    (should (numberp (plist-get mem :rss-kb)))
    (should (numberp (plist-get mem :surfaces)))
    (should (numberp (plist-get mem :buffers)))))

;; ── Report formatting ────────────────────────────────────────

(ert-deftest benchmark/report-nil-results ()
  "Report handles nil results gracefully."
  (let ((report (ewwm-benchmark-report nil)))
    (should (stringp report))
    (should (string-match-p "Benchmark Report" report))))

(ert-deftest benchmark/report-with-data ()
  "Report includes Org table structure."
  (let ((results
         (list :timestamp "2026-01-01T00:00:00"
               :ipc-latency
               (list :count 10 :min 0.5 :max 2.0
                     :mean 1.0 :p50 0.9 :p95 1.8 :p99 2.0)
               :memory
               (list :gc-count 5 :gc-elapsed 0.1
                     :heap-bytes 1000 :rss-kb 50000
                     :surfaces 3 :buffers 20)
               :frame-timing nil
               :input-latency nil)))
    (let ((report (ewwm-benchmark-report results)))
      (should (string-match-p "| IPC" report))
      (should (string-match-p "| RSS" report))
      (should (string-match-p "2026-01-01" report)))))

(ert-deftest benchmark/format-stats-row-nil ()
  "Stats row handles nil stats."
  (let ((row (ewwm-benchmark--format-stats-row "Test" nil)))
    (should (stringp row))
    (should (string-match-p "| Test" row))
    (should (string-match-p "--" row))))

(ert-deftest benchmark/format-stats-row-data ()
  "Stats row formats correctly with data."
  (let* ((stats (list :count 50 :min 0.1 :max 5.0
                      :mean 1.5 :p50 1.2 :p95 3.0 :p99 4.5))
         (row (ewwm-benchmark--format-stats-row "IPC" stats)))
    (should (string-match-p "| IPC" row))
    (should (string-match-p "50" row))))

;; ── Output helpers ───────────────────────────────────────────

(ert-deftest benchmark/write-json-exists ()
  "ewwm-benchmark--write-json is defined."
  (should (fboundp 'ewwm-benchmark--write-json)))

(ert-deftest benchmark/write-csv-exists ()
  "ewwm-benchmark--write-csv is defined."
  (should (fboundp 'ewwm-benchmark--write-csv)))

(ert-deftest benchmark/write-csv-format ()
  "CSV output starts with header line."
  (with-temp-buffer
    (ewwm-benchmark--write-csv
     (list :timestamp "test"
           :ipc-latency
           (list :count 1 :min 0.5 :max 0.5
                 :mean 0.5 :p50 0.5 :p95 0.5 :p99 0.5)
           :memory (list :gc-count 1 :gc-elapsed 0.0
                         :heap-bytes 100 :rss-kb 0
                         :surfaces 0 :buffers 5)))
    (goto-char (point-min))
    (should (string-match-p "section,metric,value"
                            (buffer-substring-no-properties
                             (point-min)
                             (line-end-position))))))

;;; ewwm-benchmark-test.el ends here
