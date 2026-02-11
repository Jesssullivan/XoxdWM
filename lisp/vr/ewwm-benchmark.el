;;; ewwm-benchmark.el --- Benchmark harness for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Performance benchmarking harness for EWWM subsystems.  Measures IPC
;; round-trip latency, memory usage, frame timing, and input latency.
;; Results can be exported as JSON or CSV, and formatted as Org tables.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)
(require 'json)

(declare-function ewwm-ipc-send "ewwm-ipc")
(declare-function ewwm-ipc-send-sync "ewwm-ipc")
(declare-function ewwm-ipc-connected-p "ewwm-ipc")

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-benchmark nil
  "Benchmark settings for EWWM."
  :group 'ewwm
  :prefix "ewwm-benchmark-")

(defcustom ewwm-benchmark-output-dir
  (expand-file-name "benchmarks/"
                    (or (getenv "XDG_DATA_HOME")
                        "~/.local/share/ewwm"))
  "Directory for benchmark output files."
  :type 'directory
  :group 'ewwm-benchmark)

(defcustom ewwm-benchmark-output-format 'json
  "Output format for benchmark results."
  :type '(choice (const :tag "JSON" json)
                 (const :tag "CSV" csv))
  :group 'ewwm-benchmark)

(defcustom ewwm-benchmark-warmup-iterations 10
  "Number of warmup iterations before measurement."
  :type 'integer
  :group 'ewwm-benchmark)

(defcustom ewwm-benchmark-iterations 100
  "Number of measurement iterations per benchmark."
  :type 'integer
  :group 'ewwm-benchmark)

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-benchmark--results nil
  "Plist of most recent benchmark results.
Keys include :ipc-latency, :memory, :frame-timing, :input-latency.
Each value is a plist of statistics.")

(defvar ewwm-benchmark--running nil
  "Non-nil when a benchmark suite is currently running.")

;; ── Statistics helpers ───────────────────────────────────────

(defun ewwm-benchmark--percentile (sorted-list pct)
  "Return the PCTth percentile from SORTED-LIST.
SORTED-LIST must be sorted ascending.  PCT is a float 0.0-1.0."
  (if (null sorted-list)
      0.0
    (let ((idx (min (1- (length sorted-list))
                    (truncate (* (length sorted-list) pct)))))
      (nth idx sorted-list))))

(defun ewwm-benchmark--compute-stats (samples)
  "Compute statistics from SAMPLES (list of floats).
Returns a plist with :count, :min, :max, :mean, :p50, :p95, :p99."
  (if (null samples)
      (list :count 0 :min 0.0 :max 0.0
            :mean 0.0 :p50 0.0 :p95 0.0 :p99 0.0)
    (let* ((sorted (sort (copy-sequence samples) #'<))
           (count (length sorted))
           (total (apply #'+ sorted)))
      (list :count count
            :min (car sorted)
            :max (car (last sorted))
            :mean (/ total (float count))
            :p50 (ewwm-benchmark--percentile sorted 0.50)
            :p95 (ewwm-benchmark--percentile sorted 0.95)
            :p99 (ewwm-benchmark--percentile sorted 0.99)))))

;; ── IPC latency benchmark ────────────────────────────────────

(defun ewwm-benchmark-ipc-latency (&optional iterations)
  "Measure IPC round-trip latency over ITERATIONS pings.
ITERATIONS defaults to `ewwm-benchmark-iterations'.
Returns a stats plist.  Requires an active IPC connection."
  (interactive
   (list (read-number "Iterations: "
                      ewwm-benchmark-iterations)))
  (let ((n (or iterations ewwm-benchmark-iterations))
        (warmup ewwm-benchmark-warmup-iterations)
        (samples nil))
    ;; Warmup phase
    (dotimes (_ warmup)
      (condition-case nil
          (ewwm-ipc-send-sync '(:type :ping))
        (error nil)))
    ;; Measurement phase
    (dotimes (_ n)
      (let ((start (float-time)))
        (condition-case nil
            (progn
              (ewwm-ipc-send-sync
               `(:type :ping
                 :timestamp ,(truncate (* start 1000))))
              (push (* (- (float-time) start) 1000.0)
                    samples))
          (error nil))))
    (let ((stats (ewwm-benchmark--compute-stats
                  (nreverse samples))))
      (when (called-interactively-p 'interactive)
        (message
         "IPC latency (%d): mean=%.2fms p50=%.2fms p99=%.2fms"
         (plist-get stats :count)
         (plist-get stats :mean)
         (plist-get stats :p50)
         (plist-get stats :p99)))
      stats)))

;; ── Memory benchmark ─────────────────────────────────────────

(defun ewwm-benchmark--read-proc-rss ()
  "Read RSS from /proc/self/status on Linux.
Returns RSS in kilobytes, or nil if unavailable."
  (condition-case nil
      (when (file-readable-p "/proc/self/status")
        (with-temp-buffer
          (insert-file-contents "/proc/self/status")
          (goto-char (point-min))
          (when (re-search-forward
                 "^VmRSS:\\s-*\\([0-9]+\\)" nil t)
            (string-to-number (match-string 1)))))
    (error nil)))

(defvar gc-elapsed-time)
(defvar gcs-done)

(defun ewwm-benchmark--gc-entry-bytes (entry)
  "Compute bytes used from a `garbage-collect' ENTRY.
Handles both Emacs 29 (symbol . size-or-cons) and
Emacs 30 (symbol size used free) formats."
  (cond
   ;; Emacs 30: (type size used free) - 4-element list
   ((and (listp entry) (>= (length entry) 4))
    (* (nth 1 entry) (nth 2 entry)))
   ;; (type size . count) where count is a number
   ((and (consp entry) (consp (cdr entry))
         (numberp (cadr entry)) (numberp (cddr entry)))
    (* (cadr entry) (cddr entry)))
   ;; (type . bytes) simple cons
   ((and (consp entry) (numberp (cdr entry)))
    (cdr entry))
   (t 0)))

(defun ewwm-benchmark--emacs-memory ()
  "Return Emacs memory usage as a plist.
Keys: :gc-count, :gc-elapsed, :heap-bytes, :rss-kb.
Falls back gracefully when /proc is unavailable."
  (let ((gc-stats (garbage-collect)))
    (list :gc-count (if (boundp 'gcs-done) gcs-done 0)
          :gc-elapsed (if (boundp 'gc-elapsed-time)
                          (float gc-elapsed-time)
                        0.0)
          :heap-bytes (cl-reduce
                       #'+
                       (mapcar
                        #'ewwm-benchmark--gc-entry-bytes
                        gc-stats))
          :rss-kb (or (ewwm-benchmark--read-proc-rss) 0)
          :surfaces (length ewwm--surface-buffer-alist)
          :buffers (length (buffer-list)))))

(defun ewwm-benchmark-memory ()
  "Snapshot current memory usage.
Returns a plist of memory metrics.  On Linux, includes RSS
from /proc/self/status."
  (interactive)
  (let ((mem (ewwm-benchmark--emacs-memory)))
    (when (called-interactively-p 'interactive)
      (message
       "Memory: rss=%dKB heap=%dB gc=%d surfaces=%d buffers=%d"
       (plist-get mem :rss-kb)
       (plist-get mem :heap-bytes)
       (plist-get mem :gc-count)
       (plist-get mem :surfaces)
       (plist-get mem :buffers)))
    mem))

;; ── Frame timing benchmark ───────────────────────────────────

(defun ewwm-benchmark-frame-timing (&optional duration-secs)
  "Collect frame timing data from compositor for DURATION-SECS.
DURATION-SECS defaults to 5.  Sends vr-get-frame-timing IPC
and returns stats on frame intervals."
  (interactive
   (list (read-number "Duration (seconds): " 5)))
  (let ((duration (or duration-secs 5))
        (samples nil))
    ;; Query compositor for frame timing snapshots
    (let ((end-time (+ (float-time) duration)))
      (while (< (float-time) end-time)
        (condition-case nil
            (let ((resp (ewwm-ipc-send-sync
                         '(:type :vr-get-frame-timing))))
              (when (eq (plist-get resp :status) :ok)
                (let ((ms (plist-get resp :frame-ms)))
                  (when (numberp ms)
                    (push ms samples)))))
          (error nil))
        (sleep-for 0.016)))
    (let ((stats (ewwm-benchmark--compute-stats
                  (nreverse samples))))
      (when (called-interactively-p 'interactive)
        (message
         "Frame timing (%d): mean=%.2fms p95=%.2fms"
         (plist-get stats :count)
         (plist-get stats :mean)
         (plist-get stats :p95)))
      stats)))

;; ── Input latency benchmark ─────────────────────────────────

(defun ewwm-benchmark-input-latency (&optional iterations)
  "Measure input event processing latency.
ITERATIONS defaults to `ewwm-benchmark-iterations'.
Sends simulated input events via IPC and measures round-trip.
Returns stats plist."
  (interactive
   (list (read-number "Iterations: "
                      ewwm-benchmark-iterations)))
  (let ((n (or iterations ewwm-benchmark-iterations))
        (warmup ewwm-benchmark-warmup-iterations)
        (samples nil))
    ;; Warmup
    (dotimes (_ warmup)
      (condition-case nil
          (ewwm-ipc-send-sync '(:type :input-latency-probe))
        (error nil)))
    ;; Measurement
    (dotimes (_ n)
      (let ((start (float-time)))
        (condition-case nil
            (progn
              (ewwm-ipc-send-sync
               `(:type :input-latency-probe
                 :timestamp ,(truncate (* start 1000))))
              (push (* (- (float-time) start) 1000.0)
                    samples))
          (error nil))))
    (let ((stats (ewwm-benchmark--compute-stats
                  (nreverse samples))))
      (when (called-interactively-p 'interactive)
        (message
         "Input latency (%d): mean=%.2fms p99=%.2fms"
         (plist-get stats :count)
         (plist-get stats :mean)
         (plist-get stats :p99)))
      stats)))

;; ── Run all benchmarks ───────────────────────────────────────

(defun ewwm-benchmark-run-all ()
  "Run all benchmarks and store results.
Returns the combined results plist.  Also writes output to
`ewwm-benchmark-output-dir' in the configured format."
  (interactive)
  (setq ewwm-benchmark--running t)
  (unwind-protect
      (let ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S"))
            (ipc-stats (ewwm-benchmark-ipc-latency))
            (mem-stats (ewwm-benchmark-memory))
            (frame-stats (ewwm-benchmark-frame-timing))
            (input-stats (ewwm-benchmark-input-latency)))
        (setq ewwm-benchmark--results
              (list :timestamp timestamp
                    :ipc-latency ipc-stats
                    :memory mem-stats
                    :frame-timing frame-stats
                    :input-latency input-stats))
        (ewwm-benchmark--write-output ewwm-benchmark--results)
        (when (called-interactively-p 'interactive)
          (message "Benchmarks complete.  Results in %s"
                   ewwm-benchmark-output-dir))
        ewwm-benchmark--results)
    (setq ewwm-benchmark--running nil)))

;; ── Output writing ───────────────────────────────────────────

(defun ewwm-benchmark--ensure-output-dir ()
  "Create the output directory if it does not exist."
  (unless (file-directory-p ewwm-benchmark-output-dir)
    (make-directory ewwm-benchmark-output-dir t)))

(defun ewwm-benchmark--write-output (results)
  "Write RESULTS to the output directory.
Format is determined by `ewwm-benchmark-output-format'."
  (ewwm-benchmark--ensure-output-dir)
  (let* ((ts (plist-get results :timestamp))
         (base (format "benchmark-%s" (or ts "unknown")))
         (ext (if (eq ewwm-benchmark-output-format 'csv)
                  "csv" "json"))
         (path (expand-file-name (format "%s.%s" base ext)
                                 ewwm-benchmark-output-dir)))
    (with-temp-file path
      (if (eq ewwm-benchmark-output-format 'csv)
          (ewwm-benchmark--write-csv results)
        (ewwm-benchmark--write-json results)))
    path))

(defun ewwm-benchmark--write-json (results)
  "Write RESULTS as JSON to the current buffer."
  (insert (json-encode results)))

(defun ewwm-benchmark--write-csv (results)
  "Write RESULTS as CSV to the current buffer."
  (insert "section,metric,value\n")
  (dolist (section '(:ipc-latency :frame-timing :input-latency))
    (let ((stats (plist-get results section))
          (name (substring (symbol-name section) 1)))
      (when stats
        (dolist (key '(:count :min :max :mean :p50 :p95 :p99))
          (let ((val (plist-get stats key)))
            (when val
              (insert (format "%s,%s,%s\n"
                              name
                              (substring (symbol-name key) 1)
                              val))))))))
  ;; Memory section
  (let ((mem (plist-get results :memory)))
    (when mem
      (dolist (key '(:gc-count :gc-elapsed :heap-bytes
                     :rss-kb :surfaces :buffers))
        (let ((val (plist-get mem key)))
          (when val
            (insert (format "memory,%s,%s\n"
                            (substring (symbol-name key) 1)
                            val))))))))

;; ── Org-mode report ──────────────────────────────────────────

(defun ewwm-benchmark--format-stats-row (label stats)
  "Format LABEL and STATS plist as an Org table row."
  (if (null stats)
      (format "| %s | -- | -- | -- | -- | -- | -- |\n" label)
    (format "| %s | %d | %.2f | %.2f | %.2f | %.2f | %.2f |\n"
            label
            (or (plist-get stats :count) 0)
            (or (plist-get stats :min) 0.0)
            (or (plist-get stats :mean) 0.0)
            (or (plist-get stats :p50) 0.0)
            (or (plist-get stats :p95) 0.0)
            (or (plist-get stats :p99) 0.0))))

(defun ewwm-benchmark-report (&optional results)
  "Format RESULTS as an Org-mode table string.
RESULTS defaults to `ewwm-benchmark--results'.
Returns the formatted string.  When called interactively,
displays in a temporary buffer."
  (interactive)
  (let* ((res (or results ewwm-benchmark--results))
         (ts (or (plist-get res :timestamp) "N/A"))
         (lines
          (list
           (format "#+TITLE: EWWM Benchmark Report\n")
           (format "#+DATE: %s\n\n" ts)
           "* Latency (ms)\n\n"
           "| Benchmark | N | Min | Mean | P50 | P95 | P99 |\n"
           "|---+---+---+---+---+---+---|\n"
           (ewwm-benchmark--format-stats-row
            "IPC" (plist-get res :ipc-latency))
           (ewwm-benchmark--format-stats-row
            "Frame" (plist-get res :frame-timing))
           (ewwm-benchmark--format-stats-row
            "Input" (plist-get res :input-latency))
           "\n* Memory\n\n"
           "| Metric | Value |\n"
           "|---+---|\n"))
         (mem (plist-get res :memory)))
    (when mem
      (dolist (pair (list (cons "RSS (KB)" :rss-kb)
                          (cons "Heap (B)" :heap-bytes)
                          (cons "GC count" :gc-count)
                          (cons "GC elapsed (s)" :gc-elapsed)
                          (cons "Surfaces" :surfaces)
                          (cons "Buffers" :buffers)))
        (let ((val (plist-get mem (cdr pair))))
          (push (format "| %s | %s |\n" (car pair) (or val "--"))
                lines))))
    (let ((report (apply #'concat (nreverse lines))))
      (when (called-interactively-p 'interactive)
        (with-current-buffer
            (get-buffer-create "*ewwm-benchmark-report*")
          (erase-buffer)
          (insert report)
          (goto-char (point-min))
          (when (fboundp 'org-mode)
            (org-mode))
          (display-buffer (current-buffer))))
      report)))

(provide 'ewwm-benchmark)
;;; ewwm-benchmark.el ends here
