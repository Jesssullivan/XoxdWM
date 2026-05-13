;;; runner-reachability-test.el --- Shared runner proof guard tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(defconst runner-reachability-test--script
  (expand-file-name
   "packaging/scripts/xoxdwm-runner-reachability-truth"
   (expand-file-name ".." (file-name-directory load-file-name))))

(defun runner-reachability-test--run (packet &rest args)
  "Run the proof checker against JSON PACKET with ARGS."
  (let ((file (make-temp-file "xoxdwm-runner-proof-" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert packet))
          (with-temp-buffer
            (let ((status (apply #'call-process
                                 runner-reachability-test--script
                                 nil t nil
                                 (append args (list file)))))
              (list status (buffer-string)))))
      (delete-file file))))

(ert-deftest runner-reachability/blocked-state-is-honest ()
  "Current blocked state should be valid but not proof."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\",\"USE_VR_HARDWARE\":\"false\"},\"repo_runners\":{\"total_count\":0},\"jobs\":[{\"name\":\"Nix Runner Health\",\"requested_label\":\"ubuntu-latest\",\"runner_name\":\"GitHub Actions 2\",\"status\":\"completed\",\"conclusion\":\"success\"}]}")))
    (should (= 0 status))
    (should (string-match-p "runner_reachability=blocked" output))
    (should (string-match-p "GF_SHARED_RUNNERS_REACHABLE is not true" output))))

(ert-deftest runner-reachability/requires-proof-when-requested ()
  "Strict proof mode should reject the normal blocked state."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\"},\"jobs\":[]}"
                "--require-proof")))
    (should (= 64 status))
    (should (string-match-p
             "shared runner proof requires GF_SHARED_RUNNERS_REACHABLE=true"
             output))))

(ert-deftest runner-reachability/rejects-premature-shared-flag ()
  "The shared flag should not be true without assigned tinyland-nix proof."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\",\"GF_SHARED_RUNNERS_REACHABLE\":\"true\"},\"jobs\":[{\"name\":\"Nix Runner Health\",\"requested_label\":\"tinyland-nix\",\"runner_name\":null,\"status\":\"queued\"}]}")))
    (should (= 64 status))
    (should (string-match-p
             "GF_SHARED_RUNNERS_REACHABLE=true but no assigned tinyland-nix job proof was found"
             output))
    (should (string-match-p
             "queued tinyland-nix job with runner_name=null is not proof"
             output))))

(ert-deftest runner-reachability/rejects-hosted-fallback-with-shared-flag ()
  "Hosted success should not be promoted to shared-runner proof."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\",\"GF_SHARED_RUNNERS_REACHABLE\":\"true\"},\"jobs\":[{\"name\":\"Nix Runner Health\",\"labels\":[\"ubuntu-latest\"],\"runner_name\":\"GitHub Actions 1000040848\",\"status\":\"completed\",\"conclusion\":\"success\"}]}")))
    (should (= 64 status))
    (should (string-match-p
             "GF_SHARED_RUNNERS_REACHABLE=true but no assigned tinyland-nix job proof was found"
             output))))

(ert-deftest runner-reachability/accepts-assigned-tinyland-nix-proof ()
  "An assigned shared tinyland-nix job packet should count as proof."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\",\"GF_SHARED_RUNNERS_REACHABLE\":\"true\"},\"jobs\":[{\"name\":\"Nix Runner Health\",\"requested_label\":\"tinyland-nix\",\"runner_name\":\"tinyland-nix-runner-abc\",\"status\":\"in_progress\",\"nix_health\":\"success\",\"cache_check\":\"success\"}]}")))
    (should (= 0 status))
    (should (string-match-p "runner_reachability=proved" output))))

(ert-deftest runner-reachability/rejects-repo-shaped-runner-proof ()
  "Repo-shaped xoxdwm runners should not satisfy the shared lane contract."
  (pcase-let ((`(,status ,output)
               (runner-reachability-test--run
                "{\"variables\":{\"USE_SELFHOSTED\":\"true\",\"GF_SHARED_RUNNERS_REACHABLE\":\"true\"},\"jobs\":[{\"name\":\"Nix Runner Health\",\"requested_label\":\"tinyland-nix\",\"runner_name\":\"xoxdwm-nix-runner-1\",\"status\":\"in_progress\",\"nix_health\":\"success\"}]}")))
    (should (= 64 status))
    (should (string-match-p
             "repo-shaped xoxdwm runner is not compliant tinyland-nix proof"
             output))))

(provide 'runner-reachability-test)
;;; runner-reachability-test.el ends here
