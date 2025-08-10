(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(let ((test-dir (expand-file-name "exec-sql-parser" (file-name-directory load-file-name))))
  (dolist (test-file (directory-files test-dir t "\\.el$"))
    (load test-file nil 'nomessage)))

(let ((ert-quiet t))
  (ert-run-tests-batch-and-exit))
