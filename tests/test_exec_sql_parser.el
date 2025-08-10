(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(let ((test-dir (file-name-directory load-file-name)))
  (dolist (test-file '("exec-sql-parser/test-exec-sql-get-prior.el"
                      "exec-sql-parser/test-exec-sql-goto-prior.el"
                      "exec-sql-parser/test-exec-sql-goto-next.el"
                      "exec-sql-parser/test-exec-sql-count-remaining.el"
                      "exec-sql-parser/test-exec-sql-load-registry.el"))
    (load (expand-file-name test-file test-dir))))

(ert-run-tests-batch-and-exit)
