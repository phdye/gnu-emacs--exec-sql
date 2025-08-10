(require 'ert)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))

(let ((test-dir (file-name-directory load-file-name)))
  (load (expand-file-name "exec-sql-parser/test-exec-sql-get-prior.el" test-dir))
  (load (expand-file-name "exec-sql-parser/test-exec-sql-goto-prior.el" test-dir)))

(ert-run-tests-batch-and-exit)
