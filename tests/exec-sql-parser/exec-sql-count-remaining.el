(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-count-remaining-basic ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "complex.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (should (= 5 (exec-sql-count-remaining)))
    (goto-char (point-min))
    (forward-line 15)
    (let ((start (point)))
      (goto-char (point-max))
      (push-mark (point) t t)
      (goto-char start)
      (should (= 2 (exec-sql-count-remaining))))))

(ert-deftest exec-sql-count-remaining-oracle+addtl ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (should (= 6 (exec-sql-count-remaining)))))

(provide 'exec-sql-count-remaining-test)
