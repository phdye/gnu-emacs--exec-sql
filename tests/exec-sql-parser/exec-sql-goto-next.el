(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-goto-next-sequence ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "complex.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (let ((lines '()))
      (while (exec-sql-goto-next)
        (push (line-number-at-pos (point)) lines))
      (setq lines (nreverse lines))
      (should (equal lines '(4 5 6 10 11 16 18 20))))))

(ert-deftest exec-sql-goto-next-example-file ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (let ((lines '()))
      (while (exec-sql-goto-next)
        (push (line-number-at-pos (point)) lines))
      (setq lines (nreverse lines))
      (should (equal lines '(4 7 9 13 15 21 23 27 29))))))

(ert-deftest exec-sql-goto-next-execute-block-skips-end ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "basic/oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (search-forward "EXEC SQL EXECUTE")
    (goto-char (match-beginning 0))
    (exec-sql-goto-next)
    (should (looking-at "EXEC SQL INCLUDE"))))

(ert-deftest exec-sql-goto-next-execute-block-skips-end-exec ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "basic/oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (search-forward "EXEC SQL EXECUTE")
    (goto-char (match-beginning 0))
    (exec-sql-goto-next)
    (exec-sql-goto-next)
    (should (looking-at "EXEC SQL EXECUTE IMMEDIATE"))))

(provide 'exec-sql-goto-next-test)
