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
      (should (equal lines '(4 5 6 10 11 14 15 16 18 20))))))

(ert-deftest exec-sql-goto-next-example-file ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (let ((lines '()))
      (while (exec-sql-goto-next)
        (push (line-number-at-pos (point)) lines))
      (setq lines (nreverse lines))
      (should (equal (car lines) 4))
      (should (equal (car (last lines)) 29))
      (should-not (memq 25 lines)))))

(provide 'exec-sql-goto-next-test)
