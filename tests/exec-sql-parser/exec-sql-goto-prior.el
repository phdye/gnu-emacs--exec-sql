(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-goto-prior-sequence ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-max))
    (let ((expected '(29 27 23 21 19 15 13 9 7 4))
          (lines '()))
      (while (exec-sql-goto-prior)
        (push (line-number-at-pos (point)) lines))
      (should (equal (nreverse lines) expected)))))

(provide 'exec-sql-goto-prior-test)
