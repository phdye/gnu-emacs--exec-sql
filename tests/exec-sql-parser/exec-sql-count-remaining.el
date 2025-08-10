(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-count-remaining-basic ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "complex.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (should (= 8 (exec-sql-count-remaining)))
    (goto-char (point-min))
    (forward-line 15)
    (let ((start (point)))
      (goto-char (point-max))
      (push-mark (point) t t)
      (goto-char start)
      (should (= 3 (exec-sql-count-remaining))))))

(ert-deftest exec-sql-count-remaining-oracle+addtl ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (should (= 9 (exec-sql-count-remaining)))))

(ert-deftest exec-sql-count-remaining-comment-toggle ()
  (with-temp-buffer
    (c-mode)
    (insert "/*\nEXEC SQL SELECT 1 FROM dual;\n*/\nEXEC SQL SELECT 1 FROM dual;\n")
    (goto-char (point-min))
    (let ((exec-sql-parser-ignore-comments t))
      (should (= 1 (exec-sql-count-remaining))))
    (goto-char (point-min))
    (let ((exec-sql-parser-ignore-comments nil))
      (should (= 2 (exec-sql-count-remaining))))))

(provide 'exec-sql-count-remaining-test)
