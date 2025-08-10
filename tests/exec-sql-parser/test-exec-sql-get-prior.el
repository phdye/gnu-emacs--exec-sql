(require 'ert)
(require 'exec-sql-parser)

(ert-deftest exec-sql-get-prior-basic ()
  (with-temp-buffer
    (insert "int a;\nEXEC SQL SELECT * FROM dual;\nint b;\nEXEC SQL COMMIT;\nint c;\n")
    (goto-char (point-max))
    (let ((info (exec-sql-get-prior)))
      (should info)
      (should (equal (plist-get info :start) '(4 . 0)))
      (should (equal (plist-get info :end) '(4 . 15))))))

(ert-deftest exec-sql-get-prior-skip-comments ()
  (with-temp-buffer
    (insert "/*\nEXEC SQL SELECT * FROM dual;\n*/\nEXEC SQL COMMIT;\n")
    (goto-char (point-max))
    (let ((info (exec-sql-get-prior)))
      (should info)
      (should (equal (plist-get info :start) '(4 . 0))))))

(provide 'test-exec-sql-get-prior)
