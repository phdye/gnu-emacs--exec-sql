(require 'ert)
(require 'exec-sql-parser)

(ert-deftest exec-sql-goto-prior-move ()
  (with-temp-buffer
    (insert "int a;\nEXEC SQL SELECT * FROM dual;\nint b;\nEXEC SQL COMMIT;\nint c;\n")
    (goto-char (point-max))
    (exec-sql-goto-prior)
    (should (= (line-number-at-pos (point)) 4))
    (should (= (current-column) 0))))

(provide 'test-exec-sql-goto-prior)
