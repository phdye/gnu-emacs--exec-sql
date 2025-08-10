(require 'ert)
(require 'exec-sql-parser)

(ert-deftest exec-sql-parser--strip-comments-removes-line ()
  (let* ((input "code // comment\nnext")
         (expected "code \nnext"))
    (should (string= expected (exec-sql-parser--strip-comments input)))))

(ert-deftest exec-sql-parser--strip-comments-idempotent ()
  (let ((input "select * from dual;"))
    (should (string= input (exec-sql-parser--strip-comments input)))))

(provide 'exec-sql-parser--strip-comments-test)
