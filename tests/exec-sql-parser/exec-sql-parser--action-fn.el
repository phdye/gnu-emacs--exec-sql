(require 'ert)
(require 'exec-sql-parser)

(defun exec-sql-test-helper (x)
  (+ x 1))

(ert-deftest exec-sql-parser--action-fn-symbol ()
  (should (eq (exec-sql-parser--action-fn 'exec-sql-test-helper)
             'exec-sql-test-helper)))

(ert-deftest exec-sql-parser--action-fn-lambda ()
  (let ((fn (lambda (x) (+ x 2))))
    (should (= 4 (funcall (exec-sql-parser--action-fn fn) 2)))))

(ert-deftest exec-sql-parser--action-fn-function-form ()
  (should (= 5 (funcall (exec-sql-parser--action-fn '(function +)) 2 3))))

(provide 'exec-sql-parser--action-fn-test)
