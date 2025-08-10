(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-get-prior-traverses-example ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-max))
    (let ((expected '(29 27 23 21 19 18 15 13 9 7 4))
          (lines '()) info)
      (while (setq info (exec-sql-get-prior))
        (push (car (plist-get info :start)) lines)
        (goto-char (+ (point-min) (plist-get info :offset) -1)))
      (should (equal (nreverse lines) expected)))))

(provide 'exec-sql-get-prior-test)
