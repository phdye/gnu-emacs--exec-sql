(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-get-next-traverses-examples ()
  (dolist (spec '(("complex.pc" . (4 5 6 10 11 16 18 20))
                  ("oracle+addtl.pc" . (4 7 8 13 14 20 22 26 28))))
    (with-temp-buffer
      (insert-file-contents (expand-file-name (car spec) exec-sql-test-examples-dir))
      (goto-char (point-min))
      (let ((lines '()) info)
        (while (setq info (exec-sql-get-next))
          (push (car (plist-get info :start)) lines)
          (goto-char (+ (point-min) (plist-get info :offset) (plist-get info :length))))
        (should (equal (nreverse lines) (cdr spec)))))))

(provide 'exec-sql-get-next-test)
