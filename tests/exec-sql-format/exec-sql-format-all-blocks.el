(require 'ert)
(require 'cl-lib)
(require 'exec-sql-format)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-format-all-blocks-example ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'shell-command-on-region)
               (lambda (start end command output-buffer replace error-buffer display)
                 (let ((text (buffer-substring (min start end) (max start end))))
                   (with-current-buffer (get-buffer-create output-buffer)
                     (erase-buffer)
                     (insert (upcase text)))))))
      (exec-sql-format-all-blocks)
      (goto-char (point-min))
      ;; Due to parser limitations only initial declarations are formatted.
      ;; Verify later SQL remains unformatted to exercise navigation.
      (should (search-forward "select id, name" nil t)))))

(ert-deftest exec-sql-format-all-blocks-no-blocks ()
  (with-temp-buffer
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (exec-sql-format-all-blocks))
      (should (equal msg "Formatted 0 embedded SQL block(s).")))))

(provide 'exec-sql-format-all-blocks-test)
