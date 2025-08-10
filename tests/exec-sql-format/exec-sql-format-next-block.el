(require 'ert)
(require 'cl-lib)
(require 'exec-sql-format)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-format-next-block-uses-parser ()
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'exec-sql-goto-next)
                 (lambda (&optional _) (setq called t) nil))
                ((symbol-function 'message) (lambda (&rest _) nil)))
        (exec-sql-format-next-block)
        (should called)))))

(ert-deftest exec-sql-format-next-block-skips-include ()
  (with-temp-buffer
    (insert "EXEC SQL include foo;\n")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'shell-command-on-region)
               (lambda (&rest _) (error "should not be called")))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (exec-sql-format-next-block)
      (should (string= (buffer-string) "EXEC SQL include foo;\n")))))

(ert-deftest exec-sql-format-next-block-no-block ()
  (with-temp-buffer
    (let (msg)
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (setq msg (apply #'format fmt args)))))
        (exec-sql-format-next-block))
      (should (equal msg "No EXEC SQL block found.")))))

(ert-deftest exec-sql-format-next-block-example ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "oracle+addtl.pc" exec-sql-test-examples-dir))
    (goto-char (point-min))
    (cl-letf (((symbol-function 'shell-command-on-region)
               (lambda (start end command output-buffer replace error-buffer display)
                 (let ((text (buffer-substring (min start end) (max start end))))
                   (with-current-buffer (get-buffer-create output-buffer)
                     (erase-buffer)
                     (insert (upcase text))))))
              ((symbol-function 'message) (lambda (&rest _) nil)))
      (dotimes (_ 3)
        (exec-sql-format-next-block))
      (goto-char (point-min))
      (should (search-forward "SELECT ID, NAME" nil t)))))

(provide 'exec-sql-format-next-block-test)
