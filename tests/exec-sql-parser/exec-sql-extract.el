(require 'ert)
(require 'exec-sql-parser)

(defconst exec-sql-test-examples-dir
  (expand-file-name "../examples" (file-name-directory load-file-name)))

(ert-deftest exec-sql-extract-basic ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "complex.pc" exec-sql-test-examples-dir))
    (let ((count 0) info res)
      (save-excursion
        (goto-char (point-min))
        (while (setq info (exec-sql-goto-next))
          (setq count (1+ count))
          (goto-char (+ (point-min)
                        (plist-get info :offset)
                        (plist-get info :length)))))
      (setq res (exec-sql-extract))
      (unwind-protect
          (with-current-buffer res
            (should (= (count-matches "^EXEC SQL" (point-min) (point-max))
                       count))
            (should (= (count-matches "^//=+" (point-min) (point-max))
                       (1- count))))
        (when (buffer-live-p res)
          (kill-buffer res))))))

(provide 'exec-sql-extract-test)

