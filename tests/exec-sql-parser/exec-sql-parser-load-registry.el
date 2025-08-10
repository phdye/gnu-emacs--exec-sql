(require 'ert)
(require 'exec-sql-parser)

(ert-deftest exec-sql-parser-load-registry-override ()
  (let ((dir (make-temp-file "exec-sql-registry" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".exec-sql-parser" dir)
            (insert "{\n  \"STATEMENT-Single-Line [1]\": {\"pattern\": \"^EXEC SQL CUSTOM\\\\b.*;\"}\n}"))
          (let ((registry (exec-sql-parser-load-registry dir)))
            (let* ((entry (assoc "STATEMENT-Single-Line [1]" registry))
                   (pattern (plist-get (cdr entry) :pattern)))
              (should (string= pattern "^EXEC SQL CUSTOM\\b.*;")))))
      (delete-directory dir t))))

(ert-deftest exec-sql-parser-load-registry-removal-and-root ()
  (let* ((parent (make-temp-file "exec-sql-registry-parent" t))
         (child (expand-file-name "child" parent)))
    (make-directory child)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".exec-sql-parser" parent)
            (insert "{\n  \"STATEMENT-Single-Line [1]\": null,\n  \"root\": true\n}"))
          (let ((registry (exec-sql-parser-load-registry child t)))
            (should (null (assoc "STATEMENT-Single-Line [1]" registry)))))
      (delete-directory parent t))))

(provide 'exec-sql-parser-load-registry-test)
