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

(provide 'test-exec-sql-load-registry)
