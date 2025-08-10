(require 'ert)
(require 'exec-sql-parser)

(ert-deftest exec-sql-parser--marker-basic ()
  (should (string= (exec-sql-parser--marker 3)
                   (format "%s:3:" exec-sql-parser--marker-prefix))))

(provide 'exec-sql-parser--marker-test)
