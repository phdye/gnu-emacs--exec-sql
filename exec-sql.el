;;; exec-sql.el


(require 'exec-sql-parser)

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-n") #'execl-sql-goto-next)))

;;;
