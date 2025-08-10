;;;

(require 'exec-sql-parser)

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-n") #'execl-sql-goto-next)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-p") #'execl-sql-goto-prior)))

;;;

(require 'exec-sql-format)

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-f") #'exec-sql-format)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-e") #'exec-sql-format-next-block)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-a") #'exec-sql-format-all-blocks)))

;;;
