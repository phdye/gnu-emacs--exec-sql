;;;

(add-to-list 'auto-mode-alist '("\\.pc\\'"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.pcc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ec\\'"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.sqc\\'" . c-mode))

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
