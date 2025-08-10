;;;

(add-to-list 'auto-mode-alist '("\\.pc\\'"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.pcc\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ec\\'"  . c-mode))
(add-to-list 'auto-mode-alist '("\\.sqc\\'" . c-mode))

(require 'exec-sql-mmm)

;;;

(require 'exec-sql-parser)

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-n") #'exec-sql-goto-next)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-p") #'exec-sql-goto-prior)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "C-c C-e") #'exec-sql-extract)))

;;;

(require 'exec-sql-format)

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "M-f C-f") #'exec-sql-format)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "M-f C-n") #'exec-sql-format-next-block)))

(add-hook 'c-mode-hook
    (lambda ()
        (local-set-key (kbd "M-f C-a") #'exec-sql-format-all-blocks)))

;;;
