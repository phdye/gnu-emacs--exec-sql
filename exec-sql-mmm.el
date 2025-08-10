;;------------------------------------------------------------------------------

;; Oracle Pro*C
;; https://emacswiki.org/emacs?OracleProC

(require 'mmm-mode)
(set-face-background 'mmm-default-submode-face nil)
(mmm-add-classes
  '((embedded-sql
     :submode sql-mode
     :front "EXEC SQL"
     :back ";")))
(setq-default mmm-global-mode 'maybe)

(mmm-add-mode-ext-class 'c-mode "\\.pc"    'embedded-sql)
(mmm-add-mode-ext-class 'c-mode "\\.pcc"   'embedded-sql)
(mmm-add-mode-ext-class 'c-mode "\\.ec"    'embedded-sql)
(mmm-add-mode-ext-class 'c-mode "\\.sqc"   'embedded-sql)

(setq-default mmm-never-modes
               (append '(ediff-mode) '(text-mode) mmm-never-modes))

;;------------------------------------------------------------------------------
