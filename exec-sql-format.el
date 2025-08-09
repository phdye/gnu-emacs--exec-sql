;;; Usage
;;;
;;; - Select an embedded SQL region inside EXEC SQL ...;.
;;; - Run M-x exec-sql-format.
;;; 
;;; It will:
;;; 
;;; Send the region to sqlformat.
;;; 
;;; Capture the formatted result.
;;; 
;;; Replace the original region with the formatted SQL.
;;; 
;;; Optional: Bind to a Key
;;; To bind it to a convenient key (e.g. C-c C-f in c-mode):
;
; (add-hook 'c-mode-hook
;           (lambda ()
;             (local-set-key (kbd "C-c C-f") #'exec-sql-format)))

(defun exec-sql-format (start end)
  "Format embedded SQL in a selected region using sqlformat."
  (interactive "r")
  (let ((formatted-sql-buffer "*Formatted SQL*"))
    (if (use-region-p)
        (progn
          (shell-command-on-region
           start end
           "sqlformat -r -k upper -s -"  ; change options here if needed
           formatted-sql-buffer
           nil ; do not replace region automatically
           "*SQL Format Errors*" t)
          (with-current-buffer formatted-sql-buffer
            (let ((formatted (buffer-string)))
              (delete-region start end)
              (goto-char start)
              (insert formatted)))
          (kill-buffer formatted-sql-buffer))
      (message "No region selected."))))


;;; exec-sql-format-next-block.el --- Auto-format embedded SQL in Pro*C files -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This function finds and formats the next embedded SQL block in a Pro*C file.
;; It supports both styles of Pro*C embedding:
;;
;;   - EXEC SQL <statements> ;
;;   - EXEC SQL <statements> END-EXEC;
;;
;; It extracts just the SQL content (excluding the EXEC SQL / END-EXEC; wrappers),
;; sends it to the external SQL formatter `sqlformat`, and replaces the block
;; in-place with the formatted version.
;;
;; 📦 Prerequisites:
;; - Python's sqlformat CLI from the `sqlparse` package:
;;     pip install sqlparse
;; - Ensure `sqlformat` is available in your PATH.
;;
;; 🧪 Usage:
;; 1. Open a `.pc` Pro*C file.
;; 2. Place cursor anywhere before or inside an EXEC SQL block.
;; 3. Run: M-x exec-sql-format-next-block
;;
;; 🎯 Optional Keybinding:
;; (add-hook 'c-mode-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-c C-e") #'exec-sql-format-next-block)))
;;
;; ⚠️ Limitations:
;; - Assumes one SQL block per EXEC SQL.
;; - Does not parse nested EXEC SQL within C macros.
;; - Formatting replaces SQL region only—wrappers are preserved.

;;; Code:

(defun exec-sql-skip-ws-and-comments ()
  "Move point forward past whitespace and C/SQL comments."
  (while (progn
           (skip-chars-forward " \t\n")
           (cond
            ((looking-at "/\\*") (forward-comment 1) t)
            ((looking-at "--") (search-forward "\n" nil 'move) t)
            (t nil)))))

(defun exec-sql-find-semicolon ()
  "Return position of next semicolon outside comments." 
  (catch 'found
    (while (search-forward ";" nil t)
      (let ((pos (1- (point))))
        (unless (or (nth 4 (syntax-ppss pos))
                    (save-excursion
                      (goto-char pos)
                      (let ((line-start (line-beginning-position)))
                        (when (search-backward "--" line-start t)
                          (let ((pps (syntax-ppss)))
                            (not (or (nth 3 pps) (nth 4 pps))))))))
          (throw 'found pos))))))

(defun exec-sql-format-next-block ()
  "Find and format the next EXEC SQL block using sqlformat.
Supports both 'EXEC SQL ... ;' and 'EXEC SQL ... END-EXEC;' forms."
  (interactive)
  (let ((exec-sql-regexp "EXEC[ \t\n]+SQL[ \t\n]+")
        (end-block-regexp "END-EXEC[ \t]*;[ \t]*\\(?:--.*\\)?[ \t]*\n?")
        start end skip-format)
    (save-excursion
      ;; Find EXEC SQL start
      (if (re-search-forward exec-sql-regexp nil t)
          (progn
            (setq start (point))
            (let* ((case-fold-search t)
                   (next-token (save-excursion
                                 (exec-sql-skip-ws-and-comments)
                                 (buffer-substring-no-properties
                                  (point)
                                  (progn (skip-chars-forward "A-Za-z_" )
                                         (point)))))
                   (upper-token (upcase next-token)))
              (cond
               ((string= upper-token "INCLUDE")
                (let ((semi (save-excursion (goto-char start)
                                            (exec-sql-find-semicolon))))
                  (when semi
                    (goto-char (1+ semi))
                    (message "Skipped EXEC SQL INCLUDE directive.")))
                 (setq skip-format t))
               ((member upper-token '("DECLARE" "BEGIN"))
                (when (re-search-forward end-block-regexp nil t)
                  (setq end (match-beginning 0))))
               (t
                (setq end (save-excursion
                            (goto-char start)
                            (exec-sql-find-semicolon))))))
            (if (not skip-format)
                (if (and start end)
                    (let* ((sql-original (buffer-substring-no-properties start end))
                           (formatted-sql-buffer "*Formatted SQL*"))

                      ;; Format with sqlformat
                      (with-temp-buffer
                        (insert sql-original)
                        (shell-command-on-region
                         (point-min) (point-max)
                         "sqlformat -r -k upper -s -"
                         formatted-sql-buffer
                         nil "*SQL Format Errors*" t))

                      ;; Replace original SQL with formatted
                      (with-current-buffer formatted-sql-buffer
                        (let ((formatted-sql (string-trim (buffer-string))))
                          (delete-region start end)
                          (goto-char start)
                          (insert formatted-sql)))
                      (kill-buffer formatted-sql-buffer)
                      (message "Formatted embedded SQL."))
                  (message "Could not find terminating ';' or END-EXEC; for EXEC SQL block."))))
        (message "No EXEC SQL block found.")))))


;;; exec-sql-format-all-blocks.el --- Format all embedded SQL blocks in Pro*C files -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This function formats **all** embedded SQL blocks in a Pro*C `.pc` source file.
;; It wraps and repeatedly calls `exec-sql-format-next-block`, which handles:
;;
;;   - EXEC SQL ... ;
;;   - EXEC SQL ... END-EXEC;
;;
;; The SQL content inside each block is:
;;   - Extracted (excluding EXEC SQL and terminator)
;;   - Sent to the external SQL formatter `sqlformat`
;;   - Reinserted into the buffer with the formatted version
;;
;; 📦 Requirements:
;; - Install the Python `sqlparse` package:
;;     pip install sqlparse
;; - Ensure `sqlformat` is on your PATH
;;
;; 🧪 Usage:
;; 1. Open a `.pc` Pro*C file.
;; 2. Run: M-x exec-sql-format-all-blocks
;;    → All embedded SQL blocks will be formatted.
;;
;; 🔁 What It Does:
;; - Moves from the top of the buffer.
;; - Formats each EXEC SQL block in turn.
;; - Stops when no further blocks are found.
;;
;; 🎯 Optional Keybinding:
;; (add-hook 'c-mode-hook
;;   (lambda ()
;;     (local-set-key (kbd "C-c C-a") #'exec-sql-format-all-blocks)))
;;
;; 💡 Notes:
;; - Relies on `exec-sql-format-next-block` for actual formatting.
;; - Safe to re-run; formatting is idempotent.
;; - Does not modify EXEC SQL wrappers—only the SQL inside.
;;
;;; Code:

(defun exec-sql-format-all-blocks ()
  "Format all embedded EXEC SQL blocks in the current buffer using sqlformat.
This function repeatedly calls `exec-sql-format-next-block` until no more
SQL blocks are found. It supports both 'EXEC SQL ... ;' and
'EXEC SQL ... END-EXEC;' formats."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (keep-going t)
          (last-point -1))
      (while keep-going
        (setq keep-going
              (condition-case err
                  (progn
                    (let ((initial-point (point)))
                      (exec-sql-format-next-block)
                      ;; If point did not advance, assume no more blocks
                      (if (= (point) initial-point)
                          nil
                        (cl-incf count)
                        t)))
                (error
                 (message "Error during formatting: %s" err)
                 nil))))
      (message "Formatted %d embedded SQL block(s)." count))))
