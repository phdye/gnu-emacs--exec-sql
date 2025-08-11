;;; exec-sql-format --- Format the selected embedded SQL blocks in Pro*C files
;;;
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

(require 'cl-lib)
(require 'subr-x)
(require 'exec-sql-parser)

(defun exec-sql-format (start end)
  "Format embedded SQL in a selected region using sqlformat."
  (interactive "r")
  (let ((formatted-sql-buffer "*Formatted SQL*"))
    (if (use-region-p)
        (let* ((s (min start end))
               (e (max start end))
               (indent (save-excursion
                         (goto-char s)
                         (current-indentation)))
               (indent-str (make-string indent ?\s))
               (end-with-newline (and (> e s)
                                      (save-excursion
                                        (goto-char e)
                                        (eq (char-before) ?\n)))))
          (shell-command-on-region
           s e
           "sqlformat -r -k upper -s -"  ; change options here if needed
           formatted-sql-buffer
           nil ; do not replace region automatically
           "*SQL Format Errors*" t)
          (let* ((formatted (with-current-buffer formatted-sql-buffer
                               (buffer-string)))
                 (trimmed (string-trim-right formatted))
                 (lines (split-string trimmed "\n"))
                 (first-line (concat indent-str
                                      (string-trim-left (car lines))))
                 (rest-lines (mapcar (lambda (line)
                                       (concat indent-str line))
                                     (cdr lines)))
                 (indented (string-join (cons first-line rest-lines)
                                        "\n")))
            (delete-region s e)
            (goto-char s)
            (insert indented)
            (when (and end-with-newline
                       (not (eq (char-after) ?\n)))
              (insert "\n")))
          (kill-buffer formatted-sql-buffer))
      (message "No region selected."))))


;;; exec-sql-format-next-block --- Auto-format embedded SQL in Pro*C files
;;;
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

(defun exec-sql-format-next-block ()
  "Find and format the next EXEC SQL block using sqlformat.
Supports both 'EXEC SQL ... ;' and 'EXEC SQL ... END-EXEC;' forms.
Block location is determined by `exec-sql-goto-next` from
`exec-sql-parser`.  Only the SQL content inside the wrappers is
formatted."
  (interactive)
  (let ((info (exec-sql-goto-next)))
    (if (null info)
        (message "No EXEC SQL block found.")
      (let* ((block-start (+ (point-min) (plist-get info :offset)))
             (block-end   (+ block-start (plist-get info :length)))
             (block-text  (buffer-substring-no-properties block-start block-end)))
        (if (string-match "^\\s-*EXEC\\s-+SQL\\s+INCLUDE\\b" block-text)
            (progn
              (goto-char block-end)
              (message "Skipped EXEC SQL INCLUDE directive."))
          (let* ((case-fold-search t)
                 (_ (string-match "^\\s-*EXEC\\s-+SQL\\s+" block-text))
                 (sql-start (+ block-start (match-end 0)))
                 (sql-end
                  (cond
                   ((string-match "\\(END-EXEC;\\)\\s-*\\(?:--.*\\)?\\s-*$" block-text)
                    (+ block-start (match-beginning 1)))
                   ((string-match "\\(;\\)\\s-*\\(?:--.*\\)?\\s-*$" block-text)
                    (+ block-start (match-beginning 1)))
                   (t block-end)))
                 (sql-original (buffer-substring-no-properties sql-start sql-end))
                 (formatted-sql-buffer "*Formatted SQL*"))
            (with-temp-buffer
              (insert sql-original)
              (shell-command-on-region
               (point-min) (point-max)
               "sqlformat -r -k upper -s -"
               formatted-sql-buffer
               nil "*SQL Format Errors*" t))
            (let* ((formatted-sql (with-current-buffer formatted-sql-buffer
                                   (string-trim (buffer-string))))
                   (orig-len (- sql-end sql-start)))
              (delete-region sql-start sql-end)
              (goto-char sql-start)
              (insert formatted-sql)
              (setq block-end (+ block-end (- (length formatted-sql) orig-len))))
            (kill-buffer formatted-sql-buffer)
            (goto-char block-end)
            (message "Formatted embedded SQL.")))))))


;;; exec-sql-format-all-blocks --- Format all embedded SQL blocks in Pro*C files
;;;
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

(provide 'exec-sql-format)
