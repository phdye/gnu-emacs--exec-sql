# User Guide

This guide explains how to install and use **exec-sql** inside Emacs.

## Installation
1. Ensure `sqlformat` from the `sqlparse` Python package is installed and on your PATH.
2. Add the package directory to your Emacs `load-path` and require `exec-sql`:

```emacs-lisp
(add-to-list 'load-path "/path/to/exec-sql")
(require 'exec-sql)
```

## Basic Usage
Open a Pro*C (`.pc`) or related file and try the following commands:

- `M-x exec-sql-format` – Format the selected region containing `EXEC SQL`.
- `M-x exec-sql-format-next-block` – Format the next `EXEC SQL` block after point.
- `M-x exec-sql-format-all-blocks` – Format every `EXEC SQL` block in the buffer.
- `M-x exec-sql-extract` – Display all `EXEC SQL` blocks in a separate buffer.
- `M-x exec-sql-goto-next` / `exec-sql-goto-prior` – Navigate between blocks.

### Optional Keybindings
Bind helpers in `c-mode` to speed up your workflow:

```emacs-lisp
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-f") #'exec-sql-format)
            (local-set-key (kbd "C-c C-e") #'exec-sql-format-next-block)
            (local-set-key (kbd "C-c C-a") #'exec-sql-format-all-blocks)))
```

### SQL Highlighting
To enable SQL highlighting within C buffers using `mmm-mode`:

```emacs-lisp
(require 'exec-sql-mmm)
```

`exec-sql-mmm` defines an `embedded-sql` submode that activates automatically for common Pro*C extensions.

## Troubleshooting
- Ensure `sqlformat` is installed and callable.
- Formatting skips blocks such as `EXEC SQL INCLUDE` directives.
- The library is developed against Python 3.2.5, Cygwin 1.7.29 and Oracle 12c.
