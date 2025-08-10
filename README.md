# exec-sql

Exec-sql provides Emacs helpers for working with Oracle Pro*C source files containing embedded `EXEC SQL` blocks.  The package supplies navigation, extraction and formatting commands that understand the structure of embedded SQL in C and related languages.

## Features
- Detect and move between embedded `EXEC SQL` blocks.
- Extract all `EXEC SQL` blocks from a buffer.
- Format one, the next, or all embedded SQL blocks using the external `sqlformat` tool.
- Optional `mmm-mode` integration for highlighting SQL regions inside C buffers.

## Installation
The library is distributed as standard Emacs Lisp and is suitable for publication on MELPA or GNU ELPA.  Until then you can install it manually:

```emacs-lisp
(add-to-list 'load-path "/path/to/exec-sql")
(require 'exec-sql)
```

Formatting relies on the `sqlformat` command line tool from the `sqlparse` Python package.  Ensure it is available in your `PATH`.

## Usage
Open a Pro*C `.pc` buffer and invoke one of the interactive commands:

- `M-x exec-sql-format` – format the currently selected region.
- `M-x exec-sql-format-next-block` – format the next embedded SQL block.
- `M-x exec-sql-format-all-blocks` – format every embedded SQL block in the buffer.
- `M-x exec-sql-extract` – open a buffer containing all `EXEC SQL` blocks.

The library also exposes navigation helpers `exec-sql-goto-next` and `exec-sql-goto-prior` for moving between SQL blocks.  See the [User Guide](doc/User-Guide.md) and [API](doc/API.md) for more details.

## Contributing
Contributions are welcome!  Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on our workflow and development expectations.

## License
This project is licensed under the terms of the GNU General Public License v3.0 or later.  See [LICENSE](LICENSE) for the full text.
