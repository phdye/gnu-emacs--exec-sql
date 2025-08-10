# API Reference

This document lists the primary interactive commands and functions provided by **exec-sql**.

## Formatting
- `exec-sql-format` `(start end)` – Format the selected region containing `EXEC SQL`.
- `exec-sql-format-next-block` – Find and format the next `EXEC SQL` block after point.
- `exec-sql-format-all-blocks` – Format all embedded SQL blocks in the current buffer.

## Navigation and Parsing
- `exec-sql-goto-next` – Move point to the next `EXEC SQL` block.
- `exec-sql-goto-prior` – Move point to the previous block.
- `exec-sql-get-next` – Return metadata for the next block without moving point.
- `exec-sql-get-prior` – Return metadata for the prior block without moving point.
- `exec-sql-count-remaining` – Count remaining `EXEC SQL` blocks from point.
- `exec-sql-extract` – Collect all `EXEC SQL` blocks into a separate buffer.
- `exec-sql-parser-load-registry` `(start-dir &optional search-parents)` – Load pattern registry entries from JSON files.

## MMM Integration
- `exec-sql-mmm` – Enables an `mmm-mode` class for highlighting embedded SQL in C buffers.

Consult the source code for detailed docstrings and customization options.
