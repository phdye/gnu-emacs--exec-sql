# Developer Guide

Welcome to the developer documentation for **exec-sql**. This guide describes how to set up a working environment, run tests, and contribute code.

## Getting Started
1. Clone the repository and add it to your Emacs `load-path`.
2. Install [sqlparse](https://pypi.org/project/sqlparse/) which provides the `sqlformat` CLI used for formatting SQL.

## Running Tests
Automated tests are written as Emacs Lisp files. Execute them with:

```
make test
```

## Style Guidelines
- Use `lexical-binding` in all new Emacs Lisp files.
- Follow standard Emacs Lisp conventions for naming and indentation.
- Include docstrings for all public functions.

## Releasing
- Update `CHANGELOG.md` with a new section for each release.
- Tag releases in Git using semantic versioning.
- For MELPA submission, ensure package headers include `Version`, `Author`, `URL`, and `Package-Requires` fields.

## Reporting Issues
Use the GitHub issue tracker to report bugs. Provide steps to reproduce and relevant environment information.
