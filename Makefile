test: emacs-tests python-tests

emacs-tests e-tests etest :
	emacs --batch -l tests/test_exec_sql_parser.el
	emacs --batch -l tests/test_exec_sql_format.el

python-tests:
	python3 tests/exec-sql-format/test_exec_sql_format_external.py
