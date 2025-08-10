
test: emacs-tests

emacs-tests e-tests etest :
	emacs --batch -l tests/test_exec_sql_parser.el
	emacs --batch -l tests/test_exec_sql_format.el
