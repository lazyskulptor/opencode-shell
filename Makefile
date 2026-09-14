EMACS ?= emacs

.PHONY: test compile compiled-test clean verify

test:
	$(EMACS) -Q --batch -L . -L test -l test/opencode-shell-test.el -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -Q --batch -L . -L test -f batch-byte-compile opencode-shell-render.el opencode-shell.el test/opencode-shell-acceptance-test.el test/opencode-shell-test.el

compiled-test:
	$(EMACS) -Q --batch -L . -L test -l test/opencode-shell-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc

verify: clean test compile compiled-test
