EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -L test -l test/opencode-shell-test.el -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -Q --batch -L . -f batch-byte-compile opencode-shell-render.el opencode-shell.el test/opencode-shell-test.el

clean:
	rm -f *.elc test/*.elc
