EMACS ?= emacs

.PHONY: test compile compiled-test clean verify

test:
	$(EMACS) -Q --batch -L . -L test -L test/fixtures -l test/opencode-shell-sse-test.el -l test/opencode-shell-test.el -f ert-run-tests-batch-and-exit

compile: clean
	$(EMACS) -Q --batch -L . -L test -L test/fixtures -f batch-byte-compile opencode-shell-render.el opencode-shell-sse.el opencode-shell-async.el opencode-shell.el test/fixtures/completion-polling-regression.el test/opencode-shell-acceptance-test.el test/opencode-shell-sse-test.el test/opencode-shell-test.el

compiled-test:
	$(EMACS) -Q --batch -L . -L test -L test/fixtures -l test/opencode-shell-sse-test.el -l test/opencode-shell-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc test/fixtures/*.elc

verify: clean test compile compiled-test
