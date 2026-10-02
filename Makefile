EMACS=/usr/local/bin/emacs

%.elc: %.el
	$(EMACS) --batch --directory . --funcall batch-byte-compile $<

all: elhints.elc elhints-test.elc

tests: all
	$(EMACS) --batch --directory . --load ert --load elhints-test.el --funcall ert-run-tests-batch-and-exit

ui-tests: all
	./run_ui_tests.el
