EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')
TEST_DIR := tests

TESTS := column-elements--gap-column-p-aux
TESTS += column-elements--gap-column-p
TESTS += column-elements--get-buffer-width
TESTS += column-elements--column-block-boundaries-at-point

.PHONY: test

all: test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: $(TESTS)

column-elements--get-buffer-width: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/column-elements--get-buffer-width.el -f ert-run-tests-batch-and-exit

column-elements--column-block-boundaries-at-point: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/column-elements--column-block-boundaries-at-point.el -f ert-run-tests-batch-and-exit

column-elements--gap-column-p: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/column-elements--gap-column-p.el -f ert-run-tests-batch-and-exit

column-elements--gap-column-p-aux: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/column-elements--gap-column-p-aux.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc $(TEST_DIR)/*.elc
