EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')
TEST_DIR := tests

TESTS := text-blocks--gap-column-p-aux
TESTS += text-blocks--gap-column-p
TESTS += text-blocks--horizontal-gap-p
TESTS += text-blocks--get-buffer-width
TESTS += text-blocks--block-boundaries-at-point

.PHONY: test

all: test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: $(TESTS)

text-blocks--get-buffer-width: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/text-blocks--get-buffer-width.el -f ert-run-tests-batch-and-exit

text-blocks--block-boundaries-at-point: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/text-blocks--block-boundaries-at-point.el -f ert-run-tests-batch-and-exit

text-blocks--gap-column-p: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/text-blocks--gap-column-p.el -f ert-run-tests-batch-and-exit

text-blocks--gap-column-p-aux: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/text-blocks--gap-column-p-aux.el -f ert-run-tests-batch-and-exit

text-blocks--horizontal-gap-p: compile
	$(EMACS) -batch -l ert -L . -l $(TEST_DIR)/text-blocks--horizontal-gap-p.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc $(TEST_DIR)/*.elc
