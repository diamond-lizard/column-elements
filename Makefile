EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')
TEST_DIR := tests

TESTS :=
TESTS += text-blocks--search-for-consecutive-non-nils
TESTS += text-blocks--vertical-gap-column-p
TESTS += text-blocks--vertical-gap-p
TESTS += text-blocks--horizontal-gap-p
TESTS += text-blocks--get-buffer-width
TESTS += text-blocks--block-boundaries-at-point

EMACS_ERT_ARGS_1 := -batch -l ert -L . -l
EMACS_ERT_ARGS_2 := -f ert-run-tests-batch-and-exit

.PHONY: test

all: clean test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: $(TESTS)

text-blocks--get-buffer-width: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

text-blocks--block-boundaries-at-point: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

text-blocks--vertical-gap-p: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

text-blocks--vertical-gap-column-p: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

text-blocks--horizontal-gap-p: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

text-blocks--search-for-consecutive-non-nils: compile
	$(EMACS) $(EMACS_ERT_ARGS_1) $(TEST_DIR)/$@.el $(EMACS_ERT_ARGS_2)

clean:
	rm -f *.elc $(TEST_DIR)/*.elc
