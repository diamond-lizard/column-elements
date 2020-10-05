EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')

BUTTERCUP	:= ~/.emacs.d/dist/emacs-buttercup/bin/buttercup
BUTTERCUP_DIR	:= ~/.emacs.d/dist/emacs-buttercup
BUTTERCUP_ARGS  := -L . -L $(BUTTERCUP_DIR)

## Tell buttercup not to use color when run from inside emacs,
## since emacs' compilation buffer doesn't seem to like ansi color sequences
##
#ifdef INSIDE_EMACS
#  BUTTERCUP_ARGS  += --no-color
#endif

# Make sure that $(BUTTERCUP) exists
ifneq ("$(wildcard $(BUTTERCUP))","")
  # $(BUTTERCUP) exists
  all: test
else
  $(warning Warning: Could not find buttercup at '$(BUTTERCUP)')
  $(warning Warning: Skipping tests...)
  all: compile
endif


.PHONY: test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: test-column-elements

test-column-elements: compile
	$(BUTTERCUP) $(BUTTERCUP_ARGS) tests

clean:
	rm -f *.elc tests/*.elc
