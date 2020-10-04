EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')

BUTTERCUP	:= ~/.emacs.d/dist/emacs-buttercup/bin/buttercup

.PHONY: test

all: test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: test-column-elements

test-column-elements: compile
	$(BUTTERCUP) -L . -L ~/.emacs.d/dist/emacs-buttercup tests

clean:
	rm -f *.elc tests/*.elc
