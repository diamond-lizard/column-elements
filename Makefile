EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')

.PHONY: test

compile: $(patsubst %.el,%.elc,$(ELISP_FILES))

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

test: test-column-elements

test-column-elements: compile
	echo TODO

clean:
	rm -f *.elc tests/*.elc
