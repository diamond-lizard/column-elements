EMACS := emacs
ELISP_FILES := $(shell ls *.el | grep -v -- '-pkg\.el$$')

BUTTERCUP	:= ~/.emacs.d/dist/emacs-buttercup/bin/buttercup

# Make sure that $(BUTTERCUP) exists
ifneq ("$(wildcard $(BUTTERCUP))","")
  # $(BUTTERCUP) exists
else
  $(error Error: Could not find buttercup at '$(BUTTERCUP)')
endif

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
