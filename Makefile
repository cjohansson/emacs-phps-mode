EMACS = "/Users/christianjohansson/Documents/emacs/src/emacs"
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := phps-mode.el phps-wy.el
ELC := $(EL:.el=.elc)

clean:
	rm -f $(ELC)

compile:
	$(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

test:
	clean test-functions test-lexer test-parser

test-functions:
	$(EMACS_CMD) -l phps-test-functions.el

test-lexer:
	$(EMACS_CMD) -l phps-test-lexer.el


test-parser:
	$(EMACS_CMD) -l phps-test-parser.el

.PHONY: clean compile test test-functions test-lexer test-parser
