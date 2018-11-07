EMACS = "/Users/christianjohansson/Documents/emacs/src/emacs"
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := phps-mode.el phps-wy.el
ELC := $(EL:.el=.elc)

clean:
	rm -f $(ELC)

compile: $(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

test: clean lexer-test parser-test

test-lexer:
	$(EMACS_CMD) -l phps-test-lexer.el

test-parser:
	$(EMACS_CMD) -l phps-test-parser.el

.PHONY: clean compile test test-lexer test-parser
