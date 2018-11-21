EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := phps-mode.el phps-wy.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONE: compile
compile:
	$(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

.PHONY: tests
tests: clean test-functions test-lexer test-parser

.PHONY: test-functions
test-functions:
	$(EMACS_CMD) -l phps-test-functions.el

.PHONY: test-lexer
test-lexer:
	$(EMACS_CMD) -l phps-test-lexer.el

.PHONY: test-parser
test-parser:
	$(EMACS_CMD) -l phps-test-parser.el
