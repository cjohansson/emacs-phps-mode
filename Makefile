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

.PHONY: compile
compile:
	$(ELC)

%.elc: %.el
	$(EMACS_CMD) -f batch-byte-compile $<

.PHONY: tests
tests: clean test-functions test-lexer test-parser test-integration

.PHONY: test-functions
test-functions:
	$(EMACS_CMD) -l phps-mode-test-functions.el

.PHONY: test-integration
test-integration:
	$(EMACS_CMD) -l phps-mode-test-integration.el

.PHONY: test-lexer
test-lexer:
	$(EMACS_CMD) -l phps-mode-test-lexer.el

.PHONY: test-parser
test-parser:
	$(EMACS_CMD) -l phps-mode-test-parser.el
