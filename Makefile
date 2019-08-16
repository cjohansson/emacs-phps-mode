EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := phps-mode-automation.el phps-mode-flymake.el phps-mode-functions.el phps-mode-lexer.el phps-mode-semantic.el phps-mode-syntax-table.el phps-mode-tags.el phps-mode-test-functions.el phps-mode-test-integration.el phps-mode-test-lexer.el phps-mode-test-parser.el phps-mode-test-syntax-table.el phps-mode-test.el phps-mode.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -f batch-byte-compile $(EL)

.PHONY: tests
tests: test-functions test-integration test-lexer test-parser test-syntax-table

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

.PHONY: test-syntax-table
test-syntax-table:
	$(EMACS_CMD) -l phps-mode-test-syntax-table.el
