EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := admin/phps-mode-automation.el phps-mode-analyzer.el phps-mode-flymake.el phps-mode-macros.el phps-mode-semantic.el phps-mode-syntax-table.el phps-mode-tags.el phps-mode-test.el phps-mode-wy-macros.el phps-mode-wy-wy.el phps-mode.el test/phps-mode-test-functions.el test/phps-mode-test-integration.el test/phps-mode-test-lexer.el test/phps-mode-test-parser.el test/phps-mode-test-syntax-table.el
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
	$(EMACS_CMD) -l test/phps-mode-test-functions.el

.PHONY: test-integration
test-integration:
	$(EMACS_CMD) -l test/phps-mode-test-integration.el

.PHONY: test-lexer
test-lexer:
	$(EMACS_CMD) -l test/phps-mode-test-lexer.el

.PHONY: test-parser
test-parser:
	$(EMACS_CMD) -l test/phps-mode-test-parser.el

.PHONY: test-syntax-table
test-syntax-table:
	$(EMACS_CMD) -l test/phps-mode-test-syntax-table.el
