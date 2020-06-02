EMACS = emacs
ifdef emacs
	EMACS = $(emacs)
endif
EMACS_CMD := $(EMACS) -Q -batch -L .

EL  := admin/phps-mode-automation.el phps-mode-flymake.el phps-mode-lex-analyzer.el phps-mode-lexer.el phps-mode-macros.el phps-mode-syntax-table.el  phps-mode-wy-macros.el phps-mode.el test/phps-mode-test.el test/phps-mode-test-lex-analyzer.el test/phps-mode-test-integration.el test/phps-mode-test-lexer.el test/phps-mode-test-parser.el test/phps-mode-test-syntax-table.el
ELC := $(EL:.el=.elc)

.PHONY: clean
clean:
	rm -f $(ELC)

.PHONY: compile
compile:
	$(EMACS_CMD) -L ./test -f batch-byte-compile $(EL)

.PHONY: tests
tests: test-integration test-lexer test-lex-analyzer test-parser test-syntax-table

.PHONY: test-integration
test-integration:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-integration.el

.PHONY: test-lex-analyzer
test-lex-analyzer:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-lex-analyzer.el


.PHONY: test-lexer
test-lexer:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-lexer.el -f "phps-mode-test-lexer"

.PHONY: benchmark-lexer
benchmark-lexer:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-lexer.el -f "phps-mode-test-lexer-benchmark"

.PHONY: test-parser
test-parser:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-parser.el

.PHONY: test-syntax-table
test-syntax-table:
	$(EMACS_CMD) -L ./test -l test/phps-mode-test-syntax-table.el
