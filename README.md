# PHPs - Experiment with a Semantic Mode for Emacs

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

## Goals

With current progress estimates:

* Flycheck support (100%)
* Lexer based on official PHP re2c lexer (100%)
* Syntax coloring based on lexer tokens (100%)
* PSR based indentation based on lexer tokens (90%)
* Incremental lexer and syntax coloring after buffer changes (75%)
* Wisent LALR parser based on official PHP yacc parser automatically converted (60%)
* Full integration with Emacs Semantic subsystem (30%)
* Flymake support (0%)
* PSR-2 auto-formatting tool based on lexer tokens (0%)
* Add to MELPA (0%)
* Add to ELPA (0%)
* Travis support (0%)

## Unit tests

If you have emacs at a customized location prefix the commands with your path, i.e.

`export emacs="~~/Documents/emacs/src/emacs" && make test-lexer`

### Lexer

``` bash
make test-lexer
```

### Parser

Not ready yet.

``` bash
make test-parser
```

### Functions

``` bash
make test-functions
```

### Integration tests

Not working yet

``` bash
make test-integration
```

### All tests

``` bash
make tests
```

## Installation example

Download to `~/.emacs.d/phps-mode/` and then add this to your init file:

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
(use-package phps-mode
    :mode ("\\.php\\'" "\\.phtml\\'"))
```

