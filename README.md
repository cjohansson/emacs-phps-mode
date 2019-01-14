# PHPs - Experiment with a Semantic Mode for Emacs

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

## Goals

*With current progress estimates:*

* Flycheck support (100%)
* Syntax coloring based on lexer tokens (100%)
* PSR based indentation based on lexer tokens (100%)
* Lexer based on official PHP re2c lexer (100%)
* Imenu support (100%)
* Incremental lexer and syntax coloring after buffer changes (75%)
* Wisent LALR parser based on official PHP yacc parser automatically converted (50%)
* Full integration with Emacs Semantic subsystem (30%)
* GPLv3 license (0%)
* Flymake support (0%)
* PSR-2 auto-formatting tool based on lexer tokens (0%)
* Travis support (0%)
* Add to MELPA (0%)
* Add to ELPA (0%)
* Use master branch for stable releases and develop for unstable (0%)

## Unit tests

If you have emacs at a customized location prefix the commands with your path, i.e.

`export emacs="~~/Documents/emacs/src/emacs" && make test-lexer`

### Lexer

Semantic token generation.

``` bash
make test-lexer
```

### Parser

Semantic grammar. Not ready yet.

``` bash
make test-parser
```

### Functions

Indentations, imenu.

``` bash
make test-functions
```

### Integration tests

Not ready yet

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

