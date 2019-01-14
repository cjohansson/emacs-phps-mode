# PHPs - Experiment with a Semantic Mode for Emacs

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

This mode does not require PHP installed on computer because it has a elisp based lexer and parser. It supports all PHP versions.

## Goals

*With current progress estimates:*

* GPLv3 license (100%)
* Flycheck support (PHP Mess Detector, PHP Code Sniffer) (100%)
* Syntax coloring based on lexer tokens (100%)
* PSR-1 and PSR-2 indentation based on lexer tokens (100%)
* Lexer based on official PHP re2c lexer (100%)
* Imenu support (100%)
* Incremental lexer and syntax coloring after buffer changes (75%)
* Wisent LALR parser based on official PHP yacc parser automatically converted grammar (50%)
* Full integration with Emacs Semantic subsystem (30%)
* Flymake support (0%)
* PSR-2 auto-formatting tool based on lexer tokens (0%)
* Travis support (0%)
* Add to MELPA (0%)
* Add to ELPA (0%)
* From first stable release, use master branch for stable releases and develop branch for on-going work (0%)

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

Indentations, incremental processes, Imenu-support.

``` bash
make test-functions
```

### Integration tests

This should test all other parts in collaboration. Not ready yet

``` bash
make test-integration
```

### All tests

``` bash
make tests
```

## Installation example

Download to `~/.emacs.d/phps-mode/` and then add this to your init file:

### Using use-package

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
(use-package phps-mode
    :mode ("\\.php\\'" "\\.phtml\\'"))
```

