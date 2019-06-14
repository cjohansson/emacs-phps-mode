# PHPs - Experiment with a Semantic Mode for Emacs

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-phps-mode.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-phps-mode)

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

This mode does not require PHP installed on your computer because it has a built-in elisp based lexer and parser. It supports all PHP versions and Emacs >= 26.

## Goals

*With current progress estimates:*

* GPLv3 license (100%)
* Flycheck support (PHP Mess Detector, PHP Code Sniffer) (100%)
* Lexer based on official PHP re2c lexer (100%)
* Syntax coloring based on lexer tokens (100%)
* PSR-1 and PSR-2 indentation based on lexer tokens (100%)
* Integration with `(electric-pair)` (100%)
* Incremental lexer and syntax coloring after buffer changes (100%)
* Incremental indentation and imenu calculation after buffer changes (100%)
* Supports `(comment-region)` and `(uncomment-region)` (100%)
* From first stable release, use master branch for stable releases and develop branch for on-going work (100%)
* Travis support (100%)
* Imenu support (100%)
* Minimal mode map (100%)
* A set of heuristics to improve large-file incremental change handling (50%)
* Wisent LALR parser based on official PHP yacc parser automatically converted grammar (50%)
* Full integration with Emacs Semantic subsystem (30%)
* Approach flycheck about including support for this module by default (0%)
* Eldoc support (0%)
* Flymake support (0%)
* PSR-2 auto-formatting tool based on lexer tokens (0%)
* Add to MELPA (0%)
* Add to ELPA (0%)

## Tests

If you have emacs at a customized location prefix the commands with your path, i.e.

`export emacs="~/Documents/emacs/src/emacs" && make tests`

Run all tests with `make tests`.

### Functions

Indentations, incremental processes, Imenu-support.

``` bash
make test-functions
```

### Integration

This should test all other parts in collaboration.

``` bash
make test-integration
```

### Lexer

Lexer token generation.

``` bash
make test-lexer
```

### Parser

Semantic grammar. Not ready yet.

``` bash
make test-parser
```

### Syntax-table

Basic point and region behaviour.

``` bash
make test-syntax-table
```

## Byte-compilation

Plug-in should support byte-compilation and it is recommended.

### Compile

``` bash
make compile
```

### Clean

``` bash
make clean
```

## Installation example

Download to `~/.emacs.d/phps-mode/` and then add this to your init file:

### Using use-package with flycheck support

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
(use-package phps-mode
    :after flycheck
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
    (setq phps-mode-flycheck-support t))
```

