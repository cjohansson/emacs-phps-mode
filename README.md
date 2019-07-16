# PHPs - Another Semantic Major-Mode for PHP in Emacs

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-phps-mode.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-phps-mode)

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

This mode does not require PHP installed on your computer because it has a built-in elisp based semantic lexer and semantic parser. It supports all PHP versions and Emacs >= 26.

## Features

* GPLv3 license
* Flycheck support
* Semantic lexer based on official PHP re2c lexer
* Syntax coloring based on lexer tokens, make it easier to spot invalid code
* PSR-1 and PSR-2 indentation based on lexer tokens
* Integration with `(electric-pair)`
* Incremental lexer and syntax coloring after buffer changes
* Incremental indentation and imenu calculation after buffer changes
* Supports `(comment-region)` and `(uncomment-region)`
* Imenu support
* Minimal mode map* Tested using unit tests and integration tests
* Travis support
* Included in ELPA


## Develop

Make pull requests to develop branch or branches from develop branch. Tested changes are merged to master.

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

If you have downloaded manually i.e. to `~/.emacs.d/phps-mode/` you need to add this first to your init file:

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
```

### Load using use-package

``` emacs-lisp
(use-package phps-mode
    :after flycheck
    :mode ("\\.php\\'" "\\.phtml\\'"))
```

### Load using regular elisp
``` emacs-lisp
(require 'phps-mode)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|phtml\\)\\'" . phps-mode))
```

## Contribute

This project is licensed to Free Software Foundation so sign the needed papers. Then create a branch based on a updated `develop`. Make pull request.

## Docs

* [TODO](docs/todo.md)
* [Heuristics](docs/heuristics.md)
* [Imenu algorithm](docs/imenu.md)
* [Indentation algorithm](docs/indentation.md)
