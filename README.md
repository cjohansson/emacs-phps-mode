# PHPs - Another Semantic Major-Mode for PHP in Emacs

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Build Status](https://travis-ci.org/cjohansson/emacs-phps-mode.svg?branch=master)](https://travis-ci.org/cjohansson/emacs-phps-mode)

**Goal**: An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at *usable* stage.

This mode does not require PHP installed on your computer because it has a built-in elisp based semantic lexer. It supports all PHP versions and Emacs >= 26.

## Features

* GPLv3 license
* Flycheck support with `(phps-mode-flycheck-setup)`
* Semantic lexer based on official PHP re2c lexer
* Syntax coloring based on lexer tokens, makes it easier to spot invalid code
* PSR-1 and PSR-2 indentation based on lexer tokens
* Integration with `(electric-pair)`
* Incremental lexer and syntax coloring after buffer changes
* Incremental indentation and imenu generation after buffer changes
* Supports `(comment-region)` and `(uncomment-region)`
* Support indentation for inline-html areas
* Imenu support
* Minimal mode map
* Tested using unit tests and integration tests
* Continuous integration tests using Travis
* Included in GNU ELPA package archive
* A interactive function that can be used interactively to format buffers `(phps-mode-format-buffer)`

## Roadmap

* Wisent Parser support

## Keymap

* `C-c C-r` Rescan buffer
* `C-c C-f` Format buffer

## Installation

### Install manually

If you have downloaded manually i.e. to `~/.emacs.d/phps-mode/` you need to add this first to your init file:

``` emacs-lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/phps-mode/"))
```

### Install via package manager

You can install via ELPA (`M-x package-install` + `RET` + `phps-mode` + `RET`), package will now be loaded automatically when Emacs starts.

### Install, load and configure via use-package

``` emacs-lisp
(use-package phps-mode
    :after flycheck
    :ensure t
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
    (phps-mode-flycheck-setup))
```

### Load and configure using use-package

``` emacs-lisp
(use-package phps-mode
    :after flycheck
    :mode ("\\.php\\'" "\\.phtml\\'")
    :config
    (phps-mode-flycheck-setup))
```

### Load and configure using regular emacs-lisp
``` emacs-lisp
(require 'phps-mode)
(add-to-list 'auto-mode-alist '("\\.\\(php\\|phtml\\)\\'" . phps-mode))
(phps-mode-flycheck-setup)
```

## Read more

* [Development](docs/development.md)
* [Heuristics](docs/heuristics.md)
* [Indentation algorithm](docs/indentation.md)
* [Imenu algorithm](docs/imenu.md)
* [TODO](docs/todo.md)
