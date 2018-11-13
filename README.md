# PHPs - Experiment with a Semantic Mode for Emacs

An Emacs major mode for PHP scripting language which aims at making a full semantic integration. Currently at experimental stage.

## Goals

With current progress estimates:

* Flycheck support (100%)
* Lexer based on official PHP re2c lexer (100%)
* Syntax coloring based on lexer tokens (100%)
* Incremental lexer and syntax coloring after changes (0%)
* PSR based indentation based on lexer tokens (50%)
* Wisent LALR parser based on official PHP yacc parser automatically converted (60%)
* Flymake support (0%)
* Full integration with Emacs Semantic subsystem (0%)

## Unit tests

Not ready yet.

### Lexer

``` bash
make test-lexer
```

### Parser

``` bash
make test-parser
```

### Indentation

``` bash
make test-functions
```
