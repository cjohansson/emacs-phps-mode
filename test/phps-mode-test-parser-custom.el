;;; phps-mode-test-parser-custom.el --- Tests for custom parser -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Run from terminal make test-parser-custom


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(defun phps-mode-tester-parser-custom--generate-parser-table ()
  "Test this."
  (message "\n-- Run tests for generate-parser-table. --\n")

  (phps-mode-parser-custom--generate-parser)

  (message "Generated parser.\n")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '("," 1 . 2))
     'possible_comma)
    (list '(possible_comma (("," 1 . 2))))))
  (message "Passed empty test 1")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10))
     'possible_comma)
    (list '(possible_comma) '(T_STRING 1 . 10))))
  (message "Passed empty test 2")
  
  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10))
     'namespace_name)
    (list '(namespace_name ((T_STRING 1 . 10))))))
  (message "Passed non-recursive parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10) '(T_NS_SEPARATOR 11 . 12) '(T_STRING 13 . 16) '(T_NS_SEPARATOR 17 . 18) '(T_STRING 19 . 29))
     'namespace_name)
    (list '(namespace_name ((namespace_name ((namespace_name ((T_STRING 1 . 10))) (T_NS_SEPARATOR 11 . 12) (T_STRING 13 . 16))) (T_NS_SEPARATOR 17 . 18) (T_STRING 19 . 29))))))
  (message "Passed recursive parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10) '(T_NS_SEPARATOR 11 . 12) '(T_STRING 13 . 16) '(T_NS_SEPARATOR 17 . 18))
     'namespace_name)
    (list '(namespace_name ((namespace_name ((T_STRING 1 . 10))) (T_NS_SEPARATOR 11 . 12) (T_STRING 13 . 16))) '(T_NS_SEPARATOR 17 . 18))))
  (message "Passed recursive semi-parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_ECHO 7 . 11) '(T_CONSTANT_ENCAPSED_STRING 12 . 22) '(";" 22 . 23)))
    (list '(reserved_non_modifiers ((T_ECHO 7 . 11))) '(T_CONSTANT_ENCAPSED_STRING 12 . 22) '(";" 22 . 23))))

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_FUNCTION 7 . 11)))
    (list '(use_type ((T_FUNCTION 7 . 11))))))

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15) '(";" 16 . 17))
     'top_statement)
    (list '(top_statement ((T_HALT_COMPILER 7 . 11) ("(" 12 . 13) (")" 14 . 15) (";" 16 . 17))))))
  (message "Passed full top_statement")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15))
     'top_statement)
    (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15))))
  (message "Passed incomplete top_statement")

  (phps-mode-test--with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case PHP"
   ;; (message "Parse: '%s'" (phps-mode-parser-custom--parse))
   (should
    (equal
     (phps-mode-parser-custom--parse)
     (list '(T_OPEN_TAG 1 . 7) '(reserved_non_modifiers ((T_SWITCH 7 . 13))) '("(" 14 . 15) '(T_STRING 15 . 32) '("(" 32 . 33) '(")" 33 . 34) '(")" 34 . 35) '("{" 36 . 37) '(reserved_non_modifiers ((T_CASE 38 . 42))) '(T_CONSTANT_ENCAPSED_STRING 43 . 59) '(":" 59 . 60) '(reserved_non_modifiers ((T_ECHO 61 . 65))) '(T_CONSTANT_ENCAPSED_STRING 66 . 87) '(";" 87 . 88) '("}" 89 . 90)))))

  (phps-mode-test--with-buffer
   "\n\n \t<html><title>echo \"Blaha\";</title><?php\n\n\nexit?>\n\n<html><random /></html><?php exit ?>"
   "Another mixed inline HTML and PHP"
   ;; (message "Parse: '%s'" (phps-mode-parser-custom--parse))
   (should
    (equal
     (phps-mode-parser-custom--parse)
     (list '(T_INLINE_HTML 1 . 39) '(T_OPEN_TAG 39 . 45) '(reserved_non_modifiers ((T_EXIT 47 . 51))) '(";" 51 . 53) '(T_CLOSE_TAG 51 . 53) '(T_INLINE_HTML 53 . 78) '(T_OPEN_TAG 78 . 84) '(reserved_non_modifiers ((T_EXIT 84 . 88))) '(";" 89 . 91) '(T_CLOSE_TAG 89 . 91)))))

  (phps-mode-test--with-buffer
   "<?php\nclass foo\n{\n    public $foo;\n    public $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<'EOT'\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should not print a capital 'A': \x41\nEOT;\n?>\n"
   "Example #8 Nowdoc string quoting example with variables"
   (message "Parse: '%s'" (phps-mode-parser-custom--parse))
   (should
    (equal
     (phps-mode-parser-custom--parse)
     nil)))

  (message "\n-- Ran tests for generate-parser-table. --"))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-tester-parser-custom--generate-parser-table)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
