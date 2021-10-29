;;; phps-mode-test-parser.el --- Tests for parser -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

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

;; Run from terminal make test-parser


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-lex-analyzer)
(require 'phps-mode-parser)

(defun phps-mode-test-parser--buffer-contents (buffer-contents name logic)
  (with-temp-buffer
    ;; Setup buffer
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))
    
    ;; Setup lexer
    (setq
     phps-mode-lexer--generated-tokens
     nil)
    (setq
     phps-mode-lexer--state
     'ST_INITIAL)
    (setq
     phps-mode-lexer--states
     nil)
    (setq
     phps-mode-lexer--state-stack
     nil)
    (setq
     phps-mode-lexer--heredoc-label
     nil)
    (setq
     phps-mode-lexer--heredoc-label-stack
     nil)
    (setq
     phps-mode-lexer--nest-location-stack
     nil)

    ;; Run lexer
    (setq
     semantic-lex-analyzer
     #'phps-mode-lex-analyzer--re2c-lex)
    (setq
     semantic-lex-syntax-table
     phps-mode-syntax-table)
    (semantic-lex-buffer)
    (setq
     phps-mode-parser-tokens
     (phps-mode-lex-analyzer--generate-parser-tokens
      phps-mode-lexer--generated-tokens))
    (message "Lexer tokens:\n%S\n" phps-mode-lexer--generated-tokens)
    (message "Parser tokens:\n%S\n" phps-mode-parser-tokens)

    ;; Run test
    (funcall logic)
    (message "Passed %s" name)))

(defun phps-mode-test-parser-boundaries ()
  "Run test for lexer."
  (message "-- Running tests for parser boundaries... --\n")

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'hello';"
   "Basic echo test"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 449 456 403 325 324 152 102 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<? echo 'hello'; ?>"
   "Basic echo test 2 with short open tag and close tag"
   (lambda()
     (should
      (equal
       '(80 449 456 403 325 324 152 102 79 159 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?= 'hello';"
   "Basic echo test 3 with open tag with echo"
   (lambda()
     (should
      (equal
       '(80 449 456 403 325 324 152 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'\necho 'here';"
   "Basic echo test 4 with invalid code"
   (lambda()
     (should-error
      (phps-mode-parser-parse))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'"
   "Basic echo test 5 with valid code in parser mode only"
   (lambda()
     (should-error
      (phps-mode-parser-parse))))

  (phps-mode-test-parser--buffer-contents
   "<? echo '<!DOCTYPE html>'; ?><html><head><?php echo 'My Title'; ?><body></html>"
   "Advanced echo test with 2 echo sections"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 449 456 403 325 324 152 102 79 159 102 79 153 102 79 449 456 403 325 324 152 102 79 159 102 79 153 102 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nfunction myFunction($arg) { $arg = 2; return $arg; }"
   "Simple function defintion"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 419 423 420 237 243 175 177 420 241 236 233 118 231 260 421 135 492 482 488 451 403 337 154 136 134 492 482 488 334 471 149 136 134 421 174 98 103 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nfunction myFunction($arg) {\n    $arg = 2;\n    return $arg;\n}\n"
   "Simple function defintion inside un-bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 77 81 106 79 419 423 420 237 243 175 177 420 241 236 233 118 231 260 421 135 492 482 488 451 403 337 154 136 134 492 482 488 334 471 149 136 134 421 174 98 103 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace {\n    function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion inside bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 77 81 80 419 423 420 237 243 175 177 420 241 236 233 118 231 260 421 135 492 482 488 451 403 337 154 136 134 492 482 488 334 471 149 136 134 421 174 98 103 79 107 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nclass MyClass\n{\n    private $var = 'abc';\n    public function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion and property inside class inside non-bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation:\n%S\n" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 77 81 106 79 187 191 420 277 311 307 303 243 449 456 403 420 318 316 278 281 276 309 307 306 419 423 77 420 237 243 175 177 420 241 236 233 118 231 260 421 135 492 482 488 451 403 337 154 136 134 492 482 488 334 471 149 136 134 302 421 280 281 276 180 99 103 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Object oriented PHP with bracket namespace"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php if (empty($parameters['PARAMETER_CONFIGURATION_INTERNAL_FILENAME'])) { $parameters['PARAMETER_CONFIGURATION_INTERNAL_FILENAME'] = ''; }"
   "Complex if-conditional"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho \"My $array[12] random statement\";\n"
   "Long inside array offset"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n\necho \"\\$a['{$k}']\";"
   "A tricky case where variable inside double quote is escaped"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n\n//exit program normally\nexit;\nexit();\nexit(0);\n\n//exit with an error code\nexit(1);\nexit(0376); //octal\n\n?>"
   "Example #2 exit status example"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n\n//die program normally\ndie;\ndie();\ndie(0);\n\n//die with an error code\ndie(1);\ndue(0376); //octal\n\n?>"
   "Example #2 die status example"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'here' ?>"
   "Expression without trailing semi-colon but with close tag"
   (lambda()
     (phps-mode-parser-parse)))

  ;; TODO Make this work
  ;; (phps-mode-test-parser--buffer-contents
  ;;  "<?php\n$food = 'cake';\n\n$return_value = match ($food) {\n    default => 'This food is an apple',\n};\n\nvar_dump($return_value);\n?>"
  ;;  "Basic match usage 1"
  ;;  (lambda()
  ;;    (phps-mode-parser-parse)))

  ;; TODO Make this work
  ;; (phps-mode-test-parser--buffer-contents
  ;;  "<?php\n$food = 'cake';\n\n$return_value = match ($food) {\n    'apple' => 'This food is an apple',\n    'bar' => 'This food is a bar',\n    'cake' => 'This food is a cake',\n};\n\nvar_dump($return_value);\n?>"
  ;;  "Basic match usage 2"
  ;;  (lambda()
  ;;    (phps-mode-parser-parse)))

  (message "\n-- Ran tests for parser boundaries. --"))

(defun phps-mode-test-parser ()
  "Run test for lexer."
  (message "-- Running all tests for parser... --\n")

  (phps-mode-test-parser-boundaries)

  (message "\n-- Ran all tests for parser. --"))

(phps-mode-test-parser)

(provide 'phps-mode-test-parser)


;;; phps-mode-test-parser.el ends here
