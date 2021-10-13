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
(require 'phps-mode-lexer)
(require 'phps-mode-parser)

(defun phps-mode-test-parser--buffer-contents (buffer-contents name logic)
  (generate-new-buffer "*PHPs Lexer*")
  (with-current-buffer "*PHPs Lexer*"
    (kill-region (point-min) (point-max))
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))

    (funcall logic)

    (message "Passed %s" name)
    (kill-buffer)))

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
         '(80 459 466 411 333 332 154 102 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<? echo 'hello'; ?>"
   "Basic echo test 2 with short open tag and close tag"
   (lambda()
     (should
      (equal
       '(80 459 466 411 333 332 154 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?= 'hello';"
   "Basic echo test 3 with open tag with echo"
   (lambda()
     (should
      (equal
       '(80 459 466 411 333 332 154 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'\necho 'here';"
   "Basic echo test 4 with invalid code"
   (lambda()
     (should-error
      (phps-mode-parser-parse))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'"
   "Basic echo test 5 with invalid code"
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
         '(80 459 466 411 333 332 154 102 79 155 102 79 459 466 411 333 332 154 102 79 155 102 79)
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
         '(80 427 431 428 176 178 428 247 241 238 120 236 266 429 137 502 492 498 461 411 345 156 138 136 502 492 498 342 481 151 138 136 429 175 98 105 79)
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
         '(80 77 81 107 79 427 431 428 176 178 428 247 241 238 120 236 266 429 137 502 492 498 461 411 345 156 138 136 502 492 498 342 481 151 138 136 429 175 98 105 79)
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
         '(80 77 81 108 79 137 427 431 428 176 178 428 247 241 238 120 236 266 429 137 502 492 498 461 411 345 156 138 136 502 492 498 342 481 151 138 136 429 175 98 139 136 142 102 79)
         parse)))))

  ;; TODO Make following test work
  ;; (phps-mode-test-parser--buffer-contents
  ;;  "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
  ;;  "Object oriented PHP with bracket namespace"
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

;; TODO
;; phps-mode-parser.el:65:167: Warning: reference to free variable
;;     ‘phps-mode-lexer--generated-new-tokens-index’
;; phps-mode-parser.el:65:322: Warning: reference to free variable
;;     ‘phps-mode-lexer--generated-new-tokens’
;; phps-mode-parser.el:65:406: Warning: reference to free variable
;;     ‘semantic-lex-end-point’

;; In end of data:
;; phps-mode-parser.el:65:271: Warning: the function ‘phps-mode-lexer--re2c’ is
;;     not known to be defined.


;;; phps-mode-test-parser.el ends here
