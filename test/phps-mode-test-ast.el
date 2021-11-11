;;; phps-mode-test-ast.el --- Tests for AST -*- lexical-binding: t -*-

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

;; Run from terminal make test-ast


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)
(require 'phps-mode-ast)

(defun phps-mode-test-ast--buffer-contents (buffer-contents name logic)
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

    ;; Run test
    (funcall logic)
    (message "Passed test for %S\n" name)))

(defun phps-mode-test-ast-imenu ()
  "Run test for imenu generation."
  (message "-- Running tests for imenu generation... --\n")

  (phps-mode-test-ast--buffer-contents
   "<?php\nclass myClass\n{\n\n    public function myFunction1()\n    {\n        echo \"my string with variable {$variable} inside it\";\n    }\n\n    public function myFunction2()\n    {\n    }\n\n}"
   "Imenu generated via parser SDT for simple class"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myClass" ("myFunction1" . 44) ("myFunction2" . 153)))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\ninterface myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu generated via parser SDT for interface"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myInterface" . (("myFunctionA" . 51) ("myFunctionB" . 91))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n$var = function () {\n    echo 'here';\n};"
   "Imenu generated via parser SDT for function-oriented file without namespace"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myFunctionA" . 16) ("myFunctionB" . 42))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace MyNamespace;\n\nfunction aFunction() {\n    /**\n     * With some contents\n     */\n}\n\nclass MyClass\n{\n\n    /**\n     *\n     */\n    public function __construct()\n    {\n        if ($test) {\n        }\n    }\n\n    /**\n     *\n     */\n    public function myFunction1()\n    {\n        $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n    }\n    \n    /**\n     *\n     */\n    public function myFunction2()\n    {\n    }\n\n    /**\n     * It's good\n     */\n    public function myFunction3()\n    {\n    }\n\n    /**\n     *\n     */\n    public function myFunction4()\n    {\n    }\n}\n"
   "Passed imenu-generation via parser AST for basic object oriented file"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("MyNamespace" ("aFunction" . 41) ("MyClass" ("__construct" . 160) ("myFunction1" . 261) ("myFunction2" . 433) ("myFunction3" . 513) ("myFunction4" . 583))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace MyNamespaceA\n{\n    function aFunctionA() {\n        /**\n         * With some contents\n         */\n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}\nnamespace {\n    function aFunctionB()\n    {\n        \n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}"
   "Passed imenu-generation via parser AST for advanced object oriented file"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("MyNamespaceA" ("aFunctionA" . 46) ("MyClass" ("__construct" . 205) ("myFunction1" . 338) ("myFunction2" . 542) ("myFunction3" . 646) ("myFunction4" . 740))) ("aFunctionB" . 807) ("MyClass" ("__construct" . 925) ("myFunction1" . 1058) ("myFunction2" . 1262) ("myFunction3" . 1366) ("myFunction4" . 1460)))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace {\n    class myClass extends myAbstract {\n        public function myFunctionA() {}\n        protected function myFunctionB() {}\n    }\n}\n"
   "Imenu object-oriented file with namespace, class that extends and functions"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace" ("myClass" ("myFunctionA" . 94) ("myFunctionB" . 138))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 148))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nclass myClass {}"
   "Imenu empty class"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myClass" . 13))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace {}"
   "Imenu empty bracketed namespace"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace" . 17))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace;"
   "Imenu empty namespace without brackets"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace" . 17))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace myNamespace\\myNamespace2;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("myNamespace\\myNamespace2" ("myClass" ("myFunctionA" . 121) ("myFunctionB" . 174))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nnamespace {}"
   "Imenu empty unnamed bracketed namespace"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       nil))))

  (message "\n-- Ran tests for imenu generation. --"))

(defun phps-mode-test-ast-bookkeeping ()
  "Run test for bookkeeping generation."
  (message "-- Running tests for bookkeeping generation... --\n")

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$var = 'abc';\n\nif ($var2) {\n    echo 'This never happens';\n}\nif ($var) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #1."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $var" 1) (list (list 8 12) 1) (list (list 27 32) 0) (list (list 73 77) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$var = 'abc';\n\nif ($var) {\n    echo 'This never happens';\n}\nif ($var2) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #2."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
   (should (equal
            (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
            (list (list " id $var" 1) (list (list 8 12) 1) (list (list 27 31) 1) (list (list 72 77) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$var2 = 4;\n\nfunction myFunction($var)\n{\n    $var3 = 3;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Hit';\n    }\n}\n\nfunction myFunction2($abc)\n{\n    if ($var) {\n        echo 'Miss';\n    }\n    if ($abc) {\n        echo 'Hit';\n    }\n}\n\nif ($var) {\n    echo 'Miss';\n}\nif ($var2) {\n    echo 'Hit';\n}"
   "Bookkeeping in function level with variable assignments."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" id $var2" 1) ((8 13) 1) (" function myFunction id $var" 1) ((40 44) 1) (" function myFunction id $var3" 1) ((52 57) 1) ((71 75) 1) ((113 118) 0) ((157 162) 1) (" function myFunction2 id $abc" 1) ((216 220) 1) ((232 236) 0) ((275 279) 1) ((316 320) 0) ((347 352) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n// Super-globals\n\nif ($_GET) {\n    echo 'Hit';\n}\nif ($_POST) {\n    echo 'Hit';\n}\nif ($_COOKIE) {\n    echo 'Hit';\n}\nif ($_SESSION) {\n    echo 'Hit';\n}\nif ($_REQUEST) {\n    echo 'Hit';\n}\nif ($GLOBALS) {\n    echo 'Hit';\n}\nif ($_SERVER) {\n    echo 'Hit';\n}\nif ($_FILES) {\n    echo 'Hit';\n}\nif ($_ENV) {\n    echo 'Hit';\n}\nif ($argc) {\n    echo 'Hit';\n}\nif ($argv) {\n    echo 'Hit';\n}\nif ($http_​response_​header) {\n    echo 'Hit';\n}"
   "Bookkeeping of super-globals"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should
      (equal
       (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
       (list (list (list 30 35) 1) (list (list 61 67) 1) (list (list 93 101) 1) (list (list 127 136) 1) (list (list 162 171) 1) (list (list 197 205) 1) (list (list 231 239) 1) (list (list 265 272) 1) (list (list 298 303) 1) (list (list 329 334) 1) (list (list 360 365) 1)  (list (list 391 414) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping in maximum level with namespaces, classes and functions."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should
      (equal
       (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping 1)
       (list (list " id $var" 1) (list (list 37 41) 1) (list " namespace myNamespaceA class myClassA id $var2" 1) (list (list 86 91) 1) (list " namespace myNamespaceA class myClassA function myFunctionA id $this" 1) (list " namespace myNamespaceA class myClassA function myFunctionA id $var3" 1) (list (list 128 133) 1) (list " namespace myNamespaceA class myClassA function myFunctionA id $var4" 1) (list (list 149 154) 1) (list (list 178 182) 0) (list (list 245 250) 0) (list (list 313 318) 1) (list (list 380 385) 1) (list " namespace myNamespaceA class myClassA function myFunctionB id $this" 1) (list " namespace myNamespaceA class myClassA function myFunctionB id $var5" 1) (list (list 471 476) 1) (list " namespace myNamespaceA class myClassA function myFunctionB id $var6" 1) (list (list 500 505) 1) (list (list 529 533) 0) (list (list 596 601) 0) (list (list 664 669) 0) (list (list 732 737) 0) (list (list 800 805) 1) (list (list 867 872) 1) (list (list 943 947) 1) (list (list 985 990) 0) (list (list 1029 1034) 0) (list (list 1073 1078) 0) (list (list 1117 1122) 0) (list (list 1161 1166) 0) (list " id $var7" 1) (list (list 1229 1234) 1) (list " namespace myNamespaceB class myClassB id $var8" 1) (list (list 1279 1284) 1) (list " namespace myNamespaceB class myClassB function myFunctionA id $this" 1) (list " namespace myNamespaceB class myClassB function myFunctionA id $var10" 1) (list (list 1321 1327) 1) (list " namespace myNamespaceB class myClassB function myFunctionA id $var9" 1) (list (list 1343 1348) 1) (list (list 1372 1376) 0) (list (list 1439 1444) 0) (list (list 1507 1512) 0) (list (list 1575 1580) 0) (list (list 1643 1648) 0) (list (list 1711 1716) 0) (list (list 1779 1784) 0) (list (list 1847 1852) 0) (list (list 1915 1920) 1) (list (list 1982 1988) 1) (list " namespace myNamespaceB class myClassB function myFunctionB id $this" 1) (list " namespace myNamespaceB class myClassB function myFunctionB id $var12" 1) (list (list 2074 2080) 1) (list " namespace myNamespaceB class myClassB function myFunctionB id $var11" 1) (list (list 2104 2110) 1) (list (list 2134 2138) 0) (list (list 2201 2206) 0) (list (list 2269 2274) 0) (list (list 2337 2342) 0) (list (list 2405 2410) 0) (list (list 2472 2477) 0) (list (list 2539 2544) 0) (list (list 2607 2612) 0) (list (list 2675 2680) 0) (list (list 2743 2749) 0) (list (list 2812 2818) 1) (list (list 2880 2886) 1) (list (list 2957 2961) 1) (list (list 2999 3004) 0) (list (list 3043 3048) 0) (list (list 3087 3092) 0) (list (list 3131 3136) 0) (list (list 3175 3180) 0) (list (list 3219 3224) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n// Conditional assignments\n\n$items = array(1, 2, 3);\nforeach ($items as $item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => $value) {\n    if ($key || $value) {\n        echo 'Hit';\n    }\n}\nfor ($i = 0; $i < count($items); $i++) {\n    if ($i) {\n        echo 'Hit';\n    }\n}\nif ($a = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\nwhile ($b = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\ndo {\n    echo 'Hit';\n} while ($c = 456);\n"
   "Bookkeeping of conditional assignments"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $items" 1) (list (list 36 42) 1) (list (list 70 76) 1) (list " id $item" 1) (list (list 80 85) 1) (list (list 97 102) 1) (list (list 143 149) 1) (list " id $key" 1) (list (list 153 157) 1) (list " id $value" 1) (list (list 161 167) 1) (list (list 179 183) 1) (list (list 187 193) 1) (list " id $i" 1) (list (list 230 232) 1) (list (list 238 240) 1) (list (list 249 255) 1) (list (list 258 260) 1) (list (list 274 276) 1) (list " id $a" 1) (list (list 312 314) 1) (list (list 332 334) 1) (list " id $b" 1) (list (list 373 375) 1) (list (list 393 395) 1) (list " id $c" 1) (list (list 457 459) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n// Class properties\n\nclass myParent {}\n\nclass myClass extends myParent {\n    private $var1 = 123;\n    protected static $var2;\n    public $var3;\n    var $var4;\n    function __construct() {\n        if ($this) {\n            echo 'Hit';\n        }\n        if ($this->var1) {\n            echo 'Hit';\n        }\n        if (self::$var1) {\n            echo 'Miss';\n        }\n        if (self::$var2) {\n            echo 'Hit';\n        }\n        if ($this->var3) {\n            echo 'Hit';\n        }\n        if ($this->var4) {\n            echo 'Hit';\n        }\n        if ($this->var5) {\n            echo 'Miss';\n        }\n        if (paren1) {\n            echo 'Hit';\n        }\n    }\n}\n\nif ($this) {\n    echo 'Miss';\n}\nif (self) {\n    echo 'Miss';\n}\nif (paren1) {\n    echo 'Miss';\n}"
   "Bookkeeping of class properties"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " class myParent id $var1" 1) (list (list 93 98) 1) (list " class myParent static id $var2" 1) (list (list 127 132) 1) (list " class myParent id $var3" 1) (list (list 145 150) 1) (list " class myParent id $var4" 1) (list (list 160 165) 1) (list " class myParent function __construct id $this" 1) (list (list 208 213) 1) (list (list 263 268) 1) (list (list 270 274) 1) (list (list 330 335) 0) (list (list 392 397) 1) (list (list 447 452) 1) (list (list 454 458) 1) (list (list 508 513) 1) (list (list 515 519) 1) (list (list 569 574) 1) (list (list 576 580) 0) (list (list 688 693) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\ntry {\n    \n} catch (\\Exception $e) {\n    if ($e) {\n        echo 'Hit';\n    }\n}\n\nif ($e) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of try catch variable assignment"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $e" 1) (list (list 39 41) 1) (list (list 53 55) 1) (list (list 92 94) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$example = function ($test) {\n    if ($test) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Miss';\n    }\n};\n$example2 = function ($test2) use ($example) {\n    if ($test2) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Hit';\n    }\n    if ($example2) {\n        echo 'Miss';\n    }\n    if ($example3) {\n        echo 'Miss';\n    }\n};\nif ($test) {\n    echo 'Miss';\n}\nif ($test2) {\n    echo 'Miss';\n}"
   "Bookkeeping of anonymous function variable assignments"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $example" 1) (list (list 8 16) 1) (list " anonymous function 1 id $test" 1) (list (list 29 34) 1) (list (list 46 51) 1) (list (list 89 97) 0) (list " id $example2" 1) (list (list 131 140) 1) (list " anonymous function 2 id $test2" 1) (list (list 153 159) 1) (list " anonymous function 2 id $example" 1) (list (list 166 174) 1) (list (list 186 192) 1) (list (list 230 238) 1) (list (list 276 285) 0) (list (list 324 333) 0) (list (list 371 376) 0) (list (list 403 409) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nclass myClass {\n    function random() {}\n    function __construct()\n    {\n        $this->random();\n        $this->random['abc'] = 123;\n    }\n}"
   "Method calls should be avoided in bookkeeping"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " class myClass function __construct id $this" 1) (list (list 89 94) 1) (list (list 114 119) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n$items = array(1, 2, 3);\nforeach ($items as &$item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => &$item2) {\n    if ($item) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of foreach reference variable declaration"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $items" 1) (list (list 7 13) 1) (list (list 41 47) 1) (list " id $item" 1) (list (list 52 57) 1) (list (list 69 74) 1) (list (list 115 121) 1) (list " id $key" 1) (list (list 125 129) 1) (list " id $item2" 1) (list (list 134 140) 1) (list (list 152 157) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n[$random, $bandom] = myValues();\nif ($random) {\n    echo 'Hit';\n}\nif ($bandom) {\n    echo 'Hit';\n}\n\narray($random2, $bandom2) = myValues2();\nif ($random2) {\n    echo 'Hit';\n}\nif ($bandom3) {\n    echo 'Hit';\n}\n\n    "
   "Bookkeeping of variable declarations in array"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $random" 1) (list (list 9 16) 1) (list " id $bandom" 1) (list (list 18 25) 1) (list (list 45 52) 1) (list (list 78 85) 1) (list " id $random2" 1) (list (list 114 122) 1) (list " id $bandom2" 1) (list (list 124 132) 1) (list (list 153 161) 1) (list (list 187 195) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    global $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of global variable declaration in function"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $var" 1) (list (list 8 12) 1) (list " function test id $abc" 1) (list (list 35 39) 1) (list " function test id $var" 1) (list (list 54 58) 1) (list (list 68 72) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n$y = 1;\n$fn1 = fn($x) => $x + $y;\n$z = 1;\n$fn = fn($x2) => fn($y2) => $x2 * $y2 + $z;\nfn(array $x3) => $x3;\n$x4 = 4;\nstatic fn(): int => $x4;\nfn($x5 = 42) => $x5;\nfn(&$x6) => $x6;\nfn&($x7) => $x7;\nfn($x8, ...$rest) => $rest;"
   "Bookkeeping in arrow functions"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $y" 1) (list (list 7 9) 1) (list " id $fn1" 1) (list (list 15 19) 1) (list " arrow function 1 id $x" 1) (list (list 25 27) 1) (list (list 32 34) 1) (list (list 37 39) 1) (list " id $z" 1) (list (list 41 43) 1) (list " id $fn" 1) (list (list 49 52) 1) (list " arrow function 2 id $x2" 1) (list (list 58 61) 1) (list " arrow function 2 id $y2" 1) (list (list 69 72) 1) (list (list 77 80) 1) (list (list 83 86) 1) (list (list 89 91) 1) (list " arrow function 3 id $x3" 1) (list (list 102 105) 1) (list (list 110 113) 1) (list " id $x4" 1) (list (list 115 118) 1) (list (list 144 147) 1) (list " arrow function 5 id $x5" 1) (list (list 152 155) 1) (list (list 165 168) 1) (list " arrow function 6 id $x6" 1) (list (list 174 177) 1) (list (list 182 185) 1) (list " arrow function 7 id $x7" 1) (list (list 191 194) 1) (list (list 199 202) 1) (list " arrow function 8 id $x8" 1) (list (list 207 210) 1) (list " arrow function 8 id $rest" 1) (list (list 215 220) 1) (list (list 225 230) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n$z = (object) array('name' => 'random');\nif ($z->name) {\n    echo 'Hit';\n}"
   "Bookkeeping object properties."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $z" 1) (list (list 7 9) 1) (list (list 52 54) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nif (!$var = false) {\n    echo 'Hit';\n}\n"
   "Bookkeeping negative conditional assignment"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " id $var" 1) (list (list 12 16) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nif (isset($x)) {\n    if ($x) {\n        echo 'Hit';\n        if (isset($i, $u)) {\n            if ($i) {\n                echo 'Hit';\n            }\n            if ($u) {\n                echo 'Hit';\n            }\n            if ($x) {\n                echo 'Hit';\n            }\n        }\n        if ($i) {\n            echo 'Miss';\n        }\n        if ($u) {\n            echo 'Miss';\n        }\n    }\n}\nif ($x) {\n    echo 'Miss';\n}\n\nif (!empty($y)) {\n    if ($y) {\n        echo 'Hit';\n        if (!empty($k) && !empty($L)) {\n            if ($k) {\n                echo 'Hit';\n            }\n            if ($L) {\n                echo 'Hit';\n            }\n            if ($y) {\n                echo 'Hit';\n            }\n        }\n        if ($k) {\n            echo 'Miss';\n        }\n        if ($L) {\n            echo 'Miss';\n        }\n    }\n}\nif ($y) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of isset() and !empty() scoped variables."
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " defined 1 id $x" 1) (list (list 18 20) 1) (list (list 33 35) 1) (list " defined 2 id $i" 1) (list (list 77 79) 1) (list " defined 2 id $u" 1) (list (list 81 83) 1) (list (list 104 106) 1) (list (list 168 170) 1) (list (list 232 234) 1) (list (list 302 304) 0) (list (list 355 357) 0) (list (list 408 410) 0) (list " defined 3 id $y" 1) (list (list 445 447) 1) (list (list 460 462) 1) (list " defined 4 id $k" 1) (list (list 505 507) 1) (list " defined 4 id $L" 1) (list (list 519 521) 1) (list (list 542 544) 1) (list (list 606 608) 1) (list (list 670 672) 1) (list (list 740 742) 0) (list (list 793 795) 0) (list (list 846 848) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\ninterface myInterface\n{\n    function myFunction1();\n    function myFunction2($x);\n}\n"
   "Bookkeeping variable in interface function"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              (list (list " class myInterface function myFunction2 id $x" 1) (list (list 84 86) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nfunction myFunction1()\n{\n    return isset($a);\n}\n\nfunction myFunction2()\n{\n    $b = 2;\n    if ($b) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping after definition condition"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '(((50 52) 0) (" function myFunction2 id $b" 1) ((87 89) 1) ((103 105) 1) ((143 145) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$a = array(1, 2, 3);\nforeach ($a as $uri => $page)\n{\n    if (isset($pages)) {\n        if ($a) {\n            echo 'Hit';\n        }\n        if ($uri) {\n            echo 'Hit';\n        }\n        if ($page) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "Bookkeeping of foreach variable inside if (isset()) block"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" id $a" 1) ((8 10) 1) ((38 40) 1) (" id $uri" 1) ((44 48) 1) (" id $page" 1) ((52 57) 1) (" defined 1 id $pages" 1) ((75 81) 1) ((98 100) 1) ((150 154) 1) ((204 209) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nif (isset($b)) {\n    $b = false;\n}\n$c = 2;\n\nif ($c) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable after isset() block"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" defined 1 id $b" 2) ((17 19) 1) ((28 30) 1) (" id $c" 1) ((42 44) 1) ((55 57) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nif (!isset($a)) {\n    if ($a) {\n        echo 'Miss';\n    }\n}"
   "Bookkeeping for variable in negative isset() conditional"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '(((18 20) 0) ((33 35) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nfunction myFunction($a, $b, $c, $d)\n{\n    global $f, $g;\n    if (isset($f)) {\n        if (!empty($g)) {\n            if ($a) {\n                echo 'Hit';\n            }\n            if ($b) {\n                echo 'Hit';\n            }\n            if ($c) {\n                echo 'Hit';\n            }\n            if ($d) {\n                echo 'Hit';\n            }\n        }\n    }\n}\n"
   "Bookkeeping variables inside nested isset() !empty() blocks"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" function myFunction id $a" 1) ((28 30) 1) (" function myFunction id $b" 1) ((32 34) 1) (" function myFunction id $c" 1) ((36 38) 1) (" function myFunction id $d" 1) ((40 42) 1) (" function myFunction id $f" 1) ((57 59) 1) (" function myFunction id $g" 1) ((61 63) 1) (" function myFunction defined 1 id $f" 1) ((79 81) 1) (" function myFunction defined 2 id $g" 1) ((105 107) 1) ((128 130) 1) ((192 194) 1) ((256 258) 1) ((320 322) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    static $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of static variable declaration in function"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" id $var" 1) ((8 12) 1) (" function test id $abc" 1) ((35 39) 1) (" function test id $var" 1) ((54 58) 1) ((68 72) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nglobal $a, $b;\n\nif ($a) {\n    echo 'Hit';\n}\n\nfunction myFunction($c)\n{\n    global $a;\n    if ($a) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping of global variables in functional-oriented file"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" id $a" 1) ((15 17) 1) (" id $b" 1) ((19 21) 1) ((28 30) 1) (" function myFunction id $c" 1) ((73 75) 1) (" function myFunction id $a" 1) ((90 92) 1) ((102 104) 1) ((142 144) 0))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nstatic $a;\n\nif ($a) {}\n\nfunction test()\n{\n    static $a;\n    if ($a) {}\n}\n\nclass There\n{\n    function here()\n    {\n        static $a;\n        if ($a) {}\n    }\n}"
   "Bookkeeping of static variables in different scopes without namespaces"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" id $a" 1) ((15 17) 1) ((24 26) 1) (" function test id $a" 1) ((61 63) 1) ((73 75) 1) (" class There function here id $this" 1) (" class There function here id $a" 1) ((138 140) 1) ((154 156) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace Here\n{\n    function here()\n    {\n        static $a;\n        if ($a) {}\n    }\n    class There\n    {\n        public function Near()\n        {\n            static $a;\n            if ($a) {}\n        }\n    }\n}\nnamespace\n{\n    static $a;\n    if ($a) {}\n}\n"
   "Bookkeeping of static variables in different scopes with namespaces"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should (equal
              (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
              '((" namespace Here function here id $a" 1) ((66 68) 1) ((82 84) 1) (" namespace Here class There function Near id $this" 1) (" namespace Here class There function Near id $a" 1)  ((177 179) 1) ((197 199) 1) (" id $a" 1) ((245 247) 1) ((257 259) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nclass There\n{\n    private $variable;\n    private \\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of typed class variables"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should
      (equal
       (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
       '((" class There id $variable" 1) ((33 42) 1) (" class There id $variable2" 1) ((67 77) 1) (" class There id $variable3" 1) ((98 108) 1) (" class There static id $variable4" 1) ((129 139) 1) (" class There static id $variable5" 1) ((171 181) 1) (" class There static id $variable6" 1) ((209 219) 1) (" class There function here id $this" 1) ((259 264) 1) ((266 274) 1) ((291 296) 1) ((298 307) 1) ((324 329) 1) ((331 340) 1) ((357 362) 1) ((364 373) 0) ((396 406) 1) ((429 439) 1) ((462 472) 1))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\n$a = $b = $c = 3;\n\nif ($a) {\n    echo 'a=',$a;\n} else {\n    echo '$a is undefined!';\n}\nif ($b) {\n    echo 'b=',$b;\n} else {\n    echo '$b is undefined!';\n}\nif ($c) {\n    echo 'c=',$c;\n} else {\n    echo '$c is undefined!';\n}"
   "Bookkeeping of typed class variables"
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
            (car (cdr production))))))
     (phps-mode-ast-generate)
     (should
      (equal
       (phps-mode-test--hash-to-list phps-mode-ast--bookkeeping t)
       '((" id $a" 1) ((8 10) 1) (" id $b" 1) ((13 15) 1) (" id $c" 1) ((18 20) 1) ((31 33) 1) ((51 53) 1) ((99 101) 1) ((119 121) 1) ((167 169) 1) ((187 189) 1))))))

  (message "\n-- Ran tests for bookkeeping generation. --"))

(defun phps-mode-test-ast ()
  "Run test for ast."
  (message "-- Running all tests for ast... --\n")

  (phps-mode-test-ast-imenu)
  (phps-mode-test-ast-bookkeeping)

  (message "\n-- Ran all tests for ast. --"))

(phps-mode-test-ast)

(provide 'phps-mode-test-ast)


;;; phps-mode-test-ast.el ends here
