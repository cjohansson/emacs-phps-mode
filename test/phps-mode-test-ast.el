;;; phps-mode-test-ast.el --- Tests for AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make test-ast


;;; Code:


(require 'ert)
(require 'phps-mode)
(require 'phps-mode-ast)
(require 'phps-mode-ast-bookkeeping)
(require 'phps-mode-ast-imenu)
(require 'phps-mode-lex-analyzer)
(require 'phps-mode-test)

(defun phps-mode-test-ast--should-bookkeep (buffer-contents name bookkeeping)
  (phps-mode-test-ast--buffer-contents
   buffer-contents
   name
   (lambda()
      (let ((parse (phps-mode-parser-parse)))
        (message "Left-to-right with right-most derivation:\n%S\n" parse)
        (dolist (production-number parse)
          (let ((production
                 (phps-mode-parser--get-grammar-production-by-number
                  production-number)))
            (message
             "%d: %S -> %S"
             production-number
             (car (car production))
             (car (cdr production))))))
      (message "\n")
      (phps-mode-ast--generate)
      (phps-mode-ast-bookkeeping--generate)

      (message
       "expected-bookkeeping:\n%S\n"
       bookkeeping)
      (message
       "actual-bookkeeping:\n%S\n"
       (phps-mode-test--hash-to-list
        phps-mode-ast-bookkeeping--index
        t))
       (message
        "object-index:\n%S\n"
         phps-mode-ast-bookkeeping--object-index)

      (should
       (equal
        (phps-mode-test--hash-to-list
         phps-mode-ast-bookkeeping--index
         t)
        bookkeeping)))))

(defun phps-mode-test-ast--should-imenu (buffer-contents name imenu)
  (phps-mode-test-ast--buffer-contents
   buffer-contents
   name
   (lambda()
      (let ((parse (phps-mode-parser-parse)))
        (message "Left-to-right with right-most derivation:\n%S\n" parse)
        (dolist (production-number parse)
          (let ((production
                 (phps-mode-parser--get-grammar-production-by-number
                  production-number)))
            (message
             "%d: %S -> %S"
             production-number
             (car (car production))
             (car (cdr production))))))
      (message "\n")
      (phps-mode-ast--generate)
      (phps-mode-ast-imenu--generate)
      (message "imenu: %S" phps-mode-ast-imenu--index)
      (should
       (equal
        phps-mode-ast-imenu--index
        imenu)))))

(defun phps-mode-test-ast--buffer-contents (buffer-contents name logic)
  (with-temp-buffer
    ;; Setup buffer
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))
    
    ;; Setup lexer
    (setq-local
     phps-mode-lexer--generated-tokens
     nil)
    (setq-local
     phps-mode-lexer--state
     'ST_INITIAL)
    (setq-local
     phps-mode-lexer--states
     nil)
    (setq-local
     phps-mode-lexer--state-stack
     nil)
    (setq-local
     phps-mode-lexer--heredoc-label
     nil)
    (setq-local
     phps-mode-lexer--heredoc-label-stack
     nil)
    (setq-local
     phps-mode-lexer--nest-location-stack
     nil)

    ;; Run lexer
    (setq-local
     phps-mode-lex-analyzer--lexer-index
     (point-min))
    (setq-local
     phps-mode-lex-analyzer--lexer-max-index
     (point-max))
    (phps-mode-lex-analyzer--re2c-lex-analyzer)
    (setq-local
     phps-mode-parser-tokens
     (phps-mode-lex-analyzer--generate-parser-tokens
      phps-mode-lexer--generated-tokens))

    ;; Run test
    (funcall logic)
    (message "Passed test for %S\n" name)))

(defun phps-mode-test-ast-imenu ()
  "Run test for imenu generation."
  (message "-- Running tests for imenu generation... --\n")

  (phps-mode-test-ast--should-imenu
   "<?php\nclass myClass\n{\n\n    public function myFunction1()\n    {\n        echo \"my string with variable {$variable} inside it\";\n    }\n\n    public function myFunction2()\n    {\n    }\n\n}"
   "Imenu generated via parser SDT for simple class"
   '(("myClass" ("myFunction1" . 44) ("myFunction2" . 153))))

  (phps-mode-test-ast--should-imenu
   "<?php\ninterface myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu generated via parser SDT for interface"
   '(("myInterface" . (("myFunctionA" . 51) ("myFunctionB" . 91)))))

  (phps-mode-test-ast--should-imenu
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n$var = function () {\n    echo 'here';\n};"
   "Imenu generated via parser SDT for function-oriented file without namespace"
   '(("myFunctionA" . 16) ("myFunctionB" . 42)))

  (phps-mode-test-ast--should-imenu
   "<?php\n\nnamespace MyNamespace;\n\nfunction aFunction() {\n    /**\n     * With some contents\n     */\n}\n\nclass MyClass\n{\n\n    /**\n     *\n     */\n    public function __construct()\n    {\n        if ($test) {\n        }\n    }\n\n    /**\n     *\n     */\n    public function myFunction1()\n    {\n        $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n    }\n    \n    /**\n     *\n     */\n    public function myFunction2()\n    {\n    }\n\n    /**\n     * It's good\n     */\n    public function myFunction3()\n    {\n    }\n\n    /**\n     *\n     */\n    public function myFunction4()\n    {\n    }\n}\n"
   "Passed imenu-generation via parser AST for basic object oriented file"
   '(("MyNamespace" ("aFunction" . 41) ("MyClass" ("__construct" . 160) ("myFunction1" . 261) ("myFunction2" . 433) ("myFunction3" . 513) ("myFunction4" . 583)))))

  (phps-mode-test-ast--should-imenu
   "<?php\n\nnamespace MyNamespaceA\n{\n    function aFunctionA() {\n        /**\n         * With some contents\n         */\n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}\nnamespace {\n    function aFunctionB()\n    {\n        \n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}"
   "Passed imenu-generation via parser AST for advanced object oriented file"
   '(("MyNamespaceA" ("aFunctionA" . 46) ("MyClass" ("__construct" . 205) ("myFunction1" . 338) ("myFunction2" . 542) ("myFunction3" . 646) ("myFunction4" . 740))) ("aFunctionB" . 807) ("MyClass" ("__construct" . 925) ("myFunction1" . 1058) ("myFunction2" . 1262) ("myFunction3" . 1366) ("myFunction4" . 1460))))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace {\n    class myClass extends myAbstract {\n        public function myFunctionA() {}\n        protected function myFunctionB() {}\n    }\n}\n"
   "Imenu object-oriented file with namespace, class that extends and functions"
   '(("myNamespace" ("myClass" ("myFunctionA" . 94) ("myFunctionB" . 138)))))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions"
   '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 148)))))

  (phps-mode-test-ast--should-imenu
   "<?php\nclass myClass {}"
   "Imenu empty class"
   '(("myClass" . 13)))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace {}"
   "Imenu empty bracketed namespace"
   '(("myNamespace" . 17)))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace;"
   "Imenu empty namespace without brackets"
   '(("myNamespace" . 17)))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace myNamespace\\myNamespace2;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments"
   '(("myNamespace\\myNamespace2" ("myClass" ("myFunctionA" . 121) ("myFunctionB" . 174)))))

  (phps-mode-test-ast--should-imenu
   "<?php\nnamespace {}"
   "Imenu empty unnamed bracketed namespace"
   nil)

  ;; TODO Make this test pass
  ;; (phps-mode-test-ast--should-imenu
  ;;  "<?php\n\nnamespace myNamespace;\n\nif (!function_exists('myFunction')) {\n    function myFunction() {\n        if (!class_exists('myClassA')) {\n            class myClassA {\n                public function myMethodA()\n                {\n                    \n                }\n            }\n        }\n    }\n}\n\nif (!class_exists('myClassB')) {\n    class myClassB\n    {\n        function myMethodB()\n        {\n        }\n    }\n}"
  ;;  "Imenu for conditionally declared function and class"
  ;;  '(("myNamespace" ("myFunction" . 183) ("myClassA" ("myMethodA" . 200)) ("myClassB" . ("myMethodB" . 377)))))

  (message "\n-- Ran tests for imenu generation. --"))

(defun phps-mode-test-ast-bookkeeping ()
  "Run test for bookkeeping generation."
  (message "-- Running tests for bookkeeping generation... --\n")

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var2) {\n    echo 'This never happens';\n}\nif ($var) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #1"
   '((" id $var" ((8 12))) ((8 12) 1) ((27 32) 0) ((73 77) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var) {\n    echo 'This never happens';\n}\nif ($var2) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #2"
   '((" id $var" ((8 12))) ((8 12) 1) ((27 31) 1) ((72 77) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var2 = 4;\n\nfunction myFunction($var)\n{\n    $var3 = 3;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Hit';\n    }\n}\n\nfunction myFunction2($abc)\n{\n    if ($var) {\n        echo 'Miss';\n    }\n    if ($abc) {\n        echo 'Hit';\n    }\n}\n\nif ($var) {\n    echo 'Miss';\n}\nif ($var2) {\n    echo 'Hit';\n}"
   "Bookkeeping in function level with variable assignments"
   '((" id $var2" ((8 13))) ((8 13) 1) (" function myFunction id $var" ((40 44))) (" function myFunction id $var3" ((52 57))) ((157 162) 1) ((113 118) 0) ((71 75) 1) ((52 57) 1) (" function myFunction2 id $abc" ((216 220))) ((275 279) 1) ((232 236) 0) ((316 320) 0) ((347 352) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Super-globals\n\nif ($_GET) {\n    echo 'Hit';\n}\nif ($_POST) {\n    echo 'Hit';\n}\nif ($_COOKIE) {\n    echo 'Hit';\n}\nif ($_SESSION) {\n    echo 'Hit';\n}\nif ($_REQUEST) {\n    echo 'Hit';\n}\nif ($GLOBALS) {\n    echo 'Hit';\n}\nif ($_SERVER) {\n    echo 'Hit';\n}\nif ($_FILES) {\n    echo 'Hit';\n}\nif ($_ENV) {\n    echo 'Hit';\n}\nif ($argc) {\n    echo 'Hit';\n}\nif ($argv) {\n    echo 'Hit';\n}\nif ($http_​response_​header) {\n    echo 'Hit';\n}"
   "Bookkeeping of super-globals"
   '(((30 35) 1) ((61 67) 1) ((93 101) 1) ((127 136) 1) ((162 171) 1) ((197 205) 1) ((231 239) 1) ((265 272) 1) ((298 303) 1) ((329 334) 1) ((360 365) 1) ((391 414) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        public static function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping in maximum level with namespaces, classes and functions."
   '((" namespace myNamespaceA id $var" ((37 41))) ((37 41) 1) (" namespace myNamespaceA class myClassA function myFunctionB id $var5" ((485 490))) (" namespace myNamespaceA class myClassA function myFunctionB id $this" ((500 941))) (" namespace myNamespaceA class myClassA function myFunctionB id $var6" ((514 519))) (" namespace myNamespaceA class myClassA function myFunctionA id $var3" ((142 147))) (" namespace myNamespaceA class myClassA function myFunctionA id $var4" ((163 168))) (" namespace myNamespaceA class myClassA id $var2" ((86 91))) ((881 886) 1) ((814 819) 1) ((746 751) 0) ((678 683) 0) ((610 615) 0) ((543 547) 0) ((514 519) 1) ((394 399) 1) ((327 332) 1) ((259 264) 0) ((192 196) 0) ((163 168) 1) ((957 961) 1) ((999 1004) 0) ((1043 1048) 0) ((1087 1092) 0) ((1131 1136) 0) ((1175 1180) 0) (" namespace myNamespaceB id $var7" ((1243 1248))) ((1243 1248) 1) (" namespace myNamespaceB class myClassB function myFunctionB id $var12" ((2088 2094))) (" namespace myNamespaceB class myClassB function myFunctionB id $this" ((2104 2955))) (" namespace myNamespaceB class myClassB function myFunctionB id $var11" ((2118 2124))) (" namespace myNamespaceB class myClassB function myFunctionA id $var10" ((1335 1341))) (" namespace myNamespaceB class myClassB function myFunctionA id $this" ((1343 2057))) (" namespace myNamespaceB class myClassB function myFunctionA id $var9" ((1357 1362))) (" namespace myNamespaceB class myClassB id $var8" ((1293 1298))) ((2894 2900) 1) ((2826 2832) 1) ((2757 2763) 0) ((2689 2694) 0) ((2621 2626) 0) ((2553 2558) 0) ((2486 2491) 0) ((2419 2424) 0) ((2351 2356) 0) ((2283 2288) 0) ((2215 2220) 0) ((2148 2152) 0) ((2118 2124) 1) ((1996 2002) 1) ((1929 1934) 1) ((1861 1866) 0) ((1793 1798) 0) ((1725 1730) 0) ((1657 1662) 0) ((1589 1594) 0) ((1521 1526) 0) ((1453 1458) 0) ((1386 1390) 0) ((1357 1362) 1) ((2971 2975) 0) ((3013 3018) 0) ((3057 3062) 0) ((3101 3106) 0) ((3145 3150) 0) ((3189 3194) 0) ((3233 3238) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Conditional assignments\n\n$items = array(1, 2, 3);\nforeach ($items as $item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => $value) {\n    if ($key || $value) {\n        echo 'Hit';\n    }\n}\nfor ($i = 0; $i < count($items); $i++) {\n    if ($i) {\n        echo 'Hit';\n    }\n}\nif ($a = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\nwhile ($b = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\ndo {\n    echo 'Hit';\n} while ($c = 456);\n"
   "Bookkeeping of conditional assignments"
   '((" id $items" ((36 42))) ((36 42) 1) (" id $item" ((80 85))) ((97 102) 1) ((80 85) 1) ((70 76) 1) (" id $value" ((161 167))) (" id $key" ((153 157))) ((187 193) 1) ((179 183) 1) ((161 167) 1) ((153 157) 1) ((143 149) 1) (" id $i" ((230 232))) ((274 276) 1) ((258 260) 1) ((249 255) 1) ((238 240) 1) ((230 232) 1) (" id $a" ((312 314))) ((332 334) 1) ((312 314) 1) (" id $b" ((373 375))) ((393 395) 1) ((373 375) 1) (" id $c" ((457 459))) ((457 459) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Class properties\n\nclass myParent {}\n\nclass myClass extends myParent {\n    private $var1 = 123;\n    protected static $var2;\n    public $var3;\n    var $var4;\n    function __construct() {\n        if ($this) {\n            echo 'Hit';\n        }\n        if ($this->var1) {\n            echo 'Hit';\n        }\n        if (self::$var1) {\n            echo 'Miss';\n        }\n        if (self::$var2) {\n            echo 'Hit';\n        }\n        if (static::$var2) {\n            echo 'Hit';\n        }\n        if ($this->var3) {\n            echo 'Hit';\n        }\n        if ($this->var4) {\n            echo 'Hit';\n        }\n        if ($this->var5) {\n            echo 'Miss';\n        }\n        if (paren1) {\n            echo 'Hit';\n        }\n    }\n}\n\nif ($this) {\n    echo 'Miss';\n}\nif (self) {\n    echo 'Miss';\n}\nif (paren1) {\n    echo 'Miss';\n}"
   "Bookkeeping of class properties"
   '((" class myClass function __construct id $this" ((194 743))) (" class myClass id $var4" ((160 165))) (" class myClass id $var3" ((145 150))) (" class myClass static id $var2" ((127 132))) (" class myClass id $var1" ((93 98))) ((639 643) 0) ((632 637) 1) ((578 582) 1) ((571 576) 1) ((517 521) 1) ((510 515) 1) ((455 460) 1) ((392 397) 1) ((330 335) 0) ((270 274) 1) ((263 268) 1) ((208 213) 1) ((751 756) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\ntry {\n    \n} catch (\\Exception $e) {\n    if ($e) {\n        echo 'Hit';\n    }\n}\n\nif ($e) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of try catch variable assignment"
   '((" id $e" ((39 41))) ((39 41) 1) ((53 55) 1) ((92 94) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$example = function ($test) {\n    if ($test) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Miss';\n    }\n};\n$example2 = function ($test2) use ($example) {\n    if ($test2) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Hit';\n    }\n    if ($example2) {\n        echo 'Miss';\n    }\n    if ($example3) {\n        echo 'Miss';\n    }\n};\nif ($test) {\n    echo 'Miss';\n}\nif ($test2) {\n    echo 'Miss';\n}"
   "Bookkeeping of anonymous function variable assignments"
   '((" id $example" ((8 16))) (" anonymous function1 id $test" ((29 34))) ((29 34) 1) ((89 97) 0) ((46 51) 1) ((8 16) 1) (" id $example2" ((131 140))) (" anonymous function2 id $example" ((166 174))) (" anonymous function2 id $test2" ((153 159))) ((166 174) 1) ((153 159) 1) ((324 333) 0) ((276 285) 0) ((230 238) 1) ((186 192) 1) ((131 140) 1) ((371 376) 0) ((403 409) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass myClass {\n    function random() {}\n    function __construct()\n    {\n        $this->random();\n        $this->random['abc'] = 123;\n    }\n}"
   "Method calls should be avoided in bookkeeping"
   '((" class myClass function __construct id $this" ((79 147))) ((121 127) 0) ((114 119) 1) ((89 94) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$items = array(1, 2, 3);\nforeach ($items as &$item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => &$item2) {\n    if ($item) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of foreach reference variable declaration"
   '((" id $items" 1) ((7 13) 1) ((41 47) 1) (" id $item" 1) ((52 57) 1) ((69 74) 1) ((115 121) 1) (" id $key" 1) ((125 129) 1) (" id $item2" 1) ((134 140) 1) ((152 157) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n[$random, $bandom] = myValues();\nif ($random) {\n    echo 'Hit';\n}\nif ($bandom) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable declarations in array"
   '((" id $random" 1) ((9 16) 1) (" id $bandom" 1) ((18 25) 1) ((45 52) 1) ((78 85) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    global $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of global variable declaration in function"
   '((" id $var" 1) ((8 12) 1) (" function test id $abc" 1) ((35 39) 1) (" function test id $var" 1) ((54 58) 1) ((68 72) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$y = 1;\n$fn1 = fn($x) => $x + $y;\n$z = 1;\n$fn = fn($x2) => fn($y2) => $x2 * $y2 + $z;\nfn(array $x3) => $x3;\n$x4 = 4;\nstatic fn(): int => $x4;\nfn($x5 = 42) => $x5;\nfn(&$x6) => $x6;\nfn&($x7) => $x7;\nfn($x8, ...$rest) => $rest;"
   "Bookkeeping in arrow functions"
   '((" id $y" 1) ((7 9) 1) (" id $fn1" 1) ((15 19) 1) (" arrow function 1 id $x" 1) ((25 27) 1) ((32 34) 1) ((37 39) 1) (" id $z" 1) ((41 43) 1) (" id $fn" 1) ((49 52) 1) (" arrow function 2 id $x2" 1) ((58 61) 1) (" arrow function 2 arrow function 3 id $y2" 1) ((69 72) 1) ((77 80) 1) ((83 86) 1) ((89 91) 1) (" arrow function 4 id $x3" 1) ((102 105) 1) ((110 113) 1) (" id $x4" 1) ((115 118) 1) ((144 147) 1) (" arrow function 6 id $x5" 1) ((152 155) 1) ((165 168) 1) (" arrow function 7 id $x6" 1) ((174 177) 1) ((182 185) 1) (" arrow function 8 id $x7" 1) ((191 194) 1) ((199 202) 1) (" arrow function 9 id $x8" 1) ((207 210) 1) (" arrow function 9 id $rest" 1) ((215 220) 1) ((225 230) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$z = (object) array('name' => 'random');\nif ($z->name) {\n    echo 'Hit';\n}"
   "Bookkeeping ignoring variable properties"
   '((" id $z" 1) ((7 9) 1) ((52 54) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!$var = false) {\n    echo 'Hit';\n}\n"
   "Bookkeeping negative conditional assignment"
   '((" id $var" 1) ((12 16) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nif (isset($x)) {\n    if ($x) {\n        echo 'Hit';\n        if (isset($i, $u)) {\n            if ($i) {\n                echo 'Hit';\n            }\n            if ($u) {\n                echo 'Hit';\n            }\n            if ($x) {\n                echo 'Hit';\n            }\n        }\n        if ($i) {\n            echo 'Miss';\n        }\n        if ($u) {\n            echo 'Miss';\n        }\n    }\n}\nif ($x) {\n    echo 'Miss';\n}\n\nif (!empty($y)) {\n    if ($y) {\n        echo 'Hit';\n        if (!empty($k) && !empty($L)) {\n            if ($k) {\n                echo 'Hit';\n            }\n            if ($L) {\n                echo 'Hit';\n            }\n            if ($y) {\n                echo 'Hit';\n            }\n        }\n        if ($k) {\n            echo 'Miss';\n        }\n        if ($L) {\n            echo 'Miss';\n        }\n    }\n}\nif ($y) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of isset() and !empty() scoped variables"
   '((" defined 1 id $x" 1) ((18 20) 1) ((33 35) 1) (" defined 1 defined 2 id $i" 1) (" defined 1 defined 2 id $u" 1) ((77 79) 1) ((81 83) 1) ((104 106) 1) ((168 170) 1) ((232 234) 1) ((302 304) 0) ((355 357) 0) ((408 410) 0) (" defined 3 id $y" 1) ((445 447) 1) ((460 462) 1) (" defined 3 defined 4 id $k" 1) (" defined 3 defined 4 id $L" 1) ((505 507) 1) ((519 521) 1) ((542 544) 1) ((606 608) 1) ((670 672) 1) ((740 742) 0) ((793 795) 0) ((846 848) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ninterface myInterface\n{\n    function myFunction1();\n    function myFunction2($x);\n}\n"
   "Bookkeeping variable in interface function"
   '((" class myInterface function myFunction2 id $x" 1) ((84 86) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction1()\n{\n    return isset($a);\n}\n\nfunction myFunction2()\n{\n    $b = 2;\n    if ($b) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping after definition condition"
   '(((50 52) 0) (" function myFunction2 id $b" 1) ((87 89) 1) ((103 105) 1) ((143 145) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = array(1, 2, 3);\nforeach ($a as $uri => $page)\n{\n    if (isset($pages)) {\n        if ($a) {\n            echo 'Hit';\n        }\n        if ($uri) {\n            echo 'Hit';\n        }\n        if ($page) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "Bookkeeping of foreach variable inside if (isset()) block"
   '((" id $a" 1) ((8 10) 1) ((38 40) 1) (" id $uri" 1) ((44 48) 1) (" id $page" 1) ((52 57) 1) (" defined 1 id $pages" 1) ((75 81) 1) ((98 100) 1) ((150 154) 1) ((204 209) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (isset($b)) {\n    $b = false;\n}\n$c = 2;\n\nif ($c) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable after isset() block"
   '((" defined 1 id $b" 2) ((17 19) 1) ((28 30) 2) (" id $c" 1) ((42 44) 1) ((55 57) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!isset($a)) {\n    if ($a) {\n        echo 'Miss';\n    }\n}"
   "Bookkeeping for variable in negative isset() conditional"
   '(((18 20) 0) ((33 35) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction($a, $b, $c, $d)\n{\n    global $f, $g;\n    if (isset($f)) {\n        if (!empty($g)) {\n            if ($a) {\n                echo 'Hit';\n            }\n            if ($b) {\n                echo 'Hit';\n            }\n            if ($c) {\n                echo 'Hit';\n            }\n            if ($d) {\n                echo 'Hit';\n            }\n        }\n    }\n}\n"
   "Bookkeeping variables inside nested isset() !empty() blocks"
   '((" function myFunction id $a" 1) ((28 30) 1) (" function myFunction id $b" 1) ((32 34) 1) (" function myFunction id $c" 1) ((36 38) 1) (" function myFunction id $d" 1) ((40 42) 1) (" function myFunction id $f" 1) ((57 59) 1) (" function myFunction id $g" 1) ((61 63) 1) (" function myFunction defined 1 id $f" 1) ((79 81) 1) (" function myFunction defined 1 defined 2 id $g" 1) ((105 107) 1) ((128 130) 1) ((192 194) 1) ((256 258) 1) ((320 322) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    static $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of static variable declaration in function"
   '((" id $var" 1) ((8 12) 1) (" function test id $abc" 1) ((35 39) 1) (" function test id $var" 1) ((54 58) 1) ((68 72) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nglobal $a, $b;\n\nif ($a) {\n    echo 'Hit';\n}\n\nfunction myFunction($c)\n{\n    global $a;\n    if ($a) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping of global variables in functional-oriented file"
   '((" id $a" 1) ((15 17) 1) (" id $b" 1) ((19 21) 1) ((28 30) 1) (" function myFunction id $c" 1) ((73 75) 1) (" function myFunction id $a" 1) ((90 92) 1) ((102 104) 1) ((142 144) 0)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nstatic $a;\n\nif ($a) {}\n\nfunction test()\n{\n    static $a;\n    if ($a) {}\n}\n\nclass There\n{\n    function here()\n    {\n        static $a;\n        if ($a) {}\n    }\n}"
   "Bookkeeping of static variables in different scopes without namespaces"
   '((" id $a" 1) ((15 17) 1) ((24 26) 1) (" function test id $a" 1) ((61 63) 1) ((73 75) 1) (" class There function here id $this" 1) (" class There function here id $a" 1) ((138 140) 1) ((154 156) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private \\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of typed class variables"
   '((" class There id $variable" 1) ((33 42) 1) (" class There id $variable2" 1) ((67 77) 1) (" class There id $variable3" 1) ((98 108) 1) (" class There static id $variable4" 1) ((129 139) 1) (" class There static id $variable5" 1) ((171 181) 1) (" class There static id $variable6" 1) ((209 219) 1) (" class There function here id $this" 1) ((259 264) 1) ((266 274) 1) ((291 296) 1) ((298 307) 1) ((324 329) 1) ((331 340) 1) ((357 362) 1) ((364 373) 0) ((396 406) 1) ((429 439) 1) ((462 472) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = $b = $c = 3;\n\nif ($a) {\n    echo 'a=',$a;\n} else {\n    echo '$a is undefined!';\n}\nif ($b) {\n    echo 'b=',$b;\n} else {\n    echo '$b is undefined!';\n}\nif ($c) {\n    echo 'c=',$c;\n} else {\n    echo '$c is undefined!';\n}"
   "Bookkeeping of chained variable assignments"
   '((" id $a" 1) ((8 10) 1) (" id $b" 1) ((13 15) 1) (" id $c" 1) ((18 20) 1) ((31 33) 1) ((51 53) 1) ((99 101) 1) ((119 121) 1) ((167 169) 1) ((187 189) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private ?\\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static ?string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of nullable typed class variables"
   '((" class There id $variable" 1) ((33 42) 1) (" class There id $variable2" 1) ((68 78) 1) (" class There id $variable3" 1) ((99 109) 1) (" class There static id $variable4" 1) ((130 140) 1) (" class There static id $variable5" 1) ((172 182) 1) (" class There static id $variable6" 1) ((211 221) 1) (" class There function here id $this" 1) ((261 266) 1) ((268 276) 1) ((293 298) 1) ((300 309) 1) ((326 331) 1) ((333 342) 1) ((359 364) 1) ((366 375) 0) ((398 408) 1) ((431 441) 1) ((464 474) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass MyClass\n{\n    static function here()\n    {\n        if ($this) {\n            // Miss;\n        }\n    }\n    function there()\n    {\n        if ($this) {\n            // Hit\n        }\n    }\n}\n"
   "Bookkeeping of $this not available inside static method"
   '(((68 73) 0) (" class MyClass function there id $this" 1) ((153 158) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    private $test = 'abc';\n    public function test($d)\n    {\n        return fn($e) => $this->test . $d . $e;\n    }\n}\n\n$a = new myClass();\necho $a->test('def')('ghi');"
   "Bookkeeping of $this reference inside arrow function inside of method"
   '((" class myClass id $test" 1) ((36 41) 1) (" class myClass function test id $this" 1) (" class myClass function test id $d" 1) ((76 78) 1) (" class myClass function test arrow function 1 id $e" 1) ((104 106) 1) ((111 116) 1) ((118 122) 1) ((125 127) 1) ((130 132) 1) (" id $a" 1) ((143 145) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php class myClass { static $var = '123'; static function myMethod($a) { return fn($b) => self::$var . $a . $b; }} echo myClass::myMethod('4')('5');"
   "Bookkeeping of self reference inside arrow function inside of static method"
   '((" class myClass static id $var" 1) ((30 34) 1) (" class myClass function myMethod id $a" 1) ((69 71) 1) (" class myClass function myMethod arrow function 1 id $b" 1) ((85 87) 1) ((98 102) 1) ((105 107) 1) ((110 112) 1)))

  ;; TODO Add trait class bookkeping test here

  ;; TODO Make this test pass
  ;; (phps-mode-test-ast--should-bookkeep
  ;;  "<?php\nnamespace myNamespace;\nclass myClass\n{\n    private $property1 = '';\n    private $property2;\n    protected function myMethod(\n        $argument1,\n        $argument2,\n        $argument3\n    ) {\n        if ($this->property2) {\n            echo 'was here';\n        }\n        /* @codingStandardsIgnoreEnd */\n        if (\n            $argument1\n            && $argument2\n            && $argument3\n            && $argument4\n            && !empty($argument1['index'])\n            && $this->property1\n            && $argument1['index'] == $this->property1\n        ) {\n        }\n    }\n}\n"
  ;;  "Bookkeeping of properties inside if condition list"
  ;;  '((" namespace myNamespace class myClass id $property1" 1) ((58 68) 1) (" namespace myNamespace class myClass id $property2" 1) ((87 97) 1) (" namespace myNamespace class myClass function myMethod id $this" 1) (" namespace myNamespace class myClass function myMethod id $argument1" 1) ((140 150) 1) (" namespace myNamespace class myClass function myMethod id $argument2" 1) ((160 170) 1) (" namespace myNamespace class myClass function myMethod id $argument3" 1) ((180 190) 1) ((211 216) 1) ((218 227) 1) (" namespace myNamespace class myClass function myMethod defined 1 id nil" 1) ((335 345) 1) ((361 371) 1) ((387 397) 1) ((413 423) 0) ((482 487) 1) ((489 498) 1) ((537 542) 1) ((544 553) 1)))

  (message "\n-- Ran tests for bookkeeping generation. --"))

(defun phps-mode-test-ast ()
  "Run test for ast."
  (message "-- Running all tests for ast... --\n")
  (phps-mode-test-ast-bookkeeping)
  (phps-mode-test-ast-imenu)
  (message "\n-- Ran all tests for ast. --"))

(phps-mode-test-ast)

(provide 'phps-mode-test-ast)


;;; phps-mode-test-ast.el ends here
