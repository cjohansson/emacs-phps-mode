;;; phps-mode-test-ast.el --- Tests for AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make test-ast


;;; Code:


(require 'ert)
(require 'phps-mode)
(require 'phps-mode-ast)
(require 'phps-mode-lex-analyzer)
(require 'phps-mode-test)

(defun phps-mode-test-ast--should-bookkeep
    (buffer-contents name expected-bookkeeping &optional expected-imenu)
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

     ;; (message "symbol-table: \n%S\n" phps-mode-parser-sdt-symbol-table)
     ;; (message "phps-mode-parser-sdt-symbol-table-by-uri: \n%S\n" phps-mode-parser-sdt-symbol-table-by-uri)

     (unless
         (equal
          (phps-mode-test--hash-to-list
           phps-mode-parser-sdt-bookkeeping
           t)
          expected-bookkeeping)
       (message
        "expected-bookkeeping:\n%S\n"
        expected-bookkeeping)
       (message
        "actual-bookkeeping:\n%S\n"
        (phps-mode-test--hash-to-list
         phps-mode-parser-sdt-bookkeeping
         t)))

     (should
      (equal
       (phps-mode-test--hash-to-list
        phps-mode-parser-sdt-bookkeeping
        t)
       expected-bookkeeping))

     (when expected-imenu
       (unless
           (equal
            phps-mode-parser-sdt-symbol-imenu
            expected-imenu)
         (message
          "expected-imenu:\n%S\n"
          expected-imenu)
         (message
          "actual-imenu:\n%S\n"
          phps-mode-parser-sdt-symbol-imenu))

       (should
        (equal
         phps-mode-parser-sdt-symbol-imenu
         expected-imenu))))))

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

(defun phps-mode-test-ast-bookkeeping ()
  "Run test for bookkeeping generation."
  (message "-- Running tests for bookkeeping generation... --\n")

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var2) {\n    echo 'This never happens';\n}\nif ($var) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #1"
   '(((8 12) 1) ((27 32) 0) ((73 77) 1))
   '(("id $var" . 8)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var) {\n    echo 'This never happens';\n}\nif ($var2) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #2"
   '(((8 12) 1) ((27 31) 1) ((72 77) 0))
   '(("id $var" . 8)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var2 = 4;\n\nfunction myFunction($var)\n{\n    $var3 = 3;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Hit';\n    }\n}\n\nfunction myFunction2($abc)\n{\n    if ($var) {\n        echo 'Miss';\n    }\n    if ($abc) {\n        echo 'Hit';\n    }\n}\n\nif ($var) {\n    echo 'Miss';\n}\nif ($var2) {\n    echo 'Hit';\n}"
   "Bookkeeping in function level with variable assignments"
   '(((8 13) 1) ((40 44) 3) ((157 162) 4) ((113 118) 0) ((71 75) 3) ((52 57) 4) ((216 220) 6) ((275 279) 6) ((232 236) 0) ((316 320) 0) ((347 352) 1))
   '(("id $var2" . 8) ("function myFunction" . 29) ("function myFunction id $var" . 40) ("function myFunction id $var3" . 52) ("function myFunction2" . 204) ("function myFunction2 id $abc" . 216)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Super-globals\n\nif ($_GET) {\n    echo 'Hit';\n}\nif ($_POST) {\n    echo 'Hit';\n}\nif ($_COOKIE) {\n    echo 'Hit';\n}\nif ($_SESSION) {\n    echo 'Hit';\n}\nif ($_REQUEST) {\n    echo 'Hit';\n}\nif ($GLOBALS) {\n    echo 'Hit';\n}\nif ($_SERVER) {\n    echo 'Hit';\n}\nif ($_FILES) {\n    echo 'Hit';\n}\nif ($_ENV) {\n    echo 'Hit';\n}\nif ($argc) {\n    echo 'Hit';\n}\nif ($argv) {\n    echo 'Hit';\n}\nif ($http_​response_​header) {\n    echo 'Hit';\n}"
   "Bookkeeping of super-globals"
   '(((30 35) -1) ((61 67) -1) ((93 101) -1) ((127 136) -1) ((162 171) -1) ((197 205) -1) ((231 239) -1) ((265 272) -1) ((298 303) -1) ((329 334) -1) ((360 365) -1) ((391 414) -1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        public static function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping in maximum level with namespaces, classes and functions."
   '(((37 41) 2) ((485 490) 5) ((881 886) 7) ((814 819) 5) ((746 751) 0) ((678 683) 0) ((610 615) 0) ((543 547) 0) ((514 519) 7) ((142 147) 9) ((394 399) 10) ((327 332) 9) ((259 264) 0) ((192 196) 0) ((163 168) 10) ((86 91) 11) ((957 961) 2) ((999 1004) 0) ((1043 1048) 0) ((1087 1092) 0) ((1131 1136) 0) ((1175 1180) 0) ((1243 1248) 13) ((2088 2094) 16) ((2894 2900) 16) ((2826 2832) 18) ((2757 2763) 0) ((2689 2694) 0) ((2621 2626) 0) ((2553 2558) 0) ((2486 2491) 0) ((2419 2424) 0) ((2351 2356) 0) ((2283 2288) 0) ((2215 2220) 0) ((2148 2152) 0) ((2118 2124) 18) ((1335 1341) 20) ((1996 2002) 20) ((1929 1934) 22) ((1861 1866) 0) ((1793 1798) 0) ((1725 1730) 0) ((1657 1662) 0) ((1589 1594) 0) ((1521 1526) 0) ((1453 1458) 0) ((1386 1390) 0) ((1357 1362) 22) ((1293 1298) 23) ((2971 2975) 0) ((3013 3018) 0) ((3057 3062) 0) ((3101 3106) 0) ((3145 3150) 0) ((3189 3194) 0) ((3233 3238) 13))
   '(("namespace myNamespaceA" . 18) ("namespace myNamespaceA id $var" . 37) ("namespace myNamespaceA class myClassA" . 59) ("namespace myNamespaceA class myClassA function myFunctionB" . 500) ("namespace myNamespaceA class myClassA function myFunctionB id $var5" . 485) ("namespace myNamespaceA class myClassA function myFunctionB id $this" . 500) ("namespace myNamespaceA class myClassA function myFunctionB id $var6" . 514) ("namespace myNamespaceA class myClassA function myFunctionA" . 149) ("namespace myNamespaceA class myClassA function myFunctionA id $var3" . 142) ("namespace myNamespaceA class myClassA function myFunctionA id $var4" . 163) ("namespace myNamespaceA class myClassA id $var2" . 86) ("namespace myNamespaceB" . 1224) ("namespace myNamespaceB id $var7" . 1243) ("namespace myNamespaceB class myClassB" . 1266) ("namespace myNamespaceB class myClassB function myFunctionB" . 2104) ("namespace myNamespaceB class myClassB function myFunctionB id $var12" . 2088) ("namespace myNamespaceB class myClassB function myFunctionB id $this" . 2104) ("namespace myNamespaceB class myClassB function myFunctionB id $var11" . 2118) ("namespace myNamespaceB class myClassB function myFunctionA" . 1343) ("namespace myNamespaceB class myClassB function myFunctionA id $var10" . 1335) ("namespace myNamespaceB class myClassB function myFunctionA id $this" . 1343) ("namespace myNamespaceB class myClassB function myFunctionA id $var9" . 1357) ("namespace myNamespaceB class myClassB id $var8" . 1293)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Conditional assignments\n\n$items = array(1, 2, 3);\nforeach ($items as $item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => $value) {\n    if ($key || $value) {\n        echo 'Hit';\n    }\n}\nfor ($i = 0; $i < count($items); $i++) {\n    if ($i) {\n        echo 'Hit';\n    }\n}\nif ($a = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\nwhile ($b = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\ndo {\n    echo 'Hit';\n} while ($c = 456);\n"
   "Bookkeeping of conditional assignments"
   '(((36 42) 1) ((97 102) 2) ((80 85) 2) ((70 76) 1) ((187 193) 3) ((179 183) 4) ((161 167) 3) ((153 157) 4) ((143 149) 1) ((274 276) 5) ((258 260) 5) ((249 255) 1) ((238 240) 5) ((230 232) 5) ((332 334) 6) ((312 314) 6) ((393 395) 6) ((373 375) 7) ((457 459) 8))
   '(("id $items" . 36) ("id $item" . 80) ("id $value" . 161) ("id $key" . 153) ("id $i" . 230) ("id $a" . 312) ("id $b" . 373) ("id $c" . 457)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Class properties\n\nclass myParent {}\n\nclass myClass extends myParent {\n    private $var1 = 123;\n    protected static $var2;\n    public $var3;\n    var $var4;\n    function __construct() {\n        if ($this) {\n            echo 'Hit';\n        }\n        if ($this->var1) {\n            echo 'Hit';\n        }\n        if (self::$var1) {\n            echo 'Miss';\n        }\n        if (self::$var2) {\n            echo 'Hit';\n        }\n        if (static::$var2) {\n            echo 'Hit';\n        }\n        if ($this->var3) {\n            echo 'Hit';\n        }\n        if ($this->var4) {\n            echo 'Hit';\n        }\n        if ($this->var5) {\n            echo 'Miss';\n        }\n        if (paren1) {\n            echo 'Hit';\n        }\n    }\n}\n\nif ($this) {\n    echo 'Miss';\n}\nif (self) {\n    echo 'Miss';\n}\nif (paren1) {\n    echo 'Miss';\n}"
   "Bookkeeping of class properties"
   '(((639 643) 0) ((632 637) 3) ((578 582) 4) ((571 576) 3) ((517 521) 5) ((510 515) 3) ((455 460) 6) ((392 397) 6) ((330 335) 0) ((270 274) 7) ((263 268) 3) ((208 213) 3) ((160 165) 4) ((145 150) 5) ((127 132) 6) ((93 98) 7) ((751 756) 0))
   '(("class myClass" . 54) ("class myClass function __construct" . 194) ("class myClass function __construct id $this" . 194) ("class myClass id $var4" . 160) ("class myClass id $var3" . 145) ("class myClass static id $var2" . 127) ("class myClass id $var1" . 93)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\ntry {\n    \n} catch (\\Exception $e) {\n    if ($e) {\n        echo 'Hit';\n    }\n}\n\nif ($e) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of try catch variable assignment"
   '(((39 41) 1) ((53 55) 1) ((92 94) 1))
   '(("id $e" . 39)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$example = function ($test) {\n    if ($test) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Miss';\n    }\n};\n$example2 = function ($test2) use ($example) {\n    if ($test2) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Hit';\n    }\n    if ($example2) {\n        echo 'Miss';\n    }\n    if ($example3) {\n        echo 'Miss';\n    }\n};\n$example3 = function ($test3) use ($example4) {\n    if ($test3) {\n        echo 'Hit';\n    }\n    if ($example4) {\n        echo 'Hit';\n    }\n};\nif ($test) {\n    echo 'Miss';\n}\nif ($test2) {\n    echo 'Miss';\n}"
   "Bookkeeping of anonymous function variable assignments and lexical vars"
   '(((29 34) 2) ((89 97) 0) ((46 51) 2) ((8 16) 1) ((166 174) 1) ((153 159) 5) ((324 333) 0) ((276 285) 0) ((230 238) 4) ((186 192) 5) ((131 140) 3) ((402 411) 0) ((389 395) 8) ((467 476) 7) ((423 429) 8) ((367 376) 6) ((513 518) 0) ((545 551) 0))
   '(("id $example" . 8) ("anonymous 1 id $test" . 29) ("id $example2" . 131) ("anonymous 2 id $example" . 166) ("anonymous 2 id $test2" . 153) ("id $example3" . 367) ("anonymous 3 id $example4" . 402) ("anonymous 3 id $test3" . 389)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass myClass {\n    function random() {}\n    function __construct()\n    {\n        $this->random();\n        $this->random['abc'] = 123;\n    }\n}"
   "Method calls should be avoided in bookkeeping"
   '(((121 127) 0) ((114 119) 3) ((89 94) 3))
   '(("class myClass" . 13) ("class myClass function __construct" . 79) ("class myClass function __construct id $this" . 79)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$items = array(1, 2, 3);\nforeach ($items as &$item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => &$item2) {\n    if ($item) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of foreach reference variable declaration"
   '(((7 13) 1) ((69 74) 2) ((52 57) 2) ((41 47) 1) ((152 157) 2) ((134 140) 3) ((125 129) 4) ((115 121) 1))
   '(("id $items" . 7) ("id $item" . 52) ("id $item2" . 134) ("id $key" . 125)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n[$random, $bandom] = myValues();\nif ($random) {\n    echo 'Hit';\n}\nif ($bandom) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable declarations in array"
   '(((18 25) 1) ((9 16) 2) ((45 52) 2) ((78 85) 1))
   '(("id $bandom" . 18) ("id $random" . 9)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    global $var, $var2;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of global variable declaration in function"
   '(((8 12) 1) ((35 39) 3) ((117 122) 4) ((75 79) 5) ((60 65) 0) ((54 58) 1))
   '(("id $var" . 8) ("function test" . 30) ("function test id $abc" . 35) ("function test id $var2" . 60) ("function test id $var" . 54)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$y = 1;\n$fn1 = fn($x) => $x + $y;\n$z = 1;\n$fn = fn($x2) => fn($y2) => $x2 * $y2 + $z;\nfn(array $x3) => $x3;\n$x4 = 4;\nstatic fn(): int => $x4;\nfn($x5 = 42) => $x5;\nfn(&$x6) => $x6;\nfn&($x7) => $x7;\nfn($x8, ...$rest) => $rest;"
   "Bookkeeping in arrow functions"
   '(((7 9) 1) ((25 27) 3) ((37 39) 1) ((32 34) 3) ((15 19) 2) ((41 43) 4) ((58 61) 6) ((69 72) 7) ((89 91) 4) ((83 86) 7) ((77 80) 6) ((49 52) 5) ((102 105) 8) ((110 113) 8) ((115 118) 9) ((144 147) 9) ((152 155) 10) ((165 168) 10) ((174 177) 11) ((182 185) 11) ((191 194) 12) ((199 202) 12) ((215 220) 13) ((207 210) 14) ((225 230) 13))
   '(("id $y" . 7) ("id $fn1" . 15) ("arrow 1 id $x" . 25) ("id $z" . 41) ("id $fn" . 49) ("arrow 3 id $x2" . 58) ("arrow 2 id $y2" . 69) ("arrow 4 id $x3" . 102) ("id $x4" . 115) ("arrow 6 id $x5" . 152) ("arrow 7 id $x6" . 174) ("arrow 8 id $x7" . 191) ("arrow 9 id $rest" . 215) ("arrow 9 id $x8" . 207)))

  ;; (phps-mode-test-ast--should-bookkeep
  ;;  "<?php\n$z = (object) array('name' => 'random');\nif ($z->name) {\n    echo 'Hit';\n}"
  ;;  "Bookkeeping ignoring variable properties"
  ;;  '((" id $z" 1) ((7 9) 1) ((52 54) 1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!$var = false) {\n    echo 'Hit';\n}\n"
   "Bookkeeping negative conditional assignment"
   '(((12 16) 1))
   '(("id $var" . 12)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nif (isset($x)) {\n    if ($x) {\n        echo 'Hit';\n        if (isset($i, $u)) {\n            if ($i) {\n                echo 'Hit';\n            }\n            if ($u) {\n                echo 'Hit';\n            }\n            if ($x) {\n                echo 'Hit';\n            }\n        }\n        if ($i) {\n            echo 'Miss';\n        }\n        if ($u) {\n            echo 'Miss';\n        }\n    }\n}\nif ($x) {\n    echo 'Miss';\n}\n\nif (!empty($y)) {\n    if ($y) {\n        echo 'Hit';\n        if (!empty($k) && !empty($L)) {\n            if ($k) {\n                echo 'Hit';\n            }\n            if ($L) {\n                echo 'Hit';\n            }\n            if ($y) {\n                echo 'Hit';\n            }\n        }\n        if ($k) {\n            echo 'Miss';\n        }\n        if ($L) {\n            echo 'Miss';\n        }\n    }\n}\nif ($y) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of isset() and !empty() scoped variables"
   '(((355 357) 2) ((302 304) 1) ((232 234) 3) ((168 170) 2) ((104 106) 1) ((81 83) 2) ((77 79) 1) ((33 35) 3) ((18 20) 3) ((408 410) 3) ((793 795) 4) ((740 742) 5) ((670 672) 6) ((606 608) 4) ((542 544) 5) ((519 521) 4) ((505 507) 5) ((460 462) 6) ((445 447) 6) ((846 848) 6))
   '(("id $i" . 77) ("id $u" . 81) ("id $x" . 18) ("id $L" . 519) ("id $k" . 505) ("id $y" . 445)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ninterface myInterface\n{\n    function myFunction1();\n    function myFunction2($x);\n}\n"
   "Bookkeeping variable in interface function"
   '(((84 86) 3))
   '(("interface myInterface" . 17) ("interface myInterface function myFunction2") ("interface myInterface function myFunction2 id $x" . 84)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction1()\n{\n    return isset($a);\n}\n\nfunction myFunction2()\n{\n    $b = 2;\n    if ($b) {\n        echo 'Hit';\n    }\n    if ($a) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping after definition condition"
   '(((50 52) 2) ((143 145) 0) ((103 105) 4) ((87 89) 4))
   '(("function myFunction1" . 17) ("function myFunction1 id $a" . 50) ("function myFunction2" . 67) ("function myFunction2 id $b" . 87)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = array(1, 2, 3);\nforeach ($a as $uri => $page)\n{\n    if (isset($pages)) {\n        if ($a) {\n            echo 'Hit';\n        }\n        if ($uri) {\n            echo 'Hit';\n        }\n        if ($page) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "Bookkeeping of foreach variable inside if (isset()) block"
   '(((8 10) 1) ((204 209) 3) ((150 154) 4) ((98 100) 1) ((75 81) 2) ((52 57) 3) ((44 48) 4) ((38 40) 1))
   '(("id $a" . 8) ("id $pages" . 75) ("id $page" . 52) ("id $uri" . 44)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (isset($b)) {\n    $b = false;\n}\n$c = 2;\n\nif ($c) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable after isset() block"
   '(((28 30) 1) ((17 19) 0) ((42 44) 3) ((55 57) 3))
   '(("id $b" . 28) ("id $b (2)" . 17) ("id $c" . 42)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!isset($a)) {\n    if ($a) {\n        echo 'Miss';\n    }\n}"
   "Bookkeeping for variable in negative isset() conditional"
   '(((33 35) 1) ((18 20) 1))
   '(("id $a" . 18)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction($a, $b, $c, $d)\n{\n    global $f, $g;\n    if (isset($f)) {\n        if (!empty($g)) {\n            if ($a) {\n                echo 'Hit';\n            }\n            if ($b) {\n                echo 'Hit';\n            }\n            if ($c) {\n                echo 'Hit';\n            }\n            if ($d) {\n                echo 'Hit';\n            }\n        }\n    }\n}\n"
   "Bookkeeping variables inside nested isset() !empty() blocks"
   '(((40 42) 2) ((36 38) 3) ((32 34) 4) ((28 30) 5) ((320 322) 2) ((256 258) 3) ((192 194) 4) ((128 130) 5) ((105 107) 6) ((79 81) 7) ((61 63) 0) ((57 59) 0))
   '(("function myFunction" . 17) ("function myFunction id $d" . 40) ("function myFunction id $c" . 36) ("function myFunction id $b" . 32) ("function myFunction id $a" . 28) ("function myFunction id $g" . 105) ("function myFunction id $f" . 79) ("function myFunction id $g (2)" . 61) ("function myFunction id $f (2)" . 57)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    static $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of static variable declaration in function"
   '(((8 12) 1) ((35 39) 3) ((68 72) 4) ((54 58) 4))
   '(("id $var" . 8) ("function test" . 30) ("function test id $abc" . 35) ("function test id $var" . 54)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nglobal $a, $b;\n\nif ($a) {\n    echo 'Hit';\n}\n\nfunction myFunction($c)\n{\n    global $a;\n    if ($a) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping of global variables in functional-oriented file"
   '(((19 21) 1) ((15 17) 2) ((28 30) 2) ((73 75) 4) ((142 144) 0) ((102 104) 5) ((90 92) 2))
   '(("id $b" . 19) ("id $a" . 15) ("function myFunction" . 62) ("function myFunction id $c" . 73) ("function myFunction id $a" . 90)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nstatic $a;\n\nif ($a) {}\n\nfunction test()\n{\n    static $a;\n    if ($a) {}\n}\n\nclass There\n{\n    function here()\n    {\n        static $a;\n        if ($a) {}\n    }\n}"
   "Bookkeeping of static variables in different scopes without namespaces"
   '(((15 17) 1) ((24 26) 1) ((73 75) 3) ((61 63) 3) ((154 156) 7) ((138 140) 7))
   '(("id $a" . 15) ("function test" . 41) ("function test id $a" . 61) ("class There" . 89) ("class There function here" . 121) ("class There function here id $this" . 121) ("class There function here id $a" . 138)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private \\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of typed class variables"
   '(((462 472) 4) ((429 439) 5) ((396 406) 6) ((364 373) 0) ((357 362) 3) ((331 340) 7) ((324 329) 3) ((298 307) 8) ((291 296) 3) ((266 274) 9) ((259 264) 3) ((209 219) 4) ((171 181) 5) ((129 139) 6) ((98 108) 7) ((67 77) 8) ((33 42) 9))
   '(("class There" . 13) ("class There function here" . 245) ("class There function here id $this" . 245) ("class There static id $variable6" . 209) ("class There static id $variable5" . 171) ("class There static id $variable4" . 129) ("class There id $variable3" . 98) ("class There id $variable2" . 67) ("class There id $variable" . 33)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = $b = $c = 3;\n\nif ($a) {\n    echo 'a=',$a;\n} else {\n    echo '$a is undefined!';\n}\nif ($b) {\n    echo 'b=',$b;\n} else {\n    echo '$b is undefined!';\n}\nif ($c) {\n    echo 'c=',$c;\n} else {\n    echo '$c is undefined!';\n}"
   "Bookkeeping of chained variable assignments"
   '(((18 20) 3) ((13 15) 2) ((8 10) 1) ((51 53) 1) ((31 33) 1) ((119 121) 2) ((99 101) 2) ((187 189) 3) ((167 169) 3))
   '(("id $a" . 8) ("id $b" . 13) ("id $c" . 18)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private ?\\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static ?string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of nullable typed class variables"
   '(((464 474) 4) ((431 441) 5) ((398 408) 6) ((366 375) 0) ((359 364) 3) ((333 342) 7) ((326 331) 3) ((300 309) 8) ((293 298) 3) ((268 276) 9) ((261 266) 3) ((211 221) 4) ((172 182) 5) ((130 140) 6) ((99 109) 7) ((68 78) 8) ((33 42) 9))
   '(("class There" . 13) ("class There function here" . 247) ("class There function here id $this" . 247) ("class There static id $variable6" . 211) ("class There static id $variable5" . 172) ("class There static id $variable4" . 130) ("class There id $variable3" . 99) ("class There id $variable2" . 68) ("class There id $variable" . 33)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass MyClass\n{\n    static function here()\n    {\n        if ($this) {\n            // Miss;\n        }\n    }\n    function there()\n    {\n        if ($this) {\n            // Hit\n        }\n    }\n}\n"
   "Bookkeeping of $this not available inside static method"
   '(((153 158) 3) ((68 73) 0))
   '(("class MyClass" . 13) ("class MyClass function there" . 139) ("class MyClass function there id $this" . 139) ("class MyClass function here" . 54)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    private $tost = 'abc';\n    public function test($d)\n    {\n        return fn($e) => $this->tost . $d . $e;\n    }\n}\n\n$a = new myClass();\necho $a->test('def')('ghi');"
   "Bookkeeping of $this reference inside arrow function inside of method"
   '(((76 78) 3) ((104 106) 5) ((130 132) 5) ((125 127) 3) ((118 122) 6) ((111 116) 4) ((36 41) 6) ((143 145) 7) ((168 170) 7))
   '(("class myClass" . 14) ("class myClass function test" . 84) ("class myClass function test id $d" . 76) ("class myClass function test id $this" . 84) ("class myClass arrow 1 function test id $e" . 104) ("class myClass id $tost" . 36) ("id $a" . 143)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    static $var = '123';\n    static function myMethod($a)\n    {\n        return fn($b) => self::$var . $a . $b;\n    }\n}\n\necho myClass::myMethod('4')('5');"
   "Bookkeeping of self reference inside arrow function inside of static method"
   '(((78 80) 3) ((106 108) 4) ((131 133) 4) ((126 128) 3) ((119 123) 5) ((35 39) 5))
  '(("class myClass" . 14) ("class myClass function myMethod" . 86) ("class myClass function myMethod id $a" . 78) ("class myClass arrow 1 function myMethod id $b" . 106) ("class myClass static id $var" . 35)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nnamespace myNamespace;\nclass myClass\n{\n    private $property1 = '';\n    private $property2;\n    protected function myMethod(\n        $argument1,\n        $argument2,\n        $argument3\n    ) {\n        if ($this->property2) {\n            echo 'was here';\n        }\n        /* @codingStandardsIgnoreEnd */\n        if (\n            $argument1\n            && $argument2\n            && $argument3\n            && $argument4\n            && !empty($argument1['index'])\n            && $this->property1\n            && $argument1['index'] == $this->property1\n        ) {\n        }\n    }\n}\n"
   "Bookkeeping of properties inside if condition list"
   '(((180 190) 4) ((160 170) 5) ((140 150) 6) ((544 553) 10) ((537 542) 7) ((514 524) 6) ((489 498) 10) ((482 487) 7) ((446 456) 6) ((413 423) 0) ((387 397) 4) ((361 371) 5) ((335 345) 6) ((218 227) 9) ((211 216) 7) ((87 97) 9) ((58 68) 10))
  '(("namespace myNamespace" . 17) ("namespace myNamespace class myClass" . 36) ("namespace myNamespace class myClass function myMethod" . 197) ("namespace myNamespace class myClass function myMethod id $argument3" . 180) ("namespace myNamespace class myClass function myMethod id $argument2" . 160) ("namespace myNamespace class myClass function myMethod id $argument1" . 140) ("namespace myNamespace class myClass function myMethod id $this" . 197) ("namespace myNamespace class myClass function myMethod id $argument1 (2)" . 446) ("namespace myNamespace class myClass id $property2" . 87) ("namespace myNamespace class myClass id $property1" . 58)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ntrait MyTrait {\n    private $var = 'abc';\n    public function sayHello() {\n        if ($this->var) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "A basic trait class"
   '(((101 104) 4) ((94 99) 3) ((35 39) 4))
   '(("trait MyTrait" . 13) ("trait MyTrait function sayHello" . 80) ("trait MyTrait function sayHello id $this" . 80) ("trait MyTrait id $var" . 35)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass Person {\n    public function __construct(\n        private string $name,\n        private int $age,\n        public $address\n    ) {}\n}"
   "Class with class properties in constructor."
   '(((126 134) 3) ((105 109) 5) ((78 83) 7))
   '(("class Person" . 13) ("class Person function __construct") ("class Person function __construct id $address" . 126) ("class Person id $address" . 126) ("class Person function __construct id $age" . 105) ("class Person id $age" . 105) ("class Person function __construct id $name" . 78) ("class Person id $name" . 78)))

  (message "\n-- Ran tests for bookkeeping generation. --"))

(defun phps-mode-test-ast ()
  "Run test for ast."
  (message "-- Running all tests for ast... --\n")
  (phps-mode-test-ast-bookkeeping)
  (message "\n-- Ran all tests for ast. --"))

(phps-mode-test-ast)

(provide 'phps-mode-test-ast)


;;; phps-mode-test-ast.el ends here
