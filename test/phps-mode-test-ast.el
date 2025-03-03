;;; phps-mode-test-ast.el --- Tests for AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


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
     (setq phps-mode-lexer--cached nil)
     (setq phps-mode-lexer--cached-point nil)
     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse))
     (phps-mode-ast--generate)
     
     (unless (equal
              (phps-mode-test--hash-to-list
               phps-mode-parser-sdt-bookkeeping)
              expected-bookkeeping)
       (message "symbol-table: \n%S\n"
                (phps-mode-test--hash-to-list
                 phps-mode-parser-sdt-symbol-table))
       (message "phps-mode-parser-sdt-symbol-table-by-uri: \n%S\n" phps-mode-parser-sdt-symbol-table-by-uri)
       (message
        "expected-bookkeeping:\n%S\n"
        expected-bookkeeping)
       (message
        "actual-bookkeeping:\n%S\n"
        (phps-mode-test--hash-to-list
         phps-mode-parser-sdt-bookkeeping)))

     (should
      (equal
       (phps-mode-test--hash-to-list
        phps-mode-parser-sdt-bookkeeping)
       expected-bookkeeping))

     (when expected-imenu
       (unless
           (equal
            phps-mode-parser-sdt-symbol-imenu
            expected-imenu)
         (message "phps-mode-parser-sdt-symbol-imenu--table: %S" phps-mode-parser-sdt-symbol-imenu--table)
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
    

    ;; Run test
    (funcall logic)
    (message "Passed test for %S\n" name)))

(defun phps-mode-test-ast-bookkeeping ()
  "Run test for bookkeeping generation."
  (message "-- Running tests for bookkeeping generation... --\n")

  ;; TODO v1 Symbol namespace should be class | interface | trait / symbol
  ;; TODO v2 Should have more delicate handling of isset, !empty condition blocks
  ;; TODO v2 Should properly bookkeep inside potentially endlessly nested anonymous functions / arrow functions / anonymous classes
  ;; TODO v2 bookkeep and include all kind of constants in imenu

  ;; (phps-mode-test-ast--should-bookkeep
  ;;  "<?php\n\nnamespace mySpace\n{\n    define('MY_CONSTANT', 'abc123');\n    const MY_CONSTANT2 = 'def456';\n\n    if (\\MY_CONSTANT) {\n        echo 'hit';\n    }\n    if (MY_CONSTANT) {\n        echo 'hit';\n    }\n    if (MY_CONSTANT2) {\n        echo 'hit';\n    }\n    if (\\mySpace\\MY_CONSTANT2) {\n        echo 'hit';\n    }\n\n    if (\\YOUR_CONSTANT) {\n        echo 'miss';\n    }\n    if (YOUR_CONSTANT) {\n        echo 'miss';\n    }\n    if (\\MY_CONSTANT2) {\n        echo 'miss';\n    }\n    if (\\mySpace\\MY_CONSTANT) {\n        echo 'miss';\n    }\n\n    class myClass\n    {\n        public const MY_CONSTANT3 = 'abc123';\n        function myFunction()\n        {\n            if (self::MY_CONSTANT3) {\n                echo 'hit';\n            }\n        }\n    }\n}\nnamespace {\n    define('THEIR_CONSTANT', 'abc123');\n    if (\\THEIR_CONSTANT) {\n        echo 'hit';\n    }\n    if (THEIR_CONSTANT) {\n        echo 'hit';\n    }\n    if (MY_CONSTANT) {\n        echo 'miss';\n    }\n    if (\\MY_CONSTANT) {\n        echo 'hit';\n    }\n    if (MY_CONSTANT) {\n        echo 'hit';\n    }\n    if (\\mySpace\\MY_CONSTANT2) {\n        echo 'hit';\n    }\n}\n"
  ;;  "Constants in all possible scopes"
  ;;  '(((159 170) 1) ((208 220) 1) ((371 384) 0) ((848 862) 5) ((900 911) 1) ((1000 1011) 1))
  ;;  '(("abc")))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ntrait Foo\n{\n    public const CONSTANT = 1;\n}\n\nclass Bar\n{\n    use Foo;\n}\n\nvar_dump(Bar::CONSTANT); // 1\nvar_dump(Foo::CONSTANT); // Error"
   "PHP 8.2 trait constants"
   nil
   '(("trait Foo" ("declaration" . 13) ("CONSTANT" . 36)) ("class Bar" ("declaration" . 59))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var2) {\n    echo 'This never happens';\n}\nif ($var) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #1"
   '(((8 12) 1) ((27 32) 0) ((73 77) 1))
   '(("$var" . 8)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 'abc';\n\nif ($var) {\n    echo 'This never happens';\n}\nif ($var2) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #2"
   '(((8 12) 1) ((27 31) 1) ((72 77) 0))
   '(("$var" . 8)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$abc = 123;\n\n$var = & $abc;\nif ($var) {\n    echo 'Hit';\n}"
   "Bookkeeping in root level variable assignments #3"
   '(((8 12) 1) ((21 25) 2) ((30 34) 1) ((40 44) 2))
   '(("$abc" . 8) ("$var" . 21)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = ['abc' => 123];\n\n$ref = &$var['abc'];"
   "Bookkeeping in root level variable assignments #4"
   '(((8 12) 1) ((32 36) 2) ((40 44) 1))
   '(("$var" . 8) ("$ref" . 32)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var2 = 4;\n\nfunction myFunction($var)\n{\n    $var3 = 3;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Hit';\n    }\n}\n\nfunction myFunction2($abc)\n{\n    if ($var) {\n        echo 'Miss';\n    }\n    if ($abc) {\n        echo 'Hit';\n    }\n}\n\nif ($var) {\n    echo 'Miss';\n}\nif ($var2) {\n    echo 'Hit';\n}"
   "Bookkeeping in function level with variable assignments"
   '(((8 13) 1) ((40 44) 3) ((52 57) 2) ((71 75) 3) ((113 118) 0) ((157 162) 2) ((216 220) 4) ((232 236) 0) ((275 279) 4) ((316 320) 0) ((347 352) 1))
   '(("$var2" . 8) ("function myFunction" ("declaration" . 29) ("$var" . 40) ("$var3" . 52)) ("function myFunction2" ("declaration" . 204) ("$abc" . 216))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Super-globals\n\nif ($_GET) {\n    echo 'Hit';\n}\nif ($_POST) {\n    echo 'Hit';\n}\nif ($_COOKIE) {\n    echo 'Hit';\n}\nif ($_SESSION) {\n    echo 'Hit';\n}\nif ($_REQUEST) {\n    echo 'Hit';\n}\nif ($GLOBALS) {\n    echo 'Hit';\n}\nif ($_SERVER) {\n    echo 'Hit';\n}\nif ($_FILES) {\n    echo 'Hit';\n}\nif ($_ENV) {\n    echo 'Hit';\n}\nif ($argc) {\n    echo 'Hit';\n}\nif ($argv) {\n    echo 'Hit';\n}\nif ($http_​response_​header) {\n    echo 'Hit';\n}"
   "Bookkeeping of super-globals"
   '(((30 35) -1) ((61 67) -1) ((93 101) -1) ((127 136) -1) ((162 171) -1) ((197 205) -1) ((231 239) -1) ((265 272) -1) ((298 303) -1) ((329 334) -1) ((360 365) -1) ((391 414) -1)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        public static function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping in maximum level with namespaces, classes and functions."
   '(((37 41) 1) ((86 91) 2) ((142 147) 4) ((163 168) 3) ((192 196) 0) ((259 264) 0) ((327 332) 4) ((394 399) 3) ((485 490) 7) ((514 519) 5) ((543 547) 0) ((610 615) 0) ((678 683) 0) ((746 751) 0) ((814 819) 7) ((881 886) 5) ((957 961) 1) ((999 1004) 0) ((1043 1048) 0) ((1087 1092) 0) ((1131 1136) 0) ((1175 1180) 0) ((1243 1248) 8) ((1293 1298) 9) ((1335 1341) 12) ((1357 1362) 10) ((1386 1390) 0) ((1453 1458) 0) ((1521 1526) 0) ((1589 1594) 0) ((1657 1662) 0) ((1725 1730) 0) ((1793 1798) 0) ((1861 1866) 0) ((1929 1934) 10) ((1996 2002) 12) ((2088 2094) 15) ((2118 2124) 13) ((2148 2152) 0) ((2215 2220) 0) ((2283 2288) 0) ((2351 2356) 0) ((2419 2424) 0) ((2486 2491) 0) ((2553 2558) 0) ((2621 2626) 0) ((2689 2694) 0) ((2757 2763) 0) ((2826 2832) 13) ((2894 2900) 15) ((2971 2975) 0) ((3013 3018) 0) ((3057 3062) 0) ((3101 3106) 0) ((3145 3150) 0) ((3189 3194) 0) ((3233 3238) 8))
   '(("namespace myNamespaceA" ("declaration" . 18) ("$var" . 37) ("class myClassA" ("declaration" . 59) ("$var2" . 86) ("function myFunctionA" ("declaration" . 130) ("$var3" . 142) ("$var4" . 163)) ("function myFunctionB" ("declaration" . 473) ("$var5" . 485) ("$var6" . 514)))) ("namespace myNamespaceB" ("declaration" . 1224) ("$var7" . 1243) ("class myClassB" ("declaration" . 1266) ("$var8" . 1293) ("function myFunctionA" ("declaration" . 1323) ("$var10" . 1335) ("$var9" . 1357)) ("function myFunctionB" ("declaration" . 2076) ("$var12" . 2088) ("$var11" . 2118))))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Conditional assignments\n\n$items = array(1, 2, 3);\nforeach ($items as $item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => $value) {\n    if ($key || $value) {\n        echo 'Hit';\n    }\n}\nfor ($i = 0; $i < count($items); $i++) {\n    if ($i) {\n        echo 'Hit';\n    }\n}\nif ($a = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\nwhile ($b = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\ndo {\n    echo 'Hit';\n} while ($c = 456);\n"
   "Bookkeeping of conditional assignments"
   '(((36 42) 1) ((70 76) 1) ((80 85) 2) ((97 102) 2) ((143 149) 1) ((153 157) 3) ((161 167) 4) ((179 183) 3) ((187 193) 4) ((230 232) 5) ((238 240) 5) ((249 255) 1) ((258 260) 5) ((274 276) 5) ((312 314) 6) ((332 334) 6) ((373 375) 7) ((393 395) 6) ((457 459) 8))
   '(("$items" . 36) ("$item" . 80) ("$key" . 153) ("$value" . 161) ("$i" . 230) ("$a" . 312) ("$b" . 373) ("$c" . 457)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n// Class properties\n\nclass myParent {}\n\nclass myClass extends myParent {\n    private $var1 = 123;\n    protected static $var2;\n    public $var3;\n    var $var4;\n    function __construct() {\n        if ($this) {\n            echo 'Hit';\n        }\n        if ($this->var1) {\n            echo 'Hit';\n        }\n        if (self::$var1) {\n            echo 'Miss';\n        }\n        if (self::$var2) {\n            echo 'Hit';\n        }\n        if (static::$var2) {\n            echo 'Hit';\n        }\n        if ($this->var3) {\n            echo 'Hit';\n        }\n        if ($this->var4) {\n            echo 'Hit';\n        }\n        if ($this->var5) {\n            echo 'Miss';\n        }\n        if (paren1) {\n            echo 'Hit';\n        }\n    }\n}\n\nif ($this) {\n    echo 'Miss';\n}\nif (self) {\n    echo 'Miss';\n}\nif (paren1) {\n    echo 'Miss';\n}"
   "Bookkeeping of class properties"
   '(((93 98) 1) ((127 132) 2) ((145 150) 3) ((160 165) 4) ((208 213) 5) ((263 268) 5) ((270 274) 1) ((330 335) 0) ((392 397) 2) ((455 460) 2) ((510 515) 5) ((517 521) 3) ((571 576) 5) ((578 582) 4) ((632 637) 5) ((639 643) 0) ((751 756) 0))
   '(("class myParent" ("declaration" . 35)) ("class myClass" ("declaration" . 54) ("$var1" . 93) ("$var2" . 127) ("$var3" . 145) ("$var4" . 160) ("function __construct" ("declaration" . 180)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass myClass\n{\n    private $var = 123, $def = 'acb';\n}"
   "Multiple class properties assigned on the same line."
   '(((35 39) 1) ((47 51) 2))
   '(("class myClass" ("declaration" . 13) ("$var" . 35) ("$def" . 47))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\ntry {\n    \n} catch (\\Exception $e) {\n    if ($e) {\n        echo 'Hit';\n    }\n}\n\nif ($e) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of try catch variable assignment"
   '(((39 41) 1) ((53 55) 1) ((92 94) 1))
   '(("$e" . 39)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$example = function ($test) {\n    if ($test) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Miss';\n    }\n};\n$example2 = function ($test2) use ($example) {\n    if ($test2) {\n        echo 'Hit';\n    }\n    if ($example) {\n        echo 'Hit';\n    }\n    if ($example2) {\n        echo 'Miss';\n    }\n    if ($example3) {\n        echo 'Miss';\n    }\n};\n$example3 = function ($test3) use ($example4) {\n    if ($test3) {\n        echo 'Hit';\n    }\n    if ($example4) {\n        echo 'Hit';\n    }\n};\nif ($test) {\n    echo 'Miss';\n}\nif ($test2) {\n    echo 'Miss';\n}"
   "Bookkeeping of anonymous function variable assignments and lexical vars"
   '(((8 16) 2) ((29 34) 1) ((46 51) 1) ((89 97) 0) ((131 140) 5) ((153 159) 3) ((166 174) 2) ((186 192) 3) ((230 238) 4) ((276 285) 0) ((324 333) 0) ((367 376) 8) ((389 395) 6) ((402 411) 0) ((423 429) 6) ((467 476) 7) ((513 518) 0) ((545 551) 0))
   '(("$example" . 8) ("$test" . 29) ("$example2" . 131) ("$test2" . 153) ("$example3" . 367) ("$test3" . 389) ("$example4" . 402)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nfinal class myClass {\n    function random() {}\n    function __construct()\n    {\n        $this->random();\n        $this->random['abc'] = 123;\n    }\n}"
   "Method calls should be avoided in bookkeeping"
   '(((95 100) 2) ((120 125) 2) ((127 133) 0))
   '(("class myClass" ("declaration" . 19) ("function random" ("declaration" . 42)) ("function __construct" ("declaration" . 67)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$items = array(1, 2, 3);\nforeach ($items as &$item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => &$item2) {\n    if ($item) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of foreach reference variable declaration"
   '(((7 13) 1) ((41 47) 1) ((52 57) 2) ((69 74) 2) ((115 121) 1) ((125 129) 3) ((134 140) 4) ((152 157) 2))
   '(("$items" . 7) ("$item" . 52) ("$key" . 125) ("$item2" . 134)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n[$random, $bandom] = myValues();\nif ($random) {\n    echo 'Hit';\n}\nif ($bandom) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable declarations in array"
   '(((9 16) 1) ((18 25) 2) ((45 52) 1) ((78 85) 2))
   '(("$random" . 9) ("$bandom" . 18)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nlist($random, $bandom) = myValues();\nif ($random) {\n    echo 'Hit';\n}\nif ($bandom) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable declarations in array via list()"
   '(((13 20) 1) ((22 29) 2) ((49 56) 1) ((82 89) 2))
   '(("$random" . 13) ("$bandom" . 22)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    global $var, $var2;\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of global variable declaration in function"
   '(((8 12) 1) ((35 39) 4) ((54 58) 1) ((60 65) 0) ((75 79) 2) ((117 122) 3))
   '(("$var" . 8) ("function test" ("declaration" . 30) ("$abc" . 35) ("$var" . 54) ("$var2" . 60))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$y = 1;\n$fn1 = fn($x) => $x + $y;\n$z = 1;\n$fn = fn($x2) => fn($y2) => $x2 * $y2 + $z;\nfn(array $x3) => $x3;\n$x4 = 4;\nstatic fn(): int => $x4;\nfn($x5 = 42) => $x5;\nfn(&$x6) => $x6;\nfn&($x7) => $x7;\nfn($x8, ...$rest) => $rest;"
   "Bookkeeping in arrow functions"
   '(((7 9) 1) ((15 19) 3) ((25 27) 2) ((32 34) 2) ((37 39) 1) ((41 43) 4) ((49 52) 7) ((58 61) 6) ((69 72) 5) ((77 80) 6) ((83 86) 5) ((89 91) 4) ((102 105) 8) ((110 113) 8) ((115 118) 9) ((144 147) 9) ((152 155) 10) ((165 168) 10) ((174 177) 11) ((182 185) 11) ((191 194) 12) ((199 202) 12) ((207 210) 13) ((215 220) 14) ((225 230) 14))
   '(("$y" . 7) ("$fn1" . 15) ("$x" . 25) ("$z" . 41) ("$fn" . 49) ("$x2" . 58) ("$y2" . 69) ("$x3" . 102) ("$x4" . 115) ("$x5" . 152) ("$x6" . 174) ("$x7" . 191) ("$x8" . 207) ("$rest" . 215)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n$z = (object) ['name' => 'random', 'doThis' => function() {}];\nif ($z->name) {\n    echo 'Hit';\n}\nif ($z->doThis()) {\n    echo 'Hit';\n}"
   "Bookkeeping ignoring variable properties that is not $this"
   '(((7 9) 1) ((74 76) 1) ((108 110) 1))
   '(("$z" . 7)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!$var = false) {\n    echo 'Hit';\n}\n"
   "Bookkeeping negative conditional assignment"
   '(((12 16) 1))
   '(("$var" . 12)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nif (isset($x)) {\n    if ($x) {\n        echo 'Hit';\n        if (isset($i, $u)) {\n            if ($i) {\n                echo 'Hit';\n            }\n            if ($u) {\n                echo 'Hit';\n            }\n            if ($x) {\n                echo 'Hit';\n            }\n        }\n        if ($i) {\n            echo 'Miss';\n        }\n        if ($u) {\n            echo 'Miss';\n        }\n    }\n}\nif ($x) {\n    echo 'Miss';\n}\n\nif (!empty($y)) {\n    if ($y) {\n        echo 'Hit';\n        if (!empty($k) && !empty($L)) {\n            if ($k) {\n                echo 'Hit';\n            }\n            if ($L) {\n                echo 'Hit';\n            }\n            if ($y) {\n                echo 'Hit';\n            }\n        }\n        if ($k) {\n            echo 'Miss';\n        }\n        if ($L) {\n            echo 'Miss';\n        }\n    }\n}\nif ($y) {\n    echo 'Miss';\n}\n"
   "Bookkeeping of isset() and !empty() scoped variables"
   '(((18 20) 1) ((33 35) 1) ((77 79) 3) ((81 83) 2) ((104 106) 3) ((168 170) 2) ((232 234) 1) ((302 304) 3) ((355 357) 2) ((408 410) 1) ((445 447) 4) ((460 462) 4) ((505 507) 5) ((519 521) 6) ((542 544) 5) ((606 608) 6) ((670 672) 4) ((740 742) 5) ((793 795) 6) ((846 848) 4))
   '(("$x" . 18) ("$i" . 77) ("$u" . 81) ("$y" . 445) ("$k" . 505) ("$L" . 519)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ninterface myInterface\n{\n    function myFunction1();\n    function myFunction2($x);\n}\n"
   "Bookkeeping variable in interface function"
   '(((84 86) 1))
   '(("interface myInterface" ("declaration" . 17) ("function myFunction1" ("declaration" . 44)) ("function myFunction2" ("declaration" . 72) ("$x" . 84)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction1()\n{\n    return isset($a);\n}\n\nfunction myFunction2()\n{\n    $b = 2;\n    if ($b) {\n        echo 'Hit';\n    }\n    if ($a) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping after definition condition"
   '(((50 52) 1) ((87 89) 2) ((103 105) 2) ((143 145) 0))
   '(("function myFunction1" ("declaration" . 17) ("$a" . 50)) ("function myFunction2" ("declaration" . 67) ("$b" . 87))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = array(1, 2, 3);\nforeach ($a as $uri => $page)\n{\n    if (isset($pages)) {\n        if ($a) {\n            echo 'Hit';\n        }\n        if ($uri) {\n            echo 'Hit';\n        }\n        if ($page) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "Bookkeeping of foreach variable inside if (isset()) block"
   '(((8 10) 1) ((38 40) 1) ((44 48) 2) ((52 57) 3) ((75 81) 4) ((98 100) 1) ((150 154) 2) ((204 209) 3))
   '(("$a" . 8) ("$uri" . 44) ("$page" . 52) ("$pages" . 75)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (isset($b)) {\n    $b = false;\n}\n$c = 2;\n\nif ($c) {\n    echo 'Hit';\n}\n"
   "Bookkeeping of variable after isset() block"
   '(((17 19) 1) ((28 30) 1) ((42 44) 3) ((55 57) 3))
   '(("$b" . 17) ("$c" . 42)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nif (!isset($a)) {\n    if ($a) {\n        echo 'Miss';\n    }\n}"
   "Bookkeeping for variable in negative isset() conditional"
   '(((18 20) 1) ((33 35) 1))
   '(("$a" . 18)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nfunction myFunction($a, $b, $c, $d)\n{\n    global $f, $g;\n    if (isset($f)) {\n        if (!empty($g)) {\n            if ($a) {\n                echo 'Hit';\n            }\n            if ($b) {\n                echo 'Hit';\n            }\n            if ($c) {\n                echo 'Hit';\n            }\n            if ($d) {\n                echo 'Hit';\n            }\n        }\n    }\n}\n"
   "Bookkeeping variables inside nested isset() !empty() blocks"
   '(((28 30) 5) ((32 34) 6) ((36 38) 7) ((40 42) 8) ((57 59) 0) ((61 63) 0) ((79 81) 1) ((105 107) 2) ((128 130) 5) ((192 194) 6) ((256 258) 7) ((320 322) 8))
   '(("function myFunction" ("declaration" . 17) ("$a" . 28) ("$b" . 32) ("$c" . 36) ("$d" . 40) ("$f" . 57) ("$g" . 61))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$var = 123;\n\nfunction test($abc) {\n    static $var;\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of static variable declaration in function"
   '(((8 12) 1) ((35 39) 3) ((54 58) 2) ((68 72) 2))
   '(("$var" . 8) ("function test" ("declaration" . 30) ("$abc" . 35) ("$var" . 54))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nglobal $a, $b;\n\nif ($a) {\n    echo 'Hit';\n}\n\nfunction myFunction($c)\n{\n    global $a;\n    if ($a) {\n        echo 'Hit';\n    }\n    if ($b) {\n        echo 'Miss';\n    }\n}\n"
   "Bookkeeping of global variables in functional-oriented file"
   '(((15 17) 1) ((19 21) 2) ((28 30) 1) ((73 75) 4) ((90 92) 1) ((102 104) 3) ((142 144) 0))
   '(("$a" . 15) ("$b" . 19) ("function myFunction" ("declaration" . 62) ("$c" . 73) ("$a" . 90))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nstatic $a;\n\nif ($a) {}\n\nfunction test()\n{\n    static $a;\n    if ($a) {}\n}\n\nclass There\n{\n    function here()\n    {\n        static $a;\n        if ($a) {}\n    }\n}"
   "Bookkeeping of static variables in different scopes without namespaces"
   '(((15 17) 1) ((24 26) 1) ((61 63) 2) ((73 75) 2) ((138 140) 3) ((154 156) 3))
   '(("$a" . 15) ("function test" ("declaration" . 41) ("$a" . 61)) ("class There" ("declaration" . 89) ("function here" ("declaration" . 110) ("$a" . 138)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private \\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of typed class variables"
   '(((33 42) 1) ((67 77) 2) ((98 108) 3) ((129 139) 4) ((171 181) 5) ((209 219) 6) ((259 264) 7) ((266 274) 1) ((291 296) 7) ((298 307) 2) ((324 329) 7) ((331 340) 3) ((357 362) 7) ((364 373) 0) ((396 406) 4) ((429 439) 5) ((462 472) 6))
   '(("class There" ("declaration" . 13) ("$variable" . 33) ("$variable2" . 67) ("$variable3" . 98) ("$variable4" . 129) ("$variable5" . 171) ("$variable6" . 209) ("function here" ("declaration" . 234)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\n$a = $b = $c = 3;\n\nif ($a) {\n    echo 'a=',$a;\n} else {\n    echo '$a is undefined!';\n}\nif ($b) {\n    echo 'b=',$b;\n} else {\n    echo '$b is undefined!';\n}\nif ($c) {\n    echo 'c=',$c;\n} else {\n    echo '$c is undefined!';\n}"
   "Bookkeeping of chained variable assignments"
   '(((8 10) 3) ((13 15) 2) ((18 20) 1) ((31 33) 3) ((51 53) 3) ((99 101) 2) ((119 121) 2) ((167 169) 1) ((187 189) 1))
   '(("$a" . 8) ("$b" . 13) ("$c" . 18)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass There\n{\n    private $variable;\n    private ?\\My\\Random $variable2;\n    private string $variable3;\n    private static $variable4;\n    private static \\My\\Random $variable5;\n    private static ?string $variable6;\n    function here()\n    {\n        if ($this->variable) {}\n        if ($this->variable2) {}\n        if ($this->variable3) {}\n        if ($this->variable4) {}\n        if (self::$variable4) {}\n        if (self::$variable5) {}\n        if (self::$variable6) {}\n    }\n}\n"
   "Bookkeeping of nullable typed class variables"
   '(((33 42) 1) ((68 78) 2) ((99 109) 3) ((130 140) 4) ((172 182) 5) ((211 221) 6) ((261 266) 7) ((268 276) 1) ((293 298) 7) ((300 309) 2) ((326 331) 7) ((333 342) 3) ((359 364) 7) ((366 375) 0) ((398 408) 4) ((431 441) 5) ((464 474) 6))
   '(("class There" ("declaration" . 13) ("$variable" . 33) ("$variable2" . 68) ("$variable3" . 99) ("$variable4" . 130) ("$variable5" . 172) ("$variable6" . 211) ("function here" ("declaration" . 236)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass MyClass\n{\n    static function here()\n    {\n        if ($this) {\n            // Miss;\n        }\n    }\n    function there()\n    {\n        if ($this) {\n            // Hit\n        }\n    }\n}\n"
   "Bookkeeping of $this not available inside static method"
   '(((68 73) 0) ((153 158) 1))
   '(("class MyClass" ("declaration" . 13) ("function here" ("declaration" . 43)) ("function there" ("declaration" . 127)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    private $tost = 'abc';\n    public function test($d)\n    {\n        return fn($e) => $this->tost . $d . $e;\n    }\n}\n\n$a = new myClass();\necho $a->test('def')('ghi');"
   "Bookkeeping of $this reference inside arrow function inside of method"
   '(((36 41) 1) ((76 78) 4) ((104 106) 2) ((111 116) 3) ((118 122) 1) ((125 127) 4) ((130 132) 2) ((143 145) 5) ((168 170) 5))
   '(("class myClass" ("declaration" . 14) ("$tost" . 36) ("function test" ("declaration" . 71) ("$d" . 76) ("$e" . 104))) ("$a" . 143)))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    static $var = '123';\n    static function myMethod($a)\n    {\n        return fn($b) => self::$var . $a . $b;\n    }\n}\n\necho myClass::myMethod('4')('5');"
   "Bookkeeping of self reference inside arrow function inside of static method"
   '(((35 39) 1) ((78 80) 3) ((106 108) 2) ((119 123) 1) ((126 128) 3) ((131 133) 2))
  '(("class myClass" ("declaration" . 14) ("$var" . 35) ("function myMethod" ("declaration" . 69) ("$a" . 78) ("$b" . 106)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nnamespace myNamespace;\nclass myClass\n{\n    private $property1 = '';\n    private $property2;\n    protected function myMethod(\n        $argument1,\n        $argument2,\n        $argument3\n    ) {\n        if ($this->property2) {\n            echo 'was here';\n        }\n        /* @codingStandardsIgnoreEnd */\n        if (\n            $argument1\n            && $argument2\n            && $argument3\n            && $argument4\n            && !empty($argument1['index'])\n            && $this->property1\n            && $argument1['index'] == $this->property1\n        ) {\n        }\n    }\n}\n"
   "Bookkeeping of properties inside if condition list"
   '(((58 68) 1) ((87 97) 2) ((140 150) 5) ((160 170) 6) ((180 190) 7) ((211 216) 4) ((218 227) 2) ((335 345) 5) ((361 371) 6) ((387 397) 7) ((413 423) 0) ((446 456) 3) ((482 487) 4) ((489 498) 1) ((514 524) 3) ((537 542) 4) ((544 553) 1))
  '(("namespace myNamespace" ("declaration" . 17) ("class myClass" ("declaration" . 36) ("$property1" . 58) ("$property2" . 87) ("function myMethod" ("declaration" . 122) ("$argument2" . 160) ("$argument3" . 180) ("$argument1" . 446))))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\ntrait MyTrait {\n    private $var = 'abc';\n    public function sayHello() {\n        if ($this->var) {\n            echo 'Hit';\n        }\n    }\n}\n"
   "A basic trait class"
   '(((35 39) 1) ((94 99) 2) ((101 104) 1))
   '(("trait MyTrait" ("declaration" . 13) ("$var" . 35) ("function sayHello" ("declaration" . 69)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nclass Person {\n    public function __construct(\n        private string $name,\n        private int $age,\n        public $address\n    ) {}\n}"
   "Class with class properties in constructor."
   '(((78 83) 3) ((105 109) 5) ((126 134) 7))
   '(("class Person" ("declaration" . 13) ("function __construct" ("declaration" . 42) ("$name" . 78) ("$age" . 105) ("$address" . 126)) ("$name" . 78) ("$age" . 105) ("$address" . 126))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\nfunction myFunction()\n{\n    $variable = 123;\n    if ($variable === 456) {\n        $variable = 789;\n    }\n}\n"
   "Variable inside function with assignment inside conditional block"
   '(((35 44) 1) ((60 69) 1) ((89 98) 1))
   '(("function myFunction" ("declaration" . 16) ("$variable" . 35))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nclass myClass\n{\n    static $abc = 123;\n    public function __construct()\n    {\n        if (self::$abc) {\n            echo 'Hit';\n        }\n        if (self::$abc2) {\n            echo 'Miss';\n        }\n        if ($this->random()) {\n            echo 'Hit';\n        }\n        if (myClass::$random) {\n            echo 'Hit';\n        }\n        if (self::random()) {\n            echo 'Hit';\n        }\n        if (myClass::random()) {\n            echo 'Hit';\n        }\n    }\n}"
   "Do not bookkeep class static properties other than self and static"
   '(((35 39) 1) ((105 109) 1) ((165 170) 0) ((221 226) 2))
   '(("class myClass" ("declaration" . 14) ("$abc" . 35) ("function __construct" ("declaration" . 67)))))


  (phps-mode-test-ast--should-bookkeep
   "<?php\nabstract class AbstractClass\n{\n    // Force Extending class to define this method\n    abstract protected function getValue();\n    abstract protected function prefixValue($prefix);\n\n    // Common method\n    public function printOut() {\n        print $this->getValue() . \"\\n\";\n    }\n}\n\nclass ConcreteClass1 extends AbstractClass\n{\n    protected function getValue() {\n        return \"ConcreteClass1\";\n    }\n\n    public function prefixValue($prefix) {\n        return \"{$prefix}ConcreteClass1\";\n    }\n}\n\nclass ConcreteClass2 extends AbstractClass\n{\n    public function getValue() {\n        return \"ConcreteClass2\";\n    }\n\n    public function prefixValue($prefix) {\n        return \"{$prefix}ConcreteClass2\";\n    }\n}\n\n$class1 = new ConcreteClass1;\n$class1->printOut();\necho $class1->prefixValue('FOO_') .\"\\n\";\n\n$class2 = new ConcreteClass2;\n$class2->printOut();\necho $class2->prefixValue('FOO_') .\"\\n\";\n?>"
   "Bookkeeping of abstract class"
   '(((177 184) 1) ((256 261) 2) ((444 451) 5) ((472 479) 5) ((656 663) 8) ((684 691) 8) ((718 725) 9) ((748 755) 9) ((774 781) 9) ((811 818) 10) ((841 848) 10) ((867 874) 10))
   '(("class AbstractClass" ("declaration" . 22) ("function getValue" ("declaration" . 121)) ("function prefixValue" ("declaration" . 165) ("$prefix" . 177)) ("function printOut" ("declaration" . 229))) ("class ConcreteClass1" ("declaration" . 297) ("function getValue" ("declaration" . 359)) ("function prefixValue" ("declaration" . 432) ("$prefix" . 444))) ("class ConcreteClass2" ("declaration" . 512) ("function getValue" ("declaration" . 571)) ("function prefixValue" ("declaration" . 644) ("$prefix" . 656))) ("$class1" . 718) ("$class2" . 811)))

  (phps-mode-test-ast--should-bookkeep
   "<?php
\nnamespace myNamespace;\n\nfunction myFunction()\n{\n    $var = 'abc';\n    if ($var) {\n        echo 'Hit';\n    }\n}"
   "Bookkeeping of namespaced function with variables."
   '(((60 64) 1) ((82 86) 1))
   '(("namespace myNamespace" ("declaration" . 18) ("function myFunction" ("declaration" . 41) ("function myFunction" ("declaration" . 41)) ("$var" . 60)))))

  (phps-mode-test-ast--should-bookkeep
   "<?php\n\nnamespace myNamespace;\n\ntrait myTrait\n{\n    public function myFunction($arg): string {\n        if ($arg) {\n            echo 'hit';\n        }\n    }\n}"
   "Bookkeeping of namespaced trait"
   '(((79 83) 2) ((107 111) 2))
   '(("namespace myNamespace" ("declaration" . 18) ("trait myTrait" ("declaration" . 38) ("function myFunction" ("declaration" . 68) ("$arg" . 79))))))

  (message "\n-- Ran tests for bookkeeping generation. --"))

(defun phps-mode-test-ast ()
  "Run test for ast."
  (message "-- Running all tests for ast... --\n")
  (phps-mode-test-ast-bookkeeping)
  (message "\n-- Ran all tests for ast. --"))

(phps-mode-test-ast)

(provide 'phps-mode-test-ast)


;;; phps-mode-test-ast.el ends here
