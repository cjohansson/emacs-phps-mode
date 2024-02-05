;;; phps-mode-test-parser.el --- Tests for parser -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make test-parser


;;; Code:

(require 'ert)
(require 'phps-mode-test)
(require 'phps-mode-lex-analyzer)

(defun phps-mode-test-parser--buffer-contents (buffer-contents name logic)
  (with-temp-buffer
    ;; Setup buffer
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))

    ;; Run test
    (funcall logic)
    (message "Passed %s" name)))

(defun phps-mode-test-parser-parse ()
  "Run test for parser."
  (message "-- Running tests for parser basic... --\n")

  (phps-mode-test-parser--buffer-contents
   "<?php\nclass Falsy\n{\n    public function alwaysFalse(): false { /* ... */ }\n\n    public function alwaysTrue(): true { /* ... */ }\n\n    public function alwaysNull(): null { /* ... */ }\n}\n"
   "PHP 8.2 - allow null, false, and true as stand-alone types"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 205 209 454 304 340 338 335 453 457 81 454 250 91 279 265 261 287 455 140 331 455 308 310 303 340 338 335 453 457 81 454 250 91 279 265 261 287 455 140 331 455 308 310 303 340 338 335 453 457 81 454 250 91 279 265 261 287 455 140 331 455 308 310 303 187 103 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\ntrait Foo\n{\n    public const CONSTANT = 1;\n}\n\nclass Bar\n{\n    use Foo;\n}\n"
   "PHP 8.2 - Constants in traits"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 450 304 337 335 334 81 482 433 450 350 349 306 309 303 193 104 108 83 201 205 450 304 91 467 312 314 311 303 187 103 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nreadonly class BlogData\n{\n    public string $title;\n\n    public Status $status;\n\n    public function __construct(string $title, Status $status)\n    {\n        $this->title = $title;\n        $this->status = $status;\n    }\n}\n"
   "PHP 8.2 - readonly classes"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 192 188 201 205 450 304 337 335 331 91 279 273 260 450 346 345 305 309 303 337 335 331 91 279 273 260 450 346 345 305 309 303 337 335 334 449 453 81 450 251 91 279 273 260 182 184 450 257 250 247 251 91 279 273 260 182 184 450 257 250 248 123 245 286 451 140 523 513 519 504 508 538 521 523 513 519 363 366 159 141 139 523 513 519 504 508 538 521 523 513 519 363 366 159 141 139 330 451 307 309 303 186 103 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nclass Foo {\n    public function bar((A&B)|null $entity) {\n        return $entity;\n    }\n}\n"
   "PHP 8.2 - disjunctive normal form (DNF) types"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 201 205 450 304 337 335 334 449 453 81 450 251 91 279 91 279 284 281 91 279 280 282 275 260 182 184 450 257 250 247 123 245 286 451 140 523 513 519 363 502 154 141 139 330 451 307 309 303 187 103 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'hello';"
   "Basic echo test"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 480 487 433 354 353 157 107 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<? echo 'hello'; ?>"
   "Basic echo test 2 with short open tag and close tag"
   (lambda()
     (should
      (equal
       '(84 480 487 433 354 353 157 107 83 164 107 83)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?= 'hello';"
   "Basic echo test 3 with open tag with echo"
   (lambda()
     (should
      (equal
       '(84 480 487 433 354 353 157 107 83)
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
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 480 487 433 354 353 157 107 83 164 107 83 158 107 83 480 487 433 354 353 157 107 83 164 107 83 158 107 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nfunction myFunction($arg) { $arg = 2; return $arg; }"
   "Simple function defintion"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 449 453 179 450 251 259 182 184 450 257 250 247 123 245 286 451 140 523 513 519 482 433 366 159 141 139 523 513 519 363 502 154 141 139 451 181 102 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nfunction myFunction($arg) {\n    $arg = 2;\n    return $arg;\n}\n"
   "Simple function defintion inside un-bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 81 85 111 83 449 453 179 450 251 259 182 184 450 257 250 247 123 245 286 451 140 523 513 519 482 433 366 159 141 139 523 513 519 363 502 154 141 139 451 181 102 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace {\n    function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion inside bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 81 85 84 449 453 179 450 251 259 182 184 450 257 250 247 123 245 286 451 140 523 513 519 482 433 366 159 141 139 523 513 519 363 502 154 141 139 451 181 102 108 83 112 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nclass MyClass\n{\n    private $var = 'abc';\n    public function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion and property inside class inside non-bracketed namespace"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (phps-mode-test--output-parse-productions parse)
       (should
        (equal
         '(84 81 85 111 83 201 205 450 304 339 335 331 259 480 487 433 450 347 345 305 309 303 337 335 334 449 453 81 450 251 259 182 184 450 257 250 247 123 245 286 451 140 523 513 519 482 433 366 159 141 139 523 513 519 363 502 154 141 139 330 451 307 309 303 187 103 108 83)
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

  (phps-mode-test-parser--buffer-contents
   "<?php match (55) {\n    22,33 => 22,\n    25 => 20,\n    default => 33\n};"
   "Simple match expression"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n$food = 'cake';\n\n$return_value = match ($food) {\n    default => 'This food is an apple',\n};\n\nvar_dump($return_value);\n?>"
   "Basic match usage 1"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n$food = 'cake';\n\n$return_value = match ($food) {\n    'apple' => 'This food is an apple',\n    'bar' => 'This food is a bar',\n    'cake' => 'This food is a cake',\n};\n\nvar_dump($return_value);\n?>"
   "Basic match usage 2"
   (lambda()
     (phps-mode-parser-parse)))

  (phps-mode-test-parser--buffer-contents
   "<?php\n$case = 'First';\nswitch ($case)\n{\n    case 'First':\n    case 'Second':\n        echo 'was here';\n}"
   "Switch case with multiple conditions for same case"
   (lambda()
     (phps-mode-parser-parse)))

  (message "\n-- Ran tests for parser parse. --"))

(defun phps-mode-test-parser ()
  "Run test for lexer."
  (message "-- Running all tests for parser... --\n")

  (phps-mode-test-parser-parse)

  (message "\n-- Ran all tests for parser. --"))

(phps-mode-test-parser)

(provide 'phps-mode-test-parser)


;;; phps-mode-test-parser.el ends here
