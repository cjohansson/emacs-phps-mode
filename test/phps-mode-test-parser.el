;;; phps-mode-test-parser.el --- Tests for parser -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make test-parser


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-lex-analyzer)

(defun phps-mode-test-parser--buffer-contents (buffer-contents name logic)
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
    (message "Lexer tokens:\n%S\n" phps-mode-lexer--generated-tokens)
    (message "Parser tokens:\n%S\n" phps-mode-parser-tokens)

    ;; Run test
    (funcall logic)
    (message "Passed %s" name)))

(defun phps-mode-test-parser-parse ()
  "Run test for parser."
  (message "-- Running tests for parser basic... --\n")

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'hello';"
   "Basic echo test"
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
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 472 479 426 347 346 157 107 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<? echo 'hello'; ?>"
   "Basic echo test 2 with short open tag and close tag"
   (lambda()
     (should
      (equal
       '(84 472 479 426 347 346 157 107 83 164 107 83)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?= 'hello';"
   "Basic echo test 3 with open tag with echo"
   (lambda()
     (should
      (equal
       '(84 472 479 426 347 346 157 107 83)
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
       (message "Left-to-right with right-most derivation:\n%S\n" parse)
       (dolist (production-number parse)
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 472 479 426 347 346 157 107 83 164 107 83 158 107 83 472 479 426 347 346 157 107 83 164 107 83 158 107 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nfunction myFunction($arg) { $arg = 2; return $arg; }"
   "Simple function defintion"
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
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 442 446 443 248 256 180 182 443 254 247 244 123 242 279 444 140 515 505 511 474 426 359 159 141 139 515 505 511 356 494 154 141 139 444 179 102 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nfunction myFunction($arg) {\n    $arg = 2;\n    return $arg;\n}\n"
   "Simple function defintion inside un-bracketed namespace"
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
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 81 85 111 83 442 446 443 248 256 180 182 443 254 247 244 123 242 279 444 140 515 505 511 474 426 359 159 141 139 515 505 511 356 494 154 141 139 444 179 102 108 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace {\n    function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion inside bracketed namespace"
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
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 81 85 84 442 446 443 248 256 180 182 443 254 247 244 123 242 279 444 140 515 505 511 474 426 359 159 141 139 515 505 511 356 494 154 141 139 444 179 102 108 83 112 83)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\nnamespace myNamespace;\nclass MyClass\n{\n    private $var = 'abc';\n    public function myFunction($arg) {\n        $arg = 2;\n        return $arg;\n    }\n}\n"
   "Simple function defintion and property inside class inside non-bracketed namespace"
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
            (car (cdr production)))))
       (message "\n")
       (should
        (equal
         '(84 81 85 111 83 198 202 443 297 332 328 324 256 472 479 426 443 340 338 298 302 296 330 328 327 442 446 81 443 248 256 180 182 443 254 247 244 123 242 279 444 140 515 505 511 474 426 359 159 141 139 515 505 511 356 494 154 141 139 323 444 300 302 296 185 103 108 83)
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
