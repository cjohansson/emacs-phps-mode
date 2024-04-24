;;; phps-mode-test-lexer.el --- Tests for lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make lexer-test


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(defun phps-mode-test-lexer--script-boundaries ()
  "Run test for lexer."

  (phps-mode-test--with-buffer
   "<?php\t$öar=1; exit;\t?>"
   "Simple PHP with two expression"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 11 . 12) (T_LNUMBER 12 . 13) (";" 13 . 14) (T_EXIT 15 . 19) (";" 19 . 20) (T_CLOSE_TAG 21 . 23)))))

  (phps-mode-test--with-buffer
   "<?php\nexit;\n?>"
   "Minimal PHP expression"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode-test--with-buffer
   "<?php exit; ?>"
   "Small PHP file"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode-test--with-buffer
   "<html><head>blabla</head<body>\n\n \t<?php\nexit;\n?>\n\n</body></html>"
   "Mixed inline HTML and PHP"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_INLINE_HTML 1 . 35) (T_OPEN_TAG 35 . 41) (T_EXIT 41 . 45) (";" 45 . 46) (T_CLOSE_TAG 47 . 49) (T_INLINE_HTML 49 . 65)))))

  (phps-mode-test--with-buffer
   "\n\n \t<html><title>echo \"Blahs\";</title><?php\n\n\nexit; ?>\n\n<html><random /></html><?php exit; ?>"
   "Another mixed inline HTML and PHP"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_INLINE_HTML 1 . 39) (T_OPEN_TAG 39 . 45) (T_EXIT 47 . 51) (";" 51 . 52) (T_CLOSE_TAG 53 . 55) (T_INLINE_HTML 55 . 80) (T_OPEN_TAG 80 . 86) (T_EXIT 86 . 90) (";" 90 . 91) (T_CLOSE_TAG 92 . 94)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$k = 'key';\n\necho \"\\$a['{$k}']\";"
   "A tricky case where variable inside double quote is escaped"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should (equal
            phps-mode-lex-analyzer--tokens
            '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 10) ("=" 11 . 12) (T_CONSTANT_ENCAPSED_STRING 13 . 18) (";" 18 . 19) (T_ECHO 21 . 25) ("\"" 26 . 27) (T_ENCAPSED_AND_WHITESPACE 27 . 32) (T_CURLY_OPEN 32 . 33) (T_VARIABLE 33 . 35) ("}" 35 . 36) (T_ENCAPSED_AND_WHITESPACE 36 . 38) ("\"" 38 . 39) (";" 39 . 40)))))

  (phps-mode-test--with-buffer
   "<?php echo \"\\\\\\\"\";"
   "Another tricky case where escape character is escaped"
   (should (equal
            phps-mode-lex-analyzer--tokens
            '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_CONSTANT_ENCAPSED_STRING 12 . 18) (";" 18 . 19)))))

  )

(defun phps-mode-test-lexer--simple-tokens ()
  "Run test for simple tokens."

  (phps-mode-test--with-buffer
   "<?php echo $vür = array('');"
   "Simple PHP via array declaration"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode-test--with-buffer
   "<?php if (empty($parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME])) { $parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME] = ''; }"
   "Complex PHP with conditional"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_EMPTY 11 . 16) ("(" 16 . 17) (T_VARIABLE 17 . 28) ("[" 28 . 29) (T_STRING 29 . 33) (T_PAAMAYIM_NEKUDOTAYIM 33 . 35) (T_STRING 35 . 76) ("]" 76 . 77) (")" 77 . 78) (")" 78 . 79) ("{" 80 . 81) (T_VARIABLE 82 . 93) ("[" 93 . 94) (T_STRING 94 . 98) (T_PAAMAYIM_NEKUDOTAYIM 98 . 100) (T_STRING 100 . 141) ("]" 141 . 142) ("=" 143 . 144) (T_CONSTANT_ENCAPSED_STRING 145 . 147) (";" 147 . 148) ("}" 149 . 150)))))

  (phps-mode-test--with-buffer
   "<?php echo $var = array(\"\");"
   "Simple PHP with empty array assignment"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode-test--with-buffer
   "<?php echo $var = array('abc' => '123');"
   "Simple PHP with associative array assignment"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (T_DOUBLE_ARROW 31 . 33) (T_CONSTANT_ENCAPSED_STRING 34 . 39) (")" 39 . 40) (";" 40 . 41)))))

  (phps-mode-test--with-buffer
   "<?php $var = []; "
   "PHP with short-handed array declaration assignment"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) ("[" 14 . 15) ("]" 15 . 16) (";" 16 . 17)))))

  (phps-mode-test--with-buffer
   "<?php $var = ''; $var = 'abc'; "
   "PHP with string assignments"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31)))))

  (phps-mode-test--with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case PHP"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_SWITCH 7 . 13) ("(" 14 . 15) (T_STRING 15 . 32) ("(" 32 . 33) (")" 33 . 34) (")" 34 . 35) ("{" 36 . 37) (T_CASE 38 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 59) (":" 59 . 60) (T_ECHO 61 . 65) (T_CONSTANT_ENCAPSED_STRING 66 . 87) (";" 87 . 88) ("}" 89 . 90)))))

  (phps-mode-test--with-buffer
   "<?php $var = \"\"; $var = \"abc\"; $var = \"abc\\def\\ghj\";"
   "PHP with three string assignments"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31) (T_VARIABLE 32 . 36) ("=" 37 . 38) (T_CONSTANT_ENCAPSED_STRING 39 . 52) (";" 52 . 53)))))

  (phps-mode-test--with-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no'; "
   "PHP with short-handed conditional echo"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56)))))

  (phps-mode-test--with-buffer
   "<?php myFunction(); "
   "A single function call"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 17) ("(" 17 . 18) (")" 18 . 19) (";" 19 . 20)))))

  (phps-mode-test--with-buffer
   "<?php // echo 'random';?><!--</div>-->"
   "Commented out PHP expression and inline-html"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 24) (T_CLOSE_TAG 24 . 26) (T_INLINE_HTML 26 . 39)))))

  (phps-mode-test--with-buffer
   "<?php //echo $contact_position;?><!--</div>-->"
   "Commented out PHP expression and inline-html 2"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 32) (T_CLOSE_TAG 32 . 34) (T_INLINE_HTML 34 . 47)))))

  (phps-mode-test--with-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no';\n//a random comment\n// another random comment\n/**\n * More comments\n* More\n **/\necho $backtrace; ?>"
   "Conditional echo, comment and doc-comment block"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56) (T_COMMENT 57 . 75) (T_COMMENT 76 . 101) (T_DOC_COMMENT 102 . 134) (T_ECHO 135 . 139) (T_VARIABLE 140 . 150) (";" 150 . 151) (T_CLOSE_TAG 152 . 154)))))

  (phps-mode-test--with-buffer
   "<?php forgerarray($arg1, $arg2);"
   "A function call containing keywords in its name"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 18) ("(" 18 . 19) (T_VARIABLE 19 . 24) ("," 24 . 25) (T_VARIABLE 26 . 31) (")" 31 . 32) (";" 32 . 33)))))

  (phps-mode-test--with-buffer
   "<?php\n$username = $_GET['user'] ?? 'nobody';\n$this->request->data['comments']['user_id'] ??= 'value';\n"
   "Coalescing comparison operator and coalescing assignment operator"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 16) ("=" 17 . 18) (T_VARIABLE 19 . 24) ("[" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 31) ("]" 31 . 32) (T_COALESCE 33 . 35) (T_CONSTANT_ENCAPSED_STRING 36 . 44) (";" 44 . 45) (T_VARIABLE 46 . 51) (T_OBJECT_OPERATOR 51 . 53) (T_STRING 53 . 60) (T_OBJECT_OPERATOR 60 . 62) (T_STRING 62 . 66) ("[" 66 . 67) (T_CONSTANT_ENCAPSED_STRING 67 . 77) ("]" 77 . 78) ("[" 78 . 79) (T_CONSTANT_ENCAPSED_STRING 79 . 88) ("]" 88 . 89) (T_COALESCE_EQUAL 90 . 93) (T_CONSTANT_ENCAPSED_STRING 94 . 101) (";" 101 . 102)))))

  (phps-mode-test--with-buffer
   "<?php\necho $array['abc'];\necho \"My $array[12] random statement\";\n"
   "Long inside array offset"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 18) ("[" 18 . 19) (T_CONSTANT_ENCAPSED_STRING 19 . 24) ("]" 24 . 25) (";" 25 . 26) (T_ECHO 27 . 31) ("\"" 32 . 33) (T_ENCAPSED_AND_WHITESPACE 33 . 36) (T_VARIABLE 36 . 42) ("[" 42 . 43) (T_NUM_STRING 43 . 45) ("]" 45 . 46) (T_ENCAPSED_AND_WHITESPACE 46 . 63) ("\"" 63 . 64) (";" 64 . 65)))))

  (phps-mode-test--with-buffer
   "<?php\n/*my comment */\n/** my doc comment */"
   "Comment vs doc-comment"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 22) (T_DOC_COMMENT 23 . 44)))))

  ;; https://www.php.net/releases/8.0/en.php
  (phps-mode-test--with-buffer
   "<?php\nhtmlspecialchars($string, double_encode: false);"
   "PHP 8.0 Named arguments"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 23) ("(" 23 . 24) (T_VARIABLE 24 . 31) ("," 31 . 32) (T_STRING 33 . 46) (":" 46 . 47) (T_STRING 48 . 53) (")" 53 . 54) (";" 54 . 55)))))

  (phps-mode-test--with-buffer
   "<?php\nclass PostsController\n{\n    #[Route(\"/api/posts/{id}\", methods: [\"GET\"])]\n    public function get($id) { /* ... */ }\n}"
   "PHP 8.0 Attributes"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 28) ("{" 29 . 30) (T_ATTRIBUTE 35 . 37) (T_STRING 37 . 42) ("(" 42 . 43) (T_CONSTANT_ENCAPSED_STRING 43 . 60) ("," 60 . 61) (T_STRING 62 . 69) (":" 69 . 70) ("[" 71 . 72) (T_CONSTANT_ENCAPSED_STRING 72 . 77) ("]" 77 . 78) (")" 78 . 79) ("]" 79 . 80) (T_PUBLIC 85 . 91) (T_FUNCTION 92 . 100) (T_STRING 101 . 104) ("(" 104 . 105) (T_VARIABLE 105 . 108) (")" 108 . 109) ("{" 110 . 111) (T_COMMENT 112 . 121) ("}" 122 . 123) ("}" 124 . 125)))))
  
  (phps-mode-test--with-buffer
   "<?php\nclass Point {\n  public function __construct(\n    public float $x = 0.0,\n    public float $y = 0.0,\n    public float $z = 0.0,\n  ) {}\n}"
   "PHP 8.0 Constructor property promotion"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 18) ("{" 19 . 20) (T_PUBLIC 23 . 29) (T_FUNCTION 30 . 38) (T_STRING 39 . 50) ("(" 50 . 51) (T_PUBLIC 56 . 62) (T_STRING 63 . 68) (T_VARIABLE 69 . 71) ("=" 72 . 73) (T_DNUMBER 74 . 77) ("," 77 . 78) (T_PUBLIC 83 . 89) (T_STRING 90 . 95) (T_VARIABLE 96 . 98) ("=" 99 . 100) (T_DNUMBER 101 . 104) ("," 104 . 105) (T_PUBLIC 110 . 116) (T_STRING 117 . 122) (T_VARIABLE 123 . 125) ("=" 126 . 127) (T_DNUMBER 128 . 131) ("," 131 . 132) (")" 135 . 136) ("{" 137 . 138) ("}" 138 . 139) ("}" 140 . 141)))))

  (phps-mode-test--with-buffer
   "<?php\nclass Number {\n  public function __construct(\n    private int|float $number\n  ) {}\n}\n\nnew Number('NaN'); // TypeError"
   "PHP 8.0 Union types"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 19) ("{" 20 . 21) (T_PUBLIC 24 . 30) (T_FUNCTION 31 . 39) (T_STRING 40 . 51) ("(" 51 . 52) (T_PRIVATE 57 . 64) (T_STRING 65 . 68) ("|" 68 . 69) (T_STRING 69 . 74) (T_VARIABLE 75 . 82) (")" 85 . 86) ("{" 87 . 88) ("}" 88 . 89) ("}" 90 . 91) (T_NEW 93 . 96) (T_STRING 97 . 103) ("(" 103 . 104) (T_CONSTANT_ENCAPSED_STRING 104 . 109) (")" 109 . 110) (";" 110 . 111) (T_COMMENT 112 . 124)))))

  (phps-mode-test--with-buffer
   "<?php\necho match (8.0) {\n  '8.0' => \"Oh no!\",\n  8.0 => \"This is what I expected\",\n};\n//> This is what I expected"
   "PHP 8.0 Match expression"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_MATCH 12 . 17) ("(" 18 . 19) (T_DNUMBER 19 . 22) (")" 22 . 23) ("{" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 28 . 33) (T_DOUBLE_ARROW 34 . 36) (T_CONSTANT_ENCAPSED_STRING 37 . 45) ("," 45 . 46) (T_DNUMBER 49 . 52) (T_DOUBLE_ARROW 53 . 55) (T_CONSTANT_ENCAPSED_STRING 56 . 81) ("," 81 . 82) ("}" 83 . 84) (";" 84 . 85) (T_COMMENT 86 . 113)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$country = $session?->user?->getAddress()?->country;\n"
   "PHP 8.0 Nullsafe operator"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 16) ("=" 17 . 18) (T_VARIABLE 19 . 27) (T_NULLSAFE_OBJECT_OPERATOR 27 . 30) (T_STRING 30 . 34) (T_NULLSAFE_OBJECT_OPERATOR 34 . 37) (T_STRING 37 . 47) ("(" 47 . 48) (")" 48 . 49) (T_NULLSAFE_OBJECT_OPERATOR 49 . 52) (T_STRING 52 . 59) (";" 59 . 60)))))

  (phps-mode-test--with-buffer
   "<?php\n\n0 == 'foobar'; // false\n"
   "PHP 8.0 Saner string to number comparisons"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_LNUMBER 8 . 9) (T_IS_EQUAL 10 . 12) (T_CONSTANT_ENCAPSED_STRING 13 . 21) (";" 21 . 22) (T_COMMENT 23 . 31)))))

  (phps-mode-test--with-buffer
   "<?php\n\nstrlen([]); // TypeError: strlen(): Argument #1 ($str) must be of type string, array given\n\narray_chunk([], -1); // ValueError: array_chunk(): Argument #2 ($length) must be greater than 0\n"
   "PHP 8.0 Consistent type errors for internal functions"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 8 . 14) ("(" 14 . 15) ("[" 15 . 16) ("]" 16 . 17) (")" 17 . 18) (";" 18 . 19) (T_COMMENT 20 . 98) (T_STRING 100 . 111) ("(" 111 . 112) ("[" 112 . 113) ("]" 113 . 114) ("," 114 . 115) ("-" 116 . 117) (T_LNUMBER 117 . 118) (")" 118 . 119) (";" 119 . 120) (T_COMMENT 121 . 195)))))

  (phps-mode-test--with-buffer
   "<?php\nFoo;\n// Before: T_STRING\n// After:  T_STRING\n// Rule:   {LABEL}\n \nFoo\\Bar;\n// Before: T_STRING T_NS_SEPARATOR T_STRING\n// After:  T_NAME_QUALIFIED\n// Rule:   {LABEL}(\"\\\"{LABEL})+\n \n\\Foo;\n// Before: T_NS_SEPARATOR T_STRING\n// After:  T_NAME_FULLY_QUALIFIED\n// Rule:   (\"\\\"{LABEL})+\n \nnamespace\\Foo;\n// Before: T_NAMESPACE T_NS_SEPARATOR T_STRING\n// After:  T_NAME_RELATIVE\n// Rule:   \"namespace\"(\"\\\"{LABEL})+"
   "PHP 8.0 Treat namespaced names as single token"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 10) (";" 10 . 11) (T_COMMENT 12 . 31) (T_COMMENT 32 . 51) (T_COMMENT 52 . 70) (T_NAME_QUALIFIED 73 . 80) (";" 80 . 81) (T_COMMENT 82 . 125) (T_COMMENT 126 . 153) (T_COMMENT 154 . 185) (T_NAME_FULLY_QUALIFIED 188 . 192) (";" 192 . 193) (T_COMMENT 194 . 228) (T_COMMENT 229 . 262) (T_COMMENT 263 . 287) (T_NAME_RELATIVE 290 . 303) (";" 303 . 304) (T_COMMENT 305 . 351) (T_COMMENT 352 . 378) (T_COMMENT 379 . 414)))))

  (phps-mode-test--with-buffer
   "<?php\nenum Suit\n{\n    case Hearts;\n    case Diamonds;\n    case Clubs;\n    case Spades;\n}"
   "Basic Enumerations"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ENUM 7 . 11) (T_STRING 12 . 16) ("{" 17 . 18) (T_CASE 23 . 27) (T_STRING 28 . 34) (";" 34 . 35) (T_CASE 40 . 44) (T_STRING 45 . 53) (";" 53 . 54) (T_CASE 59 . 63) (T_STRING 64 . 69) (";" 69 . 70) (T_CASE 75 . 79) (T_STRING 80 . 86) (";" 86 . 87) ("}" 88 . 89)))))

  (phps-mode-test--with-buffer
   "<?php\n\n\nclass MyClass\n{\n    public function __construct(private readonly type $propertyName)\n    {\n    }\n}"
   "Read-only auto-injected properties"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 9 . 14) (T_STRING 15 . 22) ("{" 23 . 24) (T_PUBLIC 29 . 35) (T_FUNCTION 36 . 44) (T_STRING 45 . 56) ("(" 56 . 57) (T_PRIVATE 57 . 64) (T_READONLY 65 . 73) (T_STRING 74 . 78) (T_VARIABLE 79 . 92) (")" 92 . 93) ("{" 98 . 99) ("}" 104 . 105) ("}" 106 . 107)))))

  (phps-mode-test--with-buffer
   "<?php\nclass User {\n    public readonly int $uid;\n\n    public function __construct(int $uid) {\n        $this->uid = $uid;\n    }\n}"
   "Read-only Properties"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 17) ("{" 18 . 19) (T_PUBLIC 24 . 30) (T_READONLY 31 . 39) (T_STRING 40 . 43) (T_VARIABLE 44 . 48) (";" 48 . 49) (T_PUBLIC 55 . 61) (T_FUNCTION 62 . 70) (T_STRING 71 . 82) ("(" 82 . 83) (T_STRING 83 . 86) (T_VARIABLE 87 . 91) (")" 91 . 92) ("{" 93 . 94) (T_VARIABLE 103 . 108) (T_OBJECT_OPERATOR 108 . 110) (T_STRING 110 . 113) ("=" 114 . 115) (T_VARIABLE 116 . 120) (";" 120 . 121) ("}" 126 . 127) ("}" 128 . 129)))))

  (phps-mode-test--with-buffer
   "<?php\n$a = 1234; // decimal number\n$a = 0123; // octal number (equivalent to 83 decimal)\n$a = 0o123; // octal number (as of PHP 8.1.0)\n$a = 0x1A; // hexadecimal number (equivalent to 26 decimal)\n$a = 0b11111111; // binary number (equivalent to 255 decimal)\n$a = 1_234_567; // decimal number (as of PHP 7.4.0)\n?>\n"
   "Integers with underscores"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 9) ("=" 10 . 11) (T_LNUMBER 12 . 16) (";" 16 . 17) (T_COMMENT 18 . 35) (T_VARIABLE 36 . 38) ("=" 39 . 40) (T_LNUMBER 41 . 45) (";" 45 . 46) (T_COMMENT 47 . 89) (T_VARIABLE 90 . 92) ("=" 93 . 94) (T_LNUMBER 95 . 100) (";" 100 . 101) (T_COMMENT 102 . 135) (T_VARIABLE 136 . 138) ("=" 139 . 140) (T_LNUMBER 141 . 145) (";" 145 . 146) (T_COMMENT 147 . 195) (T_VARIABLE 196 . 198) ("=" 199 . 200) (T_LNUMBER 201 . 211) (";" 211 . 212) (T_COMMENT 213 . 257) (T_VARIABLE 258 . 260) ("=" 261 . 262) (T_LNUMBER 263 . 272) (";" 272 . 273) (T_COMMENT 274 . 309) (T_CLOSE_TAG 310 . 312) (T_INLINE_HTML 312 . 313)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$b = 0.3;\n\n$a = 0.;\n\n"
   "Double with and without decimals"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 10) ("=" 11 . 12) (T_DNUMBER 13 . 16) (";" 16 . 17) (T_VARIABLE 19 . 21) ("=" 22 . 23) (T_DNUMBER 24 . 26) (";" 26 . 27))
     )))

  (phps-mode-test--with-buffer
   "<?php\n\n$a = 3 ** 2;\n$a **= 2;"
   "Exponentiation and assignment exponentiation"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 10) ("=" 11 . 12) (T_LNUMBER 13 . 14) (T_POW 15 . 17) (T_LNUMBER 18 . 19) (";" 19 . 20) (T_VARIABLE 21 . 23) (T_POW_EQUAL 24 . 27) (T_LNUMBER 28 . 29) (";" 29 . 30))
     )))

  )

(defun phps-mode-test-lexer--complex-tokens ()
  "Run test for complex tokens."

  (phps-mode-test--with-buffer
   "<?php $var->property;"
   "Object property"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) (T_OBJECT_OPERATOR 11 . 13) (T_STRING 13 . 21) (";" 21 . 22)))))

  (phps-mode-test--with-buffer
   "<?php echo \"My $variable is here\"; echo \"you know\";"
   "Double quoted strings with variables"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_VARIABLE 16 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 33) ("\"" 33 . 34) (";" 34 . 35) (T_ECHO 36 . 40) (T_CONSTANT_ENCAPSED_STRING 41 . 51) (";" 51 . 52)))))

  (phps-mode-test--with-buffer
   "<?php echo \"My ${variable} is here 1\";"
   "Double quoted string with variable"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_DOLLAR_OPEN_CURLY_BRACES 16 . 18) (T_STRING_VARNAME 18 . 26) ("}" 26 . 27) (T_ENCAPSED_AND_WHITESPACE 27 . 37) ("\"" 37 . 38) (";" 38 . 39)))))

  (phps-mode-test--with-buffer
   "<?php echo \"Mine {$first_variable} is here and my $second is there.\";"
   "Another double quoted string with variable"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 18) (T_CURLY_OPEN 18 . 19) (T_VARIABLE 19 . 34) ("}" 34 . 35) (T_ENCAPSED_AND_WHITESPACE 35 . 51) (T_VARIABLE 51 . 58) (T_ENCAPSED_AND_WHITESPACE 58 . 68) ("\"" 68 . 69) (";" 69 . 70)))))

  (phps-mode-test--with-buffer
   "<?php echo \" Hello $variable[0], how are you?\";"
   "Simple interpolated string with indexed variable"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 20) (T_VARIABLE 20 . 29) ("[" 29 . 30) (T_NUM_STRING 30 . 31) ("]" 31 . 32) (T_ENCAPSED_AND_WHITESPACE 32 . 46) ("\"" 46 . 47) (";" 47 . 48)))))

  ;; HEREDOC

  (phps-mode-test--with-buffer
   "<?php\n// no indentation\necho <<<END\n      a\n     b\n    c\n\n\nEND;\n\n// 4 spaces of indentation\necho <<<END\n      a\n     b\n    c\n    END;\n"
   "Example #1 Basic Heredoc example as of PHP 7.3.0"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 24) (T_ECHO 25 . 29) (T_START_HEREDOC 30 . 37) (T_ENCAPSED_AND_WHITESPACE 37 . 60) (T_END_HEREDOC 60 . 63) (";" 63 . 64) (T_COMMENT 66 . 92) (T_ECHO 93 . 97) (T_START_HEREDOC 98 . 105) (T_ENCAPSED_AND_WHITESPACE 105 . 130) (T_END_HEREDOC 130 . 133) (";" 133 . 134)))))

  (phps-mode-test--with-buffer
   "<?php\necho <<<END\n  a\n b\nc\n   END;\n"
   "Example #2 Closing identifier must not be indented further than any lines of the body"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 19) (T_ENCAPSED_AND_WHITESPACE 19 . 31) (T_END_HEREDOC 31 . 34) (";" 34 . 35)))))

  (should-error
   (phps-mode-test--with-buffer
    "<?php\n// All the following code do not work.\n\n// different indentation for body (spaces) ending marker (tabs)\n{\n    echo <<<END\n     a\n        END;\n\n\n// mixing spaces and tabs in body\n{\n    echo <<<END\n        a\n     END;\n\n\n// mixing spaces and tabs in ending marker\n{\n    echo <<<END\n          a\n         END;\n\n"
    "Example #3 Different indentation for body (spaces) closing identifier"))
  (message "Passed tests for 'Example #3 Different indentation for body (spaces) closing identifier'\n")

  (phps-mode-test--with-buffer
   "<?php\n$values = [<<<END\na\n  b\n    c\nEND, 'd e f'];\nvar_dump($values);\n"
   "Example #4 Continuing an expression after a closing identifier"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 14) ("=" 15 . 16) ("[" 17 . 18) (T_START_HEREDOC 18 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 37) (T_END_HEREDOC 37 . 40) ("," 40 . 41) (T_CONSTANT_ENCAPSED_STRING 42 . 49) ("]" 49 . 50) (";" 50 . 51) (T_STRING 52 . 60) ("(" 60 . 61) (T_VARIABLE 61 . 68) (")" 68 . 69) (";" 69 . 70)))))

  (should-error
   (phps-mode-test--with-buffer
    "<?php\n$values = [<<<END\na\nb\nEND ING\nEND, 'd e f'];\n"
    "Example #5 Closing identifier in body of the string tends to cause ParseError"))
  (message "Passed tests for 'Example #5 Closing identifier in body of the string tends to cause ParseError'\n")

  (phps-mode-test--with-buffer
   "<?php\nclass foo {\n    public $bar = <<<EOT\nbar\n    EOT;\n}\n// Identifier must not be indented\n?>\n"
   "Example #6 Invalid example, prior to PHP 7.3.0"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 16) ("{" 17 . 18) (T_PUBLIC 23 . 29) (T_VARIABLE 30 . 34) ("=" 35 . 36) (T_START_HEREDOC 37 . 44) (T_ENCAPSED_AND_WHITESPACE 44 . 52) (T_END_HEREDOC 52 . 55) (";" 55 . 56) ("}" 57 . 58) (T_COMMENT 59 . 93) (T_CLOSE_TAG 94 . 96) (T_INLINE_HTML 96 . 97)))))

  (phps-mode-test--with-buffer
   "<?php\nclass foo {\n    public $bar = <<<EOT\nbar\nEOT;\n}\n?>\n"
   "Example #7 Valid example, even if prior to PHP 7.3.0"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 16) ("{" 17 . 18) (T_PUBLIC 23 . 29) (T_VARIABLE 30 . 34) ("=" 35 . 36) (T_START_HEREDOC 37 . 44) (T_ENCAPSED_AND_WHITESPACE 44 . 48) (T_END_HEREDOC 48 . 51) (";" 51 . 52) ("}" 53 . 54) (T_CLOSE_TAG 55 . 57) (T_INLINE_HTML 57 . 58)))))

  (phps-mode-test--with-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n\n/* More complex example, with variables. */\nclass foo\n{\n    var $foo;\n    var $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<EOT\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should print a capital 'A': \x41\nEOT;\n?>\n"
   "Example #8 Heredoc string quoting example"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_START_HEREDOC 14 . 21) (T_ENCAPSED_AND_WHITESPACE 21 . 85) (T_END_HEREDOC 85 . 88) (";" 88 . 89) (T_COMMENT 91 . 134) (T_CLASS 135 . 140) (T_STRING 141 . 144) ("{" 145 . 146) (T_VAR 151 . 154) (T_VARIABLE 155 . 159) (";" 159 . 160) (T_VAR 165 . 168) (T_VARIABLE 169 . 173) (";" 173 . 174) (T_FUNCTION 180 . 188) (T_STRING 189 . 200) ("(" 200 . 201) (")" 201 . 202) ("{" 207 . 208) (T_VARIABLE 217 . 222) (T_OBJECT_OPERATOR 222 . 224) (T_STRING 224 . 227) ("=" 228 . 229) (T_CONSTANT_ENCAPSED_STRING 230 . 235) (";" 235 . 236) (T_VARIABLE 245 . 250) (T_OBJECT_OPERATOR 250 . 252) (T_STRING 252 . 255) ("=" 256 . 257) (T_ARRAY 258 . 263) ("(" 263 . 264) (T_CONSTANT_ENCAPSED_STRING 264 . 270) ("," 270 . 271) (T_CONSTANT_ENCAPSED_STRING 272 . 278) ("," 278 . 279) (T_CONSTANT_ENCAPSED_STRING 280 . 286) (")" 286 . 287) (";" 287 . 288) ("}" 293 . 294) ("}" 295 . 296) (T_VARIABLE 298 . 302) ("=" 303 . 304) (T_NEW 305 . 308) (T_STRING 309 . 312) ("(" 312 . 313) (")" 313 . 314) (";" 314 . 315) (T_VARIABLE 316 . 321) ("=" 322 . 323) (T_CONSTANT_ENCAPSED_STRING 324 . 332) (";" 332 . 333) (T_ECHO 335 . 339) (T_START_HEREDOC 340 . 347) (T_ENCAPSED_AND_WHITESPACE 347 . 359) (T_VARIABLE 359 . 364) (T_ENCAPSED_AND_WHITESPACE 364 . 386) (T_VARIABLE 386 . 390) (T_OBJECT_OPERATOR 390 . 392) (T_STRING 392 . 395) (T_ENCAPSED_AND_WHITESPACE 395 . 421) (T_CURLY_OPEN 421 . 422) (T_VARIABLE 422 . 426) (T_OBJECT_OPERATOR 426 . 428) (T_STRING 428 . 431) ("[" 431 . 432) (T_LNUMBER 432 . 433) ("]" 433 . 434) ("}" 434 . 435) (T_ENCAPSED_AND_WHITESPACE 435 . 472) (T_END_HEREDOC 472 . 475) (";" 475 . 476) (T_CLOSE_TAG 477 . 479) (T_INLINE_HTML 479 . 480)))))

  (phps-mode-test--with-buffer
   "<?php\nvar_dump(array(<<<EOD\nfoobar!\nEOD\n));\n?>\n"
   "Example #9 Heredoc in arguments example"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 15) ("(" 15 . 16) (T_ARRAY 16 . 21) ("(" 21 . 22) (T_START_HEREDOC 22 . 29) (T_ENCAPSED_AND_WHITESPACE 29 . 37) (T_END_HEREDOC 37 . 40) (")" 41 . 42) (")" 42 . 43) (";" 43 . 44) (T_CLOSE_TAG 45 . 47) (T_INLINE_HTML 47 . 48)))))

  (phps-mode-test--with-buffer
   "<?php\n// Static variables\nfunction foo()\n{\n    static $bar = <<<LABEL\nNothing in here...\nLABEL;\n}\n\n// Class properties/constants\nclass foo\n{\n    const BAR = <<<FOOBAR\nConstant example\nFOOBAR;\n\n    public $baz = <<<FOOBAR\nProperty example\nFOOBAR;\n}\n?>\n"
   "Example #10 Using Heredoc to initialize static values"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 26) (T_FUNCTION 27 . 35) (T_STRING 36 . 39) ("(" 39 . 40) (")" 40 . 41) ("{" 42 . 43) (T_STATIC 48 . 54) (T_VARIABLE 55 . 59) ("=" 60 . 61) (T_START_HEREDOC 62 . 71) (T_ENCAPSED_AND_WHITESPACE 71 . 90) (T_END_HEREDOC 90 . 95) (";" 95 . 96) ("}" 97 . 98) (T_COMMENT 100 . 129) (T_CLASS 130 . 135) (T_STRING 136 . 139) ("{" 140 . 141) (T_CONST 146 . 151) (T_STRING 152 . 155) ("=" 156 . 157) (T_START_HEREDOC 158 . 168) (T_ENCAPSED_AND_WHITESPACE 168 . 185) (T_END_HEREDOC 185 . 191) (";" 191 . 192) (T_PUBLIC 198 . 204) (T_VARIABLE 205 . 209) ("=" 210 . 211) (T_START_HEREDOC 212 . 222) (T_ENCAPSED_AND_WHITESPACE 222 . 239) (T_END_HEREDOC 239 . 245) (";" 245 . 246) ("}" 247 . 248) (T_CLOSE_TAG 249 . 251) (T_INLINE_HTML 251 . 252)))))

  (phps-mode-test--with-buffer
   "\n<?php\necho <<<\"FOOBAR\"\nHello World!\nFOOBAR;\n?>\n"
   "Example #11 Using double quotes in Heredoc"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_INLINE_HTML 1 . 2) (T_OPEN_TAG 2 . 8) (T_ECHO 8 . 12) (T_START_HEREDOC 13 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 38) (T_END_HEREDOC 38 . 44) (";" 44 . 45) (T_CLOSE_TAG 46 . 48) (T_INLINE_HTML 48 . 49)))))

  (phps-mode-test--with-buffer
   "<?php\n$var = <<<QUERY\n    {\n        shop {\n            name\n        }\n    }\n    QUERY;\n"
   "Another HEREDOC example"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_START_HEREDOC 14 . 23) (T_ENCAPSED_AND_WHITESPACE 23 . 81) (T_END_HEREDOC 81 . 86) (";" 86 . 87)))))


  ;; NOWDOC

  (phps-mode-test--with-buffer
   "<?php\necho <<<'EOD'\nExample of string spanning multiple lines\nusing nowdoc syntax. Backslashes are always treated literally,\ne.g. \\ and \\'.\nEOD;\n"
   "Example #12 Nowdoc string quoting example"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 21) (T_ENCAPSED_AND_WHITESPACE 21 . 141) (T_END_HEREDOC 141 . 144) (";" 144 . 145)))))

  (phps-mode-test--with-buffer
   "<?php\nclass foo\n{\n    public $foo;\n    public $bar;\n\n    function __construct()\n    {\n        $this->foo = 'Foo';\n        $this->bar = array('Bar1', 'Bar2', 'Bar3');\n    }\n}\n\n$foo = new foo();\n$name = 'MyName';\n\necho <<<'EOT'\nMy name is \"$name\". I am printing some $foo->foo.\nNow, I am printing some {$foo->bar[1]}.\nThis should not print a capital 'A': \x41\nEOT;\n?>\n"
   "Example #13 Nowdoc string quoting example with variables"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 16) ("{" 17 . 18) (T_PUBLIC 23 . 29) (T_VARIABLE 30 . 34) (";" 34 . 35) (T_PUBLIC 40 . 46) (T_VARIABLE 47 . 51) (";" 51 . 52) (T_FUNCTION 58 . 66) (T_STRING 67 . 78) ("(" 78 . 79) (")" 79 . 80) ("{" 85 . 86) (T_VARIABLE 95 . 100) (T_OBJECT_OPERATOR 100 . 102) (T_STRING 102 . 105) ("=" 106 . 107) (T_CONSTANT_ENCAPSED_STRING 108 . 113) (";" 113 . 114) (T_VARIABLE 123 . 128) (T_OBJECT_OPERATOR 128 . 130) (T_STRING 130 . 133) ("=" 134 . 135) (T_ARRAY 136 . 141) ("(" 141 . 142) (T_CONSTANT_ENCAPSED_STRING 142 . 148) ("," 148 . 149) (T_CONSTANT_ENCAPSED_STRING 150 . 156) ("," 156 . 157) (T_CONSTANT_ENCAPSED_STRING 158 . 164) (")" 164 . 165) (";" 165 . 166) ("}" 171 . 172) ("}" 173 . 174) (T_VARIABLE 176 . 180) ("=" 181 . 182) (T_NEW 183 . 186) (T_STRING 187 . 190) ("(" 190 . 191) (")" 191 . 192) (";" 192 . 193) (T_VARIABLE 194 . 199) ("=" 200 . 201) (T_CONSTANT_ENCAPSED_STRING 202 . 210) (";" 210 . 211) (T_ECHO 213 . 217) (T_START_HEREDOC 218 . 227) (T_ENCAPSED_AND_WHITESPACE 227 . 356) (T_END_HEREDOC 356 . 359) (";" 359 . 360) (T_CLOSE_TAG 361 . 363) (T_INLINE_HTML 363 . 364)))))

  (phps-mode-test--with-buffer
   "<?php\nclass foo {\n    public $bar = <<<'EOT'\nbar\nEOT;\n}\n?>\n"
   "Example #14 Static data example"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 16) ("{" 17 . 18) (T_PUBLIC 23 . 29) (T_VARIABLE 30 . 34) ("=" 35 . 36) (T_START_HEREDOC 37 . 46) (T_ENCAPSED_AND_WHITESPACE 46 . 50) (T_END_HEREDOC 50 . 53) (";" 53 . 54) ("}" 55 . 56) (T_CLOSE_TAG 57 . 59) (T_INLINE_HTML 59 . 60)))))

  (phps-mode-test--with-buffer
   "<?php\n\nclass MyClass\n{\n    public const MY_CONSTANT = <<<'DELIMITER'\n    {\n        some {\n            json\n        }\n    }\n    DELIMITER;\n}\n"
   "Nowdoc where ending delimiter is not first on line (PHP > 7.3)"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 8 . 13) (T_STRING 14 . 21) ("{" 22 . 23) (T_PUBLIC 28 . 34) (T_CONST 35 . 40) (T_STRING 41 . 52) ("=" 53 . 54) (T_START_HEREDOC 55 . 70) (T_ENCAPSED_AND_WHITESPACE 70 . 128) (T_END_HEREDOC 128 . 137) (";" 137 . 138) ("}" 139 . 140)))))

  ;; Backquotes
  (phps-mode-test--with-buffer
   "<?php `echo \"HELLO\"`;"
   "Backquote basic test"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_ENCAPSED_AND_WHITESPACE 8 . 20) ("`" 20 . 21) (";" 21 . 22)))))

  (phps-mode-test--with-buffer
   "<?php `echo \"HELLO $variable or {$variable2} or ${variable3} or $variable[index][0] here\"`;"
   "Double quoted strings with mixed variables"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_ENCAPSED_AND_WHITESPACE 8 . 20) (T_VARIABLE 20 . 29) (T_ENCAPSED_AND_WHITESPACE 29 . 33) (T_CURLY_OPEN 33 . 34) (T_VARIABLE 34 . 44) ("}" 44 . 45) (T_ENCAPSED_AND_WHITESPACE 45 . 49) (T_DOLLAR_OPEN_CURLY_BRACES 49 . 51) (T_STRING_VARNAME 51 . 60) ("}" 60 . 61) (T_ENCAPSED_AND_WHITESPACE 61 . 65) (T_VARIABLE 65 . 74) ("[" 74 . 75) (T_STRING 75 . 80) ("]" 80 . 81) (T_ENCAPSED_AND_WHITESPACE 81 . 90) ("`" 90 . 91) (";" 91 . 92)))))

  (phps-mode-test--with-buffer
   "<?php $wpdb->posts; ?>"
   "Wordpress example object operator"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 19) (";" 19 . 20) (T_CLOSE_TAG 21 . 23)))))

  (phps-mode-test--with-buffer
   "<?php $var = \"SELECT post_parent FROM $wpdb->posts WHERE ID = '\".$id.\"'\"; ?>"
   "Wordpress example SQL query in double-quoted string with variables"
   ;; (message "Tokens 1: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) ("\"" 14 . 15) (T_ENCAPSED_AND_WHITESPACE 15 . 39) (T_VARIABLE 39 . 44) (T_OBJECT_OPERATOR 44 . 46) (T_STRING 46 . 51) (T_ENCAPSED_AND_WHITESPACE 51 . 64) ("\"" 64 . 65) ("." 65 . 66) (T_VARIABLE 66 . 69) ("." 69 . 70) (T_CONSTANT_ENCAPSED_STRING 70 . 73) (";" 73 . 74) (T_CLOSE_TAG 75 . 77)))))

  (phps-mode-test--with-buffer
   "<?php $wpdb->get_var(\"SELECT post_parent FROM $wpdb->posts WHERE ID = '\".$id.\"'\"); ?>"
   "Wordpress example object operator with sql in double quotes with variable inside"
   ;; (message "Tokens 2: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 21) ("(" 21 . 22) ("\"" 22 . 23) (T_ENCAPSED_AND_WHITESPACE 23 . 47) (T_VARIABLE 47 . 52) (T_OBJECT_OPERATOR 52 . 54) (T_STRING 54 . 59) (T_ENCAPSED_AND_WHITESPACE 59 . 72) ("\"" 72 . 73) ("." 73 . 74) (T_VARIABLE 74 . 77) ("." 77 . 78) (T_CONSTANT_ENCAPSED_STRING 78 . 81) (")" 81 . 82) (";" 82 . 83) (T_CLOSE_TAG 84 . 86)))))

  (phps-mode-test--with-buffer
   "<?php $this->add($option['style']['selectors'], array('background' => \"{$value['color']} url('{$value['image']}')\"));"
   "Complex tokens with tokens inside double-quoted string"
   ;; (message "Tokens 2: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 25) ("[" 25 . 26) (T_CONSTANT_ENCAPSED_STRING 26 . 33) ("]" 33 . 34) ("[" 34 . 35) (T_CONSTANT_ENCAPSED_STRING 35 . 46) ("]" 46 . 47) ("," 47 . 48) (T_ARRAY 49 . 54) ("(" 54 . 55) (T_CONSTANT_ENCAPSED_STRING 55 . 67) (T_DOUBLE_ARROW 68 . 70) ("\"" 71 . 72) (T_CURLY_OPEN 72 . 73) (T_VARIABLE 73 . 79) ("[" 79 . 80) (T_CONSTANT_ENCAPSED_STRING 80 . 87) ("]" 87 . 88) ("}" 88 . 89) (T_ENCAPSED_AND_WHITESPACE 89 . 95) (T_CURLY_OPEN 95 . 96) (T_VARIABLE 96 . 102) ("[" 102 . 103) (T_CONSTANT_ENCAPSED_STRING 103 . 110) ("]" 110 . 111) ("}" 111 . 112) (T_ENCAPSED_AND_WHITESPACE 112 . 114) ("\"" 114 . 115) (")" 115 . 116) (")" 116 . 117) (";" 117 . 118)))))

  (phps-mode-test--with-buffer
   "<?php\n$var = <<<EOD\nrandom {$value['color']->property} again {$value->head()}; random\nEOD;\n"
   "Complex tokens with tokens inside HEREDOC string"
   ;; (message "Tokens 2: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_START_HEREDOC 14 . 21) (T_ENCAPSED_AND_WHITESPACE 21 . 28) (T_CURLY_OPEN 28 . 29) (T_VARIABLE 29 . 35) ("[" 35 . 36) (T_CONSTANT_ENCAPSED_STRING 36 . 43) ("]" 43 . 44) (T_OBJECT_OPERATOR 44 . 46) (T_STRING 46 . 54) ("}" 54 . 55) (T_ENCAPSED_AND_WHITESPACE 55 . 62) (T_CURLY_OPEN 62 . 63) (T_VARIABLE 63 . 69) (T_OBJECT_OPERATOR 69 . 71) (T_STRING 71 . 75) ("(" 75 . 76) (")" 76 . 77) ("}" 77 . 78) (T_ENCAPSED_AND_WHITESPACE 78 . 87) (T_END_HEREDOC 87 . 90) (";" 90 . 91)))))

  (phps-mode-test--with-buffer
   "<?php echo \"\\\"$string\\\"\";"
   "Escaped double quotes with variable in it"
   ;; (message "Tokens 2: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 15) (T_VARIABLE 15 . 22) (T_ENCAPSED_AND_WHITESPACE 22 . 24) ("\"" 24 . 25) (";" 25 . 26)))))

  (phps-mode-test--with-buffer
   "<?php $var = \"\\\\\";"
   "Double quoted string containing only two backslashes"
   ;; (message "Tokens 2: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 18) (";" 18 . 19)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$object = (object) array(\n    'field_random' => 25\n);\n$field = 'random';\necho $object->{\"field_$field\"};"
   "Dynamic object property"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 15) ("=" 16 . 17) (T_OBJECT_CAST 18 . 26) (T_ARRAY 27 . 32) ("(" 32 . 33) (T_CONSTANT_ENCAPSED_STRING 38 . 52) (T_DOUBLE_ARROW 53 . 55) (T_LNUMBER 56 . 58) (")" 59 . 60) (";" 60 . 61) (T_VARIABLE 62 . 68) ("=" 69 . 70) (T_CONSTANT_ENCAPSED_STRING 71 . 79) (";" 79 . 80) (T_ECHO 81 . 85) (T_VARIABLE 86 . 93) (T_OBJECT_OPERATOR 93 . 95) ("{" 95 . 96) ("\"" 96 . 97) (T_ENCAPSED_AND_WHITESPACE 97 . 103) (T_VARIABLE 103 . 109) ("\"" 109 . 110) ("}" 110 . 111) (";" 111 . 112)))))

  (phps-mode-test--with-buffer
   "<?php\nclass MyClass { function myFunction() { return 'hello'; }}\n$class = new MyClass();\n$function = \"Function\";\necho $class->{\"my$function\"}();"
   "Dynamic object method"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_CLASS 7 . 12) (T_STRING 13 . 20) ("{" 21 . 22) (T_FUNCTION 23 . 31) (T_STRING 32 . 42) ("(" 42 . 43) (")" 43 . 44) ("{" 45 . 46) (T_RETURN 47 . 53) (T_CONSTANT_ENCAPSED_STRING 54 . 61) (";" 61 . 62) ("}" 63 . 64) ("}" 64 . 65) (T_VARIABLE 66 . 72) ("=" 73 . 74) (T_NEW 75 . 78) (T_STRING 79 . 86) ("(" 86 . 87) (")" 87 . 88) (";" 88 . 89) (T_VARIABLE 90 . 99) ("=" 100 . 101) (T_CONSTANT_ENCAPSED_STRING 102 . 112) (";" 112 . 113) (T_ECHO 114 . 118) (T_VARIABLE 119 . 125) (T_OBJECT_OPERATOR 125 . 127) ("{" 127 . 128) ("\"" 128 . 129) (T_ENCAPSED_AND_WHITESPACE 129 . 131) (T_VARIABLE 131 . 140) ("\"" 140 . 141) ("}" 141 . 142) ("(" 142 . 143) (")" 143 . 144) (";" 144 . 145)))))

  (phps-mode-test--with-buffer
   "<?php\n$product_path = \"${filename[0]}/${filename[1]}/\";\n    echo 'here';\n"
   "String with two dollar_open_curly_braces with indexes"
   ;; (message "Tokens: %s" phps-mode-lex-analyzer--tokens)
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 20) ("=" 21 . 22) ("\"" 23 . 24) (T_DOLLAR_OPEN_CURLY_BRACES 24 . 26) (T_STRING_VARNAME 26 . 34) ("[" 34 . 35) (T_LNUMBER 35 . 36) ("]" 36 . 37) ("}" 37 . 38) (T_ENCAPSED_AND_WHITESPACE 38 . 39) (T_DOLLAR_OPEN_CURLY_BRACES 39 . 41) (T_STRING_VARNAME 41 . 49) ("[" 49 . 50) (T_LNUMBER 50 . 51) ("]" 51 . 52) ("}" 52 . 53) (T_ENCAPSED_AND_WHITESPACE 53 . 54) ("\"" 54 . 55) (";" 55 . 56) (T_ECHO 61 . 65) (T_CONSTANT_ENCAPSED_STRING 66 . 72) (";" 72 . 73)))))

  )

(defun phps-mode-test-lexer--namespaces ()
  "Run test for namespaces."

  (phps-mode-test--with-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   "Capitalized object-oriented namespace file"
   (should
    (equal
     phps-mode-lex-analyzer--tokens
     '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) (";" 28 . 29) (T_CLASS 30 . 35) (T_STRING 36 . 43) ("{" 44 . 45) (T_PUBLIC 47 . 53) (T_FUNCTION 54 . 62) (T_STRING 63 . 74) ("(" 74 . 75) (")" 75 . 76) ("{" 77 . 78) (T_EXIT 81 . 85) (";" 85 . 86) ("}" 88 . 89) ("}" 90 . 91)))))

  )

(defun phps-mode-test-lexer--errors ()
  "Run test for errors."

  (should-error
   (phps-mode-test--with-buffer
    "<?php\necho \"My neverending double quotation\n"
    "Neverending double quotation"))
  (message "Passed tests for 'Neverending double quotation'")

  (should-error
   (phps-mode-test--with-buffer
    "<?php\n`My neverending backquotes\n"
    "Neverending backquotes"))
  (message "Passed tests for 'Neverending backquotes'")

  (should-error
   (phps-mode-test--with-buffer
    "<?php\n<<<LABEL\nMy neverending heredoc\ngoes on forever\n"
    "Neverending heredoc"))
  (message "Passed tests for 'Neverending heredoc'")

  )

(defun phps-mode-test-lexer--benchmark ()
  "Benchmark the lexer test."
  (require 'benchmark)
  (let ((iteration 1)
        (iterations 50))
    (message "Benchmarking %s iterations" iterations)
    (let ((elapsed
           (benchmark-run
               iterations
             (progn
               (phps-mode-test-lexer--script-boundaries)
               (phps-mode-test-lexer--simple-tokens)
               (phps-mode-test-lexer--complex-tokens)
               (phps-mode-test-lexer--namespaces)
               (phps-mode-test-lexer--errors)
               (message "Finished iteration %s" iteration)
               (setq iteration (1+ iteration))))))
      (message "Lexer tests completed in: %ss." elapsed))))

(defun phps-mode-test-lexer ()
  "Run test for lexer."
  ;; (message "-- Running all tests for lexer... --\n")
  ;; (setq debug-on-error t)
  
  (phps-mode-test-lexer--script-boundaries)
  (phps-mode-test-lexer--simple-tokens)
  (phps-mode-test-lexer--complex-tokens)
  (phps-mode-test-lexer--namespaces)
  (phps-mode-test-lexer--errors)
  ;; (message "\n-- Ran all tests for lexer. --")

  )

(provide 'phps-mode-test-lexer)

;;; phps-mode-test-lexer.el ends here
