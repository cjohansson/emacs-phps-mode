;;; phps-mode-test-lexer.el --- Tests for lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

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

;; Run from terminal make lexer-test


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(defun phps-mode-test-lexer-script-boundaries ()
  "Run test for lexer."

  (phps-mode-test-with-buffer
   "<?php\t$öar=1; exit $var;\t?>"
   "Simple PHP with two expression"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 11 . 12) (T_LNUMBER 12 . 13) (";" 13 . 14) (T_EXIT 15 . 19) (T_VARIABLE 20 . 24) (";" 24 . 25) (";" 26 . 28) (T_CLOSE_TAG 26 . 28)))))

  (phps-mode-test-with-buffer
   "<?php\nexit;\n?>"
   "Minimal PHP expression"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (";" 13 . 15) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode-test-with-buffer
   "<?php exit; ?>"
   "Small PHP file"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (";" 13 . 15) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode-test-with-buffer
   "<html><head>blabla</head<body>\n\n \t<?php\nexit;\n?>\n\n</body></html>"
   "Mixed inline HTML and PHP"
   (should (equal phps-mode-lexer-tokens
                  '((T_INLINE_HTML 1 . 35) (T_OPEN_TAG 35 . 41) (T_EXIT 41 . 45) (";" 45 . 46) (";" 47 . 49) (T_CLOSE_TAG 47 . 49) (T_INLINE_HTML 49 . 65)))))

  (phps-mode-test-with-buffer
   "\n\n \t<html><title>echo \"Blaha\";</title><?php\n\n\nexit?>\n\n<html><random /></html><?php exit ?>"
   "Another mixed inline HTML and PHP"
   (should (equal phps-mode-lexer-tokens
                  '((T_INLINE_HTML 1 . 39) (T_OPEN_TAG 39 . 45) (T_EXIT 47 . 51) (";" 51 . 53) (T_CLOSE_TAG 51 . 53) (T_INLINE_HTML 53 . 78) (T_OPEN_TAG 78 . 84) (T_EXIT 84 . 88) (";" 89 . 91) (T_CLOSE_TAG 89 . 91)))))

  (phps-mode-test-with-buffer
   "<?php\n\n$k = 'key';\n\necho \"\\$a['{$k}']\";"
   "A tricky case where variable inside double quote is escaped"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 8 . 10) ("=" 11 . 12) (T_CONSTANT_ENCAPSED_STRING 13 . 18) (";" 18 . 19) (T_ECHO 21 . 25) ("\"" 26 . 27) (T_ENCAPSED_AND_WHITESPACE 27 . 32) (T_CURLY_OPEN 32 . 33) (T_VARIABLE 33 . 35) ("}" 35 . 36) (T_CONSTANT_ENCAPSED_STRING 36 . 38) ("\"" 38 . 39) (";" 39 . 40)))))

  )

(defun phps-mode-test-lexer-simple-tokens ()
  "Run test for simple tokens."

  (phps-mode-test-with-buffer
   "<?php echo $vür = array('');"
   "Simple PHP via array declaration"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode-test-with-buffer
   "<?php if (empty($parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME])) { $parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME] = ''; }"
   "Complex PHP with conditional"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_EMPTY 11 . 16) ("(" 16 . 17) (T_VARIABLE 17 . 28) ("[" 28 . 29) (T_STRING 29 . 33) (T_PAAMAYIM_NEKUDOTAYIM 33 . 35) (T_STRING 35 . 76) ("]" 76 . 77) (")" 77 . 78) (")" 78 . 79) ("{" 80 . 81) (T_VARIABLE 82 . 93) ("[" 93 . 94) (T_STRING 94 . 98) (T_PAAMAYIM_NEKUDOTAYIM 98 . 100) (T_STRING 100 . 141) ("]" 141 . 142) ("=" 143 . 144) (T_CONSTANT_ENCAPSED_STRING 145 . 147) (";" 147 . 148) ("}" 149 . 150)))))

  (phps-mode-test-with-buffer
   "<?php echo $var = array(\"\");"
   "Simple PHP with empty array assignment"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode-test-with-buffer
   "<?php echo $var = array('abc' => '123');"
   "Simple PHP with associative array assignment"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (T_DOUBLE_ARROW 31 . 33) (T_CONSTANT_ENCAPSED_STRING 34 . 39) (")" 39 . 40) (";" 40 . 41)))))

  (phps-mode-test-with-buffer
   "<?php $var = []; "
   "PHP with short-handed array declaration assignment"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) ("[" 14 . 15) ("]" 15 . 16) (";" 16 . 17)))))

  (phps-mode-test-with-buffer
   "<?php $var = ''; $var = 'abc'; "
   "PHP with string assignments"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31)))))

  (phps-mode-test-with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case PHP"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_SWITCH 7 . 13) ("(" 14 . 15) (T_STRING 15 . 32) ("(" 32 . 33) (")" 33 . 34) (")" 34 . 35) ("{" 36 . 37) (T_CASE 38 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 59) (":" 59 . 60) (T_ECHO 61 . 65) (T_CONSTANT_ENCAPSED_STRING 66 . 87) (";" 87 . 88) ("}" 89 . 90)))))

  (phps-mode-test-with-buffer
   "<?php $var = \"\"; $var = \"abc\"; $var = \"abc\\def\\ghj\";"
   "PHP with three string assignments"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31) (T_VARIABLE 32 . 36) ("=" 37 . 38) (T_CONSTANT_ENCAPSED_STRING 39 . 52) (";" 52 . 53)))))

  (phps-mode-test-with-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no'; "
   "PHP with short-handed conditional echo"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56)))))

  (phps-mode-test-with-buffer
   "<?php myFunction(); "
   "A single function call"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 17) ("(" 17 . 18) (")" 18 . 19) (";" 19 . 20)))))

  (phps-mode-test-with-buffer
   "<?php // echo 'random';?><!--</div>-->"
   "Commented out PHP expression and inline-html"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 24) (";" 24 . 26) (T_CLOSE_TAG 24 . 26) (T_INLINE_HTML 26 . 39)))))

  (phps-mode-test-with-buffer
   "<?php //echo $contact_position;?><!--</div>-->"
   "Commented out PHP expression and inline-html 2"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 32) (";" 32 . 34) (T_CLOSE_TAG 32 . 34) (T_INLINE_HTML 34 . 47)))))

  (phps-mode-test-with-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no';\n//a random comment\n// another random comment\n/**\n * More comments\n* More\n **/\necho $backtrace; ?>"
   "Conditional echo, comment and doc-comment block"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56) (T_COMMENT 57 . 75) (T_COMMENT 76 . 101) (T_DOC_COMMENT 102 . 134) (T_ECHO 135 . 139) (T_VARIABLE 140 . 150) (";" 150 . 151) (";" 152 . 154) (T_CLOSE_TAG 152 . 154)))))

  (phps-mode-test-with-buffer
   "<?php forgerarray($arg1, $arg2)"
   "A function call containing keywords in its name"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 18) ("(" 18 . 19) (T_VARIABLE 19 . 24) ("," 24 . 25) (T_VARIABLE 26 . 31) (")" 31 . 32)))))

  (phps-mode-test-with-buffer
   "<?php\n$username = $_GET['user'] ?? 'nobody';\n$this->request->data['comments']['user_id'] ??= 'value';\n"
   "Coalescing comparison operator and coalescing assignment operator"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 16) ("=" 17 . 18) (T_VARIABLE 19 . 24) ("[" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 31) ("]" 31 . 32) (T_COALESCE 33 . 35) (T_CONSTANT_ENCAPSED_STRING 36 . 44) (";" 44 . 45) (T_VARIABLE 46 . 51) (T_OBJECT_OPERATOR 51 . 53) (T_STRING 53 . 60) (T_OBJECT_OPERATOR 60 . 62) (T_STRING 62 . 66) ("[" 66 . 67) (T_CONSTANT_ENCAPSED_STRING 67 . 77) ("]" 77 . 78) ("[" 78 . 79) (T_CONSTANT_ENCAPSED_STRING 79 . 88) ("]" 88 . 89) (T_COALESCE_EQUAL 90 . 93) (T_CONSTANT_ENCAPSED_STRING 94 . 101) (";" 101 . 102)))))

  (phps-mode-test-with-buffer
   "<?php\necho $array['abc'];\necho \"My $array[12] random statement\";\n"
   "Long inside array offset"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 18) ("[" 18 . 19) (T_CONSTANT_ENCAPSED_STRING 19 . 24) ("]" 24 . 25) (";" 25 . 26) (T_ECHO 27 . 31) ("\"" 32 . 33) (T_ENCAPSED_AND_WHITESPACE 33 . 36) (T_VARIABLE 36 . 43) (T_NUM_STRING 43 . 45) ("]" 45 . 46) (T_CONSTANT_ENCAPSED_STRING 46 . 63) ("\"" 63 . 64) (";" 64 . 65)))))

  (phps-mode-test-with-buffer
   "<?php\n/*my comment */\n/** my doc comment */"
   "Comment vs doc-comment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 22) (T_DOC_COMMENT 23 . 44)))))

  ;; (phps-mode-test-with-buffer
  ;;  "<?php ??= $var EXIT die function return yield from yield try catch finally throw if elseif endif else while endwhile do for endfor foreach endforeach declare enddeclare instanceof as switch endswitch case default break continue goto echo print class interface trait extends implements :: \\ ... ?? new clone var (int) (integer) (real) (double) (float) (string) (binary) (array) (object) (boolean) (bool) (unset) eval include include_once require require_once namespace use insteadof global isset empty __halt_compiler static abstract final private protected public unset => list array callable ++ -- === !== == != <> <= >= <=> += -= *= *\\*= *\\* /= .= %= <<= >>= &= |= ^= || && OR AND XOR << >> { } 0xAF 0b10 200 2147483650 2.5 2.5e10 __CLASS__ __TRAIT__ __FUNCTION__ __METHOD__ __LINE__ __FILE__ __DIR__ __NAMESPACE__\n// My comment \n# My comment 2\n/*blaha blaha2*/ /** blaha\n blaha2 **/ 'test' 'my first \\'comment\\' really' \"sentence\" \"words \\\\comment\\\" really\" 'this single quoted string never ends"
  ;;  "All PHP tokens after each other"
  ;;  (message "Tokens: %s" phps-mode-lexer-tokens)
  ;;  (should (equal phps-mode-lexer-tokens
  ;;                 '((T_OPEN_TAG 1 . 7) (T_COALESCE_EQUAL 7 . 10) (T_VARIABLE 11 . 15) (T_EXIT 16 . 20) (T_DIE 21 . 24) (T_FUNCTION 25 . 33) (T_RETURN 34 . 40) (T_YIELD_FROM 41 . 52) (T_YIELD 52 . 57) (T_TRY 58 . 61) (T_CATCH 62 . 67) (T_FINALLY 68 . 75) (T_THROW 76 . 81) (T_IF 82 . 84) (T_ELSEIF 85 . 91) (T_ENDIF 92 . 97) (T_ELSE 98 . 102) (T_WHILE 103 . 108) (T_ENDWHILE 109 . 117) (T_DO 118 . 120) (T_FOR 121 . 124) (T_ENDFOR 125 . 131) (T_FOREACH 132 . 139) (T_ENDFOREACH 140 . 150) (T_DECLARE 151 . 158) (T_ENDDECLARE 159 . 169) (T_INSTANCEOF 170 . 180) (T_AS 181 . 183) (T_SWITCH 184 . 190) (T_ENDSWITCH 191 . 200) (T_CASE 201 . 205) (T_DEFAULT 206 . 213) (T_BREAK 214 . 219) (T_CONTINUE 220 . 228) (T_GOTO 229 . 233) (T_ECHO 234 . 238) (T_PRINT 239 . 244) (T_CLASS 245 . 250) (T_INTERFACE 251 . 260) (T_TRAIT 261 . 266) (T_EXTENDS 267 . 274) (T_IMPLEMENTS 275 . 285) (T_PAAMAYIM_NEKUDOTAYIM 286 . 288) (T_NS_SEPARATOR 289 . 290) (T_ELLIPSIS 291 . 294) (T_COALESCE 295 . 297) (T_NEW 298 . 301) (T_CLONE 302 . 307) (T_VAR 308 . 311) (T_INT_CAST 312 . 317) (T_INT_CAST 318 . 327) (T_DOUBLE_CAST 328 . 334) (T_DOUBLE_CAST 335 . 343) (T_DOUBLE_CAST 344 . 351) (T_STRING_CAST 352 . 360) (T_STRING_CAST 361 . 369) (T_ARRAY_CAST 370 . 377) (T_OBJECT_CAST 378 . 386) (T_BOOL_CAST 387 . 396) (T_BOOL_CAST 397 . 403) (T_UNSET_CAST 404 . 411) (T_EVAL 412 . 416) (T_INCLUDE 417 . 424) (T_INCLUDE_ONCE 425 . 437) (T_REQUIRE 438 . 445) (T_REQUIRE_ONCE 446 . 458) (T_NAMESPACE 459 . 468) (T_USE 469 . 472) (T_INSTEADOF 473 . 482) (T_GLOBAL 483 . 489) (T_ISSET 490 . 495) (T_EMPTY 496 . 501) (T_HALT_COMPILER 502 . 517) (T_STATIC 518 . 524) (T_ABSTRACT 525 . 533) (T_FINAL 534 . 539) (T_PRIVATE 540 . 547) (T_PROTECTED 548 . 557) (T_PUBLIC 558 . 564) (T_UNSET 565 . 570) (T_DOUBLE_ARROW 571 . 573) (T_LIST 574 . 578) (T_ARRAY 579 . 584) (T_CALLABLE 585 . 593) (T_INC 594 . 596) (T_DEC 597 . 599) (T_IS_IDENTICAL 600 . 603) (T_IS_NOT_IDENTICAL 604 . 607) (T_IS_EQUAL 608 . 610) (T_IS_NOT_EQUAL 611 . 613) (T_IS_NOT_EQUAL 614 . 616) (T_IS_SMALLER_OR_EQUAL 617 . 619) (T_IS_GREATER_OR_EQUAL 620 . 622) (T_SPACESHIP 623 . 626) (T_PLUS_EQUAL 627 . 629) (T_MINUS_EQUAL 630 . 632) (T_MUL_EQUAL 633 . 635) (T_POW_EQUAL 636 . 640) (T_POW 641 . 644) (T_DIV_EQUAL 645 . 647) (T_CONCAT_EQUAL 648 . 650) (T_MOD_EQUAL 651 . 653) (T_SL_EQUAL 654 . 657) (T_SR_EQUAL 658 . 661) (T_AND_EQUAL 662 . 664) (T_OR_EQUAL 665 . 667) (T_XOR_EQUAL 668 . 670) (T_BOOLEAN_OR 671 . 673) (T_BOOLEAN_AND 674 . 676) (T_LOGICAL_OR 677 . 679) (T_LOGICAL_AND 680 . 683) (T_LOGICAL_XOR 684 . 687) (T_SL 688 . 690) (T_SR 691 . 693) ("{" 694 . 695) ("}" 696 . 697) (T_LNUMBER 698 . 702) (T_LNUMBER 703 . 707) (T_LNUMBER 708 . 711) (T_DNUMBER 712 . 722) (T_DNUMBER 723 . 726) (T_DNUMBER 727 . 733) (T_CLASS_C 734 . 743) (T_TRAIT_C 744 . 753) (T_FUNC_C 754 . 766) (T_METHOD_C 767 . 777) (T_LINE 778 . 786) (T_FILE 787 . 795) (T_DIR 796 . 803) (T_NS_C 804 . 817) (T_COMMENT 818 . 832) (T_COMMENT 833 . 847) (/ 848 . 849) (* 849 . 850) (T_STRING 850 . 855) (T_STRING 856 . 862) (* 862 . 863) (/ 863 . 864) (T_DOC_COMMENT 865 . 886) (T_CONSTANT_ENCAPSED_STRING 887 . 893) (T_CONSTANT_ENCAPSED_STRING 894 . 923) (T_CONSTANT_ENCAPSED_STRING 924 . 934) (T_CONSTANT_ENCAPSED_STRING 935 . 961) (T_ENCAPSED_AND_WHITESPACE 962 . 999)))))


  )

(defun phps-mode-test-lexer-complex-tokens ()
  "Run test for complex tokens."

  (phps-mode-test-with-buffer
   "<?php $var->property;"
   "Object property"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) (T_OBJECT_OPERATOR 11 . 13) (T_STRING 13 . 21) (";" 21 . 22)))))

  (phps-mode-test-with-buffer
   "<?php echo \"My $variable is here\"; echo \"you know\";"
   "Double quoted strings with variables"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_VARIABLE 16 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 33) ("\"" 33 . 34) (";" 34 . 35) (T_ECHO 36 . 40) (T_CONSTANT_ENCAPSED_STRING 41 . 51) (";" 51 . 52)))))

  (phps-mode-test-with-buffer
   "<?php echo \"My ${variable} is here 1\";"
   "Double quoted string with variable"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_DOLLAR_OPEN_CURLY_BRACES 16 . 18) (T_STRING_VARNAME 18 . 26) ("}" 26 . 27) (T_CONSTANT_ENCAPSED_STRING 27 . 37) ("\"" 37 . 38) (";" 38 . 39)))))

  (phps-mode-test-with-buffer
   "<?php echo \"Mine {$first_variable} is here and my $second is there.\";"
   "Another double quoted string with variable"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 18) (T_CURLY_OPEN 18 . 19) (T_VARIABLE 19 . 34) ("}" 34 . 35) (T_CONSTANT_ENCAPSED_STRING 35 . 51) (T_VARIABLE 51 . 58) (T_CONSTANT_ENCAPSED_STRING 58 . 68) ("\"" 68 . 69) (";" 69 . 70)))))

  (phps-mode-test-with-buffer
   "<?php echo \" Hello $variable[0], how are you?\";"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 20) (T_VARIABLE 20 . 30) (T_NUM_STRING 30 . 31) ("]" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 46) ("\"" 46 . 47) (";" 47 . 48)))))

  ;; HEREDOC
  (phps-mode-test-with-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1\n line 2\nMYLABEL\n;"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 39) (T_END_HEREDOC 39 . 47) (";" 48 . 49)))))

  (phps-mode-test-with-buffer
   "<?php echo <<<MYLABEL\nline 1\n line 2\nMYLABEL\n;"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 23) (T_ENCAPSED_AND_WHITESPACE 23 . 37) (T_END_HEREDOC 37 . 45) (";" 46 . 47)))))

  (phps-mode-test-with-buffer
   "<?php echo <<<\"MYLABEL\"\nMYLABEL\n"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_END_HEREDOC 25 . 33)))))

  ;; Test heredoc with variables $, {$, ${ here
  (phps-mode-test-with-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1 $variable1\n line 2\n${variable2} line 3\n line {$variable3} here\nline 5 $variable[3] here\nMYLABEL;\n"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 32) (T_VARIABLE 32 . 42) (T_ENCAPSED_AND_WHITESPACE 42 . 51) (T_DOLLAR_OPEN_CURLY_BRACES 51 . 53) (T_STRING_VARNAME 53 . 62) ("}" 62 . 63) (T_ENCAPSED_AND_WHITESPACE 63 . 77) (T_CURLY_OPEN 77 . 78) (T_VARIABLE 78 . 88) ("}" 88 . 89) (T_ENCAPSED_AND_WHITESPACE 89 . 102) (T_VARIABLE 102 . 112) (T_NUM_STRING 112 . 113) ("]" 113 . 114) (T_ENCAPSED_AND_WHITESPACE 114 . 119) (T_END_HEREDOC 119 . 127) (";" 127 . 128)))))

  ;; Nowdoc
  (phps-mode-test-with-buffer
   "<?php echo <<<'MYLABEL'\nline 1\n line 2\nMYLABEL;\n"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 39) (T_END_HEREDOC 39 . 47) (";" 47 . 48)))))

  ;; Backquotes
  (phps-mode-test-with-buffer
   "<?php `echo \"HELLO\"`;"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_CONSTANT_ENCAPSED_STRING 8 . 20) ("`" 20 . 21) (";" 21 . 22)))))

  (phps-mode-test-with-buffer
   "<?php `echo \"HELLO $variable or {$variable2} or ${variable3} or $variable[index][0] here\"`;"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_CONSTANT_ENCAPSED_STRING 8 . 20) (T_VARIABLE 20 . 29) (T_CONSTANT_ENCAPSED_STRING 29 . 33) (T_CURLY_OPEN 33 . 34) (T_VARIABLE 34 . 44) ("}" 44 . 45) (T_CONSTANT_ENCAPSED_STRING 45 . 49) (T_DOLLAR_OPEN_CURLY_BRACES 49 . 51) (T_STRING_VARNAME 51 . 60) ("}" 60 . 61) (T_CONSTANT_ENCAPSED_STRING 61 . 65) (T_VARIABLE 65 . 75) (T_STRING 75 . 80) ("]" 80 . 81) (T_CONSTANT_ENCAPSED_STRING 81 . 90) ("`" 90 . 91) (";" 91 . 92)))))

  (phps-mode-test-with-buffer
   "<?php $wpdb->posts; ?>"
   nil
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 19) (";" 19 . 20) (";" 21 . 23) (T_CLOSE_TAG 21 . 23)))))

  (phps-mode-test-with-buffer
   "<?php $var = \"SELECT post_parent FROM $wpdb->posts WHERE ID = '\".$id.\"'\"; ?>"
   nil
   ;; (message "Tokens 1: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) ("\"" 14 . 15) (T_ENCAPSED_AND_WHITESPACE 15 . 39) (T_VARIABLE 39 . 44) (T_OBJECT_OPERATOR 44 . 46) (T_STRING 46 . 51) (T_CONSTANT_ENCAPSED_STRING 51 . 64) ("\"" 64 . 65) ("." 65 . 66) (T_VARIABLE 66 . 69) ("." 69 . 70) (T_CONSTANT_ENCAPSED_STRING 70 . 73) (";" 73 . 74) (";" 75 . 77) (T_CLOSE_TAG 75 . 77)))))

  (phps-mode-test-with-buffer
   "<?php $wpdb->get_var(\"SELECT post_parent FROM $wpdb->posts WHERE ID = '\".$id.\"'\"); ?>"
   nil
   ;; (message "Tokens 2: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 21) ("(" 21 . 22) ("\"" 22 . 23) (T_ENCAPSED_AND_WHITESPACE 23 . 47) (T_VARIABLE 47 . 52) (T_OBJECT_OPERATOR 52 . 54) (T_STRING 54 . 59) (T_CONSTANT_ENCAPSED_STRING 59 . 72) ("\"" 72 . 73) ("." 73 . 74) (T_VARIABLE 74 . 77) ("." 77 . 78) (T_CONSTANT_ENCAPSED_STRING 78 . 81) (")" 81 . 82) (";" 82 . 83) (";" 84 . 86) (T_CLOSE_TAG 84 . 86)))))

  (phps-mode-test-with-buffer
   "<?php $this->add($option['style']['selectors'], array('background' => \"{$value['color']} url('{$value['image']}')\"));"
   "Complex tokens with tokens inside double-quoted string"
   ;; (message "Tokens 2: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 12) (T_OBJECT_OPERATOR 12 . 14) (T_STRING 14 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 25) ("[" 25 . 26) (T_CONSTANT_ENCAPSED_STRING 26 . 33) ("]" 33 . 34) ("[" 34 . 35) (T_CONSTANT_ENCAPSED_STRING 35 . 46) ("]" 46 . 47) ("," 47 . 48) (T_ARRAY 49 . 54) ("(" 54 . 55) (T_CONSTANT_ENCAPSED_STRING 55 . 67) (T_DOUBLE_ARROW 68 . 70) ("\"" 71 . 72) (T_ENCAPSED_AND_WHITESPACE 72 . 72) (T_CURLY_OPEN 72 . 73) (T_VARIABLE 73 . 79) ("[" 79 . 80) (T_CONSTANT_ENCAPSED_STRING 80 . 87) ("]" 87 . 88) ("}" 88 . 89) (T_CONSTANT_ENCAPSED_STRING 89 . 95) (T_CURLY_OPEN 95 . 96) (T_VARIABLE 96 . 102) ("[" 102 . 103) (T_CONSTANT_ENCAPSED_STRING 103 . 110) ("]" 110 . 111) ("}" 111 . 112) (T_CONSTANT_ENCAPSED_STRING 112 . 114) ("\"" 114 . 115) (")" 115 . 116) (")" 116 . 117) (";" 117 . 118)))))

  (phps-mode-test-with-buffer
   "<?php\n$var = <<<EOD\nrandom {$value['color']->property} again {$value->head()}; random\nEOD;\n"
   "Complex tokens with tokens inside HEREDOC string"
   ;; (message "Tokens 2: %s" phps-mode-lexer-tokens)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_START_HEREDOC 14 . 21) (T_ENCAPSED_AND_WHITESPACE 21 . 28) (T_CURLY_OPEN 28 . 29) (T_VARIABLE 29 . 35) ("[" 35 . 36) (T_CONSTANT_ENCAPSED_STRING 36 . 43) ("]" 43 . 44) (T_OBJECT_OPERATOR 44 . 46) (T_STRING 46 . 54) ("}" 54 . 55) (T_ENCAPSED_AND_WHITESPACE 55 . 62) (T_CURLY_OPEN 62 . 63) (T_VARIABLE 63 . 69) (T_OBJECT_OPERATOR 69 . 71) (T_STRING 71 . 75) ("(" 75 . 76) (")" 76 . 77) ("}" 77 . 78) (T_ENCAPSED_AND_WHITESPACE 78 . 86) (T_END_HEREDOC 86 . 90) (";" 90 . 91)))))

  )

(defun phps-mode-test-lexer-namespaces ()
  "Run test for namespaces."

  (phps-mode-test-with-buffer
   "<?php\nnamespace MyNameSpace{\n\tclass MyClass {\n\t\tpublic function __construct() {\n\t\t\texit;\n\t\t}\n\t}\n}\n"
   "Object-oriented namespace file"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) ("{" 28 . 29) (T_CLASS 31 . 36) (T_STRING 37 . 44) ("{" 45 . 46) (T_PUBLIC 49 . 55) (T_FUNCTION 56 . 64) (T_STRING 65 . 76) ("(" 76 . 77) (")" 77 . 78) ("{" 79 . 80) (T_EXIT 84 . 88) (";" 88 . 89) ("}" 92 . 93) ("}" 95 . 96) ("}" 97 . 98)))))

  (phps-mode-test-with-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   "Capitalized object-oriented namespace file"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) (";" 28 . 29) (T_CLASS 30 . 35) (T_STRING 36 . 43) ("{" 44 . 45) (T_PUBLIC 47 . 53) (T_FUNCTION 54 . 62) (T_STRING 63 . 74) ("(" 74 . 75) (")" 75 . 76) ("{" 77 . 78) (T_EXIT 81 . 85) (";" 85 . 86) ("}" 88 . 89) ("}" 90 . 91)))))
  )

(defun phps-mode-test-lexer-errors ()
  "Run test for errors."

  (phps-mode-test-with-buffer
   "<?php\necho \"My neverending double quotation\n"
   "Neverending double quotation"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11)))))

  (phps-mode-test-with-buffer
   "<?php\n`My neverending backquotes\n"
   "Neverending backquotes"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8)))))

  (phps-mode-test-with-buffer
   "<?php\n<<<LABEL\nMy neverending heredoc\ngoes on forever\n"
   "Neverending heredoc"
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16)))))

  )

(defun phps-mode-test-lexer-get-moved-lexer-tokens ()
  "Run test for get moved lexer tokens."

  (should (equal
           '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 21) (T_ERROR 21 . 60))
           (phps-mode-lexer-get-moved-tokens '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)) 8 5)))

  (should (equal
           '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 11) (T_ERROR 11 . 50))
           (phps-mode-lexer-get-moved-tokens '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)) 8 -5)))

  (should (equal
           '((T_OPEN_TAG 1 . 8) (T_START_HEREDOC 8 . 17) (T_ERROR 17 . 56))
           (phps-mode-lexer-get-moved-tokens '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)) 6 1)))

  (should (equal
           '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 56))
           (phps-mode-lexer-get-moved-tokens '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)) 20 1)))

  (should (equal
           '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 54))
           (phps-mode-lexer-get-moved-tokens '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)) 20 -1)))

  )

(defun phps-mode-test-lexer-get-moved-lexer-states ()
  "Run test for get moved lexer states."

  (should (equal
           '((68 76 1 '(1))
             (10 67 1 '(1))
             (1 9 1 '(1)))
           
           (phps-mode-lexer-get-moved-states
            '((66 74 1 '(1))
              (8 65 1 '(1))
              (1 7 1 '(1)))
            6
            2)))

  (should (equal
           '((67 75 1 '(1))
             (9 66 1 '(1))
             (2 8 1 '(1)))
           
           (phps-mode-lexer-get-moved-states
            '((66 74 1 '(1))
              (8 65 1 '(1))
              (1 7 1 '(1)))
            0
            1)))

  (should (equal
           '((66 74 1 '(1))
             (8 65 1 '(1))
             (1 7 1 '(1)))
           
           (phps-mode-lexer-get-moved-states
            '((66 74 1 '(1))
              (8 65 1 '(1))
              (1 7 1 '(1)))
            100
            1)))

  (should (equal
           '((64 72 1 '(1))
             (6 63 1 '(1))
             (1 7 1 '(1)))
           
           (phps-mode-lexer-get-moved-states
            '((66 74 1 '(1))
              (8 65 1 '(1))
              (3 9 1 '(1)))
            3
            -2)))
  )

(defun phps-mode-test-lexer ()
  "Run test for lexer."
  ;; (message "-- Running all tests for lexer... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-test-lexer-script-boundaries)
  (phps-mode-test-lexer-simple-tokens)
  (phps-mode-test-lexer-complex-tokens)
  (phps-mode-test-lexer-namespaces)
  (phps-mode-test-lexer-errors)
  (phps-mode-test-lexer-get-moved-lexer-tokens)
  (phps-mode-test-lexer-get-moved-lexer-states)
  ;; (message "\n-- Ran all tests for lexer. --")
  )

(phps-mode-test-lexer)

(provide 'phps-mode-test-lexer)

;;; phps-mode-test-lexer.el ends here
