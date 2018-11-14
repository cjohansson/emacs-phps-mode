;;; phps-test-lexer.el --- Tests for Semantic Lexer

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

;; Copyright (C) 2018 Christian Johansson

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Spathoftware Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:


;; Run from terminal make lexer-test


;;; Code:


(autoload 'phps-mode/with-test-buffer "phps-test")
(autoload 'phps-mode/lexer-init "phps-lexer")
(autoload 'phps-mode/lexer-get-point-data "phps-lexer")
(autoload 'should "ert")

(defun phps-mode/test-lexer--script-boundaries ()
  "Run test for lexer."

  (phps-mode/with-test-buffer
   "<?php\t$var=1; exit $var;\t?>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 11 . 12) (T_LNUMBER 12 . 13) (";" 13 . 14) (T_EXIT 15 . 19) (T_VARIABLE 20 . 24) (";" 24 . 25) (";" 26 . 28) (T_CLOSE_TAG 26 . 28)))))

  (phps-mode/with-test-buffer
   "<?php\nexit;\n?>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (";" 13 . 15) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode/with-test-buffer
   "<?php exit; ?>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_EXIT 7 . 11) (";" 11 . 12) (";" 13 . 15) (T_CLOSE_TAG 13 . 15)))))

  (phps-mode/with-test-buffer
   "<html><head>blabla</head<body>\n\n \t<?php\nexit;\n?>\n\n</body></html>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 35 . 41) (T_EXIT 41 . 45) (";" 45 . 46) (";" 47 . 50) (T_CLOSE_TAG 47 . 50)))))

  (phps-mode/with-test-buffer
   "\n\n \t<html><title>echo \"Blaha\";</title><?php\n\n\nexit?>\n\n<html><random /></html><?php exit ?>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 39 . 45) (T_EXIT 47 . 51) (";" 51 . 54) (T_CLOSE_TAG 51 . 54) (T_OPEN_TAG 78 . 84) (T_EXIT 84 . 88) (";" 89 . 91) (T_CLOSE_TAG 89 . 91)))))

  )

(defun phps-mode/test-lexer--simple-tokens ()
  "Run test for simple tokens."

  (phps-mode/with-test-buffer
   "<?php echo $var = array('');"
     (should (equal phps-mode/lexer-tokens
                    '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode/with-test-buffer
   "<?php if (empty($parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME])) { $parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME] = ''; }"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_EMPTY 11 . 16) ("(" 16 . 17) (T_VARIABLE 17 . 28) ("[" 28 . 29) (T_STRING 29 . 33) (T_PAAMAYIM_NEKUDOTAYIM 33 . 35) (T_STRING 35 . 76) ("]" 76 . 77) (")" 77 . 78) (")" 78 . 79) ("{" 80 . 81) (T_VARIABLE 82 . 93) ("[" 93 . 94) (T_STRING 94 . 98) (T_PAAMAYIM_NEKUDOTAYIM 98 . 100) (T_STRING 100 . 141) ("]" 141 . 142) ("=" 143 . 144) (T_CONSTANT_ENCAPSED_STRING 145 . 147) (";" 147 . 148) ("}" 149 . 150)))))

  (phps-mode/with-test-buffer
   "<?php echo $var = array(\"\");"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 27) (")" 27 . 28) (";" 28 . 29)))))

  (phps-mode/with-test-buffer
   "<?php echo $var = array('abc' => '123');"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_VARIABLE 12 . 16) ("=" 17 . 18) (T_ARRAY 19 . 24) ("(" 24 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (T_DOUBLE_ARROW 31 . 33) (T_CONSTANT_ENCAPSED_STRING 34 . 39) (")" 39 . 40) (";" 40 . 41)))))

  (phps-mode/with-test-buffer
   "<?php $var = []; "
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) ("[" 14 . 15) ("]" 15 . 16) (";" 16 . 17)))))

  (phps-mode/with-test-buffer
   "<?php $var = ''; $var = 'abc'; "
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31)))))

  (phps-mode/with-test-buffer
   "<?php $var = \"\"; $var = \"abc\"; $var = \"abc\\def\\ghj\";"
   ;; (message "Tokens: %s" phps-mode/lexer-tokens)
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 16) (";" 16 . 17) (T_VARIABLE 18 . 22) ("=" 23 . 24) (T_CONSTANT_ENCAPSED_STRING 25 . 30) (";" 30 . 31) (T_VARIABLE 32 . 36) ("=" 37 . 38) (T_CONSTANT_ENCAPSED_STRING 39 . 52) (";" 52 . 53)))))

  (phps-mode/with-test-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no'; "
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56)))))

  (phps-mode/with-test-buffer
   "<?php myFunction(); "
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_STRING 7 . 17) ("(" 17 . 18) (")" 18 . 19) (";" 19 . 20)))))

  (phps-mode/with-test-buffer
   "<?php // echo 'random';?><!--</div>-->"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 24) (";" 24 . 26) (T_CLOSE_TAG 24 . 26)))))

  (phps-mode/with-test-buffer
   "<?php //echo $contact_position;?><!--</div>-->"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_COMMENT 7 . 32) (";" 32 . 34) (T_CLOSE_TAG 32 . 34)))))


  (phps-mode/with-test-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no';\n//a random comment\n// another random comment\n/**\n * More comments\n* More\n **/\necho $backtrace; ?>"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ISSET 12 . 17) ("(" 17 . 18) (T_VARIABLE 18 . 28) ("[" 28 . 29) (T_LNUMBER 29 . 30) ("]" 30 . 31) ("[" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 38) ("]" 38 . 39) (")" 39 . 40) ("?" 41 . 42) (T_CONSTANT_ENCAPSED_STRING 43 . 48) (":" 49 . 50) (T_CONSTANT_ENCAPSED_STRING 51 . 55) (";" 55 . 56) (T_COMMENT 57 . 75) (T_COMMENT 76 . 101) (T_DOC_COMMENT 102 . 134) (T_ECHO 135 . 139) (T_VARIABLE 140 . 150) (";" 150 . 151) (";" 152 . 154) (T_CLOSE_TAG 152 . 154)))))

  (phps-mode/with-test-buffer
   "<?php $var EXIT die function return yield from yield try catch finally throw if elseif endif else while endwhile do for endfor foreach endforeach declare enddeclare instanceof as switch endswitch case default break continue goto echo print class interface trait extends implements :: \\ ... ?? new clone var (int) (integer) (real) (double) (float) (string) (binary) (array) (object) (boolean) (bool) (unset) eval include include_once require require_once namespace use insteadof global isset empty __halt_compiler static abstract final private protected public unset => list array callable ++ -- === !== == != <> <= >= <=> += -= *= *\\*= *\\* /= .= %= <<= >>= &= |= ^= || && OR AND XOR << >> { } 0xAF 0b10 200 2147483650 2.5 2.5e10 __CLASS__ __TRAIT__ __FUNCTION__ __METHOD__ __LINE__ __FILE__ __DIR__ __NAMESPACE__\n// My comment \n# My comment 2\n/*blaha blaha2*/ /** blaha\n blaha2 **/ 'test' 'my first \\'comment\\' really' \"sentence\" \"words \\\\comment\\\" really\" 'this single quoted string never ends"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) (T_EXIT 12 . 16) (T_DIE 17 . 20) (T_FUNCTION 21 . 29) (T_RETURN 30 . 36) (T_YIELD_FROM 37 . 48) (T_YIELD 48 . 53) (T_TRY 54 . 57) (T_CATCH 58 . 63) (T_FINALLY 64 . 71) (T_THROW 72 . 77) (T_IF 78 . 80) (T_ELSEIF 81 . 87) (T_ENDIF 88 . 93) (T_ELSE 94 . 98) (T_WHILE 99 . 104) (T_ENDWHILE 105 . 113) (T_DO 114 . 116) (T_FOR 117 . 120) (T_ENDFOR 121 . 127) (T_FOREACH 128 . 135) (T_ENDFOREACH 136 . 146) (T_DECLARE 147 . 154) (T_ENDDECLARE 155 . 165) (T_INSTANCEOF 166 . 176) (T_AS 177 . 179) (T_SWITCH 180 . 186) (T_ENDSWITCH 187 . 196) (T_CASE 197 . 201) (T_DEFAULT 202 . 209) (T_BREAK 210 . 215) (T_CONTINUE 216 . 224) (T_GOTO 225 . 229) (T_ECHO 230 . 234) (T_PRINT 235 . 240) (T_CLASS 241 . 246) (T_INTERFACE 247 . 256) (T_TRAIT 257 . 262) (T_EXTENDS 263 . 270) (T_IMPLEMENTS 271 . 281) (T_PAAMAYIM_NEKUDOTAYIM 282 . 284) (T_NS_SEPARATOR 285 . 286) (T_ELLIPSIS 287 . 290) (T_COALESCE 291 . 293) (T_NEW 294 . 297) (T_CLONE 298 . 303) (T_VAR 304 . 307) (T_INT_CAST 308 . 313) (T_INT_CAST 314 . 323) (T_DOUBLE_CAST 324 . 330) (T_DOUBLE_CAST 331 . 339) (T_DOUBLE_CAST 340 . 347) (T_STRING_CAST 348 . 356) (T_STRING_CAST 357 . 365) (T_ARRAY_CAST 366 . 373) (T_OBJECT_CAST 374 . 382) (T_BOOL_CAST 383 . 392) (T_BOOL_CAST 393 . 399) (T_UNSET_CAST 400 . 407) (T_EVAL 408 . 412) (T_INCLUDE 413 . 420) (T_INCLUDE_ONCE 421 . 433) (T_REQUIRE 434 . 441) (T_REQUIRE_ONCE 442 . 454) (T_NAMESPACE 455 . 464) (T_USE 465 . 468) (T_INSTEADOF 469 . 478) (T_GLOBAL 479 . 485) (T_ISSET 486 . 491) (T_EMPTY 492 . 497) (T_HALT_COMPILER 498 . 513) (T_STATIC 514 . 520) (T_ABSTRACT 521 . 529) (T_FINAL 530 . 535) (T_PRIVATE 536 . 543) (T_PROTECTED 544 . 553) (T_PUBLIC 554 . 560) (T_UNSET 561 . 566) (T_DOUBLE_ARROW 567 . 569) (T_LIST 570 . 574) (T_ARRAY 575 . 580) (T_CALLABLE 581 . 589) (T_INC 590 . 592) (T_DEC 593 . 595) (T_IS_IDENTICAL 596 . 599) (T_IS_NOT_IDENTICAL 600 . 603) (T_IS_EQUAL 604 . 606) (T_IS_NOT_EQUAL 607 . 609) (T_IS_NOT_EQUAL 610 . 612) (T_IS_SMALLER_OR_EQUAL 613 . 615) (T_IS_GREATER_OR_EQUAL 616 . 618) (T_SPACESHIP 619 . 622) (T_PLUS_EQUAL 623 . 625) (T_MINUS_EQUAL 626 . 628) (T_MUL_EQUAL 629 . 631) (T_POW_EQUAL 632 . 636) (T_POW 637 . 640) (T_DIV_EQUAL 641 . 643) (T_CONCAT_EQUAL 644 . 646) (T_MOD_EQUAL 647 . 649) (T_SL_EQUAL 650 . 653) (T_SR_EQUAL 654 . 657) (T_AND_EQUAL 658 . 660) (T_OR_EQUAL 661 . 663) (T_XOR_EQUAL 664 . 666) (T_BOOLEAN_OR 667 . 669) (T_BOOLEAN_AND 670 . 672) (T_LOGICAL_OR 673 . 675) (T_LOGICAL_AND 676 . 679) (T_LOGICAL_XOR 680 . 683) (T_SL 684 . 686) (T_SR 687 . 689) ("{" 690 . 691) ("}" 692 . 693) (T_LNUMBER 694 . 698) (T_LNUMBER 699 . 703) (T_LNUMBER 704 . 707) (T_DNUMBER 708 . 718) (T_DNUMBER 719 . 722) (T_DNUMBER 723 . 729) (T_CLASS_C 730 . 739) (T_TRAIT_C 740 . 749) (T_FUNC_C 750 . 762) (T_METHOD_C 763 . 773) (T_LINE 774 . 782) (T_FILE 783 . 791) (T_DIR 792 . 799) (T_NS_C 800 . 813) (T_COMMENT 814 . 828) (T_COMMENT 829 . 843) (T_COMMENT 844 . 860) (T_DOC_COMMENT 861 . 882) (T_CONSTANT_ENCAPSED_STRING 883 . 889) (T_CONSTANT_ENCAPSED_STRING 890 . 919) (T_CONSTANT_ENCAPSED_STRING 920 . 930) (T_CONSTANT_ENCAPSED_STRING 931 . 957) (T_ENCAPSED_AND_WHITESPACE 958 . 995)))))

  )

(defun phps-mode/test-lexer--complex-tokens ()
  "Run test for complex tokens."

  (phps-mode/with-test-buffer
   "<?php $var->property;"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) (T_OBJECT_OPERATOR 11 . 13) (T_STRING 13 . 21) (";" 21 . 22)))))

  ;; Double quoted strings with variables
  (phps-mode/with-test-buffer
   "<?php echo \"My $variable is here\"; echo \"you know\";"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_VARIABLE 16 . 25) (T_CONSTANT_ENCAPSED_STRING 25 . 33) ("\"" 33 . 34) (";" 34 . 35) (T_ECHO 36 . 40) (T_CONSTANT_ENCAPSED_STRING 41 . 51) (";" 51 . 52)))))

  (phps-mode/with-test-buffer
   "<?php echo \"My ${variable} is here 1\";"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 16) (T_DOLLAR_OPEN_CURLY_BRACES 16 . 18) (T_STRING_VARNAME 18 . 26) ("}" 26 . 27) (T_CONSTANT_ENCAPSED_STRING 27 . 37) ("\"" 37 . 38) (";" 38 . 39)))))

  (phps-mode/with-test-buffer
   "<?php echo \"Mine {$first_variable} is here and my $second is there.\";"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 18) (T_CURLY_OPEN 18 . 19) (T_VARIABLE 19 . 34) ("}" 34 . 35) (T_CONSTANT_ENCAPSED_STRING 35 . 51) (T_VARIABLE 51 . 58) (T_CONSTANT_ENCAPSED_STRING 58 . 68) ("\"" 68 . 69) (";" 69 . 70)))))

  (phps-mode/with-test-buffer
   "<?php echo \" Hello $variable[0], how are you?\";"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) ("\"" 12 . 13) (T_ENCAPSED_AND_WHITESPACE 13 . 20) (T_VARIABLE 20 . 30) (T_NUM_STRING 30 . 31) ("]" 31 . 32) (T_CONSTANT_ENCAPSED_STRING 32 . 46) ("\"" 46 . 47) (";" 47 . 48)))))

  ;; Heredoc
  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1\n line 2\nMYLABEL\n;"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 39) (T_END_HEREDOC 39 . 47) (";" 48 . 49)))))

  (phps-mode/with-test-buffer
   "<?php echo <<<MYLABEL\nline 1\n line 2\nMYLABEL\n;"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 23) (T_ENCAPSED_AND_WHITESPACE 23 . 37) (T_END_HEREDOC 37 . 45) (";" 46 . 47)))))

  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nMYLABEL\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_END_HEREDOC 25 . 33)))))

  ;; Test heredoc with variables $, {$, ${ here
  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1 $variable1\n line 2\n${variable2} line 3\n line {$variable3} here\nline 5 $variable[3] here\nMYLABEL;\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 32) (T_VARIABLE 32 . 42) (T_ENCAPSED_AND_WHITESPACE 42 . 51) (T_DOLLAR_OPEN_CURLY_BRACES 51 . 53) (T_STRING_VARNAME 53 . 62) ("}" 62 . 63) (T_ENCAPSED_AND_WHITESPACE 63 . 77) (T_CURLY_OPEN 77 . 78) (T_VARIABLE 78 . 88) ("}" 88 . 89) (T_ENCAPSED_AND_WHITESPACE 89 . 102) (T_VARIABLE 102 . 112) (T_NUM_STRING 112 . 113) ("]" 113 . 114) (T_ENCAPSED_AND_WHITESPACE 114 . 119) (T_END_HEREDOC 119 . 127) (";" 127 . 128)))))

  ;; Nowdoc
  (phps-mode/with-test-buffer
   "<?php echo <<<'MYLABEL'\nline 1\n line 2\nMYLABEL;\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_START_HEREDOC 12 . 25) (T_ENCAPSED_AND_WHITESPACE 25 . 39) (T_END_HEREDOC 39 . 47) (";" 47 . 48)))))

  ;; Backquotes
  (phps-mode/with-test-buffer
   "<?php `echo \"HELLO\"`;"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_CONSTANT_ENCAPSED_STRING 8 . 20) ("`" 20 . 21) (";" 21 . 22)))))

  (phps-mode/with-test-buffer
   "<?php `echo \"HELLO $variable or {$variable2} or ${variable3} or $variable[index][0] here\"`;"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_CONSTANT_ENCAPSED_STRING 8 . 20) (T_VARIABLE 20 . 29) (T_CONSTANT_ENCAPSED_STRING 29 . 33) (T_CURLY_OPEN 33 . 34) (T_VARIABLE 34 . 44) ("}" 44 . 45) (T_CONSTANT_ENCAPSED_STRING 45 . 49) (T_DOLLAR_OPEN_CURLY_BRACES 49 . 51) (T_STRING_VARNAME 51 . 60) ("}" 60 . 61) (T_CONSTANT_ENCAPSED_STRING 61 . 65) (T_VARIABLE 65 . 75) (T_STRING 75 . 80) ("]" 80 . 81) (T_CONSTANT_ENCAPSED_STRING 81 . 90) ("`" 90 . 91) (";" 91 . 92)))))

  )

(defun phps-mode/test-lexer--namespaces ()
  "Run test for namespaces."

  (phps-mode/with-test-buffer
   "<?php\nnamespace MyNameSpace{\n\tclass MyClass {\n\t\tpublic function __construct() {\n\t\t\texit;\n\t\t}\n\t}\n}\n"
   
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) ("{" 28 . 29) (T_CLASS 31 . 36) (T_STRING 37 . 44) ("{" 45 . 46) (T_PUBLIC 49 . 55) (T_FUNCTION 56 . 64) (T_STRING 65 . 76) ("(" 76 . 77) (")" 77 . 78) ("{" 79 . 80) (T_EXIT 84 . 88) (";" 88 . 89) ("}" 92 . 93) ("}" 95 . 96) ("}" 97 . 98)))))

  (phps-mode/with-test-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) (";" 28 . 29) (T_CLASS 30 . 35) (T_STRING 36 . 43) ("{" 44 . 45) (T_PUBLIC 47 . 53) (T_FUNCTION 54 . 62) (T_STRING 63 . 74) ("(" 74 . 75) (")" 75 . 76) ("{" 77 . 78) (T_EXIT 81 . 85) (";" 85 . 86) ("}" 88 . 89) ("}" 90 . 91)))))
  )

(defun phps-mode/test-lexer--errors ()
  "Run test for errors."

  (phps-mode/with-test-buffer
   "<?php\necho \"My neverending double quotation\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_ECHO 7 . 11) (T_ERROR 12 . 45)))))

  (phps-mode/with-test-buffer
   "<?php\n`My neverending backquotes\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) ("`" 7 . 8) (T_ERROR 8 . 34)))))

  (phps-mode/with-test-buffer
   "<?php\n<<<LABEL\nMy neverending heredoc\ngoes on forever\n"
   (should (equal phps-mode/lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_START_HEREDOC 7 . 16) (T_ERROR 16 . 55)))))

  )

(defun phps-mode/test-lexer--get-point-data ()
  "Return information about point in tokens."

  (phps-mode/with-test-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   (goto-char 35)
   (should (equal (list (list t 0 0 0 4) (list t 1 0 0 7)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 15)
   (should (equal (list (list nil 0 0 0 0) (list nil 0 0 0 6)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 30)
   (should (equal (list (list nil 0 0 0 0) (list nil 0 0 0 6)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 50)
   (should (equal (list (list nil 0 0 0 0) (list nil 0 0 0 6)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { \n   if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   ;; (message "Tokens: %s" phps-mode/lexer-tokens)
   (goto-char 48)
   (should (equal (list (list t 1 0 0 6) (list nil 0 0 0 18)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) {\n echo $title;\n} } ?></title><body>Bla bla</body></html>"
   (goto-char 72)
   (should (equal (list (list t 2 0 0 11) (list t 2 0 0 14)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n}\n}\n ?></title><body>Bla bla</body></html>"
   (goto-char 84)
   (should (equal (list (list t 2 0 0 14) (list t 1 0 0 15)) (phps-mode/lexer-get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   
   (goto-char 100)
   (should (equal (list (list nil 0 0 0 0) (list nil 0 0 0 18)) (phps-mode/lexer-get-point-data))))

  )

;; TODO Test this: $wpdb->get_var("SELECT post_parent FROM $wpdb->posts WHERE ID = '".$id."'");

(defun phps-mode/test-lexer ()
  "Run test for lexer."
  ;; (message "-- Running all tests for lexer... --\n")
  (setq debug-on-error t)
  (phps-mode/test-lexer--script-boundaries)
  (phps-mode/test-lexer--simple-tokens)
  (phps-mode/test-lexer--complex-tokens)
  (phps-mode/test-lexer--namespaces)
  (phps-mode/test-lexer--errors)
  (phps-mode/test-lexer--get-point-data)
  ;; (message "\n-- Ran all tests for lexer. --")
  )

(phps-mode/test-lexer)

(provide 'phps-mode/test-lexer)

;;; phps-test-lexer.el ends here
