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


(autoload 'phps-mode "phps-mode")
(autoload 'phps-mode/lexer-init "phps-lexer")

(require 'ert)

(defmacro phps-mode/with-test-buffer (source &rest body)
  "Set up test buffer with SOURCE and BODY."
  `(let ((test-buffer (generate-new-buffer "test")))
     (switch-to-buffer test-buffer)
     (insert ,source)
     (goto-char 0)
     ;;,(message "\nTesting buffer:\n'%s'\n" source)
     (phps-mode)
     ,@body
     (kill-buffer test-buffer)
     ))

(defun phps-mode/token-stream-to-string (token-stream)
  "Return a string from a TOKEN-STREAM."
  (let ((return ""))
    (dolist (item token-stream)
      (setq return (concat return (format " %s" (car item)))))
    return))

(defun phps-mode/test-lexer--script-boundaries ()
  "Run test for lexer."

  (phps-mode/with-test-buffer
   "<?php\t$var=1; exit $var;\t?>"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_VARIABLE = T_LNUMBER ; T_EXIT T_VARIABLE ; ; T_CLOSE_TAG"))))

  (phps-mode/with-test-buffer
   "<?php\nexit;\n?>"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_EXIT ; ; T_CLOSE_TAG"))))

  (phps-mode/with-test-buffer
   "<?php exit; ?>"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_EXIT ; ; T_CLOSE_TAG"))))

  (phps-mode/with-test-buffer
   "<html><head>blabla</head<body>\n\n \t<?php\nexit;\n?>\n\n</body></html>"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_EXIT ; ; T_CLOSE_TAG"))))

  (phps-mode/with-test-buffer
   "\n\n \t<html><title>echo \"Blaha\";</title><?php\n\n\nexit?>\n\n<html><random /></html><?php exit ?>"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_EXIT ; T_CLOSE_TAG T_OPEN_TAG T_EXIT ; T_CLOSE_TAG"))))

  )

(defun phps-mode/test-lexer--simple-tokens ()
  "Run test for simple tokens."

  (phps-mode/with-test-buffer
   "<?php echo $var = array('');"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_VARIABLE = T_ARRAY ( T_CONSTANT_ENCAPSED_STRING ) ;"))))

  (phps-mode/with-test-buffer
   "<?php if (empty($parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME])) { $parameters[self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME] = ''; }"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_IF ( T_EMPTY ( T_VARIABLE [ T_STRING T_PAAMAYIM_NEKUDOTAYIM T_STRING ] ) ) { T_VARIABLE [ T_STRING T_PAAMAYIM_NEKUDOTAYIM T_STRING ] = T_CONSTANT_ENCAPSED_STRING ; }"))))

  (phps-mode/with-test-buffer
   "<?php echo $var = array(\"\");"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_VARIABLE = T_ARRAY ( T_CONSTANT_ENCAPSED_STRING ) ;"))))

  (phps-mode/with-test-buffer
   "<?php echo $var = array('abc' => '123');"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_VARIABLE = T_ARRAY ( T_CONSTANT_ENCAPSED_STRING T_DOUBLE_ARROW T_CONSTANT_ENCAPSED_STRING ) ;"))))

  (phps-mode/with-test-buffer
   "<?php $var = []; "
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_VARIABLE = [ ] ;"))))

  (phps-mode/with-test-buffer
   "<?php echo isset($backtrace[1]['file']) ? 'yes' : 'no'; "
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_ISSET ( T_VARIABLE [ T_LNUMBER ] [ T_CONSTANT_ENCAPSED_STRING ] ) ? T_CONSTANT_ENCAPSED_STRING : T_CONSTANT_ENCAPSED_STRING ;"))))

  (phps-mode/with-test-buffer
   "<?php $var EXIT die function return yield from yield try catch finally throw if elseif endif else while endwhile do for endfor foreach endforeach declare enddeclare instanceof as switch endswitch case default break continue goto echo print class interface trait extends implements :: \\ ... ?? new clone var (int) (integer) (real) (double) (float) (string) (binary) (array) (object) (boolean) (bool) (unset) eval include include_once require require_once namespace use insteadof global isset empty __halt_compiler static abstract final private protected public unset => list array callable ++ -- === !== == != <> <= >= <=> += -= *= *\\*= *\\* /= .= %= <<= >>= &= |= ^= || && OR AND XOR << >> { } 0xAF 0b10 200 2147483650 2.5 2.5e10 __CLASS__ __TRAIT__ __FUNCTION__ __METHOD__ __LINE__ __FILE__ __DIR__ __NAMESPACE__\n// My comment \n# My comment 2\n/*blaha blaha2*/ /** blaha\n blaha2 **/ 'test' 'my first \\'comment\\' really' \"sentence\" \"words \\\\comment\\\" really\" 'this single quoted string never ends"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_VARIABLE T_EXIT T_DIE T_FUNCTION T_RETURN T_YIELD_FROM T_YIELD T_TRY T_CATCH T_FINALLY T_THROW T_IF T_ELSEIF T_ENDIF T_ELSE T_WHILE T_ENDWHILE T_DO T_FOR T_ENDFOR T_FOREACH T_ENDFOREACH T_DECLARE T_ENDDECLARE T_INSTANCEOF T_AS T_SWITCH T_ENDSWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE T_GOTO T_ECHO T_PRINT T_CLASS T_INTERFACE T_TRAIT T_EXTENDS T_IMPLEMENTS T_PAAMAYIM_NEKUDOTAYIM T_NS_SEPARATOR T_ELLIPSIS T_COALESCE T_NEW T_CLONE T_VAR T_INT_CAST T_INT_CAST T_DOUBLE_CAST T_DOUBLE_CAST T_DOUBLE_CAST T_STRING_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_BOOL_CAST T_UNSET_CAST T_EVAL T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE T_NAMESPACE T_USE T_INSTEADOF T_GLOBAL T_ISSET T_EMPTY T_HALT_COMPILER T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC T_UNSET T_DOUBLE_ARROW T_LIST T_ARRAY T_CALLABLE T_INC T_DEC T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_EQUAL T_IS_NOT_EQUAL T_IS_NOT_EQUAL T_IS_SMALLER_OR_EQUAL T_IS_GREATER_OR_EQUAL T_SPACESHIP T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_POW_EQUAL T_POW T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_SL_EQUAL T_SR_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_LOGICAL_OR T_LOGICAL_AND T_LOGICAL_XOR T_SL T_SR { } T_LNUMBER T_LNUMBER T_LNUMBER T_DNUMBER T_DNUMBER T_DNUMBER T_CLASS_C T_TRAIT_C T_FUNC_C T_METHOD_C T_LINE T_FILE T_DIR T_NS_C T_COMMENT T_COMMENT T_COMMENT T_DOC_COMMENT T_CONSTANT_ENCAPSED_STRING T_CONSTANT_ENCAPSED_STRING T_CONSTANT_ENCAPSED_STRING T_CONSTANT_ENCAPSED_STRING T_ENCAPSED_AND_WHITESPACE"))))

  )

(defun phps-mode/test-lexer--complex-tokens ()
  "Run test for complex tokens."

  (phps-mode/with-test-buffer
   "<?php $var->property;"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_VARIABLE T_OBJECT_OPERATOR T_STRING ;"))))

  ;; Double quoted strings with variables
  (phps-mode/with-test-buffer
   "<?php echo \"My $variable is here\"; echo \"you know\";"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO \" T_ENCAPSED_AND_WHITESPACE T_VARIABLE T_CONSTANT_ENCAPSED_STRING \" ; T_ECHO T_CONSTANT_ENCAPSED_STRING ;"))))
  (phps-mode/with-test-buffer
   "<?php echo \"My ${variable} is here 1\";"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO \" T_ENCAPSED_AND_WHITESPACE T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME } T_CONSTANT_ENCAPSED_STRING \" ;"))))
  (phps-mode/with-test-buffer
   "<?php echo \"Mine {$first_variable} is here and my $second is there.\";"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO \" T_ENCAPSED_AND_WHITESPACE T_CURLY_OPEN T_VARIABLE } T_CONSTANT_ENCAPSED_STRING T_VARIABLE T_CONSTANT_ENCAPSED_STRING \" ;"))))
  (phps-mode/with-test-buffer
   "<?php echo \" Hello $variable[0], how are you?\";"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO \" T_ENCAPSED_AND_WHITESPACE T_VARIABLE T_NUM_STRING ] T_CONSTANT_ENCAPSED_STRING \" ;"))))

  ;; Heredoc
  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1\n line 2\nMYLABEL\n;"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC ;"))))
  (phps-mode/with-test-buffer
   "<?php echo <<<MYLABEL\nline 1\n line 2\nMYLABEL\n;"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC ;"))))
  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nMYLABEL\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_START_HEREDOC T_END_HEREDOC"))))

  ;; Test heredoc with variables $, {$, ${ here
  (phps-mode/with-test-buffer
   "<?php echo <<<\"MYLABEL\"\nline 1 $variable1\n line 2\n${variable2} line 3\n line {$variable3} here\nline 5 $variable[3] here\nMYLABEL;\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_VARIABLE T_ENCAPSED_AND_WHITESPACE T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME } T_ENCAPSED_AND_WHITESPACE T_CURLY_OPEN T_VARIABLE } T_ENCAPSED_AND_WHITESPACE T_VARIABLE T_NUM_STRING ] T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC ;"))))

  ;; Nowdoc
  (phps-mode/with-test-buffer
   "<?php echo <<<'MYLABEL'\nline 1\n line 2\nMYLABEL;\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC ;"))))

  ;; Backquotes
  (phps-mode/with-test-buffer
   "<?php `echo \"HELLO\"`;"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG ` T_CONSTANT_ENCAPSED_STRING ` ;"))))
  (phps-mode/with-test-buffer
   "<?php `echo \"HELLO $variable or {$variable2} or ${variable3} or $variable[index][0] here\"`;"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG ` T_CONSTANT_ENCAPSED_STRING T_VARIABLE T_CONSTANT_ENCAPSED_STRING T_CURLY_OPEN T_VARIABLE } T_CONSTANT_ENCAPSED_STRING T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME } T_CONSTANT_ENCAPSED_STRING T_VARIABLE T_STRING ] T_CONSTANT_ENCAPSED_STRING ` ;"))))

  )

(defun phps-mode/test-lexer--namespaces ()
  "Run test for namespaces."

  (phps-mode/with-test-buffer
   "<?php\nnamespace MyNameSpace{\n\tclass MyClass {\n\t\tpublic function __construct() {\n\t\t\texit;\n\t\t}\n\t}\n}\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_NAMESPACE T_STRING { T_CLASS T_STRING { T_PUBLIC T_FUNCTION T_STRING ( ) { T_EXIT ; } } }"))))

  (phps-mode/with-test-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_NAMESPACE T_STRING { T_CLASS T_STRING { T_PUBLIC T_FUNCTION T_STRING ( ) { T_EXIT ; } } }"))))
  )

(defun phps-mode/test-lexer--errors ()
  "Run test for errors."

  (phps-mode/with-test-buffer
   "<?php\necho \"My neverending double quotation\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_ECHO T_ERROR"))))

  (phps-mode/with-test-buffer
   "<?php\n`My neverending backquotes\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG ` T_ERROR"))))

  (phps-mode/with-test-buffer
   "<?php\n<<<LABEL\nMy neverending heredoc\ngoes on forever\n"
   (let* ((tokens phps-mode/lexer-tokens)
          (string-tokens (phps-mode/token-stream-to-string tokens)))
     (should (equal string-tokens " T_OPEN_TAG T_START_HEREDOC T_ERROR"))))

)

(defun phps-mode/test-lexer ()
  "Run test for lexer."
  ;; (message "-- Running all tests for lexer... --\n")
  ;; (setq debug-on-error t)
  (phps-mode/test-lexer--script-boundaries)
  (phps-mode/test-lexer--simple-tokens)
  (phps-mode/test-lexer--complex-tokens)
  (phps-mode/test-lexer--namespaces)
  (phps-mode/test-lexer--errors)
  ;; (message "\n-- Ran all tests for lexer. --")
  )

(phps-mode/test-lexer)

(provide 'phps-mode/test-lexer)

;;; phps-test-lexer.el ends here
