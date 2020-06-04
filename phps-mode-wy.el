;;; phps-mode-wy.el --- Generated parser support file

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Christian Johansson <christianjohansson@Christians-MacBook-Air.local>
;; Created: 2020-06-04 14:45:29+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file phps-mode-parser-grammar.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;
(setq max-specpdl-size 160000)

;;; Declarations
;;
(eval-and-compile (defconst phps-mode--expected-conflicts
                    nil
                    "The number of expected shift/reduce conflicts in this grammar."))

(defconst phps-mode--keyword-table
  (semantic-lex-make-keyword-table
   '(("include (T_INCLUDE)" . T_INCLUDE)
     ("include_once (T_INCLUDE_ONCE)" . T_INCLUDE_ONCE)
     ("eval (T_EVAL)" . T_EVAL)
     ("require (T_REQUIRE)" . T_REQUIRE)
     ("require_once (T_REQUIRE_ONCE)" . T_REQUIRE_ONCE)
     ("or (T_LOGICAL_OR)" . T_LOGICAL_OR)
     ("xor (T_LOGICAL_XOR)" . T_LOGICAL_XOR)
     ("and (T_LOGICAL_AND)" . T_LOGICAL_AND)
     ("print (T_PRINT)" . T_PRINT)
     ("yield (T_YIELD)" . T_YIELD)
     ("yield from (T_YIELD_FROM)" . T_YIELD_FROM)
     ("+= (T_PLUS_EQUAL)" . T_PLUS_EQUAL)
     ("-= (T_MINUS_EQUAL)" . T_MINUS_EQUAL)
     ("*= (T_MUL_EQUAL)" . T_MUL_EQUAL)
     ("/= (T_DIV_EQUAL)" . T_DIV_EQUAL)
     (".= (T_CONCAT_EQUAL)" . T_CONCAT_EQUAL)
     ("%= (T_MOD_EQUAL)" . T_MOD_EQUAL)
     ("&= (T_AND_EQUAL)" . T_AND_EQUAL)
     ("|= (T_OR_EQUAL)" . T_OR_EQUAL)
     ("^= (T_XOR_EQUAL)" . T_XOR_EQUAL)
     ("<<= (T_SL_EQUAL)" . T_SL_EQUAL)
     (">>= (T_SR_EQUAL)" . T_SR_EQUAL)
     ("??= (T_COALESCE_EQUAL)" . T_COALESCE_EQUAL)
     ("|| (T_BOOLEAN_OR)" . T_BOOLEAN_OR)
     ("&& (T_BOOLEAN_AND)" . T_BOOLEAN_AND)
     ("== (T_IS_EQUAL)" . T_IS_EQUAL)
     ("!= (T_IS_NOT_EQUAL)" . T_IS_NOT_EQUAL)
     ("=== (T_IS_IDENTICAL)" . T_IS_IDENTICAL)
     ("!== (T_IS_NOT_IDENTICAL)" . T_IS_NOT_IDENTICAL)
     ("<= (T_IS_SMALLER_OR_EQUAL)" . T_IS_SMALLER_OR_EQUAL)
     (">= (T_IS_GREATER_OR_EQUAL)" . T_IS_GREATER_OR_EQUAL)
     ("<=> (T_SPACESHIP)" . T_SPACESHIP)
     ("<< (T_SL)" . T_SL)
     (">> (T_SR)" . T_SR)
     ("instanceof (T_INSTANCEOF)" . T_INSTANCEOF)
     ("++ (T_INC)" . T_INC)
     ("-- (T_DEC)" . T_DEC)
     ("(int) (T_INT_CAST)" . T_INT_CAST)
     ("(double) (T_DOUBLE_CAST)" . T_DOUBLE_CAST)
     ("(string) (T_STRING_CAST)" . T_STRING_CAST)
     ("(array) (T_ARRAY_CAST)" . T_ARRAY_CAST)
     ("(object) (T_OBJECT_CAST)" . T_OBJECT_CAST)
     ("(bool) (T_BOOL_CAST)" . T_BOOL_CAST)
     ("(unset) (T_UNSET_CAST)" . T_UNSET_CAST)
     ("new (T_NEW)" . T_NEW)
     ("clone (T_CLONE)" . T_CLONE)
     ("exit (T_EXIT)" . T_EXIT)
     ("if (T_IF)" . T_IF)
     ("elseif (T_ELSEIF)" . T_ELSEIF)
     ("else (T_ELSE)" . T_ELSE)
     ("endif (T_ENDIF)" . T_ENDIF)
     ("echo (T_ECHO)" . T_ECHO)
     ("do (T_DO)" . T_DO)
     ("while (T_WHILE)" . T_WHILE)
     ("endwhile (T_ENDWHILE)" . T_ENDWHILE)
     ("for (T_FOR)" . T_FOR)
     ("endfor (T_ENDFOR)" . T_ENDFOR)
     ("foreach (T_FOREACH)" . T_FOREACH)
     ("endforeach (T_ENDFOREACH)" . T_ENDFOREACH)
     ("declare (T_DECLARE)" . T_DECLARE)
     ("enddeclare (T_ENDDECLARE)" . T_ENDDECLARE)
     ("as (T_AS)" . T_AS)
     ("switch (T_SWITCH)" . T_SWITCH)
     ("endswitch (T_ENDSWITCH)" . T_ENDSWITCH)
     ("case (T_CASE)" . T_CASE)
     ("default (T_DEFAULT)" . T_DEFAULT)
     ("break (T_BREAK)" . T_BREAK)
     ("continue (T_CONTINUE)" . T_CONTINUE)
     ("goto (T_GOTO)" . T_GOTO)
     ("function (T_FUNCTION)" . T_FUNCTION)
     ("fn (T_FN)" . T_FN)
     ("const (T_CONST)" . T_CONST)
     ("return (T_RETURN)" . T_RETURN)
     ("try (T_TRY)" . T_TRY)
     ("catch (T_CATCH)" . T_CATCH)
     ("finally (T_FINALLY)" . T_FINALLY)
     ("throw (T_THROW)" . T_THROW)
     ("use (T_USE)" . T_USE)
     ("insteadof (T_INSTEADOF)" . T_INSTEADOF)
     ("global (T_GLOBAL)" . T_GLOBAL)
     ("static (T_STATIC)" . T_STATIC)
     ("abstract (T_ABSTRACT)" . T_ABSTRACT)
     ("final (T_FINAL)" . T_FINAL)
     ("private (T_PRIVATE)" . T_PRIVATE)
     ("protected (T_PROTECTED)" . T_PROTECTED)
     ("public (T_PUBLIC)" . T_PUBLIC)
     ("var (T_VAR)" . T_VAR)
     ("unset (T_UNSET)" . T_UNSET)
     ("isset (T_ISSET)" . T_ISSET)
     ("empty (T_EMPTY)" . T_EMPTY)
     ("__halt_compiler (T_HALT_COMPILER)" . T_HALT_COMPILER)
     ("class (T_CLASS)" . T_CLASS)
     ("trait (T_TRAIT)" . T_TRAIT)
     ("interface (T_INTERFACE)" . T_INTERFACE)
     ("extends (T_EXTENDS)" . T_EXTENDS)
     ("implements (T_IMPLEMENTS)" . T_IMPLEMENTS)
     ("-> (T_OBJECT_OPERATOR)" . T_OBJECT_OPERATOR)
     ("=> (T_DOUBLE_ARROW)" . T_DOUBLE_ARROW)
     ("list (T_LIST)" . T_LIST)
     ("array (T_ARRAY)" . T_ARRAY)
     ("callable (T_CALLABLE)" . T_CALLABLE)
     ("__LINE__ (T_LINE)" . T_LINE)
     ("__FILE__ (T_FILE)" . T_FILE)
     ("__DIR__ (T_DIR)" . T_DIR)
     ("__CLASS__ (T_CLASS_C)" . T_CLASS_C)
     ("__TRAIT__ (T_TRAIT_C)" . T_TRAIT_C)
     ("__METHOD__ (T_METHOD_C)" . T_METHOD_C)
     ("__FUNCTION__ (T_FUNC_C)" . T_FUNC_C)
     ("comment (T_COMMENT)" . T_COMMENT)
     ("doc comment (T_DOC_COMMENT)" . T_DOC_COMMENT)
     ("open tag (T_OPEN_TAG)" . T_OPEN_TAG)
     ("open tag with echo (T_OPEN_TAG_WITH_ECHO)" . T_OPEN_TAG_WITH_ECHO)
     ("close tag (T_CLOSE_TAG)" . T_CLOSE_TAG)
     ("whitespace (T_WHITESPACE)" . T_WHITESPACE)
     ("heredoc start (T_START_HEREDOC)" . T_START_HEREDOC)
     ("heredoc end (T_END_HEREDOC)" . T_END_HEREDOC)
     ("${ (T_DOLLAR_OPEN_CURLY_BRACES)" . T_DOLLAR_OPEN_CURLY_BRACES)
     ("{$ (T_CURLY_OPEN)" . T_CURLY_OPEN)
     (":: (T_PAAMAYIM_NEKUDOTAYIM)" . T_PAAMAYIM_NEKUDOTAYIM)
     ("namespace (T_NAMESPACE)" . T_NAMESPACE)
     ("__NAMESPACE__ (T_NS_C)" . T_NS_C)
     ("\\ (T_NS_SEPARATOR)" . T_NS_SEPARATOR)
     ("... (T_ELLIPSIS)" . T_ELLIPSIS)
     ("?? (T_COALESCE)" . T_COALESCE)
     ("** (T_POW)" . T_POW)
     ("**= (T_POW_EQUAL)" . T_POW_EQUAL)
     ("invalid character (T_BAD_CHARACTER)" . T_BAD_CHARACTER))
   'nil)
  "Table of language keywords.")

(defconst phps-mode--token-table
  (semantic-lex-make-type-table
   '(("<no-type>"
      (T_ERROR)
      (\0)
      (END))
     ("punctuation"
      (UNARY . "~")
      (SUBTRACTION . "-")
      (SINGLE_QUOTE . "'")
      (QUESTION_MARK . "?")
      (POW . "^")
      (OPEN_SQUARE_BRACKET . "[")
      (OPEN_PARENTHESIS . "(")
      (OPEN_CURLY_BRACKET . "{")
      (NEGATION . "!")
      (MULTIPLICATION . "*")
      (MODULO . "%")
      (LESSER_THAN . "<")
      (GREATER_THAN . ">")
      (DOT . ".")
      (DIVISION . "/")
      (DOUBLE_QUOTE . "\"")
      (DOLLAR_SIGN . "$")
      (COMMA . ",")
      (COLON . ":")
      (CLOSE_SQUARE_BRACKET . "]")
      (CLOSE_PARENTHESIS . ")")
      (CLOSE_CURLY_BRACKET . "]")
      (BITWISE_OR . "|")
      (BITWISE_AND . "&")
      (BACKTICK . "`")
      (AT . "@")
      (ASSIGN . "=")
      (ADDITION . "+"))
     ("ast"
      (T_NUM_STRING . "number (T_NUM_STRING)")
      (T_STRING_VARNAME . "variable name (T_STRING_VARNAME)")
      (T_CONSTANT_ENCAPSED_STRING . "quoted-string (T_CONSTANT_ENCAPSED_STRING)")
      (T_ENCAPSED_AND_WHITESPACE . "quoted-string and whitespace (T_ENCAPSED_AND_WHITESPACE)")
      (T_INLINE_HTML)
      (T_VARIABLE . "variable (T_VARIABLE)")
      (T_STRING . "identifier (T_STRING)")
      (T_DNUMBER . "floating-point number (T_DNUMBER)")
      (T_LNUMBER . "integer number (T_LNUMBER)")))
   '(("num" class_modifiers class_modifier)
     ("num" use_type backup_fn_flags)
     ("num" :declared t)
     ("num" returns_ref function)
     ("num" fn is_reference)
     ("num" is_variadic variable_modifiers)
     ("num" :declared t)
     ("ast" inline_function union_type)
     ("ast" :declared t)
     ("ast" array_pair non_empty_array_pair_list)
     ("ast" array_pair_list possible_array_pair)
     ("ast" :declared t)
     ("ast" lexical_var_list encaps_list)
     ("ast" :declared t)
     ("ast" ctor_arguments alt_if_stmt_without_else)
     ("ast" trait_adaptation_list lexical_vars)
     ("ast" :declared t)
     ("ast" class_const_list class_const_decl)
     ("ast" class_name_list trait_adaptations)
     ("ast" method_body non_empty_for_exprs)
     ("ast" :declared t)
     ("ast" non_empty_parameter_list argument_list)
     ("ast" non_empty_argument_list property_list)
     ("ast" :declared t)
     ("ast" callable_expr callable_variable)
     ("ast" static_member new_variable)
     ("ast" :declared t)
     ("ast" fully_dereferencable array_object_dereferencable)
     ("ast" :declared t)
     ("ast" variable_class_name dereferencable_scalar)
     ("ast" constant class_constant)
     ("ast" :declared t)
     ("ast" absolute_trait_method_reference trait_method_reference)
     ("ast" property echo_expr)
     ("ast" :declared t)
     ("ast" interface_declaration_statement interface_extends_list)
     ("ast" :declared t)
     ("ast" class_declaration_statement trait_declaration_statement)
     ("ast" :declared t)
     ("punctuation" :declared t)))
  "Table of lexical tokens.")

(defconst phps-mode--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_LNUMBER T_DNUMBER T_STRING T_VARIABLE T_INLINE_HTML T_ENCAPSED_AND_WHITESPACE T_CONSTANT_ENCAPSED_STRING T_STRING_VARNAME T_NUM_STRING ADDITION ASSIGN AT BACKTICK BITWISE_AND BITWISE_OR CLOSE_CURLY_BRACKET CLOSE_PARENTHESIS CLOSE_SQUARE_BRACKET COLON COMMA DOLLAR_SIGN DOUBLE_QUOTE DIVISION DOT GREATER_THAN LESSER_THAN MODULO MULTIPLICATION NEGATION OPEN_CURLY_BRACKET OPEN_PARENTHESIS OPEN_SQUARE_BRACKET POW QUESTION_MARK SINGLE_QUOTE SUBTRACTION UNARY END \0 T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PRINT T_YIELD T_YIELD_FROM T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_COALESCE_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_SMALLER_OR_EQUAL T_IS_GREATER_OR_EQUAL T_SPACESHIP T_SL T_SR T_INSTANCEOF T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_NEW T_CLONE T_EXIT T_IF T_ELSEIF T_ELSE T_ENDIF T_ECHO T_DO T_WHILE T_ENDWHILE T_FOR T_ENDFOR T_FOREACH T_ENDFOREACH T_DECLARE T_ENDDECLARE T_AS T_SWITCH T_ENDSWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE T_GOTO T_FUNCTION T_FN T_CONST T_RETURN T_TRY T_CATCH T_FINALLY T_THROW T_USE T_INSTEADOF T_GLOBAL T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC T_VAR T_UNSET T_ISSET T_EMPTY T_HALT_COMPILER T_CLASS T_TRAIT T_INTERFACE T_EXTENDS T_IMPLEMENTS T_OBJECT_OPERATOR T_DOUBLE_ARROW T_LIST T_ARRAY T_CALLABLE T_LINE T_FILE T_DIR T_CLASS_C T_TRAIT_C T_METHOD_C T_FUNC_C T_COMMENT T_DOC_COMMENT T_OPEN_TAG T_OPEN_TAG_WITH_ECHO T_CLOSE_TAG T_WHITESPACE T_START_HEREDOC T_END_HEREDOC T_DOLLAR_OPEN_CURLY_BRACES T_CURLY_OPEN T_PAAMAYIM_NEKUDOTAYIM T_NAMESPACE T_NS_C T_NS_SEPARATOR T_ELLIPSIS T_COALESCE T_POW T_POW_EQUAL T_BAD_CHARACTER T_ERROR)
       ((left T_LOGICAL_OR)
        (left T_LOGICAL_XOR)
        (left T_LOGICAL_AND %precedence T_PRINT %precedence T_YIELD %precedence T_DOUBLE_ARROW %precedence T_YIELD_FROM %precedence 61 T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL T_COALESCE_EQUAL)
        (left 63 58)
        (right T_COALESCE)
        (left T_BOOLEAN_OR)
        (left T_BOOLEAN_AND)
        (left 124)
        (left 94)
        (left 38)
        (nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP)
        (nonassoc 60 T_IS_SMALLER_OR_EQUAL 62 T_IS_GREATER_OR_EQUAL)
        (left 46)
        (left T_SL T_SR)
        (left 43 45)
        (left 42 47 37 %precedence 33 %precedence T_INSTANCEOF %precedence 126 T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST 64)
        (right T_POW %precedence T_CLONE %precedence T_NOELSE %precedence T_ELSEIF %precedence T_ELSE))
       (class_constant
        ((class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_CLASS_CONST_OR_NAME :member $3)))
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_CLASS_CONST_OR_NAME :member $3))))
       (array_pair_list
        ((non_empty_array_pair_list)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_LIST_RTRIM))))
       (possible_array_pair
        (nil
         (nil))
        ((array_pair)
         ($1)))
       (non_empty_array_pair_list
        ((non_empty_array_pair_list COMMA possible_array_pair)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_LIST_ADD :new-element $3)))
        ((possible_array_pair)
         (wisent-raw-tag
          (semantic-tag "" 'phps-mode-parser--zend_ast_array :elements @elements :size 1))))
       (array_pair
        ((expr T_DOUBLE_ARROW expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_array_elem $3 $1)))
        ((expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_array_elem $1 nil)))
        ((expr T_DOUBLE_ARROW BITWISE_AND variable)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_array_elem 'ZEND_AST_EX :operator 1 :subject $1)))
        ((BITWISE_AND variable)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_array_elem 'ZEND_AST_EX :operator 1 :subject $2)))
        ((T_ELLIPSIS expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_unpack $2)))
        ((expr T_DOUBLE_ARROW T_LIST OPEN_PARENTHESIS array_pair_list CLOSE_PARENTHESIS)
         (progn
           (semantic-tag-put-attribute $4 'attr 'phps-mode-parser--zend_array_syntax_list)
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_array_elem $5 $1))))
        ((T_LIST OPEN_PARENTHESIS array_pair_list CLOSE_PARENTHESIS)
         (progn
           (semantic-tag-put-attribute $3 'attr 'phps-mode-parser--zend_array_syntax_list)
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_array_elem $3 nil)))))
       (encaps_list
        ((encaps_list encaps_var)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_LIST_ADD :new-element $2)))
        ((encaps_list T_ENCAPSED_AND_WHITESPACE)
         (wisent-raw-tag
          (semantic-tag $1 'ZEND_AST_LIST_ADD :new-element $2)))
        ((encaps_var)
         (wisent-raw-tag
          (semantic-tag "" 'phps-mode-parser--zend_ast_encaps_list :elements @elements :size 1)))
        ((T_ENCAPSED_AND_WHITESPACE encaps_var)
         (wisent-raw-tag
          (semantic-tag "" 'phps-mode-parser--zend_ast_encaps_list :elements @elements :size 2))))
       (encaps_var
        ((T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $1)))
        ((T_VARIABLE OPEN_SQUARE_BRACKET encaps_var_offset CLOSE_SQUARE_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_dim
                        (wisent-raw-tag
                         (semantic-tag 'phps-mode-parser--zend_ast_var $1))
                        $3)))
        ((T_VARIABLE T_OBJECT_OPERATOR T_STRING)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_prop
                        (wisent-raw-tag
                         (semantic-tag 'phps-mode-parser--zend_ast_var $1))
                        $3)))
        ((T_DOLLAR_OPEN_CURLY_BRACES expr CLOSE_CURLY_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $2)))
        ((T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME CLOSE_CURLY_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $2)))
        ((T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME OPEN_SQUARE_BRACKET expr CLOSE_SQUARE_BRACKET CLOSE_CURLY_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_dim
                        (wisent-raw-tag
                         (semantic-tag 'phps-mode-parser--zend_ast_var $2))
                        $4)))
        ((T_CURLY_OPEN variable CLOSE_CURLY_BRACKET)
         ($2)))
       (encaps_var_offset
        ((T_STRING)
         ($1))
        ((T_NUM_STRING)
         ($1))
        ((45 T_NUM_STRING)
         (*
          (string-to-number $2)
          -1))
        ((T_VARIABLE)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $1))))
       (dereferencable_scalar
        ((T_ARRAY OPEN_PARENTHESIS array_pair_list CLOSE_PARENTHESIS)
         (progn
           (semantic-tag-put-attribute $3 'attr 'phps-mode-parser--zend_array_syntax_long)
           $3))
        ((OPEN_SQUARE_BRACKET array_pair_list CLOSE_SQUARE_BRACKET)
         (progn
           (semantic-tag-put-attribute $2 'attr 'phps-mode-parser--zend_array_syntax_short)
           $2))
        ((T_CONSTANT_ENCAPSED_STRING)
         ($1))
        ((DOUBLE_QUOTE encaps_list DOUBLE_QUOTE)
         ($2)))
       (fully_dereferencable
        ((variable)
         ($1))
        ((OPEN_PARENTHESIS expr CLOSE_PARENTHESIS)
         ($2))
        ((dereferencable_scalar)
         ($1))
        ((class_constant)
         ($1)))
       (array_object_dereferencable
        ((fully_dereferencable)
         ($1))
        ((constant)
         ($1)))
       (callable_variable
        ((simple_variable)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $1)))
        ((array_object_dereferencable OPEN_SQUARE_BRACKET optional_expr CLOSE_SQUARE_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_dim $1 $3)))
        ((array_object_dereferencable OPEN_CURLY_BRACKET expr CLOSE_CURLY_BRACKET)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_dim 'ZEND_AST_EX :operator 'phps-mode-parser--zend_dim_alternative_syntax :subject $3)))
        ((array_object_dereferencable T_OBJECT_OPERATOR property_name argument_list)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_method_call $1 $3 $4)))
        ((function_call)
         ($1)))
       (variable
        ((callable_variable)
         ($1))
        ((static_member)
         ($1))
        ((array_object_dereferencable T_OBJECT_OPERATOR property_name)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_prop $1 $2))))
       (simple_variable
        ((T_VARIABLE)
         ($1))
        ((DOLLAR_SIGN OPEN_CURLY_BRACKET expr CLOSE_CURLY_BRACKET)
         ($3))
        ((DOLLAR_SIGN simple_variable)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_var $2))))
       (expr
        ((variable)
         ($1))
        ((T_LIST OPEN_PARENTHESIS array_pair_list CLOSE_PARENTHESIS ASSIGN expr)
         (progn
           (semantic-tag-put-attribute $3 'attr phps-mode-parser--zend_array_syntax_list)
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_assign $3 $6))
           | OPEN_SQUARE_BRACKET array_pair_list CLOSE_SQUARE_BRACKET ASSIGN expr
           (progn
             (semantic-tag-put-attribute $2 'attr 'phps-mode-parser--zend_array_syntax_short)
             (wisent-raw-tag
              (semantic-tag 'phps-mode-parser--zend_ast_assign $2 $5)))
           | variable ASSIGN expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_assign $1 $3))
           | variable ASSIGN BITWISE_AND variable
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_assign_ref $1 $4))
           | T_CLONE expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_clone $2))
           | variable T_PLUS_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_add 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_MINUS_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sub 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_MUL_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_mul 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_POW_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_pow 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_DIV_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_div 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_CONCAT_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_concat 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_MOD_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_mod 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_AND_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_and 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_OR_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_or 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_XOR_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_xor 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_SL_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sl 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_SR_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sr 'ZEND_AST_ASSIGN_OP :object $3 :subject $1))
           | variable T_COALESCE_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_coalesce $1 $3))
           | variable T_INC
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_post_inc $1))
           | T_INC variable
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_pre_inc $2))
           | variable T_DEC
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_post_dec $1))
           | T_DEC variable
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_pre_inc $2))
           | expr T_BOOLEAN_OR expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_or $1 $3))
           | expr T_BOOLEAN_AND expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_and $1 $3))
           | expr T_LOGICAL_OR expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_or $1 $3))
           | expr T_LOGICAL_AND expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_and $1 $3))
           | expr T_LOGICAL_XOR expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bool_xor 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr BITWISE_OR expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_or 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr BITWISE_AND expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_and 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr POW expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_bw_xor 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr DOT expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_concat 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr ADDITION expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_add 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr SUBTRACTION expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sub 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr MULTIPLICATION expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_mul 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_POW expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_pow 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr DIVISION expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_div 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr MODULO expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_mod 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_SL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sl 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_SR expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_sr 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | ADDITION expr prec UNARY
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_unary_plus $1 $2))
           | SUBTRACTION expr prec UNARY
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_unary_minus $1 $2))
           | NEGATION expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_unary_op 'ZEND_AST_EX :operator 'phps-mode-parser--zend_bool_not :subject $2))
           | UNARY expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_unary_op 'ZEND_AST_EX :operator 'phps-mode-parser--zend_bw_not :subject $2))
           | expr T_IS_IDENTICAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_identical 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_IS_NOT_IDENTICAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_not_identical 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_IS_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_equal 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_IS_NOT_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_not_equal 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr LESSER_THAN expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_smaller 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_IS_SMALLER_OR_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_smaller_or_equal 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr GREATER_THAN expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_greater 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_IS_GREATER_OR_EQUAL expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_is_greater_or_equal 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_SPACESHIP expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_spaceship 'ZEND_AST_BINARY_OP :object $3 :subject $1))
           | expr T_INSTANCEOF class_name_reference
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_instanceof $1 $3))
           | OPEN_PARENTHESIS expr CLOSE_PARENTHESIS
           (progn
             (when
                 (equal
                  (semantic-tag-get-attribute $2 'kind)
                  'phps-mode-parser--zend_ast_conditional)
               (semantic-tag-put-attribute $2 'attr 'phps-mode-parser--zend_parenthesized_conditional))
             $2)
           | new_expr
           ($1)
           | expr QUESTION_MARK expr COLON expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_conditional $1 $3 $5))
           | expr QUESTION_MARK COLON expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_conditional $1 nil $4))
           | expr T_COALESCE expr
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_coalesce $1 $3))
           | internal_functions_in_yacc
           ($1)))
        ((T_INT_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_long 'ZEND_AST_CAST :subject $2)))
        ((T_DOUBLE_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_double 'ZEND_AST_CAST :subject $2)))
        ((T_STRING_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_string 'ZEND_AST_CAST :subject $2)))
        ((T_ARRAY_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_array 'ZEND_AST_CAST :subject $2)))
        ((T_OBJECT_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_object 'ZEND_AST_CAST :subject $2)))
        ((T_BOOL_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_bool 'ZEND_AST_CAST :subject $2)))
        ((T_UNSET_CAST expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--is_unset 'ZEND_AST_CAST :subject $2)))
        ((T_EXIT exit_expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_exit $2)))
        ((AT expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_silence $2)))
        ((scalar)
         ($1))
        ((BACKTICK backticks_expr BACKTICK)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_shell $2)))
        ((T_PRINT expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_print $2)))
        ((T_YIELD)
         (progn
           (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags
                                              (logior
                                               (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags)
                                               'phps-mode-parser--zend_acc_generator))
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_yield nil nil))))
        ((T_YIELD expr)
         (progn
           (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags
                                              (logior
                                               (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags)
                                               'phps-mode-parser--zend_acc_generator))
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_yield $2 nil))))
        ((T_YIELD expr T_DOUBLE_ARROW expr)
         (progn
           (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags
                                              (logior
                                               (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags)
                                               'phps-mode-parser--zend_acc_generator))
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_yield $4 $2))))
        ((T_YIELD_FROM expr)
         (progn
           (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags
                                              (logior
                                               (phps-mode-parser-grammar-macro-CG 'phps-mode-parser--extra_fn_flags)
                                               'phps-mode-parser--zend_acc_generator))
           (wisent-raw-tag
            (semantic-tag 'phps-mode-parser--zend_ast_yield_from $2))))
        ((T_THROW expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_throw $2)))
        ((inline_function)
         ($1))
        ((T_STATIC inline_function)
         (progn
           (semantic-tag-put-attribute $2 'flags
                                       (logior
                                        (semantic-tag-get-attribute $2 'flags)
                                        'phps-mode-parser--zend_acc_static))
           $2)))
       (isset_variable
        ((expr)
         (wisent-raw-tag
          (semantic-tag 'phps-mode-parser--zend_ast_isset $1)))))
     '(isset_variable)))
  "Parser table.")

(defun phps-mode--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((semantic-parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table phps-mode--parse-table
        semantic-debug-parser-source "phps-mode-parser-grammar.wy"
        semantic-flex-keywords-obarray phps-mode--keyword-table
        semantic-lex-types-obarray phps-mode--token-table)
  ;; Collect unmatched syntax lexical tokens
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-string-type-analyzer phps-mode--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((UNARY . "~")
    (SUBTRACTION . "-")
    (SINGLE_QUOTE . "'")
    (QUESTION_MARK . "?")
    (POW . "^")
    (OPEN_SQUARE_BRACKET . "[")
    (OPEN_PARENTHESIS . "(")
    (OPEN_CURLY_BRACKET . "{")
    (NEGATION . "!")
    (MULTIPLICATION . "*")
    (MODULO . "%")
    (LESSER_THAN . "<")
    (GREATER_THAN . ">")
    (DOT . ".")
    (DIVISION . "/")
    (DOUBLE_QUOTE . "\"")
    (DOLLAR_SIGN . "$")
    (COMMA . ",")
    (COLON . ":")
    (CLOSE_SQUARE_BRACKET . "]")
    (CLOSE_PARENTHESIS . ")")
    (CLOSE_CURLY_BRACKET . "]")
    (BITWISE_OR . "|")
    (BITWISE_AND . "&")
    (BACKTICK . "`")
    (AT . "@")
    (ASSIGN . "=")
    (ADDITION . "+"))
  'punctuation)


;;; Epilogue
;;


(provide 'phps-mode-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; phps-mode-wy.el ends here
