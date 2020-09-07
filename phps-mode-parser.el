;;; phps-mode-parser.el --- Generated parser support file

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; Author: Christian Johansson <christianjohansson@Christians-MacBook-Air.local>
;; Created: 2020-09-07 07:16:47+0200
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
;; generated from the grammar file phps-mode-parser.wy.

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
(eval-and-compile (defconst phps-mode-parser--expected-conflicts
                    nil
                    "The number of expected shift/reduce conflicts in this grammar."))

(defconst phps-mode-parser--keyword-table
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

(defconst phps-mode-parser--token-table
  (semantic-lex-make-type-table
   '(("<no-type>"
      (T_ERROR)
      (\0)
      (END))
     ("punctuation"
      (UNARY . "~")
      (SUBTRACTION . "-")
      (SINGLE_QUOTE . "'")
      (SEMICOLON . ";")
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

(defconst phps-mode-parser--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_LNUMBER T_DNUMBER T_STRING T_VARIABLE T_INLINE_HTML T_ENCAPSED_AND_WHITESPACE T_CONSTANT_ENCAPSED_STRING T_STRING_VARNAME T_NUM_STRING ADDITION ASSIGN AT BACKTICK BITWISE_AND BITWISE_OR CLOSE_CURLY_BRACKET CLOSE_PARENTHESIS CLOSE_SQUARE_BRACKET COLON COMMA DOLLAR_SIGN DOUBLE_QUOTE DIVISION DOT GREATER_THAN LESSER_THAN MODULO MULTIPLICATION NEGATION OPEN_CURLY_BRACKET OPEN_PARENTHESIS OPEN_SQUARE_BRACKET POW QUESTION_MARK SEMICOLON SINGLE_QUOTE SUBTRACTION UNARY END \0 T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE T_LOGICAL_OR T_LOGICAL_XOR T_LOGICAL_AND T_PRINT T_YIELD T_YIELD_FROM T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_COALESCE_EQUAL T_BOOLEAN_OR T_BOOLEAN_AND T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_IS_SMALLER_OR_EQUAL T_IS_GREATER_OR_EQUAL T_SPACESHIP T_SL T_SR T_INSTANCEOF T_INC T_DEC T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST T_NEW T_CLONE T_EXIT T_IF T_ELSEIF T_ELSE T_ENDIF T_ECHO T_DO T_WHILE T_ENDWHILE T_FOR T_ENDFOR T_FOREACH T_ENDFOREACH T_DECLARE T_ENDDECLARE T_AS T_SWITCH T_ENDSWITCH T_CASE T_DEFAULT T_BREAK T_CONTINUE T_GOTO T_FUNCTION T_FN T_CONST T_RETURN T_TRY T_CATCH T_FINALLY T_THROW T_USE T_INSTEADOF T_GLOBAL T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC T_VAR T_UNSET T_ISSET T_EMPTY T_HALT_COMPILER T_CLASS T_TRAIT T_INTERFACE T_EXTENDS T_IMPLEMENTS T_OBJECT_OPERATOR T_DOUBLE_ARROW T_LIST T_ARRAY T_CALLABLE T_LINE T_FILE T_DIR T_CLASS_C T_TRAIT_C T_METHOD_C T_FUNC_C T_COMMENT T_DOC_COMMENT T_OPEN_TAG T_OPEN_TAG_WITH_ECHO T_CLOSE_TAG T_WHITESPACE T_START_HEREDOC T_END_HEREDOC T_DOLLAR_OPEN_CURLY_BRACES T_CURLY_OPEN T_PAAMAYIM_NEKUDOTAYIM T_NAMESPACE T_NS_C T_NS_SEPARATOR T_ELLIPSIS T_COALESCE T_POW T_POW_EQUAL T_BAD_CHARACTER T_ERROR)
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
        (right T_POW %precedence T_CLONE %precedence T_NOELSE %precedence T_ELSEIF %precedence T_ELSE %precedence UNARY %precedence PREC_ARROW_FUNCTION))
       (%empty
        (nil nil))
       (start
        ((top_statement_list)
         (let
             ((r))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--ast $1)
           r)))
       (reserved_non_modifiers
        ((T_INCLUDE))
        ((T_INCLUDE_ONCE))
        ((T_EVAL))
        ((T_REQUIRE))
        ((T_REQUIRE_ONCE))
        ((T_LOGICAL_OR))
        ((T_LOGICAL_XOR))
        ((T_LOGICAL_AND))
        ((T_INSTANCEOF))
        ((T_NEW))
        ((T_CLONE))
        ((T_EXIT))
        ((T_IF))
        ((T_ELSEIF))
        ((T_ELSE))
        ((T_ENDIF))
        ((T_ECHO))
        ((T_DO))
        ((T_WHILE))
        ((T_ENDWHILE))
        ((T_FOR))
        ((T_ENDFOR))
        ((T_FOREACH))
        ((T_ENDFOREACH))
        ((T_DECLARE))
        ((T_ENDDECLARE))
        ((T_AS))
        ((T_TRY))
        ((T_CATCH))
        ((T_FINALLY))
        ((T_THROW))
        ((T_USE))
        ((T_INSTEADOF))
        ((T_GLOBAL))
        ((T_VAR))
        ((T_UNSET))
        ((T_ISSET))
        ((T_EMPTY))
        ((T_CONTINUE))
        ((T_GOTO))
        ((T_FUNCTION))
        ((T_CONST))
        ((T_RETURN))
        ((T_PRINT))
        ((T_YIELD))
        ((T_LIST))
        ((T_SWITCH))
        ((T_ENDSWITCH))
        ((T_CASE))
        ((T_DEFAULT))
        ((T_BREAK))
        ((T_ARRAY))
        ((T_CALLABLE))
        ((T_EXTENDS))
        ((T_IMPLEMENTS))
        ((T_NAMESPACE))
        ((T_TRAIT))
        ((T_INTERFACE))
        ((T_CLASS))
        ((T_CLASS_C))
        ((T_TRAIT_C))
        ((T_FUNC_C))
        ((T_METHOD_C))
        ((T_LINE))
        ((T_FILE))
        ((T_DIR))
        ((T_NS_C))
        ((T_FN)))
       (semi_reserved
        ((reserved_non_modifiers))
        ((T_STATIC))
        ((T_ABSTRACT))
        ((T_FINAL))
        ((T_PRIVATE))
        ((T_PROTECTED))
        ((T_PUBLIC)))
       (identifier
        ((T_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((semi_reserved)
         (let
             ((r)
              (zv))
           (phps-mode-parser-grammar-macro--zend_lex_tstring
            (lambda
              (return)
              (setq zv return)))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_zval zv))
           r)))
       (top_statement_list
        ((top_statement_list top_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r))
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST))
           r)))
       (namespace_name
        ((T_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((namespace_name T_NS_SEPARATOR T_STRING)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_append_str $1 $3))
           r)))
       (name
        ((namespace_name)
         (let
             ((r))
           (setq r $1)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_NAME_NOT_FQ)
           r))
        ((T_NAMESPACE T_NS_SEPARATOR namespace_name)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_NAME_RELATIVE)
           r))
        ((T_NS_SEPARATOR namespace_name)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_NAME_FQ)
           r)))
       (top_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((function_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((class_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((trait_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((interface_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_HALT_COMPILER 40 41 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_HALT_COMPILER
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_long
                                                                   (phps-mode-parser-grammar-macro--zend_get_scanned_file_offset))))
           (phps-mode-parser-grammar-macro--zend_stop_lexing)
           r))
        ((T_NAMESPACE namespace_name 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_NAMESPACE $2 nil))
           (phps-mode-parser-grammar-macro--reset_doc_comment)
           r))
        ((T_NAMESPACE namespace_name
                      (let
                          ((r))
                        (phps-mode-parser-grammar-macro--reset_doc_comment)
                        r)
                      123 top_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_NAMESPACE $2 $5))
           r))
        ((T_NAMESPACE
          (let
              ((r))
            (phps-mode-parser-grammar-macro--reset_doc_comment)
            r)
          123 top_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_NAMESPACE nil $4))
           r))
        ((T_USE mixed_group_use_declaration 59)
         (let
             ((r))
           (setq r $2)
           r))
        ((T_USE use_type group_use_declaration 59)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr $2)
           r))
        ((T_USE use_declarations 59)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_SYMBOL_CLASS)
           r))
        ((T_USE use_type use_declarations 59)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr $2)
           r))
        ((T_CONST const_list 59)
         (let
             ((r))
           (setq r $2)
           r)))
       (use_type
        ((T_FUNCTION)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_SYMBOL_FUNCTION)
           r))
        ((T_CONST)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_SYMBOL_CONST)
           r)))
       (group_use_declaration
        ((namespace_name T_NS_SEPARATOR 123 unprefixed_use_declarations possible_comma 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GROUP_USE $1 $4))
           r))
        ((T_NS_SEPARATOR namespace_name T_NS_SEPARATOR 123 unprefixed_use_declarations possible_comma 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GROUP_USE $2 $5))
           r)))
       (mixed_group_use_declaration
        ((namespace_name T_NS_SEPARATOR 123 inline_use_declarations possible_comma 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GROUP_USE $1 $4))
           r))
        ((T_NS_SEPARATOR namespace_name T_NS_SEPARATOR 123 inline_use_declarations possible_comma 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GROUP_USE $2 $5))
           r)))
       (possible_comma
        ((%empty))
        ((44)))
       (inline_use_declarations
        ((inline_use_declarations 44 inline_use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((inline_use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_USE $1))
           r)))
       (unprefixed_use_declarations
        ((unprefixed_use_declarations 44 unprefixed_use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((unprefixed_use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_USE $1))
           r)))
       (use_declarations
        ((use_declarations 44 use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((use_declaration)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_USE $1))
           r)))
       (inline_use_declaration
        ((unprefixed_use_declaration)
         (let
             ((r))
           (setq r $1)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_SYMBOL_CLASS)
           r))
        ((use_type unprefixed_use_declaration)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr $1)
           r)))
       (unprefixed_use_declaration
        ((namespace_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_USE_ELEM $1 nil))
           r))
        ((namespace_name T_AS T_STRING)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_USE_ELEM $1 $3))
           r)))
       (use_declaration
        ((unprefixed_use_declaration)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_NS_SEPARATOR unprefixed_use_declaration)
         (let
             ((r))
           (setq r $2)
           r)))
       (const_list
        ((const_list 44 const_decl)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((const_decl)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_CONST_DECL $1))
           r)))
       (inner_statement_list
        ((inner_statement_list inner_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r))
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST))
           r)))
       (inner_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((function_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((class_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((trait_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((interface_declaration_statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_HALT_COMPILER 40 41 59)
         (let
             ((r))
           (setq r nil)
           (phps-mode-parser-grammar-macro--zend_throw_exception phps-mode-parser-grammar-macro--zend_ce_compile_error "__HALT_COMPILER() can only be used from the outermost scope" 0)
           (setq r 'phps-mode-parser-grammar-macro--YYERROR)
           r)))
       (statement
        ((123 inner_statement_list 125)
         (let
             ((r))
           (setq r $2)
           r))
        ((if_stmt)
         (let
             ((r))
           (setq r $1)
           r))
        ((alt_if_stmt)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_WHILE 40 expr 41 while_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_WHILE $3 $5))
           r))
        ((T_DO statement T_WHILE 40 expr 41 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DO_WHILE $2 $5))
           r))
        ((T_FOR 40 for_exprs 59 for_exprs 59 for_exprs 41 for_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_FOR $3 $5 $7 $9))
           r))
        ((T_SWITCH 40 expr 41 switch_case_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_SWITCH $3 $5))
           r))
        ((T_BREAK optional_expr 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_BREAK $2))
           r))
        ((T_CONTINUE optional_expr 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONTINUE $2))
           r))
        ((T_RETURN optional_expr 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_RETURN $2))
           r))
        ((T_GLOBAL global_var_list 59)
         (let
             ((r))
           (setq r $2)
           r))
        ((T_STATIC static_var_list 59)
         (let
             ((r))
           (setq r $2)
           r))
        ((T_ECHO echo_expr_list 59)
         (let
             ((r))
           (setq r $2)
           r))
        ((T_INLINE_HTML)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ECHO $1))
           r))
        ((expr 59)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_UNSET 40 unset_variables possible_comma 41 59)
         (let
             ((r))
           (setq r $3)
           r))
        ((T_FOREACH 40 expr T_AS foreach_variable 41 foreach_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_FOREACH $3 $5 nil $7))
           r))
        ((T_FOREACH 40 expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable 41 foreach_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_FOREACH $3 $7 $5 $9))
           r))
        ((T_DECLARE 40 const_list 41
                    (let
                        ((r))
                      (if
                          (not
                           (phps-mode-parser-grammar-macro--zend_handle_encoding_declaration $3))
                          (setq r 'phps-mode-parser-grammar-macro--YYERROR))
                      r)
                    declare_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DECLARE $3 $6))
           r))
        ((59))
        ((T_TRY 123 inner_statement_list 125 catch_list finally_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_TRY $3 $5 $6))
           r))
        ((T_GOTO T_STRING 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GOTO $2))
           r))
        ((T_STRING 58)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_LABEL $1))
           r)))
       (catch_list
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_CATCH_LIST))
           r))
        ((catch_list T_CATCH 40 catch_name_list optional_variable 41 123 inner_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CATCH $4 $5 $8)))
           r)))
       (catch_name_list
        ((class_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_NAME_LIST $1))
           r))
        ((catch_name_list 124 class_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (optional_variable
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_VARIABLE)
         (let
             ((r))
           (setq r $1)
           r)))
       (finally_statement
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_FINALLY 123 inner_statement_list 125)
         (let
             ((r))
           (setq r $3)
           r)))
       (unset_variables
        ((unset_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST $1))
           r))
        ((unset_variables 44 unset_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (unset_variable
        ((variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_UNSET $1))
           r)))
       (function_declaration_statement
        ((function returns_ref T_STRING backup_doc_comment 40 parameter_list 41 return_type backup_fn_flags 123 inner_statement_list 125 backup_fn_flags)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_FUNC_DECL
                                                                       (logior $2 $13)
                                                                       $1 $4
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $3)
                                                                       $6 nil $11 $8))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags $9)
           r)))
       (is_reference
        ((%empty)
         (let
             ((r))
           (setq r 0)
           r))
        ((38)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_PARAM_REF)
           r)))
       (is_variadic
        ((%empty)
         (let
             ((r))
           (setq r 0)
           r))
        ((T_ELLIPSIS)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_PARAM_VARIADIC)
           r)))
       (class_declaration_statement
        ((class_modifiers T_CLASS
                          (let
                              ((r))
                            (setq r
                                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
                            r)
                          T_STRING extends_from implements_list backup_doc_comment 123 class_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS $1 $3 $7
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $4)
                                                                       $5 $6 $9 nil))
           r))
        ((T_CLASS
          (let
              ((r))
            (setq r
                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
            r)
          T_STRING extends_from implements_list backup_doc_comment 123 class_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS 0 $2 $6
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $3)
                                                                       $4 $5 $8 nil))
           r)))
       (class_modifiers
        ((class_modifier)
         (let
             ((r))
           (setq r $1)
           r))
        ((class_modifiers class_modifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_add_class_modifier $1 $2))
           (if
               (not r)
               (setq r 'phps-mode-parser-grammar-macro--YYERROR))
           r)))
       (class_modifier
        ((T_ABSTRACT)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_EXPLICIT_ABSTRACT_CLASS)
           r))
        ((T_FINAL)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_FINAL)
           r)))
       (trait_declaration_statement
        ((T_TRAIT
          (let
              ((r))
            (setq r
                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
            r)
          T_STRING backup_doc_comment 123 class_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS 'phps-mode-parser-grammar-macro--ZEND_ACC_TRAIT $2 $4
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $3)
                                                                       nil nil $6 nil))
           r)))
       (interface_declaration_statement
        ((T_INTERFACE
          (let
              ((r))
            (setq r
                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
            r)
          T_STRING interface_extends_list backup_doc_comment 123 class_statement_list 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS 'phps-mode-parser-grammar-macro--ZEND_ACC_INTERFACE $2 $5
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $3)
                                                                       nil $4 $7 nil))
           r)))
       (extends_from
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_EXTENDS class_name)
         (let
             ((r))
           (setq r $2)
           r)))
       (interface_extends_list
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_EXTENDS class_name_list)
         (let
             ((r))
           (setq r $2)
           r)))
       (implements_list
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_IMPLEMENTS class_name_list)
         (let
             ((r))
           (setq r $2)
           r)))
       (foreach_variable
        ((variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((38 variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_REF $2))
           r))
        ((T_LIST 40 array_pair_list 41)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_LIST)
           r))
        ((91 array_pair_list 93)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_SHORT)
           r)))
       (for_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((58 inner_statement_list T_ENDFOR 59)
         (let
             ((r))
           (setq r $2)
           r)))
       (foreach_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((58 inner_statement_list T_ENDFOREACH 59)
         (let
             ((r))
           (setq r $2)
           r)))
       (declare_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((58 inner_statement_list T_ENDDECLARE 59)
         (let
             ((r))
           (setq r $2)
           r)))
       (switch_case_list
        ((123 case_list 125)
         (let
             ((r))
           (setq r $2)
           r))
        ((123 59 case_list 125)
         (let
             ((r))
           (setq r $3)
           r))
        ((58 case_list T_ENDSWITCH 59)
         (let
             ((r))
           (setq r $2)
           r))
        ((58 59 case_list T_ENDSWITCH 59)
         (let
             ((r))
           (setq r $3)
           r)))
       (case_list
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_SWITCH_LIST))
           r))
        ((case_list T_CASE expr case_separator inner_statement_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_SWITCH_CASE $3 $5)))
           r))
        ((case_list T_DEFAULT case_separator inner_statement_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_SWITCH_CASE nil $4)))
           r)))
       (case_separator
        ((58))
        ((59)))
       (while_statement
        ((statement)
         (let
             ((r))
           (setq r $1)
           r))
        ((58 inner_statement_list T_ENDWHILE 59)
         (let
             ((r))
           (setq r $2)
           r)))
       (if_stmt_without_else
        ((T_IF 40 expr 41 statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_IF
                                                                       (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM $3 $5)))
           r))
        ((if_stmt_without_else T_ELSEIF 40 expr 41 statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM $4 $6)))
           r)))
       (if_stmt
        ((if_stmt_without_else)
         [T_NOELSE]
         (let
             ((r))
           (setq r $1)
           r))
        ((if_stmt_without_else T_ELSE statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM nil $3)))
           r)))
       (alt_if_stmt_without_else
        ((T_IF 40 expr 41 58 inner_statement_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_IF
                                                                       (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM $3 $6)))
           r))
        ((alt_if_stmt_without_else T_ELSEIF 40 expr 41 58 inner_statement_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM $4 $7)))
           r)))
       (alt_if_stmt
        ((alt_if_stmt_without_else T_ENDIF 59)
         (let
             ((r))
           (setq r $1)
           r))
        ((alt_if_stmt_without_else T_ELSE 58 inner_statement_list T_ENDIF 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1
                                                                    (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_IF_ELEM nil $4)))
           r)))
       (parameter_list
        ((non_empty_parameter_list possible_comma)
         (let
             ((r))
           (setq r $1)
           r))
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_PARAM_LIST))
           r)))
       (non_empty_parameter_list
        ((parameter)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_PARAM_LIST $1))
           r))
        ((non_empty_parameter_list 44 parameter)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (parameter
        ((optional_type_without_static is_reference is_variadic T_VARIABLE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_PARAM
                                                                     (logior $2 $3)
                                                                     $1 $4 nil))
           r))
        ((optional_type_without_static is_reference is_variadic T_VARIABLE 61 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_PARAM
                                                                     (logior $2 $3)
                                                                     $1 $4 $6))
           r)))
       (optional_type_without_static
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((type_expr_without_static)
         (let
             ((r))
           (setq r $1)
           r)))
       (type_expr
        ((type)
         (let
             ((r))
           (setq r $1)
           r))
        ((63 type)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr
                                       (logior
                                        (semantic-tag-get-attribute r 'attr)
                                        'phps-mode-parser-grammar-macro--ZEND_TYPE_NULLABLE))
           r))
        ((union_type)
         (let
             ((r))
           (setq r $1)
           r)))
       (type
        ((type_without_static)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_STATIC)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_TYPE 'phps-mode-parser-grammar-macro--IS_STATIC))
           r)))
       (union_type
        ((type 124 type)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 2 'phps-mode-parser-grammar-macro--ZEND_AST_TYPE_UNION $1 $3))
           r))
        ((union_type 124 type)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (type_expr_without_static
        ((type_without_static)
         (let
             ((r))
           (setq r $1)
           r))
        ((63 type_without_static)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr
                                       (logior
                                        (semantic-tag-get-attribute r 'attr)
                                        'phps-mode-parser-grammar-macro--ZEND_TYPE_NULLABLE))
           r))
        ((union_type_without_static)
         (let
             ((r))
           (setq r $1)
           r)))
       (type_without_static
        ((T_ARRAY)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_TYPE 'phps-mode-parser-grammar-macro--IS_ARRAY))
           r))
        ((T_CALLABLE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_TYPE 'phps-mode-parser-grammar-macro--IS_CALLABLE))
           r))
        ((name)
         (let
             ((r))
           (setq r $1)
           r)))
       (union_type_without_static
        ((type_without_static 124 type_without_static)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 2 'phps-mode-parser-grammar-macro--ZEND_AST_TYPE_UNION $1 $3))
           r))
        ((union_type_without_static 124 type_without_static)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (return_type
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((58 type_expr)
         (let
             ((r))
           (setq r $2)
           r)))
       (argument_list
        ((40 41)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_ARG_LIST))
           r))
        ((40 non_empty_argument_list possible_comma 41)
         (let
             ((r))
           (setq r $2)
           r)))
       (non_empty_argument_list
        ((argument)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_ARG_LIST $1))
           r))
        ((non_empty_argument_list 44 argument)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (argument
        ((expr)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_ELLIPSIS expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_UNPACK $2))
           r)))
       (global_var_list
        ((global_var_list 44 global_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((global_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST $1))
           r)))
       (global_var
        ((simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GLOBAL
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1)))
           r)))
       (static_var_list
        ((static_var_list 44 static_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((static_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST $1))
           r)))
       (static_var
        ((T_VARIABLE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC $1 nil))
           r))
        ((T_VARIABLE 61 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC $1 $3))
           r)))
       (class_statement_list
        ((class_statement_list class_statement)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r))
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST))
           r)))
       (class_statement
        ((variable_modifiers optional_type_without_static property_list 59)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP_GROUP $2 $3))
           (semantic-tag-put-attribute r 'attr $1)
           r))
        ((method_modifiers T_CONST class_const_list 59)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr $1)
           r))
        ((T_USE class_name_list trait_adaptations)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_USE_TRAIT $2 $3))
           r))
        ((method_modifiers function returns_ref identifier backup_doc_comment 40 parameter_list 41 return_type backup_fn_flags method_body backup_fn_flags)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_METHOD
                                                                       (logior $3
                                                                               (logior $1 $12))
                                                                       $2 $5
                                                                       (phps-mode-parser-grammar-macro--zend_ast_get_str $4)
                                                                       $7 nil $11 $9))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags $10)
           r)))
       (class_name_list
        ((class_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_NAME_LIST $1))
           r))
        ((class_name_list 44 class_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r)))
       (trait_adaptations
        ((59)
         (let
             ((r))
           (setq r nil)
           r))
        ((123 125)
         (let
             ((r))
           (setq r nil)
           r))
        ((123 trait_adaptation_list 125)
         (let
             ((r))
           (setq r $2)
           r)))
       (trait_adaptation_list
        ((trait_adaptation)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_ADAPTATIONS $1))
           r))
        ((trait_adaptation_list trait_adaptation)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r)))
       (trait_adaptation
        ((trait_precedence 59)
         (let
             ((r))
           (setq r $1)
           r))
        ((trait_alias 59)
         (let
             ((r))
           (setq r $1)
           r)))
       (trait_precedence
        ((absolute_trait_method_reference T_INSTEADOF class_name_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_PRECEDENCE $1 $3))
           r)))
       (trait_alias
        ((trait_method_reference T_AS T_STRING)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_ALIAS $1 $3))
           r))
        ((trait_method_reference T_AS reserved_non_modifiers)
         (let
             ((r)
              (zv))
           (phps-mode-parser-grammar-macro--zend_lex_tstring
            (lambda
              (return)
              (setq zv return)))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_ALIAS $1
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create_zval zv)))
           r))
        ((trait_method_reference T_AS member_modifier identifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_ALIAS $3 $1 $4))
           r))
        ((trait_method_reference T_AS member_modifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_TRAIT_ALIAS $3 $1 nil))
           r)))
       (trait_method_reference
        ((identifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_METHOD_REFERENCE nil $1))
           r))
        ((absolute_trait_method_reference)
         (let
             ((r))
           (setq r $1)
           r)))
       (absolute_trait_method_reference
        ((class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_METHOD_REFERENCE $1 $3))
           r)))
       (method_body
        ((59))
        ((123 inner_statement_list 125)
         (let
             ((r))
           (setq r $2)
           r)))
       (variable_modifiers
        ((non_empty_member_modifiers)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_VAR)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_PUBLIC)
           r)))
       (method_modifiers
        ((%empty)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_PUBLIC)
           r))
        ((non_empty_member_modifiers)
         (let
             ((r))
           (setq r $1)
           (if
               (not
                (logand r 'phps-mode-parser-grammar-macro--ZEND_ACC_PPP_MASK))
               (setq r
                     (logior r 'phps-mode-parser-grammar-macro--ZEND_ACC_PUBLIC)))
           r)))
       (non_empty_member_modifiers
        ((member_modifier)
         (let
             ((r))
           (setq r $1)
           r))
        ((non_empty_member_modifiers member_modifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_add_member_modifier $1 $2))
           (if
               (not r)
               (setq r 'phps-mode-parser-grammar-macro--YYERROR))
           r)))
       (member_modifier
        ((T_PUBLIC)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_PUBLIC)
           r))
        ((T_PROTECTED)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_PROTECTED)
           r))
        ((T_PRIVATE)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_PRIVATE)
           r))
        ((T_STATIC)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_STATIC)
           r))
        ((T_ABSTRACT)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_ABSTRACT)
           r))
        ((T_FINAL)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_FINAL)
           r)))
       (property_list
        ((property_list 44 property)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((property)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_PROP_DECL $1))
           r)))
       (property
        ((T_VARIABLE backup_doc_comment)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP_ELEM $1 nil
                                                                  (if $2
                                                                      (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str $2)
                                                                    nil)))
           r))
        ((T_VARIABLE 61 expr backup_doc_comment)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP_ELEM $1 $3
                                                                  (if $4
                                                                      (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str $4)
                                                                    nil)))
           r)))
       (class_const_list
        ((class_const_list 44 class_const_decl)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((class_const_decl)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS_CONST_DECL $1))
           r)))
       (class_const_decl
        ((identifier 61 expr backup_doc_comment)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONST_ELEM $1 $3
                                                                  (if $4
                                                                      (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str $4)
                                                                    nil)))
           r)))
       (const_decl
        ((T_STRING 61 expr backup_doc_comment)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONST_ELEM $1 $3
                                                                  (if $4
                                                                      (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str $4)
                                                                    nil)))
           r)))
       (echo_expr_list
        ((echo_expr_list 44 echo_expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((echo_expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_STMT_LIST $1))
           r)))
       (echo_expr
        ((expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ECHO $1))
           r)))
       (for_exprs
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((non_empty_for_exprs)
         (let
             ((r))
           (setq r $1)
           r)))
       (non_empty_for_exprs
        ((non_empty_for_exprs 44 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_EXPR_LIST $1))
           r)))
       (anonymous_class
        ((T_CLASS
          (let
              ((r))
            (setq r
                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
            r)
          ctor_arguments extends_from implements_list backup_doc_comment 123 class_statement_list 125)
         (let
             ((r)
              (decl))
           (setq phps-mode-parser-grammar-macro--decl
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLASS 'phps-mode-parser-grammar-macro--ZEND_ACC_ANON_CLASS $2 $6 nil $4 $5 $8 nil))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_NEW phps-mode-parser-grammar-macro--decl $3))
           r)))
       (new_expr
        ((T_NEW class_name_reference ctor_arguments)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_NEW $2 $3))
           r))
        ((T_NEW anonymous_class)
         (let
             ((r))
           (setq r $2)
           r)))
       (expr
        ((variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_LIST 40 array_pair_list 41 61 expr)
         (let
             ((r))
           (semantic-tag-put-attribute $3 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_LIST)
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ASSIGN $3 $6))
           r))
        ((91 array_pair_list 93 61 expr)
         (let
             ((r))
           (semantic-tag-put-attribute $2 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_SHORT)
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ASSIGN $2 $5))
           r))
        ((variable 61 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ASSIGN $1 $3))
           r))
        ((variable 61 38 variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ASSIGN_REF $1 $4))
           r))
        ((T_CLONE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CLONE $2))
           r))
        ((variable T_PLUS_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_ADD $1 $3))
           r))
        ((variable T_MINUS_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_SUB $1 $3))
           r))
        ((variable T_MUL_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_MUL $1 $3))
           r))
        ((variable T_POW_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_POW $1 $3))
           r))
        ((variable T_DIV_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_DIV $1 $3))
           r))
        ((variable T_CONCAT_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_CONCAT $1 $3))
           r))
        ((variable T_MOD_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_MOD $1 $3))
           r))
        ((variable T_AND_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_BW_AND $1 $3))
           r))
        ((variable T_OR_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_BW_OR $1 $3))
           r))
        ((variable T_XOR_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_BW_XOR $1 $3))
           r))
        ((variable T_SL_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_SL $1 $3))
           r))
        ((variable T_SR_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_assign_op 'phps-mode-parser-grammar-macro--ZEND_SR $1 $3))
           r))
        ((variable T_COALESCE_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ASSIGN_COALESCE $1 $3))
           r))
        ((variable T_INC)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_POST_INC $1))
           r))
        ((T_INC variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PRE_INC $2))
           r))
        ((variable T_DEC)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_POST_DEC $1))
           r))
        ((T_DEC variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PRE_DEC $2))
           r))
        ((expr T_BOOLEAN_OR expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_OR $1 $3))
           r))
        ((expr T_BOOLEAN_AND expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_AND $1 $3))
           r))
        ((expr T_LOGICAL_OR expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_OR $1 $3))
           r))
        ((expr T_LOGICAL_AND expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_AND $1 $3))
           r))
        ((expr T_LOGICAL_XOR expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_BOOL_XOR $1 $3))
           r))
        ((expr 124 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_BW_OR $1 $3))
           r))
        ((expr 38 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_BW_AND $1 $3))
           r))
        ((expr 94 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_BW_XOR $1 $3))
           r))
        ((expr 46 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_CONCAT $1 $3))
           r))
        ((expr 43 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_ADD $1 $3))
           r))
        ((expr 45 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_SUB $1 $3))
           r))
        ((expr 42 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_MUL $1 $3))
           r))
        ((expr T_POW expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_POW $1 $3))
           r))
        ((expr 47 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_DIV $1 $3))
           r))
        ((expr 37 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_MOD $1 $3))
           r))
        ((expr T_SL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_SL $1 $3))
           r))
        ((expr T_SR expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_SR $1 $3))
           r))
        ((43 expr)
         [126]
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_UNARY_PLUS $2))
           r))
        ((45 expr)
         [126]
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_UNARY_MINUS $2))
           r))
        ((33 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_UNARY_OP 'phps-mode-parser-grammar-macro--ZEND_BOOL_NOT $2))
           r))
        ((126 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_UNARY_OP 'phps-mode-parser-grammar-macro--ZEND_BW_NOT $2))
           r))
        ((expr T_IS_IDENTICAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_IDENTICAL $1 $3))
           r))
        ((expr T_IS_NOT_IDENTICAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_NOT_IDENTICAL $1 $3))
           r))
        ((expr T_IS_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_EQUAL $1 $3))
           r))
        ((expr T_IS_NOT_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_NOT_EQUAL $1 $3))
           r))
        ((expr 60 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_SMALLER $1 $3))
           r))
        ((expr T_IS_SMALLER_OR_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_IS_SMALLER_OR_EQUAL $1 $3))
           r))
        ((expr 62 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GREATER $1 $3))
           r))
        ((expr T_IS_GREATER_OR_EQUAL expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_GREATER_EQUAL $1 $3))
           r))
        ((expr T_SPACESHIP expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_binary_op 'phps-mode-parser-grammar-macro--ZEND_SPACESHIP $1 $3))
           r))
        ((expr T_INSTANCEOF class_name_reference)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_INSTANCEOF $1 $3))
           r))
        ((40 expr 41)
         (let
             ((r))
           (setq r $2)
           (if
               (equal
                (semantic-tag-get-attribute r 'kind)
                'phps-mode-parser-grammar-macro--ZEND_AST_CONDITIONAL)
               (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_PARENTHESIZED_CONDITIONAL))
           r))
        ((new_expr)
         (let
             ((r))
           (setq r $1)
           r))
        ((expr 63 expr 58 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONDITIONAL $1 $3 $5))
           r))
        ((expr 63 58 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONDITIONAL $1 nil $4))
           r))
        ((expr T_COALESCE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_COALESCE $1 $3))
           r))
        ((internal_functions_in_yacc)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_INT_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_LONG $2))
           r))
        ((T_DOUBLE_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_DOUBLE $2))
           r))
        ((T_STRING_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_STRING $2))
           r))
        ((T_ARRAY_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_ARRAY $2))
           r))
        ((T_OBJECT_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_OBJECT $2))
           r))
        ((T_BOOL_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--_IS_BOOL $2))
           r))
        ((T_UNSET_CAST expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_cast 'phps-mode-parser-grammar-macro--IS_NULL $2))
           r))
        ((T_EXIT exit_expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_EXIT $2))
           r))
        ((64 expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_SILENCE $2))
           r))
        ((scalar)
         (let
             ((r))
           (setq r $1)
           r))
        ((96 backticks_expr 96)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_SHELL_EXEC $2))
           r))
        ((T_PRINT expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PRINT $2))
           r))
        ((T_YIELD)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_YIELD nil nil))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags
                                               (logior
                                                (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags)
                                                'phps-mode-parser-grammar-macro--ZEND_ACC_GENERATOR))
           r))
        ((T_YIELD expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_YIELD $2 nil))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags
                                               (logior
                                                (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags)
                                                'phps-mode-parser-grammar-macro--ZEND_ACC_GENERATOR))
           r))
        ((T_YIELD expr T_DOUBLE_ARROW expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_YIELD $4 $2))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags
                                               (logior
                                                (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags)
                                                'phps-mode-parser-grammar-macro--ZEND_ACC_GENERATOR))
           r))
        ((T_YIELD_FROM expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_YIELD_FROM $2))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags
                                               (logior
                                                (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags)
                                                'phps-mode-parser-grammar-macro--ZEND_ACC_GENERATOR))
           r))
        ((T_THROW expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_THROW $2))
           r))
        ((inline_function)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_STATIC inline_function)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'flags
                                       (logior
                                        (semantic-tag-get-attribute r 'flags)
                                        'phps-mode-parser-grammar-macro--ZEND_ACC_STATIC))
           r)))
       (inline_function
        ((function returns_ref backup_doc_comment 40 parameter_list 41 lexical_vars return_type backup_fn_flags 123 inner_statement_list 125 backup_fn_flags)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_CLOSURE
                                                                       (logior $2 $13)
                                                                       $1 $3
                                                                       (phps-mode-parser-grammar-macro--zend_string_init "{closure}"
                                                                                                                         (-
                                                                                                                          (phps-mode-parser-grammar-macro--sizeof "{closure}")
                                                                                                                          1)
                                                                                                                         0)
                                                                       $5 $7 $11 $8))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags $9)
           r))
        ((fn returns_ref 40 parameter_list 41 return_type backup_doc_comment T_DOUBLE_ARROW backup_fn_flags backup_lex_pos expr backup_fn_flags)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_decl 'phps-mode-parser-grammar-macro--ZEND_AST_ARROW_FUNC
                                                                       (logior $2 $12)
                                                                       $1 $7
                                                                       (phps-mode-parser-grammar-macro--zend_string_init "{closure}"
                                                                                                                         (-
                                                                                                                          (phps-mode-parser-grammar-macro--sizeof "{closure}")
                                                                                                                          1)
                                                                                                                         0)
                                                                       $4 nil
                                                                       (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_RETURN $11)
                                                                       $6))
           (semantic-tag-put-attribute r 'lex_pos $10)
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags $9)
           r)))
       (fn
        ((T_FN)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
           r)))
       #'((T_FUNCTION)
          (let
              ((r))
            (setq r
                  (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--zend_lineno))
            r))
       (backup_doc_comment
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--doc_comment))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--doc_comment nil)
           r)))
       (backup_fn_flags
        ((%empty)
         [PREC_ARROW_FUNCTION]
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--cg phps-mode-parser-grammar-macro--extra_fn_flags))
           (phps-mode-parser-grammar-macro--cg 'phps-mode-parser-grammar-macro--extra_fn_flags 0)
           r)))
       (backup_lex_pos
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--lang_scng phps-mode-parser-grammar-macro--yy_text))
           r)))
       (returns_ref
        ((%empty)
         (let
             ((r))
           (setq r 0)
           r))
        ((38)
         (let
             ((r))
           (setq r 'phps-mode-parser-grammar-macro--ZEND_ACC_RETURN_REFERENCE)
           r)))
       (lexical_vars
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((T_USE 40 lexical_var_list 41)
         (let
             ((r))
           (setq r $3)
           r)))
       (lexical_var_list
        ((lexical_var_list 44 lexical_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((lexical_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_CLOSURE_USES $1))
           r)))
       (lexical_var
        ((T_VARIABLE)
         (let
             ((r))
           (setq r $1)
           r))
        ((38 T_VARIABLE)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_BIND_REF)
           r)))
       (function_call
        ((name argument_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CALL $1 $2))
           r))
        ((class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_CALL $1 $3 $4))
           r))
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM member_name argument_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_CALL $1 $3 $4))
           r))
        ((callable_expr argument_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CALL $1 $2))
           r)))
       (class_name
        ((T_STATIC)
         (let
             ((r)
              (zv))
           (phps-mode-parser-grammar-macro--zval_interned_str
            (lambda
              (return)
              (setq zv return))
            (phps-mode-parser-grammar-macro--zstr_known 'phps-mode-parser-grammar-macro--ZEND_STR_STATIC))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_zval_ex zv 'phps-mode-parser-grammar-macro--ZEND_NAME_NOT_FQ))
           r))
        ((name)
         (let
             ((r))
           (setq r $1)
           r)))
       (class_name_reference
        ((class_name)
         (let
             ((r))
           (setq r $1)
           r))
        ((new_variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((40 expr 41)
         (let
             ((r))
           (setq r $2)
           r)))
       (exit_expr
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((40 optional_expr 41)
         (let
             ((r))
           (setq r $2)
           r)))
       (backticks_expr
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str
                  (phps-mode-parser-grammar-macro--zstr_empty_alloc)))
           r))
        ((T_ENCAPSED_AND_WHITESPACE)
         (let
             ((r))
           (setq r $1)
           r))
        ((encaps_list)
         (let
             ((r))
           (setq r $1)
           r)))
       (ctor_arguments
        ((%empty)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 0 'phps-mode-parser-grammar-macro--ZEND_AST_ARG_LIST))
           r))
        ((argument_list)
         (let
             ((r))
           (setq r $1)
           r)))
       (dereferencable_scalar
        ((T_ARRAY 40 array_pair_list 41)
         (let
             ((r))
           (setq r $3)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_LONG)
           r))
        ((91 array_pair_list 93)
         (let
             ((r))
           (setq r $2)
           (semantic-tag-put-attribute r 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_SHORT)
           r))
        ((T_CONSTANT_ENCAPSED_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((34 encaps_list 34)
         (let
             ((r))
           (setq r $2)
           r)))
       (scalar
        ((T_LNUMBER)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_DNUMBER)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC)
         (let
             ((r))
           (setq r $2)
           r))
        ((T_START_HEREDOC T_END_HEREDOC)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_zval_from_str
                  (phps-mode-parser-grammar-macro--zstr_empty_alloc)))
           r))
        ((T_START_HEREDOC encaps_list T_END_HEREDOC)
         (let
             ((r))
           (setq r $2)
           r))
        ((dereferencable_scalar)
         (let
             ((r))
           (setq r $1)
           r))
        ((constant)
         (let
             ((r))
           (setq r $1)
           r))
        ((class_constant)
         (let
             ((r))
           (setq r $1)
           r)))
       (constant
        ((name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_CONST $1))
           r))
        ((T_LINE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_LINE))
           r))
        ((T_FILE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_FILE))
           r))
        ((T_DIR)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_DIR))
           r))
        ((T_TRAIT_C)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_TRAIT_C))
           r))
        ((T_METHOD_C)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_METHOD_C))
           r))
        ((T_FUNC_C)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_FUNC_C))
           r))
        ((T_NS_C)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_NS_C))
           r))
        ((T_CLASS_C)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_MAGIC_CONST 'phps-mode-parser-grammar-macro--T_CLASS_C))
           r)))
       (class_constant
        ((class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_class_const_or_name $1 $3))
           r))
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM identifier)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_class_const_or_name $1 $3))
           r)))
       (optional_expr
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((expr)
         (let
             ((r))
           (setq r $1)
           r)))
       (variable_class_name
        ((fully_dereferencable)
         (let
             ((r))
           (setq r $1)
           r)))
       (fully_dereferencable
        ((variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((40 expr 41)
         (let
             ((r))
           (setq r $2)
           r))
        ((dereferencable_scalar)
         (let
             ((r))
           (setq r $1)
           r))
        ((class_constant)
         (let
             ((r))
           (setq r $1)
           r)))
       (array_object_dereferencable
        ((fully_dereferencable)
         (let
             ((r))
           (setq r $1)
           r))
        ((constant)
         (let
             ((r))
           (setq r $1)
           r)))
       (callable_expr
        ((callable_variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((40 expr 41)
         (let
             ((r))
           (setq r $2)
           r))
        ((dereferencable_scalar)
         (let
             ((r))
           (setq r $1)
           r)))
       (callable_variable
        ((simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r))
        ((array_object_dereferencable 91 optional_expr 93)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DIM $1 $3))
           r))
        ((array_object_dereferencable 123 expr 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_DIM 'phps-mode-parser-grammar-macro--ZEND_DIM_ALTERNATIVE_SYNTAX $1 $3))
           r))
        ((array_object_dereferencable T_OBJECT_OPERATOR property_name argument_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_METHOD_CALL $1 $3 $4))
           r))
        ((function_call)
         (let
             ((r))
           (setq r $1)
           r)))
       (variable
        ((callable_variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((static_member)
         (let
             ((r))
           (setq r $1)
           r))
        ((array_object_dereferencable T_OBJECT_OPERATOR property_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP $1 $3))
           r)))
       (simple_variable
        ((T_VARIABLE)
         (let
             ((r))
           (setq r $1)
           r))
        ((36 123 expr 125)
         (let
             ((r))
           (setq r $3)
           r))
        ((36 simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $2))
           r)))
       (static_member
        ((class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_PROP $1 $3))
           r))
        ((variable_class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_PROP $1 $3))
           r)))
       (new_variable
        ((simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r))
        ((new_variable 91 optional_expr 93)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DIM $1 $3))
           r))
        ((new_variable 123 expr 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_DIM 'phps-mode-parser-grammar-macro--ZEND_DIM_ALTERNATIVE_SYNTAX $1 $3))
           r))
        ((new_variable T_OBJECT_OPERATOR property_name)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP $1 $3))
           r))
        ((class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_PROP $1 $3))
           r))
        ((new_variable T_PAAMAYIM_NEKUDOTAYIM simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_STATIC_PROP $1 $3))
           r)))
       (member_name
        ((identifier)
         (let
             ((r))
           (setq r $1)
           r))
        ((123 expr 125)
         (let
             ((r))
           (setq r $2)
           r))
        ((simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r)))
       (property_name
        ((T_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((123 expr 125)
         (let
             ((r))
           (setq r $2)
           r))
        ((simple_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r)))
       (array_pair_list
        ((non_empty_array_pair_list)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_rtrim $1))
           r)))
       (possible_array_pair
        ((%empty)
         (let
             ((r))
           (setq r nil)
           r))
        ((array_pair)
         (let
             ((r))
           (setq r $1)
           r)))
       (non_empty_array_pair_list
        ((non_empty_array_pair_list 44 possible_array_pair)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $3))
           r))
        ((possible_array_pair)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY $1))
           r)))
       (array_pair
        ((expr T_DOUBLE_ARROW expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM $3 $1))
           r))
        ((expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM $1 nil))
           r))
        ((expr T_DOUBLE_ARROW 38 variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM 1 $4 $1))
           r))
        ((38 variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM 1 $2 nil))
           r))
        ((T_ELLIPSIS expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_UNPACK $2))
           r))
        ((expr T_DOUBLE_ARROW T_LIST 40 array_pair_list 41)
         (let
             ((r))
           (semantic-tag-put-attribute $5 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_LIST)
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM $5 $1))
           r))
        ((T_LIST 40 array_pair_list 41)
         (let
             ((r))
           (semantic-tag-put-attribute $3 'attr 'phps-mode-parser-grammar-macro--ZEND_ARRAY_SYNTAX_LIST)
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ARRAY_ELEM $3 nil))
           r)))
       (encaps_list
        ((encaps_list encaps_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r))
        ((encaps_list T_ENCAPSED_AND_WHITESPACE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_list_add $1 $2))
           r))
        ((encaps_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 1 'phps-mode-parser-grammar-macro--ZEND_AST_ENCAPS_LIST $1))
           r))
        ((T_ENCAPSED_AND_WHITESPACE encaps_var)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_list 2 'phps-mode-parser-grammar-macro--ZEND_AST_ENCAPS_LIST $1 $2))
           r)))
       (encaps_var
        ((T_VARIABLE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r))
        ((T_VARIABLE 91 encaps_var_offset 93)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DIM
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1)
                                                                  $3))
           r))
        ((T_VARIABLE T_OBJECT_OPERATOR T_STRING)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_PROP
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1)
                                                                  $3))
           r))
        ((T_DOLLAR_OPEN_CURLY_BRACES expr 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $2))
           r))
        ((T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $2))
           r))
        ((T_DOLLAR_OPEN_CURLY_BRACES T_STRING_VARNAME 91 expr 93 125)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_DIM
                                                                  (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $2)
                                                                  $4))
           r))
        ((T_CURLY_OPEN variable 125)
         (let
             ((r))
           (setq r $2)
           r)))
       (encaps_var_offset
        ((T_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((T_NUM_STRING)
         (let
             ((r))
           (setq r $1)
           r))
        ((45 T_NUM_STRING)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_negate_num_string $2))
           r))
        ((T_VARIABLE)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_VAR $1))
           r)))
       (internal_functions_in_yacc
        ((T_ISSET 40 isset_variables possible_comma 41)
         (let
             ((r))
           (setq r $3)
           r))
        ((T_EMPTY 40 expr 41)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_EMPTY $3))
           r))
        ((T_INCLUDE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_INCLUDE_OR_EVAL 'phps-mode-parser-grammar-macro--ZEND_INCLUDE $2))
           r))
        ((T_INCLUDE_ONCE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_INCLUDE_OR_EVAL 'phps-mode-parser-grammar-macro--ZEND_INCLUDE_ONCE $2))
           r))
        ((T_EVAL 40 expr 41)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_INCLUDE_OR_EVAL 'phps-mode-parser-grammar-macro--ZEND_EVAL $3))
           r))
        ((T_REQUIRE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_INCLUDE_OR_EVAL 'phps-mode-parser-grammar-macro--ZEND_REQUIRE $2))
           r))
        ((T_REQUIRE_ONCE expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create_ex 'phps-mode-parser-grammar-macro--ZEND_AST_INCLUDE_OR_EVAL 'phps-mode-parser-grammar-macro--ZEND_REQUIRE_ONCE $2))
           r)))
       (isset_variables
        ((isset_variable)
         (let
             ((r))
           (setq r $1)
           r))
        ((isset_variables 44 isset_variable)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_AND $1 $3))
           r)))
       (isset_variable
        ((expr)
         (let
             ((r))
           (setq r
                 (phps-mode-parser-grammar-macro--zend_ast_create 'phps-mode-parser-grammar-macro--ZEND_AST_ISSET $1))
           r))))
     '(start)))
  "Parser table.")

(defun phps-mode-parser--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((semantic-parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table phps-mode-parser--parse-table
        semantic-debug-parser-source "phps-mode-parser.wy"
        semantic-flex-keywords-obarray phps-mode-parser--keyword-table
        semantic-lex-types-obarray phps-mode-parser--token-table)
  ;; Collect unmatched syntax lexical tokens
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-string-type-analyzer phps-mode-parser--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((UNARY . "~")
    (SUBTRACTION . "-")
    (SINGLE_QUOTE . "'")
    (SEMICOLON . ";")
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
;; NOTE Generated grammar ends here

(provide 'phps-mode-parser)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; phps-mode-parser.el ends here
