;;; phps-mode-parser-custom-grammar.el -- Grammar for the parser for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;;; Code:

;; Variables and constants

(defvar phps-mode-parser-custom-grammar nil
  "The current grammar.")

(defconst phps-mode-parser-custom-grammar--start-state 'start
  "The default start state for the grammar.")

(defvar-local phps-mode-parser-custom-grammar--state nil
  "The current state of grammar.")

(defvar-local phps-mode-parser-custom-grammar--state-history nil
  "History of state changes.")

;; Functions

(defun phps-mode-parser-custom-grammar--set-state (state)
  "Setup new STATE."
  (when phps-mode-parser-custom-grammar--state
    (push
     (list (point) phps-mode-parser-custom-grammar--state)
     phps-mode-parser-custom-grammar--state-history))
  (setq phps-mode-parser-custom-grammar--state state))

;; Setup grammar
(setq phps-mode-parser-custom-grammar (make-hash-table :test 'equal))

(puthash
 'empty
 (list (list (list nil)))
 phps-mode-parser-custom-grammar)

(puthash
 'start
 (list (list (list 'top_statement_list)))
 phps-mode-parser-custom-grammar)

(puthash
 'reserved_non_modifiers
 (list
  (list (list 'T_INCLUDE))
  (list (list 'T_INCLUDE_ONCE))
  (list (list 'T_EVAL))
  (list (list 'T_REQUIRE))
  (list (list 'T_REQUIRE_ONCE))
  (list (list 'T_LOGICAL_OR))
  (list (list 'T_LOGICAL_XOR))
  (list (list 'T_LOGICAL_AND))
  (list (list 'T_INSTANCEOF))
  (list (list 'T_NEW))
  (list (list 'T_CLONE))
  (list (list 'T_EXIT))
  (list (list 'T_IF))
  (list (list 'T_ELSEIF))
  (list (list 'T_ELSE))
  (list (list 'T_ENDIF))
  (list (list 'T_ECHO))
  (list (list 'T_DO))
  (list (list 'T_WHILE))
  (list (list 'T_ENDWHILE))
  (list (list 'T_FOR))
  (list (list 'T_ENDFOR))
  (list (list 'T_FOREACH))
  (list (list 'T_ENDFOREACH))
  (list (list 'T_DECLARE))
  (list (list 'T_ENDDECLARE))
  (list (list 'T_AS))
  (list (list 'T_TRY))
  (list (list 'T_CATCH))
  (list (list 'T_FINALLY))
  (list (list 'T_THROW))
  (list (list 'T_USE))
  (list (list 'T_INSTEADOF))
  (list (list 'T_GLOBAL))
  (list (list 'T_VAR))
  (list (list 'T_UNSET))
  (list (list 'T_ISSET))
  (list (list 'T_EMPTY))
  (list (list 'T_CONTINUE))
  (list (list 'T_GOTO))
  (list (list 'T_FUNCTION))
  (list (list 'T_CONST))
  (list (list 'T_RETURN))
  (list (list 'T_PRINT))
  (list (list 'T_YIELD))
  (list (list 'T_LIST))
  (list (list 'T_SWITCH))
  (list (list 'T_ENDSWITCH))
  (list (list 'T_CASE))
  (list (list 'T_DEFAULT))
  (list (list 'T_BREAK))
  (list (list 'T_ARRAY))
  (list (list 'T_CALLABLE))
  (list (list 'T_EXTENDS))
  (list (list 'T_IMPLEMENTS))
  (list (list 'T_NAMESPACE))
  (list (list 'T_TRAIT))
  (list (list 'T_INTERFACE))
  (list (list 'T_CLASS))
  (list (list 'T_CLASS_C))
  (list (list 'T_TRAIT_C))
  (list (list 'T_FUNC_C))
  (list (list 'T_METHOD_C))
  (list (list 'T_LINE))
  (list (list 'T_FILE))
  (list (list 'T_DIR))
  (list (list 'T_NS_C))
  (list (list 'T_FN))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'semi_reserved
 (list
  (list (list 'reserved_non_modifiers))
  (list (list 'T_STATIC))
  (list (list 'T_ABSTRACT))
  (list (list 'T_FINAL))
  (list (list 'T_PRIVATE))
  (list (list 'T_PROTECTED))
  (list (list 'T_PUBLIC))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'identifier
 (list
  (list (list 'T_STRING))
  (list (list 'semi_reserved))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'top_statement_list
 (list
  (list (list 'top_statement_list 'top_statement))
  (list (list 'semi_reserved) (lambda(a) (list 'zval a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'namespace_name
 (list
  (list (list 'T_STRING))
  (list (list 'namespace_name 'T_NS_SEPARATOR 'T_STRING) (lambda(a _b c) (list (concat a c))))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'name
 (list
  (list (list 'namespace_name) (lambda(a) (list 'attr 'phps-mode-parser--ZEND_NAME_NOT_FQ (car (car a)))))
  (list (list 'T_NAMESPACE 'T_NS_SEPARATOR 'namespace_name (lambda(_a _b c) (list 'attr 'phps-mode-parser--ZEND_NAME_RELATIVE c))))
  (list (list 'T_NS_SEPARATOR 'namespace_name) (lambda(_a b) (list 'attr 'phps-mode-parser--ZEND_NAME_FQ b)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'top_statement
 (list
  (list (list 'statement))
  (list (list 'function_declaration_statement))
  (list (list 'class_declaration_statement))
  (list (list 'trait_declaration_statement))
  (list (list 'interface_declaration_statement))
  (list
   (list 'T_HALT_COMPILER "(" ")" ";")
   (lambda(_a) (list 'phps-mode-parser--ZEND_AST_HALT_COMPILER (point))))
  (list
   (list 'T_NAMESPACE 'namespace_name ";")
   (lambda(_a b _c) (list 'phps-mode-parser--ZEND_AST_NAMESPACE b)))
  (list
   (list 'T_NAMESPACE 'namespace_name "{" 'top_statement_list "}")
   (lambda(_a b _c d _e)
     (list (list 'phps-mode-parser--ZEND_AST_NAMESPACE b) d)))
  (list
   (list 'T_NAMESPACE "{" 'top_statement_list "}")
   (lambda(_a _b c _d) (list 'phps-mode-parser--ZEND_AST_NAMESPACE c)))
  (list
   (list 'T_USE 'mixed_group_use_declaration ";")
   (lambda(_a b _c) b))
  (list
   (list 'T_USE 'use_type 'group_use_declaration ";")
   (lambda(_a b c _d) (list 'attr b c)))
  (list
   (list 'T_USE 'use_declarations ";")
   (lambda(_a b _c) (list 'attr b 'phps-mode-parser--ZEND_SYMBOL_CLASS)))
  (list
   (list 'T_CONST 'const_list ";")
   (lambda(_a b _c) b))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'use_type
 (list
  (list (list 'T_FUNCTION) (lambda(_a) (list 'phps-mode-parser--ZEND_SYMBOL_FUNCTION)))
  (list (list 'T_CONST) (lambda(_a) (list 'phps-mode-parser--ZEND_SYMBOL_CONST))))
 phps-mode-parser-custom-grammar)

(puthash
 'group_use_declaration:
 (list
  (list (list 'namespace_name 'T_NS_SEPARATOR "{" 'unprefixed_use_declarations 'possible_comma "}") (lambda (_a _b _c _d _e _f)))
  (list (list 'T_NS_SEPARATOR 'namespace_name 'T_NS_SEPARATOR "{" 'unprefixed_use_declarations 'possible_comma "}") (lambda (_a _b _c _d _e _f _g _h))))
 phps-mode-parser-custom-grammar)

(puthash
 'mixed_group_use_declaration:
 (list
  (list (list 'namespace_name 'T_NS_SEPARATOR "{" 'inline_use_declarations 'possible_comma "}") (lambda(_a _b _c _d _e _f)))
  (list (list 'T_NS_SEPARATOR 'namespace_name 'T_NS_SEPARATOR "{" 'inline_use_declarations 'possible_comma "}") (lambda(_a _b _c _d _e _f _g _h))))
 phps-mode-parser-custom-grammar)

(puthash
 'possible_comma
 (list
  (list (list 'empty))
  (list (list ",")))
 phps-mode-parser-custom-grammar)

(puthash
 'inline_use_declarations
 (list
  (list (list 'inline_use_declarations "," 'inline_use_declaration)(lambda(_a _b _c)))
  (list (list 'inline_use_declaration) (lambda (_a))))
 phps-mode-parser-custom-grammar)

(puthash
 'unprefixed_use_declarations
 (list
  (list (list 'unprefixed_use_declarations "," 'unprefixed_use_declaration) (lambda(_a _b _c)))
  (list (list 'unprefixed_use_declaration) (lambda (_a))))
 phps-mode-parser-custom-grammar)

(puthash
 'use_declaration
 (list
  (list (list 'unprefixed_use_declaration) (lambda(_a)))
  (list (list 'T_NS_SEPARATOR 'unprefixed_use_declaration) (lambda(_a _b)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'const_list
 (list
  (list (list 'const_list "," 'const_decl) (lambda(_a _b _c)))
  (list (list 'const_decl) (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'inner_statement_list
 (list
  (list (list 'inner_statement_list 'inner_statement) (lambda(_a _b)))
  (list (list 'empty)(lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'inner_statement
 (list
  (list (list 'statement) (lambda(_a)))
  (list (list 'function_declaration_statement) (lambda(_a)))
  (list (list 'class_declaration_statement) (lambda(_a)))
  (list (list 'trait_declaration_statement) (lambda(_a)))
  (list (list 'interface_declaration_statement) (lambda(_a)))
  (list (list 'T_HALT_COMPILER "(" ")" ";") (lambda(_a _b _c _d)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'statement
 (list
  (list (list "{" 'inner_statement_list "}") (lambda(_a _b _c)))
  (list (list 'if_stmt) (lambda(_a)))
  (list (list 'alt_if_stmt) (lambda(_a)))
  (list (list 'T_WHILE "(" 'expr ")" 'while_statement) (lambda(_a _b _c _d _e)))
  (list (list 'T_DO 'statement 'T_WHILE "(" 'expr ")" ";") (lambda(_a _b _c _d _e _f _g)))
  (list (list 'T_FOR "(" 'for_exprs ";" 'for_exprs ";" 'for_exprs ")" 'for_statement) (lambda(_a _b _c _d _e _f _g _h _i)))
  (list (list 'T_SWITCH "(" 'expr ")" 'switch_case_list) (lambda(_a)))
  (list (list 'T_BREAK 'optional_expr ";") (lambda(_a)))
  (list (list 'T_CONTINUE 'optional_expr ";") (lambda(_a)))
  (list (list 'T_RETURN 'optional_expr ";") (lambda(_a)))
  (list (list 'T_GLOBAL 'global_var_list ";") (lambda(_a)))
  (list (list 'T_STATIC 'static_var_list ";") (lambda(_a)))
  (list (list 'T_ECHO 'echo_expr_list ";") (lambda(_a)))
  (list (list 'T_INLINE_HTML) (lambda(_a)))
  (list (list 'expr ";") (lambda(_a)))
  (list (list 'T_UNSET "(" 'unset_variables 'possible_comma ")" ";") (lambda(_a)))
  (list (list 'T_FOREACH "(" 'expr 'T_AS 'foreach_variable ")" 'foreach_statement) (lambda(_a)))
  (list (list 'T_FOREACH "(" 'expr 'T_AS 'foreach_variable 'T_DOUBLE_ARROW 'foreach_variable ")" 'foreach_statement) (lambda(_a)))
  (list (list 'T_DECLARE "(" 'const_list ")") (lambda(_a)))
  (list (list ";"))
  (list (list 'T_TRY "{" 'inner_statement_list "}" 'catch_list 'finally_statement) (lambda(_a)))
  (list (list 'T_GOTO 'T_STRING ";") (lambda(_a)))
  (list (list 'T_STRING ",") (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'catch_list
 (list
  (list (list 'empty) (lambda(_a)))
  (list (list 'catch_list 'T_CATCH "(" 'catch_name_list 'optional_variable ")" "{" 'inner_statement_list "}") (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'catch_name_list
 (list
  (list (list 'class_name) (lambda(_a)))
  (list (list 'catch_name_list "|" 'class_name) (lambda(_a)))
  )
phps-mode-parser-custom-grammar)

(puthash
 'optional_variable
 (list
  (list (list 'empty) (lambda(_a)))
  (list (list 'T_VARIABLE) (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'finally_statement
 (list
  (list (list 'empty) (lambda(_a)))
  (list (list 'T_FINALLY "{" 'inner_statement_list "}") (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'unset_variables
 (list
  (list (list 'unset_variable) (lambda(_a)))
  (list (list 'unset_variables "," 'unset_variable) (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)

(puthash
 'unset_variable
 (list
  (list (list 'variable) (lambda(_a)))
  )
 phps-mode-parser-custom-grammar)




(puthash
 'dereferencable_scalar
 (list
  (list (list 'T_ARRAY "(" 'array_pair_list ")") (lambda(_a _b _c _d)))
  (list (list "{" 'array_pair_list "}") (lambda (_a _b _c)))
  (list (list 'T_CONSTANT_ENCAPSED_STRING) (lambda (_a)))
  (list (list "\"" 'encaps_list "\"") (lambda (_a _b _c))))
 phps-mode-parser-custom-grammar)




(provide 'phps-mode-parser-custom-grammar)

;;; phps-mode-parser-custom-grammar.el ends here
