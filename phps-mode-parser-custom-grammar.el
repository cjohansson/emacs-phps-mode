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

;; %empty:
(puthash
 'empty
 (list
  (list (list nil) (lambda() nil)))
 phps-mode-parser-custom-grammar)

;; start:
(puthash
 'start
 (list
  (list
   (list 'top_statement_list)
   (lambda(a) a)))
 phps-mode-parser-custom-grammar)

;; reserved_non_modifiers:
(puthash
 'reserved_non_modifiers
 (list
  (list (list 'T_INCLUDE) (lambda(a) a))
  (list (list 'T_INCLUDE_ONCE) (lambda(a) a))
  (list (list 'T_EVAL) (lambda(a) a))
  (list (list 'T_REQUIRE) (lambda(a) a))
  (list (list 'T_REQUIRE_ONCE) (lambda(a) a))
  (list (list 'T_LOGICAL_OR) (lambda(a) a))
  (list (list 'T_LOGICAL_XOR) (lambda(a) a))
  (list (list 'T_LOGICAL_AND) (lambda(a) a))
  (list (list 'T_INSTANCEOF) (lambda(a) a))
  (list (list 'T_NEW) (lambda(a) a))
  (list (list 'T_CLONE) (lambda(a) a))
  (list (list 'T_EXIT) (lambda(a) a))
  (list (list 'T_IF) (lambda(a) a))
  (list (list 'T_ELSEIF) (lambda(a) a))
  (list (list 'T_ELSE) (lambda(a) a))
  (list (list 'T_ENDIF) (lambda(a) a))
  (list (list 'T_ECHO) (lambda(a) a))
  (list (list 'T_DO) (lambda(a) a))
  (list (list 'T_WHILE) (lambda(a) a))
  (list (list 'T_ENDWHILE) (lambda(a) a))
  (list (list 'T_FOR) (lambda(a) a))
  (list (list 'T_ENDFOR) (lambda(a) a))
  (list (list 'T_FOREACH) (lambda(a) a))
  (list (list 'T_ENDFOREACH) (lambda(a) a))
  (list (list 'T_DECLARE) (lambda(a) a))
  (list (list 'T_ENDDECLARE) (lambda(a) a))
  (list (list 'T_AS) (lambda(a) a))
  (list (list 'T_TRY) (lambda(a) a))
  (list (list 'T_CATCH) (lambda(a) a))
  (list (list 'T_FINALLY) (lambda(a) a))
  (list (list 'T_THROW) (lambda(a) a))
  (list (list 'T_USE) (lambda(a) a))
  (list (list 'T_INSTEADOF) (lambda(a) a))
  (list (list 'T_GLOBAL) (lambda(a) a))
  (list (list 'T_VAR) (lambda(a) a))
  (list (list 'T_UNSET) (lambda(a) a))
  (list (list 'T_ISSET) (lambda(a) a))
  (list (list 'T_EMPTY) (lambda(a) a))
  (list (list 'T_CONTINUE) (lambda(a) a))
  (list (list 'T_GOTO) (lambda(a) a))
  (list (list 'T_FUNCTION) (lambda(a) a))
  (list (list 'T_CONST) (lambda(a) a))
  (list (list 'T_RETURN) (lambda(a) a))
  (list (list 'T_PRINT) (lambda(a) a))
  (list (list 'T_YIELD) (lambda(a) a))
  (list (list 'T_LIST) (lambda(a) a))
  (list (list 'T_SWITCH) (lambda(a) a))
  (list (list 'T_ENDSWITCH) (lambda(a) a))
  (list (list 'T_CASE) (lambda(a) a))
  (list (list 'T_DEFAULT) (lambda(a) a))
  (list (list 'T_BREAK) (lambda(a) a))
  (list (list 'T_ARRAY) (lambda(a) a))
  (list (list 'T_CALLABLE) (lambda(a) a))
  (list (list 'T_EXTENDS) (lambda(a) a))
  (list (list 'T_IMPLEMENTS) (lambda(a) a))
  (list (list 'T_NAMESPACE) (lambda(a) a))
  (list (list 'T_TRAIT) (lambda(a) a))
  (list (list 'T_INTERFACE) (lambda(a) a))
  (list (list 'T_CLASS) (lambda(a) a))
  (list (list 'T_CLASS_C) (lambda(a) a))
  (list (list 'T_TRAIT_C) (lambda(a) a))
  (list (list 'T_FUNC_C) (lambda(a) a))
  (list (list 'T_METHOD_C) (lambda(a) a))
  (list (list 'T_LINE) (lambda(a) a))
  (list (list 'T_FILE) (lambda(a) a))
  (list (list 'T_DIR) (lambda(a) a))
  (list (list 'T_NS_C) (lambda(a) a))
  (list (list 'T_FN) (lambda(a) a))
  )
 phps-mode-parser-custom-grammar)

;; semi_reserved:
(puthash
 'semi_reserved
 (list
  (list (list 'reserved_non_modifiers) (lambda(a) a))
  (list (list 'T_STATIC) (lambda(a) a))
  (list (list 'T_ABSTRACT) (lambda(a) a))
  (list (list 'T_FINAL) (lambda(a) a))
  (list (list 'T_PRIVATE) (lambda(a) a))
  (list (list 'T_PROTECTED) (lambda(a) a))
  (list (list 'T_PUBLIC) (lambda(a) a))
  )
 phps-mode-parser-custom-grammar)

;; TODO Was here


;; top_statement:
(puthash
 'top_statement
 (list
  (list
   (list 'statement)
   (lambda(a) a))
  (list
   (list 'function_declaration_statement)
   (lambda(a) a))
  (list
   (list 'class_declaration_statement)
   (lambda(a) a))
  (list
   (list 'trait_declaration_statement)
   (lambda(a) a))
  (list
   (list 'interface_declaration_statement)
   (lambda(a) a))
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




(provide 'phps-mode-parser-custom-grammar)

;;; phps-mode-parser-custom-grammar.el ends here
