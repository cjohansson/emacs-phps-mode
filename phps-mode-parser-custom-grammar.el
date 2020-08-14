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

;; top_statement_list
(puthash
 'top_statement_list
 (list
  (list
   (list 'top_statement 'top_statement_list)
   (lambda(a b) (list a b)))
  (list
   (list 'empty)
   (lambda() (list 'phps-mode-parser--ZEND_AST_STMT_LIST))))
 phps-mode-parser-custom-grammar)

;; TODO Was here
;; top_statement
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
  )
 phps-mode-parser-custom-grammar)




(provide 'phps-mode-parser-custom-grammar)

;;; phps-mode-parser-custom-grammar.el ends here
