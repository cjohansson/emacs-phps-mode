;;; phps-mode-wy-macros.el --- Macros for Wisent grammar -*- lexical-binding: t -*-

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

;;; Provides functions for the Wisent grammar.


;;; Code:


(require 'semantic/wisent/grammar)

(defun zend_add_class_modifier (class modifier)
  "Add CLASS MODIFIER."
  )

(defun zend_add_member_modifier (member modifier)
  "Add MEMBER MODIFIER."
  )

(defun zend_ast_create (a b c d)
  ;; TODO Understand this
  )

(defun zend_ast_create_assign_op (operator a b)
  "Assign A value of B via OPERATOR."
  ;; TODO Understand this
  )

(defun zend_ast_create_binary_op (operator a b)
  "Perform binary OPERATOR on A and B."
  ;; TODO Understand this
  )

(defun zend_ast_create_cast (operator subject)
  "Cast SUBJECT with OPERATOR."
  ;; TODO Understand this
  )

(defun zend_ast_create_decl (a b c d e f g h)
  ;; TODO Understand this
  )

(defun zend_ast_create_ex (a b c d)
  ;; TODO Understand this
  )

(defun zend_ast_create_list (a b c)
  ;; TODO Understand this
  )

(defun zend_ast_create_zval_from_str (subject)
  "Create zval from SUBJECT."
  ;; TODO Understand this
  )

(defun zend_ast_get_str (subject)
  "Get string from SUBJECT."
  ;; TODO Understand this
  )

(defun zend_lex_tstring(subject)
  ;; TODO Understand this
  )

(defun zend_ast_list_add (list item)
  "Add ITEM to LIST."
  ;; TODO Understand this
  )

(defun zend_ast_list_rtrim (subject)
  "Perform a right trim on SUBJECT."
  ;; TODO Understand this
  )

(defun zend_handle_encoding_declaration (subject)
  ;; TODO Understand this
  )

(defun zend_negate_num_string (subject)
  "Negate num string on SUBJECT."
  ;; TODO Understand this
  )


(provide 'phps-mode-wy-macros)
;;; phps-mode-wy-macros.el ends here
