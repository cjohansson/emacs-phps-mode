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


(defvar phps-mode-wy-macros--CG
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-wy-macros-CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-wy-macros--CG)
    (gethash subject phps-mode-wy-macros--CG)))

(defun zend_add_class_modifier (_class _modifier)
  "Add CLASS MODIFIER."
  )

(defun zend_add_member_modifier (_member _modifier)
  "Add MEMBER MODIFIER."
  )

(defun zend_ast_create (_a _b _c _d)
  ;; TODO Understand this
  )

(defun zend_ast_create_assign_op (_operator _a _b)
  "Assign A value of B via OPERATOR."
  ;; TODO Understand this
  )

(defun zend_ast_create_binary_op (_operator _a _b)
  "Perform binary OPERATOR on A and B."
  ;; TODO Understand this
  )

(defun zend_ast_create_cast (_operator _subject)
  "Cast SUBJECT with OPERATOR."
  ;; TODO Understand this
  )

(defun zend_ast_create_decl (_a _b _c _d _e _f _g _h)
  ;; TODO Understand this
  )

(defun zend_ast_create_ex (_a _b _c _d)
  ;; TODO Understand this
  )

(defun zend_ast_create_list (_a _b _c)
  ;; TODO Understand this
  )

(defun zend_ast_create_zval_from_str (_subject)
  "Create zval from SUBJECT."
  ;; TODO Understand this
  )

(defun zend_ast_get_str (_subject)
  "Get string from SUBJECT."
  ;; TODO Understand this
  )

(defun zend_lex_tstring(_subject)
  ;; TODO Understand this
  )

(defun zend_ast_list_add (_list _item)
  "Add ITEM to LIST."
  ;; TODO Understand this
  )

(defun zend_ast_list_rtrim (_subject)
  "Perform a right trim on SUBJECT."
  ;; TODO Understand this
  )

(defun zend_handle_encoding_declaration (_subject)
  ;; TODO Understand this
  )

(defun zend_negate_num_string (_subject)
  "Negate num string on SUBJECT."
  ;; TODO Understand this
  )


(provide 'phps-mode-wy-macros)
;;; phps-mode-wy-macros.el ends here
