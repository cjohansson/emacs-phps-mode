;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

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


;;; Code:


(require 'phps-mode-parser)


;;; Variables:


(defvar-local
  phps-mode-ast--bookkeeping
  nil
  "Bookkeeping for current buffer.")

(defvar-local
  phps-mode-ast--imenu
  nil
  "Imenu for current buffer.")

(defvar-local
  phps-mode-ast--tree
  nil
  "Tree for current buffer.")

(defvar
  phps-mode-ast--superglobal-variable-p
  #s(hash-table size 12 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("$_GET" 1 "$_POST" 1 "$_COOKIE" 1 "$_SESSION" 1 "$_REQUEST" 1 "$GLOBALS" 1 "$_SERVER" 1 "$_FILES" 1 "$_ENV" 1 "$argc" 1 "$argv" 1 "$http_​response_​header" 1))
  "Hash-table of super-global variables.")

;; Syntax directed translation for grammar


;; top_statement_list -> (top_statement_list top_statement)
(puthash
 79
 (lambda(args _terminals)
   ;; (message "top_statement_list: %S" args)
   (let ((ast-object))
     (if (car args)
         (setq ast-object (append (car args) (cdr args)))
       (setq ast-object (cdr args)))
     ;; (message "ast-object: %S" ast-object)
     ast-object))
 phps-mode-parser--table-translations)

;; top_statement -> (T_NAMESPACE namespace_declaration_name ";")
(puthash
 106
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'namespace
           'name
           (nth 1 args)
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 2 terminals)))
           'end
           'max)))
     ;; (message "Namespace %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; top_statement -> (T_NAMESPACE namespace_declaration_name "{" top_statement_list "}")
(puthash
 107
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'namespace
           'name
           (nth 1 args)
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 2 terminals)))
           'end
           (car (cdr (nth 4 terminals)))
           'children
           (nth 3 args))))
     ;; (message "Namespace %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ;; (message "ast-object: %S" ast-object)
     ast-object))
 phps-mode-parser--table-translations)

;; top_statement -> (T_NAMESPACE "{" top_statement_list "}")
(puthash
 108
 (lambda(args _terminals)
   ;; (message "T_NAMESPACE: %S" args)
   (nth 2 args))
 phps-mode-parser--table-translations)

;; inner_statement_list -> (inner_statement_list inner_statement)
(puthash
 134
 (lambda(args _terminals)
   ;; (message "inner_statement_list: %S" args)
   (let ((ast-object))
     (if (car args)
         (setq ast-object (append (car args) (cdr args)))
       (setq ast-object (cdr args)))
     ;; (message "ast-object: %S" ast-object)
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> ("{" inner_statement_list "}")
(puthash
 140
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; statement -> (T_ECHO echo_expr_list ";")
(puthash
 152
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'type
           'echo
           'body
           (nth 1 args))))
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> (expr ";")
(puthash
 154
 (lambda(args _terminals)
   (car args))
 phps-mode-parser--table-translations)

;; function_declaration_statement -> (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
(puthash
 174
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'function
           'name
           (nth 2 args)
           'index
           (car (cdr (nth 2 terminals)))
           'start
           (car (cdr (nth 9 terminals)))
           'end
           (car (cdr (nth 11 terminals)))
           'returns-reference-p
           (not (equal (nth 1 args) nil))
           'parameters
           (nth 5 args)
           'return-type
           (nth 7 args)
           'body
           (nth 10 args))))
     ;; (message "Function: %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; class_declaration_statement -> (T_CLASS T_STRING extends_from implements_list backup_doc_comment "{" class_statement_list "}")
(puthash
 180
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'class
           'name
           (nth 1 args)
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 5 terminals)))
           'end
           (car (cdr (nth 7 terminals)))
           'children
           (nth 6 args))))
     ;; (message "Class %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; interface_declaration_statement -> (T_INTERFACE T_STRING interface_extends_list backup_doc_comment "{" class_statement_list "}")
(puthash
 186
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'interface
           'name
           (nth 1 args)
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 4 terminals)))
           'end
           (car (cdr (nth 6 terminals)))
           'children
           (nth 5 args))))
     ;; (message "Interface %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; if_stmt_without_else -> (T_IF "(" expr ")" statement)
(puthash
 223
 (lambda(args _terminals)
   ;; (message "if_stmt_without_else: %S" args _terminals)
   (let ((ast-object
          (list
           'type
           'if
           'condition
           (nth 2 args)
           'body
           (nth 4 args))))
     ast-object))
 phps-mode-parser--table-translations)

;; 231: parameter_list -> (non_empty_parameter_list possible_comma)
(puthash
 231
 (lambda(args _terminals)
   (if (listp (car (nth 0 args)))
       (nth 0 args)
     (list (nth 0 args))))
 phps-mode-parser--table-translations)

; 234: non_empty_parameter_list -> (non_empty_parameter_list "," attributed_parameter)
(puthash
 234
 (lambda(args _terminals)
   (list (nth 0 args) (nth 2 args)))
 phps-mode-parser--table-translations)

;; 241: parameter -> (optional_visibility_modifier optional_type_without_static is_reference is_variadic T_VARIABLE backup_doc_comment)
(puthash
 241
 (lambda(args terminals)
   ;; (message "parameter: %S %S" args _terminals)
   (let ((ast-object
          (list
           'visibility
           (nth 0 args)
           'type
           (nth 1 args)
           'is-reference
           (nth 2 args)
           'is-variadic
           (nth 3 args)
           'name
           (nth 4 args)
           'start
           (car (cdr (nth 4 terminals)))
           'end
           (cdr (cdr (nth 4 terminals))))))
     ast-object))
 phps-mode-parser--table-translations)

;; class_statement_list -> (class_statement_list class_statement)
(puthash
 276
 (lambda(args _terminals)
   ;; (message "class_statement_list: %S" args)
   (let ((ast-object))
     (if (car args)
         (setq ast-object (append (car args) (cdr args)))
       (setq ast-object (cdr args)))
     ;; (message "ast-object: %S" ast-object)
     ast-object))
 phps-mode-parser--table-translations)

;; attributed_class_statement -> (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags)
(puthash
 280
 (lambda(args terminals)
   (let ((ast-object
          (list
           'type
           'method
           'name
           (nth 3 args)
           'index
           (car (cdr (nth 3 terminals)))
           'start
           (car (cdr (car (nth 10 terminals))))
           'end
           (cdr (cdr (car (cdr (cdr (nth 10 terminals)))))))))
     ;; (message "Method: %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; expr -> (variable "=" expr)
(puthash
 337
 (lambda(args _terminals)
   ;; (message "expr: %S %S" args _terminals)
   (let ((ast-object
          (list
           'type
           'assign-variable
           'key
           (nth 0 args)
           'value
           (nth 2 args))))
     ;; (message "Method: %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; simple_variable -> (T_VARIABLE)
(puthash
 492
 (lambda(args terminals)
   ;; (message "simple_variable: %S %S" args terminals)
   (let ((ast-object
          (list
           'type
           'variable
           'name
           args
           'start
           (car (cdr terminals))
           'end
           (cdr (cdr terminals)))))
     ast-object))
 phps-mode-parser--table-translations)


;; Functions:


(defun phps-mode-ast-generate ()
  "Generate AST for current buffer."
  (let ((translation (phps-mode-parser-translate))
        (namespace)
        (namespace-children)
        (ast)
        (bookkeeping (make-hash-table :test 'equal)))

    (message "\nTranslation:\n%S\n\n" translation)

    (when translation
      (dolist (item translation)
        (when (listp item)
          (cond

           ((plist-get item 'type)
            (if (and
                 (not namespace)
                 (equal (plist-get item 'type) 'namespace)
                 (equal (plist-get item 'end) 'max))
                (progn
                  ;; (message "Found global namespace: %S" item)
                  (setq
                   namespace
                   item))
              (if namespace
                  (push
                   item
                   namespace-children)
                (push
                 item
                 ast))))

           ((listp (car item))
            (dolist (sub-item item)
              (when (and
                     (listp sub-item)
                     (plist-get sub-item 'type))
                (if namespace
                    (push
                     sub-item
                     namespace-children)
                  (push
                   sub-item
                   ast)))))))))

    (when namespace
      (plist-put
       namespace
       'children
       (reverse namespace-children))
      (push
       namespace
       ast))
    (setq
     ast
     (reverse ast))

    (message "AST:\n%S\n\n" ast)

    (let ((imenu-index))
      (dolist (item ast)
        (let ((children (plist-get item 'children))
              (item-type (plist-get item 'type))
              (item-index (plist-get item 'index))
              (parent))
          (when (and
                 item-type
                 item-index)
            (if (and
                 (or
                  (equal item-type 'namespace)
                  (equal item-type 'class)
                  (equal item-type 'interface))
                 children)
                (progn
                  (dolist (child children)
                    (let ((grand-children (plist-get child 'children))
                          (child-type (plist-get child 'type))
                          (subparent))
                      (if (and
                           (or
                            (equal child-type 'class)
                            (equal child-type 'interface))
                           grand-children)
                          (progn
                            (dolist (grand-child grand-children)
                              (push
                               `(,(plist-get grand-child 'name) . ,(plist-get grand-child 'index))
                               subparent))
                            (push
                             (append
                              (list (plist-get child 'name))
                              (reverse subparent))
                             parent))
                        (push
                         `(,(plist-get child 'name) . ,(plist-get child 'index))
                         parent)))
                    )
                  (push
                   (append
                    (list (plist-get item 'name))
                    (reverse parent))
                   imenu-index))
              (push
               `(,(plist-get item 'name) . ,(plist-get item 'index))
               imenu-index)))))
      (setq
       phps-mode-ast--imenu
       (reverse imenu-index)))

    ;; (message "imenu:\n%S\n\n" phps-mode-ast--imenu)
    

    ;; TODO Build bookkeeping here
    (let ((bookkeeping-stack ast))
      (while bookkeeping-stack
        (let ((item-raw (pop bookkeeping-stack))
              (item)
              (namespace))
          (if (stringp (car item-raw))
              (progn
                (setq
                 namespace
                 (car item-raw))
                (setq
                 item
                 (car (cdr item-raw))))
            (setq
             namespace
             "")
            (setq
             item
             item-raw))
          (let ((type (plist-get item 'type)))
            (cond

             ((equal type 'variable)
              (let ((id (format
                         "%s id %s"
                         namespace
                         (plist-get item 'name)))
                    (object (list
                             (plist-get item 'start)
                             (plist-get item 'end)))
                    (defined-p 0))
                (when (gethash id bookkeeping)
                  (setq
                   defined-p
                   1))

                ;; Is a super-global variable?
                (when (gethash
                       (plist-get item 'name)
                       phps-mode-ast--superglobal-variable-p)
                  (setq
                   defined-p
                   1))
                (puthash
                 object
                 defined-p
                 bookkeeping)))

             ((equal type 'function)
              (let ((subnamespace
                     (format
                      "%s function %s"
                      namespace
                      (plist-get item 'name))))
                (when-let ((parameters (reverse (plist-get item 'parameters))))
                  (dolist (parameter parameters)
                    (let ((id (format
                               "%s id %s"
                               subnamespace
                               (plist-get parameter 'name)))
                          (object (list
                                   (plist-get parameter 'start)
                                   (plist-get parameter 'end))))
                      (puthash
                       id
                       1
                       bookkeeping)
                      (puthash
                       object
                       1
                       bookkeeping))))

                (when-let ((body (reverse (plist-get item 'body))))
                  (dolist (body-item body)
                    (push
                     (list
                      subnamespace
                      body-item)
                     bookkeeping-stack)))))

             ((equal type 'if)
              (let ((condition (plist-get item 'condition)))
                (when (equal (plist-get condition 'type) 'variable)
                  (push
                   (list
                    namespace
                    condition)
                   bookkeeping-stack))))

             ((equal type 'assign-variable)
              (let ((id (format
                         "%s id %s"
                         namespace
                         (plist-get (plist-get item 'key) 'name)))
                    (object (list
                             (plist-get (plist-get item 'key) 'start)
                             (plist-get (plist-get item 'key) 'end)))
                    (defined 1))
                ;; (message "id: %S from %S" id item)
                (when-let ((predefined (gethash id bookkeeping)))
                  (setq
                   defined
                   (1+ predefined)))
                (puthash
                 id
                 defined
                 bookkeeping)
                (puthash
                 object
                 defined
                 bookkeeping)))

             )))))
    (setq
     phps-mode-ast--bookkeeping
     bookkeeping)

    (message "\nBookkeeping\n:%S\n" bookkeeping)

    (setq
     phps-mode-ast--tree
     ast)))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
