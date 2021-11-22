;;; phps-mode-parser-sdt.el --- Syntax directed translation for grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-parser)

(defun phps-mode-parser-sdt--get-list-of-object (objects)
  "Get list of OBJECTS."
  (cond

   ((and (listp objects)
         (plist-get objects 'ast-type))
    (list objects))

   ((listp objects)
    objects)

   (t (list objects))))

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
           'ast-type
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
           'ast-type
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

;; statement -> (T_WHILE "(" expr ")" while_statement)
(puthash
 143
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'while
           'condition
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 4 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> (T_DO statement T_WHILE "(" expr ")" ";")
(puthash
 144
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'do-while
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 1 args))
           'condition
           (phps-mode-parser-sdt--get-list-of-object (nth 4 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> (T_FOR "(" for_exprs ";" for_exprs ";" for_exprs ")" for_statement)
(puthash
 145
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'for
           'initial
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'test
           (phps-mode-parser-sdt--get-list-of-object (nth 4 args))
           'incremental
           (phps-mode-parser-sdt--get-list-of-object (nth 6 args))
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 8 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> (T_ECHO echo_expr_list ";")
(puthash
 152
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'echo
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 1 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; statement -> (expr ";")
(puthash
 154
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; statement -> (T_FOREACH "(" expr T_AS foreach_variable ")" foreach_statement)
(puthash
 156
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'foreach
           'expression
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'value
           (nth 4 args)
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 6 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; 157: statement -> (T_FOREACH "(" expr T_AS foreach_variable T_DOUBLE_ARROW foreach_variable ")" foreach_statement)
(puthash
 157
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'foreach
           'expression
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'key
           (nth 4 args)
           'value
           (nth 6 args)
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 8 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; function_declaration_statement -> (function returns_ref T_STRING backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags "{" inner_statement_list "}" backup_fn_flags)
(puthash
 174
 (lambda(args terminals)
   (let ((ast-object
          (list
           'ast-type
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
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 10 args)))))
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
           'ast-type
           'class
           'name
           (nth 1 args)
           'extends
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'implements
           (phps-mode-parser-sdt--get-list-of-object (nth 3 args))
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 5 terminals)))
           'end
           (car (cdr (nth 7 terminals)))
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 6 args)))))
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
           'ast-type
           'interface
           'name
           (nth 1 args)
           'extends
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 4 terminals)))
           'end
           (car (cdr (nth 6 terminals)))
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 5 args)))))
     ;; (message "Interface %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)

;; extends_from -> (%empty)
(puthash
 187
 (lambda(_args _terminals)
   nil
   )
 phps-mode-parser--table-translations)

;; extends_from -> (T_EXTENDS class_name)
(puthash
 188
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; if_stmt_without_else -> (T_IF "(" expr ")" statement)
(puthash
 223
 (lambda(args _terminals)
   ;; (message "if_stmt_without_else: %S" args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'if
           'condition
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 4 args)))))
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
           'ast-type
           'parameter
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

;; attributed_class_statement -> (variable_modifiers optional_type_without_static property_list ";")
(puthash
 278
 (lambda(args terminals)
   (let ((ast-object
          (list
           'ast-type
           'property
           'modifiers
           (phps-mode-parser-sdt--get-list-of-object (nth 0 args))
           'type
           (nth 1 args)
           'subject
           (nth 2 args)
           'start
           (car (cdr (car (nth 2 terminals))))
           'end
           (cdr (cdr (car (nth 2 terminals)))))))
     ast-object))
 phps-mode-parser--table-translations)

;; attributed_class_statement -> (method_modifiers function returns_ref identifier backup_doc_comment "(" parameter_list ")" return_type backup_fn_flags method_body backup_fn_flags)
(puthash
 280
 (lambda(args terminals)
   (let ((ast-object
          (list
           'ast-type
           'method
           'modifiers
           (phps-mode-parser-sdt--get-list-of-object (nth 0 args))
           'returns-reference-p
           (not (equal (nth 2 args) nil))
           'name
           (nth 3 args)
           'parameters
           (nth 6 args)
           'return-type
           (nth 8 args)
           'children
           (phps-mode-parser-sdt--get-list-of-object (nth 10 args))
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

;; 304: variable_modifiers -> (T_VAR)
(puthash
 304
 (lambda(_args _terminals)
   'public)
 phps-mode-parser--table-translations)

;; 309: member_modifier -> (T_PUBLIC)
(puthash
 309
 (lambda(_args _terminals)
   'public)
 phps-mode-parser--table-translations)

;; 310: member_modifier -> (T_PROTECTED)
(puthash
 310
 (lambda(_args _terminals)
   'protected)
 phps-mode-parser--table-translations)

;; 311: member_modifier -> (T_PRIVATE)
(puthash
 311
 (lambda(_args _terminals)
   'private)
 phps-mode-parser--table-translations)

;; 312: member_modifier -> (T_STATIC)
(puthash
 312
 (lambda(_args _terminals)
   'static)
 phps-mode-parser--table-translations)

;; 313: member_modifier -> (T_ABSTRACT)
(puthash
 313
 (lambda(_args _terminals)
   'abstract)
 phps-mode-parser--table-translations)

;; 314: member_modifier -> (T_FINAL)
(puthash
 314
 (lambda(_args _terminals)
   'final)
 phps-mode-parser--table-translations)

;; 302: method_body -> ("{" inner_statement_list "}")
(puthash
 302
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; property -> (T_VARIABLE backup_doc_comment)
(puthash
 317
 (lambda(args _terminals)
   (nth 0 args))
 phps-mode-parser--table-translations)

;; property -> (T_VARIABLE "=" expr backup_doc_comment)
(puthash
 318
 (lambda(args terminals)
   ;; (message "318: %S %S" args terminals)
   (let ((ast-object
          (list
           'ast-type
           'assign-property-variable
           'key
           (nth 0 args)
           'value
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'index
           (car (cdr (nth 0 terminals)))
           'start
           (car (cdr (nth 0 terminals)))
           'end
           (cdr (cdr (nth 0 terminals))))))
     ast-object))
 phps-mode-parser--table-translations)

;; expr -> (variable T_INC)
(puthash
 353
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'increment-variable
           'variable
           (nth 0 args))))
     ast-object))
 phps-mode-parser--table-translations)

;; expr -> (variable "=" expr)
(puthash
 337
 (lambda(args terminals)
   ;; (message "337: %S %S" args terminals)
   (let ((ast-object
          (list
           'ast-type
           'assign-variable
           'key
           (nth 0 args)
           'value
           (phps-mode-parser-sdt--get-list-of-object (nth 2 args))
           'index
           (car (cdr (nth 0 terminals)))
           'start
           (car (cdr (nth 0 terminals)))
           'end
           (cdr (cdr (nth 0 terminals))))))
     ;; (message "Method: %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     ast-object))
 phps-mode-parser--table-translations)


;; function_call -> (name argument_list)
(puthash
 431
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'function_call
           'name
           (nth 0 args)
           'argument_list
           (phps-mode-parser-sdt--get-list-of-object (nth 1 args)))))
     ast-object))
 phps-mode-parser--table-translations)

;; simple_variable -> (T_VARIABLE)
(puthash
 492
 (lambda(args terminals)
   ;; (message "simple_variable: %S %S" args terminals)
   (let ((ast-object
          (list
           'ast-type
           'simple-variable
           'name
           args
           'start
           (car (cdr terminals))
           'end
           (cdr (cdr terminals)))))
     ast-object))
 phps-mode-parser--table-translations)

;; 490: variable -> (array_object_dereferencable T_OBJECT_OPERATOR property_name)
(puthash
 490
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'array-object-dereferencable
           'subject
           (nth 0 args)
           'property-name
           (nth 2 args))))
     ast-object))
 phps-mode-parser--table-translations)

;; static_member -> (class_name T_PAAMAYIM_NEKUDOTAYIM simple_variable)
(puthash
 495
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'static-member
           'class
           (nth 0 args)
           'member
           (nth 2 args))))
     ast-object))
 phps-mode-parser--table-translations)

(provide 'phps-mode-parser-sdt)
;;; phps-mode-parser-sdt.el ends here
