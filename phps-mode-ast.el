;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


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

;; Macros


(defun phps-mode-ast--get-list-of-objects (objects)
  "Get list of OBJECTS."
  (if (and (listp objects)
           (plist-get objects 'ast-type))
      (list objects)
    objects))


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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'children
           (phps-mode-ast--get-list-of-objects (nth 4 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 1 args))
           'condition
           (phps-mode-ast--get-list-of-objects (nth 4 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'test
           (phps-mode-ast--get-list-of-objects (nth 4 args))
           'incremental
           (phps-mode-ast--get-list-of-objects (nth 6 args))
           'children
           (phps-mode-ast--get-list-of-objects (nth 8 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 1 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'value
           (nth 4 args)
           'children
           (phps-mode-ast--get-list-of-objects (nth 6 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'key
           (nth 4 args)
           'value
           (nth 6 args)
           'children
           (phps-mode-ast--get-list-of-objects (nth 8 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 10 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'implements
           (phps-mode-ast--get-list-of-objects (nth 3 args))
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 5 terminals)))
           'end
           (car (cdr (nth 7 terminals)))
           'children
           (phps-mode-ast--get-list-of-objects (nth 6 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'index
           (car (cdr (nth 1 terminals)))
           'start
           (car (cdr (nth 4 terminals)))
           'end
           (car (cdr (nth 6 terminals)))
           'children
           (phps-mode-ast--get-list-of-objects (nth 5 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 2 args))
           'children
           (phps-mode-ast--get-list-of-objects (nth 4 args)))))
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
 (lambda(args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'property
           'modifiers
           (phps-mode-ast--get-list-of-objects (nth 0 args))
           'type
           (nth 1 args)
           'subject
           (nth 2 args))))
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
           (phps-mode-ast--get-list-of-objects (nth 0 args))
           'returns-reference-p
           (not (equal (nth 2 args) nil))
           'name
           (nth 3 args)
           'parameters
           (nth 6 args)
           'return-type
           (nth 8 args)
           'children
           (phps-mode-ast--get-list-of-objects (nth 10 args))
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

;; 302: method_body -> ("{" inner_statement_list "}")
(puthash
 302
 (lambda(args _terminals)
   (nth 1 args))
 phps-mode-parser--table-translations)

;; property -> (T_VARIABLE "=" expr backup_doc_comment)
(puthash
 318
 (lambda(args terminals)
   (let ((ast-object
          (list
           'ast-type
           'assign-property
           'name
           (nth 0 args)
           'value
           (nth 2 args)
           'index
           (car (cdr (nth 0 terminals)))
           'start
           (car (cdr (nth 0 terminals)))
           'end
           (cdr (cdr (nth 0 terminals))))))
     ast-object))
 phps-mode-parser--table-translations)

;; expr -> (variable "=" expr)
(puthash
 337
 (lambda(args _terminals)
   ;; (message "expr: %S %S" args _terminals)
   (let ((ast-object
          (list
           'ast-type
           'assign-variable
           'key
           (nth 0 args)
           'value
           (phps-mode-ast--get-list-of-objects (nth 2 args)))))
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
           (phps-mode-ast--get-list-of-objects (nth 1 args)))))
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
           'index
           (car (cdr terminals))
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

           ((plist-get item 'ast-type)
            (if (and
                 (not namespace)
                 (equal (plist-get item 'ast-type) 'namespace)
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
                     (plist-get sub-item 'ast-type))
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
              (item-type (plist-get item 'ast-type))
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
                          (child-type (plist-get child 'ast-type))
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
              (class)
              (function)
              (namespace)
              (variable-namespace "")
              (symbol-namespace ""))
          (if (listp (car item-raw))
              (progn
                (setq
                 class
                 (nth 0 (car item-raw)))
                (setq
                 function
                 (nth 1 (car item-raw)))
                (setq
                 namespace
                 (nth 2 (car item-raw)))
                (setq
                 item
                 (car (cdr item-raw)))
                (when namespace
                  (setq
                   symbol-namespace
                   (format
                    "%s namespace %s"
                    symbol-namespace
                    namespace)))
                (when class
                  (setq
                   symbol-namespace
                   (format
                    "%s class %s"
                    symbol-namespace
                    class))
                  (when namespace
                    (setq
                     variable-namespace
                     (format
                      "%s namespace %s"
                      variable-namespace
                      namespace)))
                  (setq
                   variable-namespace
                   (format
                    "%s class %s"
                    variable-namespace
                    class)))
                (when function
                  (setq
                   symbol-namespace
                   (format
                    "%s function %s"
                    symbol-namespace
                    function))
                  (setq
                   variable-namespace
                   (format
                    "%s function %s"
                    variable-namespace
                    function))))
            (setq
             item
             item-raw))

          (let ((type (plist-get item 'ast-type)))
            (cond

             ((equal type 'simple-variable)
              (let ((id (format
                         "%s id %s"
                         variable-namespace
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
              (let* ((name (plist-get item 'name))
                    (subnamespace
                     (format
                      "%s function %s"
                      symbol-namespace
                      name)))
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

                (when-let ((children (reverse (plist-get item 'children))))
                  (dolist (child children)
                    (push
                     (list
                      (list
                       class
                       name
                       namespace)
                      child)
                     bookkeeping-stack)))))

             ((equal type 'method)
              (let* ((name (plist-get item 'name))
                     (subnamespace
                      (format
                       "%s function %s"
                       symbol-namespace
                       name)))

                ;; TODO should only do this is method is not static
                (let ((this-id
                       (format
                        "%s id %s"
                        subnamespace
                        "$this")))
                  (puthash
                   this-id
                   1
                   bookkeeping))
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

                (when-let ((children (reverse (plist-get item 'children))))
                  (dolist (child children)
                    (push
                     (list
                      (list
                       class
                       name
                       namespace)
                      child)
                     bookkeeping-stack)))))

             ((equal type 'namespace)
              (let* ((name (plist-get item 'name))
                     (subnamespace
                      (format
                       "%s namespace %s"
                       symbol-namespace
                       name)))
                (when-let ((children (reverse (plist-get item 'children))))
                  (dolist (child children)
                    (push
                     (list
                      (list
                       class
                       function
                       name)
                      child)
                     bookkeeping-stack)))))

             ((equal type 'class)
              (let ((name (plist-get item 'name)))
                (when-let ((children (reverse (plist-get item 'children))))
                  (dolist (child children)
                    (push
                     (list
                      (list
                       name
                       function
                       namespace)
                      child)
                     bookkeeping-stack)))))

             ((equal type 'if)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((conditions (reverse (plist-get item 'condition))))
                (dolist (condition conditions)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    condition)
                   bookkeeping-stack))))

             ((equal type 'foreach)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((value (plist-get item 'value)))
                (push
                 (list
                  (list
                   class
                   function
                   namespace)
                  (list
                   'ast-type
                   'assign-variable
                   'key
                   value))
                 bookkeeping-stack))
              (when-let ((key (plist-get item 'key)))
                (push
                 (list
                  (list
                   class
                   function
                   namespace)
                  (list
                   'ast-type
                   'assign-variable
                   'key
                   key))
                 bookkeeping-stack))
              (when-let ((expression (reverse (plist-get item 'expression))))
                (dolist (expr expression)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    expr)
                   bookkeeping-stack))))

             ((equal type 'for)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((children (reverse (plist-get item 'incremental))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((children (reverse (plist-get item 'test))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((children (reverse (plist-get item 'initial))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack))))

             ((equal type 'while)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack)))
              (when-let ((conditions (reverse (plist-get item 'condition))))
                (dolist (condition conditions)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    condition)
                   bookkeeping-stack))))

             ((equal type 'do-while)
              (when-let ((conditions (reverse (plist-get item 'condition))))
                (dolist (condition conditions)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    condition)
                   bookkeeping-stack)))
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    child)
                   bookkeeping-stack))))

             ((equal type 'assign-variable)
              (let ((id (format
                         "%s id %s"
                         variable-namespace
                         (plist-get (plist-get item 'key) 'name)))
                    (object (list
                             (plist-get (plist-get item 'key) 'start)
                             (plist-get (plist-get item 'key) 'end)))
                    (defined 1))
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
                 bookkeeping)
                (when-let ((exps (plist-get item 'value)))
                  (when (listp exps)
                    (dolist (exp exps)
                      (push
                       (list
                        (list
                         class
                         function
                         namespace)
                        exp)
                       bookkeeping-stack))))))

             ((equal type 'property)
              (let ((subject (plist-get item 'subject)))
                (let ((id (format
                           "%s id %s"
                           symbol-namespace
                           (plist-get subject 'name)))
                      (object (list
                               (plist-get subject 'start)
                               (plist-get subject 'end)))
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
                   bookkeeping))))

             ((equal type 'function_call)
              (when-let ((arguments (plist-get item 'argument_list)))
                (dolist (argument arguments)
                  (push
                   (list
                    (list
                     class
                     function
                     namespace)
                    argument)
                   bookkeeping-stack))))

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
