;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-parser-sdt)


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
  (cond

   ((and (listp objects)
         (plist-get objects 'ast-type))
    (list objects))

   ((listp objects)
    objects)

   (t (list objects))))


;; Functions:


(defun phps-mode-bookkeeping-generate-symbol-namespace
    (&optional namespace class function)
  "Generate symbol namespace for NAMESPACE, CLASS and FUNCTION."
  (let ((symbol-namespace ""))
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
        class)))
    (when function
      (setq
       symbol-namespace
       (format
        "%s function %s"
        symbol-namespace
        function)))
    symbol-namespace))

(defun phps-mode-bookkeeping-generate-variable-namespace
    (&optional namespace class function)
  "Generate variable namespace for NAMESPACE, CLASS and FUNCTION."
  (let ((variable-namespace ""))
    (when class
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
       variable-namespace
       (format
        "%s function %s"
        variable-namespace
        function)))
    variable-namespace))

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
              (item-name (plist-get item 'name))
              (parent))
          (when (and
                 item-index
                 item-name
                 item-type)
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
                          (child-name (plist-get child 'name))
                          (child-index (plist-get child 'index))
                          (subparent))
                      (when (and
                             child-name
                             child-index)
                        (if (and
                             (or
                              (equal child-type 'class)
                              (equal child-type 'interface))
                             grand-children)
                            (progn
                              (dolist (grand-child grand-children)
                                (let ((grand-child-index (plist-get grand-child 'index))
                                      (grand-child-name (plist-get grand-child 'name)))
                                  (when (and
                                         grand-child-index
                                         grand-child-name)
                                    (push
                                     `(,grand-child-name . ,grand-child-index)
                                     subparent))))
                              (when subparent
                                (push
                                 (append
                                  (list child-name)
                                  (reverse subparent))
                                 parent)))
                          (push
                           `(,child-name . ,child-index)
                           parent)))))
                  (when parent
                    (push
                     (append
                      (list item-name)
                      (reverse parent))
                     imenu-index)))
              (push
               `(,item-name . ,item-index)
               imenu-index)))))
      (setq
       phps-mode-ast--imenu
       (reverse imenu-index)))

    (message "imenu:\n%S\n\n" phps-mode-ast--imenu)

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
                (setq
                 symbol-namespace
                 (phps-mode-bookkeeping-generate-symbol-namespace
                  namespace
                  class
                  function))
                (setq
                 variable-namespace
                 (phps-mode-bookkeeping-generate-variable-namespace
                  namespace
                  class
                  function)))
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
              (let* ((name (plist-get item 'name)))
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

             ((equal type 'assign-property-variable)
              (let ((id (format
                         "%s id %s"
                         variable-namespace
                         (plist-get item 'key)))
                    (object (list
                             (plist-get item 'start)
                             (plist-get item 'end)))
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
                 bookkeeping)))

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
              (let ((subject (plist-get item 'subject))
                    (static-p))
                (when-let ((modifiers (plist-get item 'modifiers)))
                  (dolist (modifier modifiers)
                    (when (equal modifier 'static)
                      (setq
                       static-p
                       t))))
                (if (stringp subject)
                    (let ((id))
                      (if static-p
                          (setq
                           id
                           (format
                            "%s static id %s"
                            variable-namespace
                            subject))
                        (setq
                         id
                         (format
                          "%s id %s"
                          variable-namespace
                          subject)))
                      (let ((object (list
                                     (plist-get item 'start)
                                     (plist-get item 'end)))
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
                  (let ((class-namespace class))
                    (when static-p
                      (setq
                       class-namespace
                       (format
                        "%s static"
                        class-namespace)))
                    (push
                     (list
                      (list
                       class-namespace
                       function
                       namespace)
                      subject)
                     bookkeeping-stack)))))

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

             ((equal type 'increment-variable)
              (push
               (list
                (list
                 class
                 function
                 namespace)
                (plist-get item 'variable))
               bookkeeping-stack))

             ((equal type 'array-object-dereferencable)
              (let* ((subject (plist-get item 'subject))
                     (property-name (plist-get item 'property))
                     (downcase-subject (downcase subject)))

                (cond

                 ((string= downcase-subject "$this")
                  (let ((sub-variable-namespace
                         (phps-mode-bookkeeping-generate-variable-namespace
                          namespace
                          nil
                          function))
                        (sub-symbol-namespace
                         (phps-mode-bookkeeping-generate-variable-namespace
                          namespace
                          nil
                          function)))
                    ;; TODO Check bookkeeping here
                    ;; (gethash id bookkeeping)
                    ))

                 )))

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
