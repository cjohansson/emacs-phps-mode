;;; phps-mode-ast-bookkeeping.el --- Bookkeeping from AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-ast)


;;; Variables:


(defvar-local
  phps-mode-ast-bookkeeping--index
  nil
  "Bookkeeping for current buffer.")

(defvar
  phps-mode-ast-bookkeeping--superglobal-variable-p
  #s(hash-table size 12 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("$_GET" 1 "$_POST" 1 "$_COOKIE" 1 "$_SESSION" 1 "$_REQUEST" 1 "$GLOBALS" 1 "$_SERVER" 1 "$_FILES" 1 "$_ENV" 1 "$argc" 1 "$argv" 1 "$http_​response_​header" 1))
  "Hash-table of super-global variables.")


;; Functions:


(defun phps-mode-ast-bookkeeping--generate-symbol-scope-string
    (scope name)
  "Generate symbol scope string from SCOPE and NAME."
  (let ((scope-string ""))
    (dolist (bubble (reverse scope))
      (let ((scope-type (plist-get bubble 'type))
            (scope-name (plist-get bubble 'name)))
        (cond

         ((and
           (equal scope-type 'namespace)
           scope-name)
          (setq
           scope-string
           (format
            "%s namespace %s"
            (setq
             scope-string
             scope-name))))

         ((and
           (or (equal scope-type 'class)
               (equal scope-type 'interface))
           scope-name)
          (setq
           scope-string
           (format
            "%s class %s"
            scope-string
            scope-name)))

         ((and
           (equal scope-type 'function)
           (setq
            scope-string
            (format
             "%s function %s"
             scope-string
             scope-name))))

         ((and
           (equal scope-type 'inline-function)
           scope-name)
          (setq
           scope-string
           (format
            "%s anonymous function %s"
            scope-string
            scope-name)))

         ((and
           (equal scope-type 'arrow-function)
           scope-name)
          (setq
           scope-string
           (format
            "%s arrow function %s"
            scope-string
            scope-name)))

         ((and
           (equal scope-type 'static)
           (setq
            scope-string
            (format
             "%s static"
             scope-string))))

         )))
    (setq
     scope-string
     (format
      "%s id %s"
      scope-string
      name))
    scope-string))

(defun phps-mode-ast-bookkeeping--generate-variable-scope-string
    (scope name &optional read-only)
  "Generate variable scope string from SCOPE and NAME and optionally READ-ONLY."
  (let ((scope-strings)
        (bubbles-stack (list (list "" nil (reverse scope))))) ;; scope-string namespace bubbles
    (while bubbles-stack
      (setq
       bubbles-data
       (pop bubbles-stack))
      (let ((scope-string (car bubbles-data))
            (namespace (car (cdr bubbles-data)))
            (bubbles (car (cdr (cdr bubbles-data)))))
        (while bubbles
          (let* ((bubble (pop bubbles))
                 (scope-type (plist-get bubble 'type))
                 (scope-name (plist-get bubble 'name)))
            (cond

             ((and
               (equal scope-type 'namespace)
               scope-name)
              (setq
               namespace
               scope-name))

             ((and
               (or (equal scope-type 'class)
                   (equal scope-type 'interface))
               scope-name)
              (if namespace
                  (setq
                   scope-string
                   (format
                    "%s namespace %s class %s"
                    scope-string
                    namespace
                    scope-name))
                (setq
                 scope-string
                 (format
                  "%s class %s"
                  scope-string
                  scope-name))))

             ((and
               (equal scope-type 'function)
               scope-name)
              (setq
               scope-string
               (format
                "%s function %s"
                scope-string
                scope-name)))

             ((and
               (equal scope-type 'inline-function)
               scope-name)
              (setq
               scope-string
               (format
                "%s anonymous function %s"
                scope-string
                scope-name)))

             ((and
               (equal scope-type 'defined)
               scope-name)
              (when read-only
                ;; Branch off here in alternative scope without this defined context
                ;; but only for read-only contexts
                (push
                 (list
                  scope-string
                  namespace
                  bubbles)
                 bubbles-stack))
              (setq
               scope-string
               (format
                "%s defined %s"
                scope-string
                scope-name)))

             ((and
               (equal scope-type 'arrow-function)
               scope-name)
              (when read-only
                ;; Branch off here in alternative scope without arrow context
                ;; but only for read-only contexts
                (push
                 (list
                  scope-string
                  namespace
                  bubbles)
                 bubbles-stack))
              (setq
               scope-string
               (format
                "%s arrow function %s"
                scope-string
                scope-name)))

             ((and
               (equal scope-type 'static)
               (setq
                scope-string
                (format
                 "%s static"
                 scope-string))))

             )))
        (setq
         scope-string
         (format
          "%s id %s"
          scope-string
          name))
        (push scope-string scope-strings)))
    scope-strings))

(defun phps-mode-ast-bookkeeping--generate ()
  "Generate AST for current buffer."
  (let ((bookkeeping (make-hash-table :test 'equal))
        (bookkeeping-stack phps-mode-ast--tree)
        (inline-function-count 0)
        (arrow-function-count 0)
        (defined-count 0))
    (while bookkeeping-stack
      (let ((item-raw (pop bookkeeping-stack))
            (item)
            (scope))
        (if (listp (car item-raw))
            (progn
              (setq
               scope
               (car item-raw))
              (setq
               item
               (car (cdr item-raw))))
          (setq
           item
           item-raw))

        (let ((type (plist-get item 'ast-type)))
          (cond

           ((equal type 'simple-variable)
            (let ((ids
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get item 'name)
                    t))
                  (object
                   (list
                    (plist-get item 'start)
                    (plist-get item 'end)))
                  (defined-p 0))

              (dolist (id ids)
                (when (gethash id bookkeeping)
                  (setq
                   defined-p
                   1)))

              ;; Is a super-global variable?
              (when (gethash
                     (plist-get item 'name)
                     phps-mode-ast-bookkeeping--superglobal-variable-p)
                (setq
                 defined-p
                 1))
              (puthash
               object
               defined-p
               bookkeeping)))

           ((equal type 'function)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type function name ,name) sub-scope)
              (when-let ((parameter-list (reverse (plist-get item 'parameter-list))))
                (dolist (parameter parameter-list)
                  (let ((ids
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
                          (plist-get parameter 'start)
                          (plist-get parameter 'end))))
                    (dolist (id ids)
                      (puthash
                       id
                       1
                       bookkeeping))
                    (puthash
                     object
                     1
                     bookkeeping))))

              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'method)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type function name ,name) sub-scope)

              ;; TODO should only do this is method is not static
              ;; TODO should only do this if method is not in a interface class
              (let ((this-ids
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      sub-scope
                      "$this")))
                (dolist (this-id this-ids)
                  (puthash
                   this-id
                   1
                   bookkeeping)))
              (when-let ((parameter-list (reverse (plist-get item 'parameter-list))))
                (dolist (parameter parameter-list)
                  (let ((ids
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
                          (plist-get parameter 'start)
                          (plist-get parameter 'end))))
                    (dolist (id ids)
                      (puthash
                       id
                       1
                       bookkeeping))
                    (puthash
                     object
                     1
                     bookkeeping))))

              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'namespace)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type namespace name ,name) sub-scope)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'class)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type class name ,name) sub-scope)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'interface)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type interface name ,name) sub-scope)
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'if)
            (let* ((conditions (reverse (plist-get item 'condition)))
                   (condition-stack conditions)
                   (condition)
                  (found-defined-scope)
                  (sub-scope scope))
              (while condition-stack
                (let ((condition (pop condition-stack)))
                  (when-let ((condition-type (plist-get condition 'ast-type)))
                    (cond

                     ((or (equal condition-type 'boolean-and-expression)
                          (equal condition-type 'boolean-or-expression))
                      (let ((as (reverse (plist-get condition 'a)))
                            (bs (reverse (plist-get condition 'b))))
                        (dolist (b bs)
                          (push b condition-stack))
                        (dolist (a as)
                          (push a condition-stack))))

                     ((equal condition-type 'isset-variables)
                      (let ((sub-scope scope))
                        (unless found-defined-scope
                          (setq defined-count (1+ defined-count))
                          (setq found-defined-scope t))
                        (push `(type defined name ,defined-count) sub-scope)
                        (let ((isset-variables (plist-get condition 'variables)))
                          (dolist (isset-variable isset-variables)
                            (let ((ids
                                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                                    sub-scope
                                    (plist-get isset-variable 'name))))
                              (dolist (id ids)
                                (puthash
                                 id
                                 1
                                 bookkeeping)))))))

                     ((and
                       (equal condition-type 'negated-expression)
                       (equal (plist-get (plist-get condition 'expression) 'ast-type) 'empty-expression))
                      (let ((sub-scope scope))
                        (unless found-defined-scope
                          (setq defined-count (1+ defined-count))
                          (setq found-defined-scope t))
                        (push `(type defined name ,defined-count) sub-scope)
                        (let ((not-empty-variables (plist-get (plist-get condition 'expression) 'variables)))
                          (dolist (not-empty-variable not-empty-variables)
                            (let ((ids
                                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                                    sub-scope
                                    (plist-get not-empty-variable 'name))))
                              (dolist (id ids)
                                (puthash
                                 id
                                 1
                                 bookkeeping)))))))

                     ))))
              (when found-defined-scope
                (push `(type defined name ,defined-count) sub-scope))
              (when-let ((children (reverse (plist-get item 'children))))
                (dolist (child children)
                  (push `(,sub-scope, child) bookkeeping-stack)))
              (when conditions
                (dolist (condition conditions)
                  (push `(,sub-scope ,condition) bookkeeping-stack)))))

           ((equal type 'isset-variables)
            (let ((isset-variables (reverse (plist-get item 'variables))))
              (dolist (isset-variable isset-variables)
                (push `(,scope ,isset-variable) bookkeeping-stack))))

           ((equal type 'empty-expression)
            (let ((not-empty-variables (reverse (plist-get item 'variables))))
              (dolist (not-empty-variable not-empty-variables)
                (push `(,scope ,not-empty-variable) bookkeeping-stack))))

           ((equal type 'foreach)
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((value (plist-get item 'value)))
              (if (equal (plist-get value 'ast-type) 'foreach-referenced-variable)
                  (push
                   (list
                    scope
                    (list
                     'ast-type
                     'assign-variable
                     'key
                     (plist-get value 'variable)))
                   bookkeeping-stack)
                (push
                 (list
                  scope
                  (list
                   'ast-type
                   'assign-variable
                   'key
                   value))
                 bookkeeping-stack)))
            (when-let ((key (plist-get item 'key)))
              (push
               (list
                scope
                (list
                 'ast-type
                 'assign-variable
                 'key
                 key))
               bookkeeping-stack))
            (when-let ((expression (reverse (plist-get item 'expression))))
              (dolist (expr expression)
                (push `(,scope ,expr) bookkeeping-stack))))

           ((equal type 'for)
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'incremental))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'test))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'initial))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack))))

           ((equal type 'while)
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((conditions (reverse (plist-get item 'condition))))
              (dolist (condition conditions)
                (push `(,scope ,condition) bookkeeping-stack))))

           ((equal type 'do-while)
            (when-let ((conditions (reverse (plist-get item 'condition))))
              (dolist (condition conditions)
                (push `(,scope ,condition) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack))))

           ((equal type 'assign-property-variable)
            (let ((ids
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get item 'key)))
                  (object
                   (list
                    (plist-get item 'start)
                    (plist-get item 'end)))
                  (defined 1))
              (dolist (id ids)
                (when-let ((predefined (gethash id bookkeeping)))
                  (setq
                   defined
                   (1+ predefined))))
              (dolist (id ids)
                (puthash
                 id
                 defined
                 bookkeeping))
              (puthash
               object
               defined
               bookkeeping)))

           ;; Infix operations
           ((or
             (equal type 'addition-expression)
             (equal type 'boolean-and-expression)
             (equal type 'boolean-or-expression)
             (equal type 'logical-and-expression)
             (equal type 'logical-or-expression)
             (equal type 'logical-xor-expression))
            (when-let ((bs (reverse (plist-get item 'b))))
              (dolist (b bs)
                (push `(,scope ,b) bookkeeping-stack)))
            (when-let ((as (reverse (plist-get item 'a))))
              (dolist (a as)
                (push `(,scope ,a) bookkeeping-stack))))

           ((equal type 'global-statement)
            (when-let ((global-var-list (reverse (plist-get item 'global-var-list))))
              (dolist (global-var global-var-list)
                (push
                 (list
                  scope
                  (list
                   'ast-type
                   'assign-variable
                   'key
                   global-var))
                 bookkeeping-stack))))

           ((equal type 'assign-variables-from-array)
            (when-let ((keys (reverse (plist-get item 'keys))))
              (dolist (key keys)
                (push
                 (list
                  scope
                  (list
                   'ast-type
                   'assign-variable
                   'key
                   key))
                 bookkeeping-stack))))

           ((and
             (equal type 'assign-variable)
             (plist-get (plist-get item 'key) 'name))
            (let ((ids
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get (plist-get item 'key) 'name)))
                  (object
                   (list
                    (plist-get (plist-get item 'key) 'start)
                    (plist-get (plist-get item 'key) 'end)))
                  (defined 1))
              (dolist (id ids)
                (when-let ((predefined (gethash id bookkeeping)))
                  (setq
                   defined
                   (1+ predefined))))
              (dolist (id ids)
                (puthash
                 id
                 defined
                 bookkeeping))
              (puthash
               object
               defined
               bookkeeping)
              (when-let ((exps (plist-get item 'value)))
                (when (listp exps)
                  (dolist (exp exps)
                    (push `(,scope ,exp) bookkeeping-stack))))))

           ((equal type 'property)
            (let ((subject (plist-get item 'subject))
                  (static-p)
                  (sub-scope scope))
              (when-let ((modifiers (plist-get item 'modifiers)))
                (dolist (modifier modifiers)
                  (when (equal modifier 'static)
                    (setq
                     static-p
                     t))))
              (if (stringp subject)
                  (let ((ids))
                    (when static-p
                      (push '(type static) sub-scope))
                    (setq
                     ids
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      sub-scope
                      subject))
                    (let ((object
                           (list
                            (plist-get item 'start)
                            (plist-get item 'end)))
                          (defined 1))
                      (dolist (id ids)
                        (when-let ((predefined (gethash id bookkeeping)))
                          (setq
                           defined
                           (1+ predefined))))
                      (dolist (id ids)
                        (puthash
                         id
                         defined
                         bookkeeping))
                      (puthash
                       object
                       defined
                       bookkeeping)))
                (push `(,sub-scope ,subject) bookkeeping-stack))))

           ((equal type 'function-call)
            (when-let ((arguments (plist-get item 'argument-list)))
              (dolist (argument arguments)
                (push `(,scope ,argument) bookkeeping-stack))))

           ((equal type 'increment-variable)
            (push `(,scope ,(plist-get item 'variable)) bookkeeping-stack))

           ((equal type 'negated-expression)
            (push `(,scope ,(plist-get item 'expression)) bookkeeping-stack))

           ((equal type 'try)
            (when-let ((children (reverse (plist-get item 'inner-statement-list))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'catch-list))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((children (reverse (plist-get item 'finally-statement))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack))))

           ((equal type 'catch)
            (when-let ((optional-variable
                        (plist-get item 'optional-variable)))
              (let ((ids
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      scope
                      optional-variable)))
                (dolist (id ids)
                  (puthash
                   id
                   1
                   bookkeeping))
                (puthash
                 (list
                  (plist-get item 'optional-variable-start)
                  (plist-get item 'optional-variable-end))
                 1
                 bookkeeping)))
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack))))

           ((equal type 'array-object-dereferencable)
            (let* ((subject (plist-get item 'subject))
                   (property-name (plist-get item 'property))
                   (downcase-subject-name (downcase (plist-get subject 'name)))
                   (property-name (plist-get item 'property-name)))

              (cond

               ((string= downcase-subject-name "$this")
                (puthash
                 (list
                  (plist-get subject 'start)
                  (plist-get subject 'end))
                 1
                 bookkeeping)

                (let* ((sub-scope (cdr scope))
                       (predefined)
                       (variable-ids
                        (phps-mode-ast-bookkeeping--generate-variable-scope-string
                         sub-scope
                         (concat "$" property-name)
                         t))
                       (symbol-id
                        (phps-mode-ast-bookkeeping--generate-symbol-scope-string
                         sub-scope
                         property-name))
                       (bookkeeping-object
                        (list
                         (plist-get item 'property-start)
                         (plist-get item 'property-end))))
                  (when (gethash symbol-id bookkeeping)
                    (setq
                     predefined
                     t))
                  (dolist (variable-id variable-ids)
                    (when (gethash variable-id bookkeeping)
                      (setq
                       predefined
                       t)))
                  (if predefined
                      (puthash
                       bookkeeping-object
                       1
                       bookkeeping)
                    (puthash
                     bookkeeping-object
                     0
                     bookkeeping))))

               (t
                (let ((variable-ids
                       (phps-mode-ast-bookkeeping--generate-variable-scope-string
                        scope
                        (plist-get subject 'name)
                        t))
                      (predefined 0))
                  (dolist (variable-id variable-ids)
                    (when (gethash
                           variable-id
                           bookkeeping)
                      (setq
                       predefined
                       1)))
                  (puthash
                   (list
                    (plist-get subject 'start)
                    (plist-get subject 'end))
                   predefined
                   bookkeeping)))

               )))

           ((equal type 'static-member)
            (let* ((parent-class (plist-get item 'class))
                   (downcased-parent-class (downcase parent-class))
                   (member (plist-get item 'member))
                   (member-type (plist-get member 'ast-type)))

              (cond

               ((or (string= downcased-parent-class "self")
                    (string= downcased-parent-class "static"))

                (cond

                 ((equal member-type 'simple-variable)
                  (let ((sub-scope (cdr scope)))
                    (push '(type static) sub-scope)
                    (let ((predefined)
                          (variable-ids
                           (phps-mode-ast-bookkeeping--generate-variable-scope-string
                            sub-scope
                            (plist-get member 'name)
                            t))
                          (bookkeeping-object
                           (list
                            (plist-get member 'start)
                            (plist-get member 'end))))
                      (dolist (variable-id variable-ids)
                        (when (gethash variable-id bookkeeping)
                          (setq
                           predefined
                           t)))
                      (if predefined
                          (puthash
                           bookkeeping-object
                           1
                           bookkeeping)
                        (puthash
                         bookkeeping-object
                         0
                         bookkeeping)))))

                 )

                ))))

           ((equal type 'static-inline-function)
            (push `(,scope ,(plist-get item 'inline-function)) bookkeeping-stack))

           ((equal type 'arrow-function)
            (let ((sub-scope scope))
              (setq arrow-function-count (1+ arrow-function-count))
              (push `(type arrow-function name ,arrow-function-count) sub-scope)
              (when-let ((inner-statement-list (reverse (plist-get item 'inner-statement-list))))
                (dolist (inner-statement inner-statement-list)
                  (push `(,sub-scope ,inner-statement) bookkeeping-stack)))
              (when-let ((parameter-list (plist-get item 'parameter-list)))
                (dolist (parameter parameter-list)
                  (let ((ids
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
                          (plist-get parameter 'start)
                          (plist-get parameter 'end))))
                    (dolist (id ids)
                      (puthash
                       id
                       1
                       bookkeeping))
                    (puthash
                     object
                     1
                     bookkeeping))))))

           ((equal type 'inline-function)
            (let ((sub-scope scope))
              (setq inline-function-count (1+ inline-function-count))
              (push `(type inline-function name ,inline-function-count) sub-scope)
              (when-let ((inner-statement-list (reverse (plist-get item 'inner-statement-list))))
                (dolist (inner-statement inner-statement-list)
                  (push `(,sub-scope ,inner-statement) bookkeeping-stack)))
              (when-let ((parameter-list (plist-get item 'parameter-list)))
                (dolist (parameter parameter-list)
                  (let ((ids
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
                          (plist-get parameter 'start)
                          (plist-get parameter 'end))))
                    (dolist (id ids)
                      (puthash
                       id
                       1
                       bookkeeping))
                    (puthash
                     object
                     1
                     bookkeeping))))
              (when-let ((lexical-vars (plist-get item 'lexical-vars)))
                (dolist (lexical-var lexical-vars)
                  (let ((ids
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get lexical-var 'name)))
                        (object
                         (list
                          (plist-get lexical-var 'start)
                          (plist-get lexical-var 'end))))
                    (dolist (id ids)
                      (puthash
                       id
                       1
                       bookkeeping))
                    (puthash
                     object
                     1
                     bookkeeping))))))

           ))))
    (setq
     phps-mode-ast-bookkeeping--index
     bookkeeping)

    (message "\nBookkeeping\n:%S\n" bookkeeping)
    phps-mode-ast-bookkeeping--index))


(provide 'phps-mode-ast-bookkeeping)
;;; phps-mode-ast-bookkeeping.el ends here
