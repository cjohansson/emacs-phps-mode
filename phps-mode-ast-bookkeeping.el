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
           (equal scope-type 'class)
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
    (scope name)
  "Generate variable scope string from SCOPE and NAME."
  (let ((scope-string "")
        (namespace))
    (dolist (bubble (reverse scope))
      (let ((scope-type (plist-get bubble 'type))
            (scope-name (plist-get bubble 'name)))
        (cond

         ((and
           (equal scope-type 'namespace)
           scope-name)
          (setq
           namespace
           scope-name))

         ((and
           (equal scope-type 'class)
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

(defun phps-mode-ast-bookkeeping--generate ()
  "Generate AST for current buffer."
  (let ((bookkeeping (make-hash-table :test 'equal))
        (bookkeeping-stack phps-mode-ast--tree)
        (inline-function-count 0))
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
            (let ((id
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get item 'name)))
                  (object
                   (list
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
              (when-let ((parameters (reverse (plist-get item 'parameters))))
                (dolist (parameter parameters)
                  (let ((id
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
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
                  (push `(,sub-scope ,child) bookkeeping-stack)))))

           ((equal type 'method)
            (let ((name (plist-get item 'name))
                  (sub-scope scope))
              (push `(type function name ,name) sub-scope)

              ;; TODO should only do this is method is not static
              (let ((this-id
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      sub-scope
                      "$this")))
                (puthash
                 this-id
                 1
                 bookkeeping))
              (when-let ((parameters (reverse (plist-get item 'parameters))))
                (dolist (parameter parameters)
                  (let ((id
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
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

           ((equal type 'if)
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope, child) bookkeeping-stack)))
            (when-let ((conditions (reverse (plist-get item 'condition))))
              (dolist (condition conditions)
                (push `(,scope ,condition) bookkeeping-stack))))

           ((equal type 'foreach)
            (when-let ((children (reverse (plist-get item 'children))))
              (dolist (child children)
                (push `(,scope ,child) bookkeeping-stack)))
            (when-let ((value (plist-get item 'value)))
              (push
               (list
                scope
                (list
                 'ast-type
                 'assign-variable
                 'key
                 value))
               bookkeeping-stack))
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
            (let ((id
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get item 'key)))
                  (object
                   (list
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
            (let ((id
                   (phps-mode-ast-bookkeeping--generate-variable-scope-string
                    scope
                    (plist-get (plist-get item 'key) 'name)))
                  (object
                   (list
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
                  (let ((id))
                    (when static-p
                      (push '(type static) sub-scope))
                    (setq
                     id
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      sub-scope
                      subject))
                    (let ((object
                           (list
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
                (push `(,sub-scope ,subject) bookkeeping-stack))))

           ((equal type 'function-call)
            (when-let ((arguments (plist-get item 'argument-list)))
              (dolist (argument arguments)
                (push `(,scope ,argument) bookkeeping-stack))))

           ((equal type 'increment-variable)
            (push `(,scope ,(plist-get item 'variable)) bookkeeping-stack))

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
              (let ((id
                     (phps-mode-ast-bookkeeping--generate-variable-scope-string
                      scope
                      optional-variable)))
                (puthash
                 id
                 1
                 bookkeeping)
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
                       (variable-id
                        (phps-mode-ast-bookkeeping--generate-variable-scope-string
                         sub-scope
                         (concat "$" property-name)))
                       (symbol-id
                        (phps-mode-ast-bookkeeping--generate-symbol-scope-string
                         sub-scope
                         property-name))
                        (bookkeeping-object
                         (list
                          (plist-get item 'property-start)
                          (plist-get item 'property-end))))
                  ;; (message "dereferenced: %S %S" variable-id symbol-id)
                  (if (or
                       (gethash variable-id bookkeeping)
                       (gethash symbol-id bookkeeping))
                      (puthash
                       bookkeeping-object
                       1
                       bookkeeping)
                    (puthash
                     bookkeeping-object
                     0
                     bookkeeping))))

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
                    (let ((variable-id
                           (phps-mode-ast-bookkeeping--generate-variable-scope-string
                            sub-scope
                            (plist-get member 'name)))
                          (bookkeeping-object
                           (list
                            (plist-get member 'start)
                            (plist-get member 'end))))
                      (if (gethash variable-id bookkeeping)
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

           ((equal type 'inline-function)
            (setq
             inline-function-count
             (1+ inline-function-count))
            (let ((sub-scope scope))
              (push `(type inline-function name ,inline-function-count) sub-scope)
              (when-let ((inner-statement-list (reverse (plist-get item 'inner-statement-list))))
                (dolist (inner-statement inner-statement-list)
                  (push `(,sub-scope ,inner-statement) bookkeeping-stack)))
              (when-let ((parameter-list (reverse (plist-get item 'parameter-list))))
                (dolist (parameter parameter-list)
                  (let ((id
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get parameter 'name)))
                        (object
                         (list
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
              (when-let ((lexical-vars (reverse (plist-get item 'lexical-vars))))
                (dolist (lexical-var lexical-vars)
                  (let ((id
                         (phps-mode-ast-bookkeeping--generate-variable-scope-string
                          sub-scope
                          (plist-get lexical-var 'name)))
                        (object
                         (list
                          (plist-get lexical-var 'start)
                          (plist-get lexical-var 'end))))
                    (puthash
                     id
                     1
                     bookkeeping)
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
