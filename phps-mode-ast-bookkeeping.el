;;; phps-mode-ast-bookkeeping.el --- Bookkeeping from AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-ast)


;;; Variables:


(defvar-local
  phps-mode-ast-bookkeeping--index
  nil
  "Bookkeeping string index for current buffer.")

(defvar-local
  phps-mode-ast-bookkeeping--object-index
  nil
  "Bookkeeping object index for current buffer.")

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
            scope-string
            scope-name)))

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
        (bubbles-stack (list (list "" nil nil (reverse scope))))
        (bubbles-data))
    (while bubbles-stack
      (setq
       bubbles-data
       (pop bubbles-stack))
      (let ((scope-string (car bubbles-data))
            (namespace (car (cdr bubbles-data)))
            (class (car (cdr (cdr bubbles-data))))
            (bubbles (car (cdr (cdr (cdr bubbles-data))))))
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
                  scope-name)))
              (setq
               class
               scope-name))

             ((and
               (equal scope-type 'function)
               scope-name)
              (if (and
                   namespace
                   (not class))
                  (setq
                   scope-string
                   (format
                    "%s namespace %s function %s"
                    scope-string
                    namespace
                    scope-name))
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
               (equal scope-type 'defined)
               scope-name)
              (when read-only
                ;; Branch off here in alternative scope without this defined context
                ;; but only for read-only contexts
                (push
                 (list
                  scope-string
                  namespace
                  class
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
                  class
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

(defun phps-mode-ast-bookkeeping--generate (&optional tree)
  "Generate AST for current buffer or optionally for TREE."
  (setq phps-mode-ast-bookkeeping--index phps-mode-parser-sdt-bookkeeping)
  phps-mode-ast-bookkeeping--index)


(provide 'phps-mode-ast-bookkeeping)
;;; phps-mode-ast-bookkeeping.el ends here
