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
  phps-mode-ast--current-namespace
  nil
  "Current namespace for AST.")

(defvar-local
  phps-mode-ast--current-namespace-children
  nil
  "Children for current namespace for AST.")

(defvar-local
  phps-mode-ast--tree
  nil
  "Tree for current buffer.")

(defvar-local
  phps-mode-ast--imenu
  nil
  "Imenu for current buffer.")

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
     (setq
      phps-mode-ast--current-namespace
      ast-object)
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
     (push
      ast-object
      phps-mode-ast--tree)
     ast-object))
 phps-mode-parser--table-translations)

;; top_statement -> (T_NAMESPACE "{" top_statement_list "}")
(puthash
 108
 (lambda(args _terminals)
   ;; (message "T_NAMESPACE: %S" args)
   (when (nth 2 args)
     (setq
      phps-mode-ast--tree
      (append phps-mode-ast--tree (nth 2 args))))
   (nth 2 args))
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
           (car (cdr (nth 11 terminals))))))
     ;; (message "Function: %S" ast-object)
     ;; (message "args: %S" args)
     ;; (message "terminals: %S" terminals)
     (when phps-mode-ast--current-namespace
       (push
        ast-object
        phps-mode-ast--current-namespace-children))
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
     (when phps-mode-ast--current-namespace
       (push
        ast-object
        phps-mode-ast--current-namespace-children))
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


;; Functions:


(defun phps-mode-ast-generate ()
  "Generate AST for current buffer."
  (setq
   phps-mode-ast--current-namespace
   nil)
  (setq
   phps-mode-ast--tree
   nil)
  (let ((_translation (phps-mode-parser-translate)))
    
    ;; (message "translation: %S" translation)

    (when phps-mode-ast--current-namespace
      (plist-put
       phps-mode-ast--current-namespace
       'children
       (reverse phps-mode-ast--current-namespace-children))
      (push
       phps-mode-ast--current-namespace
       phps-mode-ast--tree))

    (let ((imenu-index))
      (dolist (item phps-mode-ast--tree)
        (let ((children (plist-get item 'children))
              (item-type (plist-get item 'type))
              (parent))
          (if (and
               (or
                (equal item-type 'namespace)
                (equal item-type 'class))
               children)
              (progn
                (dolist (child children)
                  (let ((grand-children (plist-get child 'children))
                        (child-type (plist-get child 'type))
                        (subparent))
                    (if (and
                         (equal child-type 'class)
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
             imenu-index))))
      (setq
       phps-mode-ast--imenu
       (reverse imenu-index)))))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
