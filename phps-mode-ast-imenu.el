;;; phps-mode-ast-imenu.el --- Imenu from AST -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-ast)

(defvar-local
  phps-mode-ast-imenu--index
  nil
  "Imenu for current buffer.")

(defun phps-mode-ast-imenu--generate ()
  "Generate imenu from AST."
  (let ((imenu-index))
    (dolist (item phps-mode-ast--tree)
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
                              (let ((grand-child-index
                                     (plist-get grand-child 'index))
                                    (grand-child-name
                                     (plist-get grand-child 'name)))
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
     phps-mode-ast-imenu--index
     (reverse imenu-index)))

  (message "imenu:\n%S\n\n" phps-mode-ast-imenu--index)
  phps-mode-ast-imenu--index)


(provide 'phps-mode-ast-imenu)
;;; phps-mode-ast-imenu.el ends here
