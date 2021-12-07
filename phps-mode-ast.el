;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-parser-sdt)


;;; Variables:

(defvar-local
  phps-mode-ast--parse-trail
  nil
  "Parse trail for current buffer.")

(defvar-local
  phps-mode-ast--tree
  nil
  "Tree for current buffer.")


;; Functions:


(defun phps-mode-ast--generate ()
  "Generate AST for current buffer."
  (let* ((result (phps-mode-parser--parse t))
         (parse-trail (nth 0 result))
         (translation (nth 1 result))
         (namespace)
         (namespace-children)
         (ast))
    (setq
     phps-mode-ast--parse-trail
     parse-trail)

    ;; (message "\nTranslation:\n%S\n\n" translation)

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

    ;; (message "AST:\n%S\n\n" ast)
    (setq
     phps-mode-ast--tree
     ast)
    phps-mode-ast--tree))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
