;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


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
  (setq
   phps-mode-parser-sdt-bookkeeping
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt--bookkeeping-namespace
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-namespace-stack
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-symbol-stack
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-anonymous-function-count
   0)
  (let* ((result (phps-mode-parser--parse t))
         (parse-trail (nth 0 result))
         (translation (nth 1 result))
         (namespace)
         (namespace-children)
         (ast))
    (setq
     phps-mode-ast--parse-trail
     parse-trail)

    (message "\nTranslation:\n%S\n\n" translation)

    (setq
     phps-mode-ast--tree
     translation)
    phps-mode-ast--tree))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
