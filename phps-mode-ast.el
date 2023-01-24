;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-macros)
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
   phps-mode-parser-sdt-symbol-table-index
   0)
  (setq
   phps-mode-parser-sdt-symbol-imenu--classes
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-imenu--functions
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-imenu--namespaces
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-table
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-table-by-uri
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt--bookkeeping-namespace
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-symbol-stack
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-anonymous-function-count
   0)
  (setq
   phps-mode-parser-sdt--bookkeeping-arrow-function-count
   0)
  (let* ((result (phps-mode-parser--parse t))
         (parse-trail (nth 0 result))
         (translation (nth 1 result)))
    (setq
     phps-mode-ast--parse-trail
     parse-trail)

    (phps-mode-debug-message
     (message "\nTranslation:\n%S\n\n" translation))

    ;; TODO Build imenu  in `phps-mode-parser-sdt-symbol-imenu' by collecting:
    ;; * `phps-mode-parser-sdt-symbol-imenu--classes'
    ;; * `phps-mode-parser-sdt-symbol-imenu--functions'
    ;; * `phps-mode-parser-sdt-symbol-imenu--namespaces'

    (setq
     phps-mode-ast--tree
     translation)
    phps-mode-ast--tree))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
