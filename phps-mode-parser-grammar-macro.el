;;; phps-mode-parser-grammar-macro.el

(require 'semantic/wisent/grammar)

(defvar phps-mode-parser-grammar-macro-CG-data
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-parser-grammar-macro-CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-parser-grammar-macro-CG-data)
    (gethash subject phps-mode-parser-grammar-macro-CG-data)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE (symbol value &rest attributes)
  "Create AST SYMBOL with VALUE."
  `(wisent-raw-tag
    (semantic-tag ,symbol ,value ,@attributes)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_ASSIGN_OP (symbol subject object)
  "Create AST assignment."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_ASSIGN_OP :object ,object :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_BINARY_OP (symbol subject object)
  "Create AST assignment."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_BINARY_OP :object ,object :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_CAST (symbol subject)
  "Create cast."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_CAST :subject ,subject)))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_EX (symbol operator subject)
  "Create stuff."
  `(wisent-raw-tag
    (semantic-tag ,symbol 'ZEND_AST_EX :operator ,operator :subject ,subject)))


(provide 'phps-mode-grammar-macro)
;;; phps-mode-parser-grammar-macro.el ends here
