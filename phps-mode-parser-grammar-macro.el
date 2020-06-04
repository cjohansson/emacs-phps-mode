;;; phps-mode-parser-grammar-macro.el

(require 'semantic/wisent/grammar)

(defvar phps-mode-parser-grammar-macro-CG-data
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE (symbol value &rest attributes)
  "Create AST SYMBOL with VALUE."
  (wisent-grammar-TAG symbol value attributes))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_ASSIGN_OP (symbol subject object)
  "Create AST assignment."
  (wisent-grammar-TAG symbol subject object))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_BINARY_OP (symbol subject object)
  "Create AST assignment."
  (wisent-grammar-TAG symbol subject object))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_EX (symbol operator subject)
  "Create stuff."
  (wisent-grammar-TAG symbol operator subject))

(defun phps-mode-parser-grammar-macro-ZEND_AST_CREATE_CAST (symbol subject)
  "Create cast."
  (wisent-grammar-TAG symbol subject))

(defun phps-mode-parser-grammar-macro-CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-parser-grammar-macro-CG-data)
    (gethash subject phps-mode-parser-grammar-macro-CG-data)))


(provide 'phps-mode-grammar-macro)
;;; phps-mode-parser-grammar-macro.el ends here
