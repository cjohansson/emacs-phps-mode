;;; phps-mode-grammar-macro.el

(require 'semantic/wisent/grammar)

(defun phps-mode-grammar-macro-ZEND_AST_CREATE (symbol value &optional attributes)
  "Create AST SYMBOL with VALUE."
  (wisent-grammar-TAG symbol value attributes))

(provide 'phps-mode-grammar-macro)
;;; phps-mode-grammar-macro.el ends here
