;;; phps-automation --- Genereate a Wisent Parser file


;;; Commentary:

;;; Uses a parser to convert LALR Yacc grammar to Wisent grammar

;;; AST should be like this: (root (block (rule (logic))))


;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-wisent-grammar-converter/"))
(require 'emacs-wisent-grammar-converter)

(let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/master/Zend/zend_language_parser.y")
      (php-yacc-file (expand-file-name "zend_language_parser.y"))
      (wisent-destination (expand-file-name "zend_language_parser.wy")))

  ;; Download Yacc if not available
  (unless (file-exists-p php-yacc-file)
    (message "Downloading PHP Yacc grammar..")
    (url-copy-file php-yacc-url php-yacc-file t t)
    (message "Downlad completed"))

  ;; Generate grammar
  (message "Generating Wisent grammar..")
  (emacs-wisent-grammar-converter/generate-grammar-from-filename php-yacc-file wisent-destination)
  (message "Automation completed"))

(provide 'phps-automation2)
;;; phps-automation2.el ends here
