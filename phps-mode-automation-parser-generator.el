;;; phps-mode-automation-parser-generator --- Generate a parser for PHP YACC grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'url)

(autoload 'parser-generator-set-look-ahead-number "parser-generator")
(autoload 'parser-generator-set-grammar "parser-generator")
(autoload 'parser-generator-process-grammar "parser-generator")

(autoload 'parser-generator-lr-generate-parser-tables "parser-generator-lr")
(autoload 'parser-generator-lr-translate "parser-generator-lr")

(defvar
  phps-mode-automation-parser-generator--attributes
  nil
  "Attributes of grammar.")

(defvar
  phps-mode-automation-parser-generator--start
  nil
  "Start position of grammar.")

(defvar
  phps-mode-automation-parser-generator--terminals
  nil
  "Terminals of grammar.")

(defvar
  phps-mode-automation-parser-generator--non-terminals
  nil
  "Non-terminals of grammar.")

(defvar
  phps-mode-automation-parser-generator--symbols
  nil
  "Symbols of grammar.")

(defvar
  phps-mode-automation-parser-generator--context-sensitive-attributes
  nil
  "Context-sensitive attributes found in grammar.")

(defvar
  phps-mode-automation-parser-generator--production-lhs
  nil
  "LHS of productions of grammar.")

(defun phps-mode-automation-parser-generator--ensure-yacc-grammar-is-available ()
  "If grammar is not available, download it."
  (let ((php-yacc-url
         "https://raw.githubusercontent.com/php/php-src/PHP-8.3/Zend/zend_language_parser.y")
        (php-yacc-file
         (expand-file-name "zend_language_parser.y")))

    ;; Download YACC if not available (seems to now work in batch mode for some reason)
    (unless (file-exists-p php-yacc-file)
      (message
       ";; Downloading PHP 8.3 YACC grammar.. since %S does not exists" php-yacc-file)
      (with-current-buffer (url-retrieve-synchronously php-yacc-url)
        (write-file php-yacc-file))
      (message
       ";; Download of PHP 8.3 YACC grammar completed"))

    (unless (file-exists-p php-yacc-file)
      (error "Missing PHP YACC grammar at %s!" php-yacc-file))))

(defun phps-mode-automation-parser-generator--grammar ()
  "Generate productions here."
  (phps-mode-automation-parser-generator--ensure-yacc-grammar-is-available)

  (setq
   phps-mode-automation-parser-generator--start
   nil)
  (setq
    phps-mode-automation-parser-generator--terminals
    nil)
  (setq
   phps-mode-automation-parser-generator--non-terminals
   nil)
  (setq
   phps-mode-automation-parser-generator--production-lhs
   (make-hash-table :test 'equal))
  (setq
   phps-mode-automation-parser-generator--symbols
   (make-hash-table :test 'equal))
  (setq
   phps-mode-automation-parser-generator--context-sensitive-attributes
   nil)

  (parser-generator-set-look-ahead-number
   1)
  (when (boundp 'parser-generator--e-identifier)
    (setq
     parser-generator--e-identifier
     nil))
  (when (boundp 'parser-generator--global-attributes)
    (setq
     parser-generator--global-attributes
     nil))
  (when (boundp 'parser-generator-lr--global-precedence-attributes)
    (setq
     parser-generator-lr--global-precedence-attributes
     nil))
  (when (boundp 'parser-generator-lr--context-sensitive-precedence-attribute)
    (setq
     parser-generator-lr--context-sensitive-precedence-attribute
     nil))
  (when (boundp 'parser-generator--global-declaration)
    (setq
     parser-generator--global-declaration
     nil))
  (parser-generator-set-grammar
   '(
     (Start Productions-Block Productions-Delimiter Productions Productions Production LHS RHSS RHS RHS-Symbol RHS-Symbols Logic Symbol)
     (productions-delimiter "|" lhs-symbol logic symbol literal)
     (
      (Start
       Productions-Block)
      (Productions-Block
       (Productions-Delimiter Productions Productions-Delimiter
                              (lambda(args _terminals) (format "'(\n\n%s\n\n)" (nth 1 args))))
       )
      (Productions-Delimiter
       (productions-delimiter
        (lambda(args _terminals) ""))
       )
      (Productions
       (Production
        (lambda(args _terminals) (format "%s" args)))
       (Productions Production
                    (lambda(args _terminals) (format "%s\n\n%s" (nth 0 args) (nth 1 args))))
       )
      (Production
       (LHS RHSS
            (lambda(args _terminals)
              ;; Store distinct LHS
              (unless (gethash
                       (intern (nth 0 args))
                       phps-mode-automation-parser-generator--production-lhs)
                (puthash
                 (intern (nth 0 args))
                 t
                 phps-mode-automation-parser-generator--production-lhs)

                ;; If no start is defined - define this as start
                (unless phps-mode-automation-parser-generator--start
                  (setq
                   phps-mode-automation-parser-generator--start
                   (intern (nth 0 args)))))

              (format " (%s\n  %s\n )" (nth 0 args) (nth 1 args))))
       )
      (LHS
       (lhs-symbol
        (lambda(args _terminals) (format "%s" (substring args 0 -1))))
       )
      (RHSS
       (RHS
        (lambda(args _terminals) (format "%s" args)))
       (RHSS "|" RHS
             (lambda(args _terminals) (format "%s\n  %s" (nth 0 args) (nth 2 args))))
       )
      (RHS
       (RHS-Symbol
        (lambda(args _terminals) (format "%s" args)))
       (RHS-Symbols
        (lambda(args _terminals)
          (if (string-match-p " " args)
              (format "(%s)" args)
            (format "%s" args))))
       )
      (RHS-Symbols
       (RHS-Symbol
        RHS-Symbols
        (lambda (args _terminals)
          (if (string= (nth 1 args) "")
              (format "%s" (nth 0 args))
            (format "%s %s" (nth 0 args) (nth 1 args)))))
       (RHS-Symbol
        RHS-Symbol
        (lambda (args _terminals)
          (if (string= (nth 1 args) "")
              (format "%s" (nth 0 args))
            (format "%s %s" (nth 0 args) (nth 1 args)))))
       )
      (RHS-Symbol
       Logic
       Symbol)
      (Logic
       (logic
        (lambda(args _terminals) ""))
       )
      (Symbol
       (symbol
        (lambda(args _terminals)
          ;; Store distinct symbols
          (unless (gethash
                   (intern args)
                   phps-mode-automation-parser-generator--symbols)
            (puthash
             (intern args)
             t
             phps-mode-automation-parser-generator--symbols))
          
          (format "%s" args)))
       (literal
        (lambda(args _terminals)
          ;; Store distinct symbols
          (unless (gethash
                   (format "%s" (substring args 1 2))
                   phps-mode-automation-parser-generator--symbols)
            (puthash
             (format "%s" (substring args 1 2))
             t
             phps-mode-automation-parser-generator--symbols))

          (format "%S" (substring args 1 2))))
       )
      )
     Start))

  (when (boundp 'parser-generator-lex-analyzer--function)
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer "*buffer*"
         (let ((token)
               (move-to-index-flag))
           (when
               (<
                index
                (point-max))
             (goto-char
              index)

             ;; Skip white-space(s)
             (when (looking-at-p "[\t\n; ]+")
               (when
                   (search-forward-regexp "[^\t\n; ]" nil t)
                 (forward-char -1)
                 (setq move-to-index-flag (point))))

             (cond

              ((looking-at "\\(/\\*\\)")
               (let ((comment-start (match-beginning 0))
                     (comment-end
                      (search-forward-regexp "\\(\\*/\\)" nil t)))
                 (unless comment-end
                   (error
                    "Failed to find end of comment started at %S (1)"
                    comment-start))
                 (setq move-to-index-flag comment-end)))

              ((looking-at "\\({\\)")
               (let ((nesting-stack 1)
                     (logic-start (match-beginning 0))
                     (logic-end)
                     (continue t))
                 (forward-char 1)
                 (while (and
                         continue
                         (> nesting-stack 0)
                         (< (point) (point-max)))
                   (let ((next-stop (search-forward-regexp "\\({\\|}\\|/\\*\\)" nil t)))
                     (let ((match (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
                       (cond

                        ((not next-stop)
                         (setq
                          continue
                          nil))

                        ((string= match "{")
                         (setq
                          nesting-stack
                          (1+ nesting-stack)))

                        ((string= match "}")
                         (setq
                          nesting-stack
                          (1- nesting-stack))
                         (when
                             (= nesting-stack 0)
                           (setq
                            logic-end
                            (point))))

                        ((string= match "/*")
                         (let (
                               (comment-start (match-beginning 0))
                               (comment-end
                                (search-forward-regexp "\\*/" nil t)))
                           (unless comment-end
                             (error
                              "Failed to find end of comment inside logic that started at %S (2))"
                              comment-start))))

                        ))))
                 (unless logic-end
                   (error
                    "Failed to find end of logic started at %S"
                    logic-start))
                 (setq
                  token
                  `(logic ,logic-start . ,logic-end))))

              ((looking-at "\\(|\\)")
               (setq
                token
                `(
                  ,(buffer-substring-no-properties
                    (match-beginning 0)
                    (match-end 0))
                  ,(match-beginning 0)
                  . ,(match-end 0))))

              ((looking-at "\\(%%\\)")
               (setq
                token
                `(productions-delimiter ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\([a-zA-Z_]+:\\)")
               (setq
                token
                `(lhs-symbol ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\(%?[a-zA-Z_]+\\)")
               (setq
                token
                `(symbol ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\('.'\\)")
               (setq
                token
                `(literal ,(match-beginning 0) . ,(match-end 0))))

              ))

           (list token move-to-index-flag))))))

  (when (boundp 'parser-generator-lex-analyzer--get-function)
    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer "*buffer*"
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (let ((symbol
                    (buffer-substring-no-properties start end)))
               (when
                   (string-match-p "^\\([0-9]+\\.[0-9]+\\|[0-9]+\\)$" symbol)
                 (setq
                  symbol
                  (string-to-number symbol)))
               symbol)))))))

  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)
    (insert-file-contents (expand-file-name "zend_language_parser.y"))
    (goto-char (point-min))
    (let ((delimiter-start (search-forward "%%")))
      (setq
       delimiter-start
       (- delimiter-start 2))
      (kill-region (point-min) delimiter-start))
    (let ((delimiter-start (search-forward "%%")))
      (kill-region delimiter-start (point-max)))
    (goto-char (point-min))

    ;; (message "Buffer:\n%S" (buffer-substring-no-properties (point-min) (point-max)))
    
    (let ((productions (eval (car (read-from-string (parser-generator-lr-translate))))))

      (let ((context-sensitive-attributes)
            (context-sensitive-attributes-map (make-hash-table :test 'equal)))
        (maphash
         (lambda (k _v)
           (if (gethash
                k
                phps-mode-automation-parser-generator--production-lhs)
               (push
                k
                phps-mode-automation-parser-generator--non-terminals)

             ;; Store context-sensitive attributes and terminals
             (if
                 (and
                  (symbolp k)
                  (string-match-p "%" (symbol-name k)))
                 (unless
                     (or
                      (equal k '%empty)
                      (gethash
                       k
                       context-sensitive-attributes-map))
                   (push
                    k
                    context-sensitive-attributes)
                   (puthash
                    k
                    t
                    context-sensitive-attributes-map))
               (push
                k
                phps-mode-automation-parser-generator--terminals))))
         phps-mode-automation-parser-generator--symbols)
        (setq
         phps-mode-automation-parser-generator--context-sensitive-attributes
         context-sensitive-attributes))
      (kill-buffer)

      (list
       phps-mode-automation-parser-generator--non-terminals
       phps-mode-automation-parser-generator--terminals
       productions
       phps-mode-automation-parser-generator--start))))

(defun phps-mode-automation-parser-generator--global-declaration ()
  "Generate global declaration here."
  (require 'parser-generator-lr)
  (phps-mode-automation-parser-generator--ensure-yacc-grammar-is-available)

  (setq
   phps-mode-automation-parser-generator--attributes
   (make-hash-table :test 'equal))
  (parser-generator-set-look-ahead-number
   1)
  (when (boundp 'parser-generator--e-identifier)
    (setq
     parser-generator--e-identifier
     nil))
  (when (boundp 'parser-generator--global-attributes)
    (setq
     parser-generator--global-attributes
     nil))
  (when (boundp 'parser-generator-lr--global-precedence-attributes)
    (setq
     parser-generator-lr--global-precedence-attributes
     nil))
  (when (boundp 'parser-generator-lr--context-sensitive-precedence-attribute)
    (setq
     parser-generator-lr--context-sensitive-precedence-attribute
     '%prec))
  (when (boundp 'parser-generator--global-declaration)
    (setq
     parser-generator--global-declaration
     nil))
  (parser-generator-set-grammar
   '(
     (Start Declarations-Block Declarations Declaration Type Symbols Symbol)
     (type symbol literal comment)
     (
      (Start
       Declarations-Block
       )
      (Declarations-Block
       (Declarations
        (lambda(args _terminals)
          (format "'(\n%s)" args))))
      (Declarations
       (Declaration
        (lambda(args _terminals) (format "%s" args)))
       (Declaration Declarations
                    (lambda(args _terminals) (format "%s%s" (nth 0 args) (nth 1 args))))
       )
      (Declaration
       (comment
        (lambda(args _terminals) ""))
       (Type Symbols
             (lambda(args _terminals) (format "  (%s %s)\n" (nth 0 args) (nth 1 args))))
       )
      (Type
       (type
        (lambda(args _terminals)
          (unless
              (gethash
               args
               phps-mode-automation-parser-generator--attributes)
            (puthash
             args
             t
             phps-mode-automation-parser-generator--attributes))
          (format "%s" args)))
       )
      (Symbols
       (Symbol
        (lambda(args _terminals) (format "%s" args)))
       (Symbol Symbols
               (lambda(args _terminals) (format "%s %s" (nth 0 args) (nth 1 args))))
       )
      (Symbol
       (symbol
        (lambda(args _terminals) (format "%s" args)))
       (literal
        (lambda(args _terminals)
          (format "\"%s\"" (substring args 1 2))))
       )
      )
     Start))

  (when (boundp 'parser-generator-lex-analyzer--function)
    (setq
     parser-generator-lex-analyzer--function
     (lambda (index _state)
       (with-current-buffer "*buffer*"
         (let ((token)
               (move-to-index-flag))
           (when
               (<
                index
                (point-max))
             (goto-char
              index)

             ;; Skip white-space(s)
             (when (looking-at-p "[\t\n ]+")
               (when (search-forward-regexp "[^\t\n ]" nil t)
                 (forward-char -1)
                 (setq move-to-index-flag (point))))

             (cond

              ((looking-at "\\(%[a-z]+\\)")
               (setq
                token
                `(type ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\('.'\\)")
               (setq
                token
                `(literal ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\([a-zA-Z_]+\\)")
               (setq
                token
                `(symbol ,(match-beginning 0) . ,(match-end 0))))

              ((looking-at "\\(/\\*\\)")
               (let ((comment-start (match-beginning 0))
                     (comment-end
                      (search-forward-regexp "\\(\\*/\\)" nil t)))
                 (unless comment-end
                   (error
                    "Failed to find end of comment started at %S (1)"
                    comment-start))
                 (setq
                  token
                  `(comment ,comment-start . ,comment-end))))

              ))

           (list token move-to-index-flag))))))

  (when (boundp 'parser-generator-lex-analyzer--get-function)
    (setq
     parser-generator-lex-analyzer--get-function
     (lambda (token)
       (with-current-buffer "*buffer*"
         (let ((start (car (cdr token)))
               (end (cdr (cdr token))))
           (when (<= end (point-max))
             (buffer-substring-no-properties start end)))))))

  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)
    (insert-file-contents (expand-file-name "zend_language_parser.y"))
    (goto-char (point-min))
    (let ((delimiter-start (search-forward "%precedence")))
      (setq
       delimiter-start
       (- delimiter-start 11))
      (kill-region
       (point-min)
       delimiter-start))
    (let ((delimiter-start (search-forward "%token")))
      (setq
       delimiter-start
       (- delimiter-start 6))
      (kill-region
       delimiter-start
       (point-max)))
    (goto-char (point-min))
    (let ((global-declaration (eval (car (read-from-string (parser-generator-lr-translate))))))

      (let ((attributes))
        (maphash
         (lambda (k _v)
           (push
            (intern k)
            attributes))
         phps-mode-automation-parser-generator--attributes)
        (setq
         phps-mode-automation-parser-generator--attributes
         attributes))

      (kill-buffer)
      
      global-declaration)))

(provide 'phps-mode-automation-parser-generator)
;;; phps-mode-automation-parser-generator.el ends here
