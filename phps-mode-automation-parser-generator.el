;;; phps-mode-automation-parser-generator --- Generate a parser for PHP YACC grammar -*- lexical-binding: t -*-

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

(defun phps-mode-automation-parser-generator--ensure-yacc-grammar-is-available ()
  "If grammar is not available, download it."
  (let ((php-yacc-url
         "https://raw.githubusercontent.com/php/php-src/php-8.0.0/Zend/zend_language_parser.y")
        (php-yacc-file
         (expand-file-name "zend_language_parser.y")))

    ;; Download YACC if not available
    (unless (file-exists-p php-yacc-file)
      (message
       "Downloading PHP 8.0 YACC grammar..")
      (url-copy-file
       php-yacc-url php-yacc-file
       t
       t)
      (message
       "Download of PHP 8.0 YACC grammar completed"))

    (unless (file-exists-p php-yacc-file)
      (error "Missing PHP YACC grammar at %s!" php-yacc-file))))

(defun phps-mode-automation-parser-generator--grammar ()
  "Generate productions here."
  (require 'parser-generator-lr)
  (phps-mode-automation-parser-generator--ensure-yacc-grammar-is-available)

  (parser-generator-set-look-ahead-number
   1)
  (setq
   parser-generator--e-identifier
   nil)
  (setq
   parser-generator--global-attributes
   nil)
  (setq
   parser-generator-lr--global-precedence-attributes
   nil)
  (setq
   parser-generator-lr--context-sensitive-precedence-attribute
   nil)
  (setq
   parser-generator--global-declaration
   nil)
  (parser-generator-set-grammar
   '(
     (Start Productions-Block Productions-Delimiter Productions Productions Production Production-End LHS RHSS RHS RHS-Symbol RHS-Symbols Comment Logic Symbol)
     (productions-delimiter ":" "|" ";" comment logic symbol literal)
     (
      (Start
       Productions-Block)
      (Productions-Block
       (Productions-Delimiter Productions Productions-Delimiter
                              (lambda(args) (format "'(\n\n%s\n\n)" (nth 1 args))))
       )
      (Productions-Delimiter
       (productions-delimiter
        (lambda(args) ""))
       )
      (Productions
       (Production
        (lambda(args) (format "%s" args)))
       (Productions Production
                    (lambda(args) (format "%s\n\n%s" (nth 0 args) (nth 1 args))))
       )
      (Production
       (Comment Production
                (lambda(args) (format "%s" (nth 1 args))))
       (LHS ":" RHSS Production-End
            (lambda(args) (format " (%s\n  %s\n )" (nth 0 args) (nth 2 args))))
       )
      (Production-End
       ";"
       (";" ";"))
      (LHS
       (Symbol
        (lambda(args) (format "%s" args)))
       )
      (RHSS
       (RHS
        (lambda(args) (format "%s" args)))
       (RHSS "|" RHS
             (lambda(args) (format "%s\n  %s" (nth 0 args) (nth 2 args))))
       )
      (RHS
       (RHS-Symbol
        (lambda(args) (format "%s" args)))
       (RHS-Symbols
        (lambda(args)
          (if (string-match-p " " args)
              (format "(%s)" args)
            (format "%s" args))))
       )
      (RHS-Symbols
       (RHS-Symbol
        RHS-Symbols
        (lambda (args)
          (if (string= (nth 1 args) "")
              (format "%s" (nth 0 args))
            (format "%s %s" (nth 0 args) (nth 1 args)))))
       (RHS-Symbol
        RHS-Symbol
        (lambda (args)
          (if (string= (nth 1 args) "")
              (format "%s" (nth 0 args))
            (format "%s %s" (nth 0 args) (nth 1 args)))))
       )
      (RHS-Symbol
       Comment
       Logic
       Symbol)
      (Comment
       (comment
        (lambda(args) "")))
      (Logic
       (logic
        (lambda(args) ""))
       )
      (Symbol
       (symbol
        (lambda(args) (format "%s" args)))
       (literal
        (lambda(args) (format "%S" (substring args 1 2))))
       )
      )
     Start))

  (setq
   parser-generator-lex-analyzer--function
   (lambda (index)
     (with-current-buffer "*buffer*"
       (let ((token))
         (when
             (<
              index
              (point-max))
           (goto-char
            index)

           ;; Skip white-space(s)
           (when (looking-at-p "[\t\n ]+")
             (when
                 (search-forward-regexp "[^\t\n ]" nil t)
               (forward-char -1)
               (setq-local
                parser-generator-lex-analyzer--move-to-index-flag
                (point))))

           (cond

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
                            "Failed to find end of comment started at %S (2))"
                            comment-start))))
                      

                      ))))
               (unless logic-end
                 (error
                  "Failed to find end of logic started at %S"
                  logic-start))
               (setq
                token
                `(logic ,logic-start . ,logic-end))))

            ((looking-at "\\(:\\|;\\||\\)")
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

            ((looking-at "\\([%a-zA-Z_]+\\)")
             (setq
              token
              `(symbol ,(match-beginning 0) . ,(match-end 0))))

            ((looking-at "\\('.'\\)")
             (setq
              token
              `(literal ,(match-beginning 0) . ,(match-end 0))))

            ))

         (when token
           (let ((token-data
                  (buffer-substring-no-properties
                   (car (cdr token))
                   (cdr (cdr token)))))))
         token))))

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
             symbol))))))

  (parser-generator-process-grammar)
  (parser-generator-lr-generate-parser-tables)

  (let ((buffer (generate-new-buffer "*buffer*")))
    (switch-to-buffer buffer)
    (insert-file (expand-file-name "zend_language_parser.y"))
    (goto-char (point-min))
    (let ((delimiter-start (search-forward "%%")))
      (setq
       delimiter-start
       (- delimiter-start 2))
      (kill-region (point-min) delimiter-start))
    (let ((delimiter-start (search-forward "%%")))
      (kill-region delimiter-start (point-max)))
    (goto-char (point-min))
    (let ((productions (eval (car (read-from-string (parser-generator-lr-translate))))))
      ;; TODO Generate non-terminals here
      ;; TODO Generate terminals here
      ;; TODO Generate start here
      (list
       non-terminals
       terminals
       productions
       start))))

(provide 'phps-mode-automation-parser-generator)
;;; phps-mode-automation-parser-generator.el ends here