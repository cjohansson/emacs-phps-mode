;;; phps-mode-grammar-parser-generator --- Generate a parser for PHP YACC grammar -*- lexical-binding: t -*-

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


(require 'parser-generator-lr)

(defun phps-mod-grammar-parser-generator()
  "Generate parser here."

  (parser-generator-set-look-ahead-number
   1)
  (setq
   parser-generator--e-identifier
   '%empty)
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
     (Start Productions-Block Start-Delimiter Productions End-Delimiter Productions Production LHS RHSS RHS RHS-Symbol Comment Logic Symbol)
     (start-delimiter end-delimiter ":" "|" ";" comment logic symbol)
     (
      (Start Productions-Block)
      (Productions-Block (Start-Delimiter Productions End-Delimiter))
      (Start-Delimiter start-delimiter)
      (End-Delimiter end-delimiter)
      (Productions Production (Productions Production))
      (Production (LHS ":" RHSS ";"))
      (LHS Symbol)
      (RHSS RHS (RHSS "|" RHS))
      (RHS RHS-Symbol (RHS RHS-Symbol))
      (RHS-Symbol Comment Logic Symbol)
      (Comment comment)
      (Logic logic)
      (Symbol symbol)
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
           (when (looking-at-p "[\t ]+")
             (when
                 (search-forward-regexp "[^\t ]" nil t)
               (forward-char -1)
               (setq-local
                parser-generator-lex-analyzer--move-to-index-flag
                (point))))

           (cond

            ((looking-at "\\(/\\*.+\\*/\\)")
             (setq
              token
              `(comment ,(match-beginning 0) . ,(match-end 0))))

            ((looking-at "\\({.+}\\)")
             (setq
              token
              `(logic ,(match-beginning 0) . ,(match-end 0))))

            ((looking-at "\\(:\\|;\\||\\)")
             (setq
              token
              `(
                ,(buffer-substring-no-properties
                  (match-beginning 0)
                  (match-end 0))
                ,(match-beginning 0)
                . ,(match-end 0)
                )))

            (t (error "Unexpected input at %d!" index))))
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

    ))

(provide 'phps-mode-grammar-parser-generator)
;;; phps-mode-grammar-parser-generator.el ends here
