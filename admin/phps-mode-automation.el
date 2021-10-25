;;; phps-mode-automation --- Generate a Wisent Parser file -*- lexical-binding: t -*-

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

;;; Uses a parser to convert LALR Yacc grammar to Wisent grammar

;;; AST should be like this: (root (block (rule (logic))))


;;; Code:


(require 'phps-mode-automation-grammar)
(require 'phps-mode-automation-parser-generator)

(defun phps-mode-automation ()
  "Generate parser."
  (if (fboundp 'parser-generator-lr-export-to-elisp)
      (progn

        ;; 100 MB before garbage collection
        (setq
         gc-cons-threshold
         (* 1024 1024 100))

        (let* ((global-declaration (phps-mode-automation-parser-generator--global-declaration))
               (attributes phps-mode-automation-parser-generator--attributes)
               (grammar (phps-mode-automation-parser-generator--grammar))
               (context-sensitive-attributes phps-mode-automation-parser-generator--context-sensitive-attributes))

          ;; Prepare export
          (when (fboundp 'parser-generator-set-grammar)
            (parser-generator-set-grammar
             grammar))

          (when (boundp 'parser-generator--context-sensitive-attributes)
            (setq
             parser-generator--context-sensitive-attributes
             context-sensitive-attributes)
            (message "parser-generator--context-sensitive-attributes: %S" parser-generator--context-sensitive-attributes))

          (when (boundp 'parser-generator-lr--context-sensitive-precedence-attribute)
            (setq
             parser-generator-lr--context-sensitive-precedence-attribute
             (car context-sensitive-attributes))
            (message "parser-generator-lr--context-sensitive-precedence-attribute: %S" parser-generator-lr--context-sensitive-precedence-attribute))

          (when (boundp 'parser-generator--global-declaration)
            (setq
             parser-generator--global-declaration
             global-declaration)
            (message "parser-generator--global-declaration: %S" parser-generator--global-declaration))

          (when (boundp 'parser-generator--global-attributes)
            (setq
             parser-generator--global-attributes
             attributes))

          (when (boundp 'parser-generator-lr--global-precedence-attributes)
            (setq
             parser-generator-lr--global-precedence-attributes
             attributes)
            (message "parser-generator-lr--global-precedence-attributes: %S" parser-generator-lr--global-precedence-attributes)))

        (when (fboundp 'parser-generator-set-look-ahead-number)
          (parser-generator-set-look-ahead-number
           phps-mode-automation-grammar--look-ahead-number))

        (when (boundp 'parser-generator--e-identifier)
          (setq
           parser-generator--e-identifier
           phps-mode-automation-grammar--e-identifier))

        (when (boundp 'parser-generator--eof-identifier)
          (setq
           parser-generator--eof-identifier
           phps-mode-automation-grammar--eof-identifier))

        (when (boundp 'parser-generator-lex-analyzer--function)
          (setq
           parser-generator-lex-analyzer--function
           phps-mode-automation-grammar--lex-analyzer-function))
        ;; TODO This is not exported correctly

        (when (boundp 'parser-generator-lex-analyzer--reset-function)
          (setq
           parser-generator-lex-analyzer--reset-function
           phps-mode-automation-grammar--lex-analyzer-reset-function))
        ;; TODO This is not exported correctly

        (when (boundp 'parser-generator-lex-analyzer--get-function)
          (setq
           parser-generator-lex-analyzer--get-function
           phps-mode-automation-grammar--lex-analyzer-get-function))

        (when (boundp 'parser-generator-lr--allow-default-conflict-resolution)
          (setq
           parser-generator-lr--allow-default-conflict-resolution
           phps-mode-automation-grammar--lr--allow-default-conflict-resolution))

        (when (boundp 'parser-generator-lr--precedence-comparison-function)
          (setq
           parser-generator-lr--precedence-comparison-function
           phps-mode-automation-grammar--precedence-comparison-function))

        (when (fboundp 'parser-generator-process-grammar)
          (parser-generator-process-grammar))

        (when (fboundp 'parser-generator-lr--generate-precedence-tables)
          (parser-generator-lr--generate-precedence-tables)
          (message "Precedence Tables")
          (when (boundp 'parser-generator-lr--symbol-precedence-value)
            (message
             "(setq parser-generator-lr--symbol-precedence-value %S)"
             parser-generator-lr--symbol-precedence-value))
          (when (boundp 'parser-generator-lr--symbol-precedence-type)
            (message
             "(setq parser-generator-lr--symbol-precedence-type %S)"
             parser-generator-lr--symbol-precedence-type))
          (when (boundp 'parser-generator-lr--production-number-precedence-value)
            (message
             "(setq parser-generator-lr--production-number-precedence-value %S)"
             parser-generator-lr--production-number-precedence-value))
          (when (boundp 'parser-generator-lr--production-number-precedence-type)
            (message
             "(setq parser-generator-lr--production-number-precedence-type %S)"
             parser-generator-lr--production-number-precedence-type))

          ;; Only generate LR-items, GOTO-tables and ACTION-tables if we are lacking it
          (if (and
               (boundp 'parser-generator-lr--goto-tables)
               (boundp 'parser-generator-lr--goto-tables-resume)
               parser-generator-lr--goto-tables-resume
               (boundp 'parser-generator-lr--distinct-goto-tables)
               (boundp 'parser-generator-lr--distinct-goto-tables-resume)
               parser-generator-lr--distinct-goto-tables-resume
               (boundp 'parser-generator-lr--action-tables)
               (boundp 'parser-generator-lr--action-tables-resume)
               parser-generator-lr--action-tables-resume
               (boundp 'parser-generator-lr--distinct-action-tables)
               (boundp 'parser-generator-lr--distinct-action-tables-resume)
               parser-generator-lr--distinct-action-tables-resume)
              (progn
                (setq
                 parser-generator-lr--goto-tables
                 parser-generator-lr--goto-tables-resume)
                (setq
                 parser-generator-lr--distinct-goto-tables
                 parser-generator-lr--distinct-goto-tables-resume)
                (setq
                 parser-generator-lr--action-tables
                 parser-generator-lr--action-tables-resume)
                (setq
                 parser-generator-lr--distinct-action-tables
                 parser-generator-lr--distinct-action-tables-resume)
                (message "Parser tables are defined - skipping generation"))
            (progn
              (message "Parser tables are not defined - generating..")
              (when (fboundp 'parser-generator-lr--generate-goto-tables)
                (let ((table-lr-items
                       (parser-generator-lr--generate-goto-tables)))
                  (message
                   "table-lr-items: %S"
                   table-lr-items)
                  (when (boundp 'parser-generator-lr--goto-tables)
                    (message
                     "(setq parser-generator-lr--goto-tables-resume %S)"
                     parser-generator-lr--goto-tables))
                  (when (boundp 'parser-generator-lr--distinct-goto-tables)
                    (message
                     "(setq parser-generator-lr--distinct-goto-tables-resume %S)"
                     parser-generator-lr--distinct-goto-tables))
                  (when (fboundp 'parser-generator-lr--generate-action-tables)
                    (parser-generator-lr--generate-action-tables table-lr-items)
                    (when (boundp 'parser-generator-lr--action-tables)
                      (message
                       "(setq parser-generator-lr--action-tables-resume %S)"
                       parser-generator-lr--action-tables))
                    (when (boundp 'parser-generator-lr--distinct-action-tables)
                      (message
                       "(setq parser-generator-lr--distinct-action-tables-resume %S)"
                       parser-generator-lr--distinct-action-tables))))))))

        ;; NOTE This does not work if functions above are byte-compiled

        ;; Export
        (let ((export
               (parser-generator-lr-export-to-elisp
                "phps-mode-parser"
                phps-mode-automation-grammar--header)))
          (generate-new-buffer "*PHP Parser*")
          (switch-to-buffer "*PHP Parser*")
          (insert export)
          (write-file (expand-file-name "./phps-mode-parser.el"))
          (kill-buffer)
          (message "export: %s" export))

        (message "Automation completed"))
    (error "Emacs parser generator must be available!")))

(provide 'phps-mode-automation)
;;; phps-mode-automation.el ends here
