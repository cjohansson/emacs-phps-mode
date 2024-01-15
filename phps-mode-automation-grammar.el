;;; phps-mode-automation-grammar --- Grammar -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


;; Just to stop linter from complaining

(defvar
  phps-mode-parser-tokens
  nil
  "Tokens for parser.")

(defvar
  phps-mode-automation-grammar--lr--allow-default-conflict-resolution
  t
  "Allow shift resolution to shift/reduce conflicts were precedence is missing.")

(defvar
  phps-mode-automation-grammar--look-ahead-number
  1
  "The look-ahead number of grammar.")

(defvar
  phps-mode-parser-lex-analyzer--move-to-index-flag
  nil
  "Non-nil means to move index.")

(defvar
  phps-mode-automation-grammar--copyright
  ";; Copyright (C) 2018-2024  Free Software Foundation, Inc.\n\n;; This file is not part of GNU Emacs.\n\n\n"
  "Copyright contents for parser.")

(defvar
  phps-mode-automation-grammar--header
  "\n(defvar-local\n phps-mode-parser-tokens\n nil\n \"Tokens for parser.\")\n\n(define-error\n 'phps-parser-error\n \"PHPs Parser Error\")\n\n"
  "Header contents for parser.")

(defvar
  phps-mode-automation-grammar--e-identifier
  '%empty
  "The e-identifier of grammar.")

(defvar
  phps-mode-automation-grammar--eof-identifier
  '$
  "The EOF-identifier of grammar.")

(defvar
  phps-mode-automation-grammar--lex-analyzer-function
  (lambda (buffer-index)
    (let ((result (gethash buffer-index phps-mode-parser-tokens))
          (token))
      (when result
        (cond
         ((numberp result)
          (setq
           phps-mode-parser-lex-analyzer--move-to-index-flag
           result))
         ((listp result)
          (setq
           token
           result))))
      token))
  "The custom lex-analyzer.")

(defvar
  phps-mode-automation-grammar--lex-analyzer-reset-function
  (lambda()
    (progn
      ))
  "The reset function.")

(defvar
  phps-mode-automation-grammar--precedence-comparison-function
  (lambda(a-type a-value _b-type b-value)
    (cond

     ((and
       a-value
       b-value)
      (cond
       ((> a-value b-value)
        t)

       ((< a-value b-value)
        nil)

       ((= a-value b-value)

        (cond
         ((equal a-type '%left)
          t)

         ((equal a-type '%right)
          nil)

         ((equal a-type '%precedence)
          t))

        )))

     ((and
       a-value
       (not b-value))
      t)

     ((and
       (not a-value)
       (not b-value))
      nil)

     ))
  "The precedence comparison function of the grammar.")

(defvar
  phps-mode-automation-grammar--lex-analyzer-get-function
  (lambda (token)
    (let ((start (car (cdr token)))
          (end (cdr (cdr token))))
      (when (<= end (point-max))
        (buffer-substring-no-properties
         start
         end))))
  "Fetch token meta data.")


(provide 'phps-mode-automation-grammar)

;;; phps-mode-automation-grammar.el ends here

