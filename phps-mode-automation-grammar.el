;;; phps-mode-automation-grammar --- Grammar -*- lexical-binding: t -*-

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


(require 'phps-mode-lexer)

;; Just to stop linter from complaining
(defvar
 phps-mode-parser-position
 nil
 "Position of parser.")

(defvar
 phps-mode-parser-tokens
 nil
 "Reversed list of tokens.")

(defvar
  phps-mode-automation-grammar--global-attributes
  '(%left %nonassoc %precedence %right)
  "List of valid global attributes.")

(defvar
  phps-mode-automation-grammar--lr-global-precedence-attributes
  '(%left %nonassoc %precedence %right)
  "The LR-parser's list of global precedence attributes.")

(defvar
  phps-mode-automation-grammar--global-declaration
  '(
    (%precedence T_THROW)
    (%precedence PREC_ARROW_FUNCTION)
    (%precedence T_INCLUDE T_INCLUDE_ONCE T_REQUIRE T_REQUIRE_ONCE)
    (%left T_LOGICAL_OR)
    (%left T_LOGICAL_XOR)
    (%left T_LOGICAL_AND)
    (%precedence T_PRINT)
    (%precedence T_YIELD)
    (%precedence T_DOUBLE_ARROW)
    (%precedence T_YIELD_FROM)
    (%precedence "=" T_PLUS_EQUAL T_MINUS_EQUAL T_MUL_EQUAL T_DIV_EQUAL T_CONCAT_EQUAL T_MOD_EQUAL T_AND_EQUAL T_OR_EQUAL T_XOR_EQUAL T_SL_EQUAL T_SR_EQUAL T_POW_EQUAL T_COALESCE_EQUAL)
    (%left "?" ":")
    (%right T_COALESCE)
    (%left T_BOOLEAN_OR)
    (%left T_BOOLEAN_AND)
    (%left "|")
    (%left "^")
    (%left "&")
    (%nonassoc T_IS_EQUAL T_IS_NOT_EQUAL T_IS_IDENTICAL T_IS_NOT_IDENTICAL T_SPACESHIP)
    (%nonassoc "<" T_IS_SMALLER_OR_EQUAL ">" T_IS_GREATER_OR_EQUAL)
    (%left ".")
    (%left T_SL T_SR)
    (%left "+" "-")
    (%left "*" "/" "%")
    (%precedence "!")
    (%precedence T_INSTANCEOF)
    (%precedence "~" T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST "@" )
    (%right T_POW)
    (%precedence T_CLONE)
    (%precedence T_NOELSE)
    (%precedence T_ELSEIF)
    (%precedence T_ELSE)
    )
  "Declaration for grammar.")

(defvar
  phps-mode-automation-grammar--lr--allow-default-conflict-resolution
  t
  "Allow shift resolution to shift/reduce conflicts were precedence is missing.")

(defvar
  phps-mode-automation-grammar--look-ahead-number
  1
  "The look-ahead number of grammar.")

(defvar
  phps-mode-automation-grammar--header
  "(require 'phps-mode-lexer)\n(require 'semantic)\n(require 'semantic/lex)\n\n(defvar-local\n phps-mode-parser-position\n nil\n \"Position of parser.\")\n(defvar-local\n phps-mode-parser-tokens\n nil\n \"Reversed list of lexer tokens.\")\n"
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

    ;; Create lexer buffer if none exists
    (unless (get-buffer "*PHPs Lexer*")
      (generate-new-buffer "*PHPs Lexer*")
      (let ((old-buffer
             (buffer-substring-no-properties
              (point-min)
              (point-max))))
        (with-current-buffer "*PHPs Lexer*"
          (insert old-buffer))))
    
    (with-current-buffer "*PHPs Lexer*"
      (let ((token-list-index))

        ;; Unless we have lexed the buffer
        (unless phps-mode-parser-tokens
          (unless phps-mode-lexer--generated-tokens
            ;; Reset lexer
            (setq-local
             phps-mode-lexer--generated-tokens
             nil)
            (setq-local
             phps-mode-lexer--state
             'ST_INITIAL)
            (setq-local
             phps-mode-lexer--states
             nil)
            (setq-local
             phps-mode-lexer--state-stack
             nil)
            (setq-local
             phps-mode-lexer--heredoc-label
             nil)
            (setq-local
             phps-mode-lexer--heredoc-label-stack
             nil)
            (setq-local
             phps-mode-lexer--nest-location-stack
             nil)
            (goto-char (point-min))

            ;; Run lexer on entire buffer here
            (let ((index (point))
                  (max-index (point-max)))
              (while (< index max-index)
                (phps-mode-lexer--re2c)
                (setq
                 index
                 semantic-lex-end-point)
                (goto-char index))))
          (setq-local
           phps-mode-parser-tokens
           (reverse
            phps-mode-lexer--generated-tokens))

          ;; Reset buffer-index to token-list-index connections
          (setq-local
           phps-mode-parser-position
           nil))

        (if (and
             phps-mode-parser-position
             (= (car (car phps-mode-parser-position)) buffer-index))
            (progn
              (setq
               token-list-index
               (car (cdr (car phps-mode-parser-position)))))

          ;; Search from last requested index and forward until
          ;; we find a token starting at or after buffer-index and
          ;; use this as buffer-index, save buffer-index to
          ;; token-list-index connection
          (let ((previous-token-list-index 0))
            (when (and
                   phps-mode-parser-position
                   (< (car (car phps-mode-parser-position)) buffer-index))
              (setq
               previous-token-list-index
               (car (cdr (car phps-mode-parser-position)))))

            (let ((temp-token-list-index
                   previous-token-list-index)
                  (token-list-size
                   (length
                    phps-mode-parser-tokens))
                  (continue t))
              (while (and
                      continue
                      (<
                       temp-token-list-index
                       token-list-size))
                (let ((token
                       (nth
                        temp-token-list-index
                        phps-mode-parser-tokens)))

                  ;; When token starts at cursor we found correct index
                  ;; Save it
                  (when (= (car (cdr token)) buffer-index)
                    (let ((token-type (car token)))
                      (push
                       (list
                        buffer-index
                        temp-token-list-index)
                       phps-mode-parser-position)
                      (unless (or
                               (equal token-type 'T_OPEN_TAG)
                               (equal token-type 'T_CLOSE_TAG)
                               (equal token-type 'T_DOC_COMMENT)
                               (equal token-type 'T_COMMENT))
                        (setq
                         token-list-index
                         temp-token-list-index)
                        (setq
                         continue
                         nil))))

                  ;; When token starts after cursor, flag move of cursor
                  ;; Save it
                  (when (> (car (cdr token)) buffer-index)
                    (let ((token-type (car token)))
                      (push
                       (list
                        (car (cdr token))
                        temp-token-list-index)
                       phps-mode-parser-position)
                      (unless (or
                               (equal token-type 'T_OPEN_TAG)
                               (equal token-type 'T_CLOSE_TAG)
                               (equal token-type 'T_DOC_COMMENT)
                               (equal token-type 'T_COMMENT))
                        (setq-local
                         phps-mode-parser-lex-analyzer--move-to-index-flag
                         (car (cdr token)))
                        (setq
                         continue
                         nil))))

                  (setq
                   temp-token-list-index
                   (1+ temp-token-list-index))
                  )))))

        (when
            token-list-index
          (let ((token
                 (nth
                  token-list-index
                  phps-mode-parser-tokens)))
            (when (equal (car token) 'T_OPEN_TAG_WITH_ECHO)
              (setf
               (car token)
               'T_ECHO))
            token)))))
  "The custom lex-analyzer.")

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
    (with-current-buffer "*PHPs Lexer*"
      (let ((start (car (cdr token)))
            (end (cdr (cdr token))))
        (when (<= end (point-max))
          (buffer-substring-no-properties
           start
           end)))))
  "Fetch token meta data.")


(provide 'phps-mode-automation-grammar)

;;; phps-mode-automation-grammar.el ends here

