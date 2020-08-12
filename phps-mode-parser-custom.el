;;; phps-mode-parser-custom.el -- Parser for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Implement a custom shift/reduce parser here that follows the logic of the Yacc grammar.

;; Develop it using TDD.

;;; Code:

(defconst phps-mode-parser-custom--start-state 'open
  "The default start state for parser.")

(defvar phps-mode-parser-custom--ast nil
  "The current ast of parser.")

(defvar phps-mode-parser-custom--state nil
  "The current state of parser.")

(defvar phps-mode-parser-custom--tokens nil
  "The current stack of tokens.")

(defvar phps-mode-parser-custom--tokens-for-evaluation nil
  "Current stack of tokens to evaluate.")

(defun phps-mode-parser--block-grammar (name grammar)
  "Return evaluated rule NAME from GRAMMAR."
  (let ((block (gethash name grammar)))
    (if block
        (let ((response nil)
              (looking t))
          (while (and block looking)
            (let ((rule (pop block)))
              (when
                  (phps-mode-parser-custom--tokens-satisfy-rule
                   tokens
                   rule
                   grammar)
                (setq
                 response
                 (phps-mode-parser-custom--evalute-rule
                  tokens
                  rule
                  grammar))
                (setq looking nil))))
          response)
      (signal 'error (list (format "Could not find rule '%s' in grammar!" name))))))

(defun phps-mode-parser-custom--evalute-rule (rule grammar)
  "Evaluate TOKENS in RULE from GRAMMAR, return result."
  ;; Gather all matching rules as arguments to logic evaluation
  (let ((rule-grammar (car rule))
        (rule-logic (car (cdr rule)))
        (arguments nil)
        (response))

    ;; Gather arguments from buffer
    (while rule-grammar
      (let ((letter (pop rule-grammar))
            (head-token nil))
        (if
            (gethash letter grammar)
            (let ((recursive-rule (gethash letter grammar)))
              (push
               (phps-mode-parser-custom--evalute-rule
                recursive-rule
                grammar)
               arguments))
          (setq
           head-token
           (pop phps-mode-parser-custom--tokens-for-evaluation))
          (push
           (buffer-substring-no-properties
            (car (cdr head-token))
            (cdr (cdr head-token)))
           arguments))))

    ;; Reverse order of arguments
    (setq
     arguments
     (nreverse
      arguments))

    ;; Evaluate body with arguments and save response
    (setq
     response
     (funcall
      rule-logic
      arguments))
    response))

(defun phps-mode-parser-custom--tokens-satisfy-rule (tokens rule grammar)
  "Return t if TOKENS match RULE from GRAMMAR otherwise nil."
  (let ((matches t)
        (rule-grammar (car rule)))
    (while (and rule-grammar matches)
      (let ((letter (pop rule-grammar))
            (head-token nil))
        (if
            (gethash letter grammar)
            (let ((recursive-rule (gethash letter grammar))
                  (recursive-tokens))
              ;; (message "Letter '%s' matches rule" letter)
              (if-let
                  (recursive-tokens
                   (phps-mode-parser-custom--tokens-satisfy-rule
                    tokens
                    recursive-rule
                    grammar))
                  (if (equal recursive-tokens t)
                      (setq tokens nil)
                    (setq tokens recursive-tokens))
                (setq matches nil)))
          (setq head-token (pop tokens))
          ;; (message "Comparing letter '%s' with head-token '%s'" letter head-token)
          (unless
              (equal
               (car head-token) letter)
            (setq matches nil)))))
    (if (and
         matches
         tokens)
        tokens
      matches)))

(defun phps-mode-parser-custom--consume-token (tokens buffer ast state)
  "Try to consume TOKENS from BUFFER and build AST in STATE."
  (let ((consumed nil))

    (phps-mode-parser--block-grammar
     'open

     '('(T_OPEN_TAG)
       (set state 'start)
       (lambda()
         (push 'T_OPEN_TAG ast)))

     '('(T_OPEN_TAG_WITH_ECHO)
       (lambda()
         (set state 'start)
         (push 'T_OPEN_TAG_WITH_ECHO ast)))

     ;; TODO 1. Use macro to add all grammars here. Only apply macro logic if state matches grammar.
     
     consumed)))

(defun phps-mode-parser-custom--parse-tokens (tokens buffer &optional state)
  "Parse TOKENS from BUFFER, optionally start at STATE.  Return abstract-syntax-tree and err signal."
  (setq
   phps-mode-parser-custom--tokens
   tokens)
  (setq
   phps-mode-parser-custom--ast
   nil)
  (unless state
    (setq
     state
     phps-mode-parser-custom--start-state))
  (setq
   phps-mode-parser-custom--state
   state)
  (let ((ast)
        (err nil))
    (while (and
            phps-mode-parser-custom--tokens
            (not err))
      (let ((consume
             (phps-mode-parser-custom--consume-token
              'phps-mode-parser-custom--tokens
              buffer
              'phps-mode-parser-custom--ast
              'phps-mode-parser-custom--state)))
        (if consume
            (push consume ast)
          (setq
           err
           (format "Failed to parse at state: %s, tokens: %s" state tokens)))))
    (list ast err)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
