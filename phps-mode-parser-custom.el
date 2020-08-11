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

(defun phps-mode-parser--block-grammar (name block grammar)
  "Return evaluated BLOCK if state match NAME, from GRAMMAR."
  `(when (= state ,name)
     ;; TODO Check if head of TOKENS match rule here, if so evaluate body
     ;; TODO If rule contains other rules, recursively evaluate them

     (dolist (rule block)
       (let ((rule-grammar (car rule))
             (body (cdr rule)))
         (when
             (phps-mode-parser-custom--tokens-satisfy-rule-p
              tokens
              rule-grammar
              grammar)
           (eval body))))))

(defun phps-mode-parser-custom--tokens-satisfy-rule-p (tokens rule grammar)
  "Return t if TOKENS match RULE from GRAMMAR otherwise nil."
  (let ((matches t))
    (while (and rule matches)
      (let ((letter (pop rule))
            (head-token nil))
        (if
            (gethash letter grammar)
            (let ((recursive-rule (gethash letter grammar)))
              (message "Letter '%s' matches rule" letter)
              (if-let
                  ((recursive-tokens
                    (phps-mode-parser-custom--tokens-satisfy-rule-p
                     tokens
                     (car recursive-rule)
                     grammar)))
                  (if (equal recursive-tokens t)
                      (setq tokens nil)
                    (setq tokens recursive-tokens))
                (setq matches nil)))
          (setq head-token (pop tokens))
          (message "Comparing letter '%s' with head-token '%s'" letter head-token)
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
