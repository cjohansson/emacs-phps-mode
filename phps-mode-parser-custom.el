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

(defvar phps-mode-parser-custom--grammar nil
  "The current grammar.")

(defvar phps-mode-parser-custom--state nil
  "The current state of parser.")

(defvar phps-mode-parser-custom--tokens nil
  "The current stack of tokens.")

(defun phps-mode-parser--parse-state (name)
  "Return evaluated rule NAME."
  (let ((block
            (gethash
             name
             phps-mode-parser-custom--grammar)))
    (if block
        (let ((response nil)
              (looking t))
          (while (and block looking)
            (let ((rule (pop block)))
              (let ((parsed-rule
                     (phps-mode-parser-custom--tokens-parse-state
                      phps-mode-parser-custom--tokens
                      rule)))
                (when parsed-rule
                  (setq
                   phps-mode-parser-custom--tokens
                   (nth
                    0
                    parsed-rule))
                  (setq
                   response
                   (nth
                    1
                    parsed-rule))
                  (setq looking nil)))))
          response)
      (signal 'error (list (format "Could not find rule '%s' in grammar!" name))))))

(defun phps-mode-parser-custom--parse-state (state &optional tokens-arg)
  "Return remaining tokens and evaluated body if tokens match a rule in STATE of grammar, otherwise nil.  Use TOKENS-ARG if specified."
  (let ((block (gethash state phps-mode-parser-custom--grammar))
        (tokens))
    (if tokens-arg
        (if (symbolp tokens-arg)
            (setq tokens (symbol-value tokens-arg))
          (setq tokens tokens-arg))
      (setq tokens (symbol-value 'phps-mode-parser-custom--tokens)))
    (if block
        (let ((looking t)
              (rule nil))
          (setq rule (pop block))
          (while (and rule looking)
            (let ((arguments nil)
                  (matches t)
                  (response nil)
                  (rule-grammar (car rule))
                  (rule-logic (car (cdr rule))))

              ;; Iterate all letters of grammar, check if the match and build list of arguments
              (while (and rule-grammar matches)
                (let ((letter (pop rule-grammar))
                      (head-token nil))

                  ;; Check if letter is a reference to recursive rules or a token
                  (if (gethash letter phps-mode-parser-custom--grammar)
                      (let ((recursive-match
                             (phps-mode-parser-custom--parse-state
                              tokens
                              letter)))

                        ;; On match set response as argument and update remaining tokens
                        (if recursive-match
                            (progn
                              (setq
                               tokens
                               (nth 0 recursive-match))
                              (push
                               (nth 1 recursive-match)
                               arguments))

                        ;; No recursive match means this rule does not match
                          (setq
                           matches
                           nil)))

                    (setq head-token (pop tokens))
                    ;; (message "Comparing letter '%s' with head-token '%s'" letter head-token)
                    (if (equal (car head-token) letter)
                        (push (buffer-substring-no-properties (car (cdr head-token)) (cdr (cdr head-token))) arguments)
                      (setq matches nil)))))

              (when arguments
                ;; Reverse order of arguments
                (setq arguments (nreverse arguments)))

              (if matches
                  (progn
                    (setq
                     response
                     (funcall
                      rule-logic
                      arguments))
                    (list
                     tokens
                     response))
                nil))
            (setq rule (pop block))))
      (signal 'error (list (format "Could not find state '%s' in grammar!" state))))))

(defun phps-mode-parser-custom--parse (tokens &optional buffer state)
  "Parse TOKENS from BUFFER, optionally start at STATE.  Return abstract-syntax-tree and err signal."
  (setq phps-mode-parser-custom--tokens tokens)
  (setq phps-mode-parser-custom--ast nil)
  (unless state
    (setq state phps-mode-parser-custom--start-state))
  (unless phps-mode-parser-custom--grammar
    (signal 'error (list "Missing defined grammar!")))
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((ast)
          (err nil))
      (while (and
              phps-mode-parser-custom--tokens
              (not err))
        (let ((consume (phps-mode-parser--parse-state state)))
          (if consume
              (push consume ast)
            (setq err (format "Failed to parse at state: %s, tokens: %s" state phps-mode-parser-custom--tokens)))))
      (list ast err))))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
