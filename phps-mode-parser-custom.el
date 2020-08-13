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



(defun phps-mode-parser-custom--parse-state (&optional state tokens-arg)
  "Return remaining tokens and evaluated body if tokens match a rule in STATE of grammar, otherwise nil.  Use TOKENS-ARG if specified."
  (unless state
    (setq state phps-mode-parser-custom--state))
  (let ((block (gethash state phps-mode-parser-custom--grammar))
        (tokens)
        (initial-tokens))
    (if tokens-arg
        (if (symbolp tokens-arg)
            (setq tokens (symbol-value tokens-arg))
          (setq tokens tokens-arg))
      (setq tokens (symbol-value 'phps-mode-parser-custom--tokens)))
    (setq initial-tokens tokens)
    (if block
        (let ((looking t)
              (rule nil)
              (ret nil))
          ;; (message "Parsing block '%s'" block)
          (setq rule (pop block))
          (while (and rule looking)
            ;; Reset token stack
            (setq tokens initial-tokens)
            (let ((arguments nil)
                  (matches t)
                  (response nil)
                  (rule-grammar (car rule))
                  (rule-logic (car (cdr rule))))

              ;; (message "Rule-grammar: '%s'" rule-grammar)
              ;; (message "Rule-logic: '%s'" rule-logic)

              ;; Iterate all letters of grammar, check if the match and build list of arguments
              (while (and rule-grammar matches)
                (let ((letter (pop rule-grammar))
                      (head-token nil))

                  ;; (message "Checking letter '%s'" letter)
                  ;; (message "Does it match a recursive rule?")

                  ;; Check if letter is a reference to recursive rules or a token
                  (if (gethash letter phps-mode-parser-custom--grammar)
                      (let ((recursive-match (phps-mode-parser-custom--parse-state letter)))

                        ;; (message "Found matching recursive rule '%s'" letter)

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

                    ;; (message "Did not find matching recursive rule")

                    (setq head-token (pop tokens))

                    ;; (message "Comparing letter '%s' with head-token '%s'" letter head-token)
                    (if (equal (car head-token) letter)
                        (progn
                          ;; (message "Letter matches token")
                          (push (buffer-substring-no-properties (car (cdr head-token)) (cdr (cdr head-token))) arguments))
                      ;; (message "Letter does not match token")
                      (setq matches nil)))))

              (when arguments
                ;; Reverse order of arguments
                (setq arguments (nreverse arguments)))

              (if matches
                  (progn
                    ;; (message "All letters matched tokens")
                    ;; (message "Arguments: '%s'" arguments)
                    (setq looking nil)
                    (setq response (funcall rule-logic arguments))
                    (setq ret (list tokens response)))
                ;; (message "All letters did not match tokens")
                )
              (setq rule (pop block))))
          ret)
      (signal 'error (list (format "Could not find state '%s' in grammar!" state))))))

(defun phps-mode-parser-custom--parse (&optional tokens buffer state grammar)
  "Parse TOKENS from BUFFER, optionally start at STATE in GRAMMAR.  Return abstract-syntax-tree and err signal."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (setq phps-mode-parser-custom--ast nil)
    (when tokens
      (setq phps-mode-parser-custom--tokens tokens))
    (when state
      (setq phps-mode-parser-custom--state state))
    (unless phps-mode-parser-custom--state
      (setq phps-mode-parser-custom--state phps-mode-parser-custom--start-state))
    (when grammar
      (setq phps-mode-parser-custom--grammar grammar))
    (unless phps-mode-parser-custom--grammar
      (signal 'error (list "Missing defined grammar!")))
    (let ((ast)
          (err nil))
      (while (and
              phps-mode-parser-custom--tokens
              (not err))
        (let ((consume (phps-mode-parser--parse-state)))
          (if consume
              (push consume ast)
            (setq err (format "Failed to parse at state: %s, tokens: %s" phps-mode-parser-custom--state phps-mode-parser-custom--tokens)))))
      (list ast err))))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
