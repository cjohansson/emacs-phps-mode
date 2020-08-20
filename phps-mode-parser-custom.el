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

(require 'phps-mode-parser-custom-grammar)

;; Variables and constants:



(defvar-local phps-mode-parser-custom--ast nil
  "The current ast of parser.")

(defvar-local phps-mode-parser-custom--error nil
  "The current error of parser.")

(defvar-local phps-mode-parser-custom--tokens nil
  "The current stack of tokens.")

(defvar phps-mode-parser-custom--parser-action-table nil
  "The LR(1) parser action-table for grammar.")

(defvar phps-mode-parser-custom--parser-goto-table nil
  "The LR(1) parser goto-table for grammar.")

(defvar phps-mode-parser-custom--parser-leaf-states nil
  "The LR(1) parser leaf-states for grammar.")


;; Functions:

(defun phps-mode-parser-custom--lr-parse (unscanned state parser-table)
  "Parse UNSCANNED at STATE in PARSER-TABLE."
  (let ((parse-tree)
        (parse-stack)
        (step 0)
        (look-ahead)
        (parse-action))
    (setq look-ahead (car (car unscanned)))
    (while look-ahead
      (setq parse-stack (car parse-tree))
      (message "Parse-state: '%s'" state)
      (message "Parse-stack: '%s'" parse-stack)
      (message "Look-ahead: '%s'" look-ahead)
      (message "Unscanned: '%s'" unscanned)
      (setq parse-action nil)
      (when parse-stack
        (when (gethash (list state parse-stack) parser-table)
          (setq parse-action 'reduce)
          (pop parse-tree)
          (push (list (gethash (list state parse-stack) parser-table)) parse-tree)
          (push nil parse-tree)
          (setq state (gethash (list state parse-stack) parser-table) parse-tree)
          (message "Action: 'reduce '%s' -> '%s'" parse-stack (gethash parse-stack parser-table))))
      (unless parse-action
        (push look-ahead parse-stack)
        (pop unscanned)
        (message "Action: 'shift '%s'" look-ahead)
        (pop parse-tree)
        (push parse-stack parse-tree))
      (message "Parse-tree: '%s'\n" parse-tree)
      (setq look-ahead (car (car unscanned))))
    parse-tree))

(defun phps-mode-parser-custom--generate-parser (&optional grammar start)
  "Generate parser-table for GRAMMAR starting at START."
  (unless grammar
    (setq grammar phps-mode-parser-custom-grammar))
  (unless start
    (setq start phps-mode-parser-custom-grammar--start-state))
  (let ((state-queue (list start))
        (action-table (make-hash-table :test 'equal))
        (goto-table (make-hash-table :test 'equal))
        (state-name)
        (parsed-states (make-hash-table :test 'equal))
        (leaf-states))
    (setq state-name (pop state-queue))
    (while state-name
      (let ((is-leaf t))
        (let ((state (gethash state-name grammar)))
          (dolist (state-block state)
            (let ((state-patterns (car state-block))
                  (state-logic (cdr state-block)))
              (let ((match-patterns (nreverse state-patterns)))
                (message "Reduction: '%s' -> '%s'" match-patterns state-name)
                (puthash (list state-name match-patterns) state-name action-table)
                (dolist (state-pattern state-patterns)

                  ;; Does rule contain a branch?
                  (when (gethash state-pattern grammar)

                    ;; This state is not a leaf
                    (when is-leaf
                      (setq is-leaf nil))

                    (let ((state-connections nil)
                          (has-link))
                      (when (gethash state-pattern goto-table)
                        (setq state-connections (gethash state-pattern goto-table)))

                      ;; Check if relationship is already saved
                      (dolist (connection state-connections)
                        (when (equal connection state-name)
                          (setq has-link t)))

                      ;; Save new relationship
                      (unless has-link
                        (push state-name state-connections)
                        (message "Relationship: '%s' -> '%s'" state-pattern state-name))

                      (puthash state-pattern state-connections goto-table))
                    (when (and (not (equal state-pattern state-name))
                               (not (gethash state-pattern parsed-states)))
                      (message "State: '%s'" state-pattern)
                      (push state-pattern state-queue))))))))

        (when is-leaf
          (message "Leaf: '%s'" state-name)
          (push state-name leaf-states))

        ;; Mark state as parsed
        (puthash state-name t parsed-states)

        ;; Process next state in queue
        (setq state-name (pop state-queue))))
    (setq phps-mode-parser-custom--parser-leaf-states leaf-states)
    (setq phps-mode-parser-custom--parser-goto-table goto-table)
    (setq phps-mode-parser-custom--parser-action-table action-table)))

(defun phps-mode-parser-custom--parse-state (&optional state tokens-arg)
  "Return remaining tokens and evaluated body if tokens match a rule in STATE of grammar, otherwise nil.  Use TOKENS-ARG if specified."
  (unless state
    (setq state phps-mode-parser-custom-grammar--state))
  (let ((block (gethash state phps-mode-parser-custom-grammar))
        (tokens)
        (initial-tokens))
    (if tokens-arg
        (if (symbolp tokens-arg)
            (setq tokens (symbol-value tokens-arg))
          (setq tokens tokens-arg))
      (setq tokens (symbol-value 'phps-mode-parser-custom--tokens)))
    (setq initial-tokens tokens)
    (if block
        (let ((rule nil)
              (ret nil)
              (no-matching-rule t))
          (message "Parsing block '%s' and tokens '%s'" block initial-tokens)
          (setq rule (pop block))

          (while (and rule no-matching-rule)

            ;; Reset token stack
            (setq tokens initial-tokens)

            (let ((arguments nil)
                  (rule-matches t)
                  (response nil)
                  (response-size nil)
                  (rule-grammar (car rule))
                  (rule-logic (car (cdr rule))))

              ;; Empty rule only matches empty tokens
              (if (equal rule-grammar (list nil))
                  (if (equal tokens nil)
                      (if rule-logic
                          (progn
                            (message "Rule-logic is reduce, executing")
                            (setq response (funcall rule-logic arguments)))
                        (message "Rule-logic is shift"))
                    (setq rule-matches nil))

                ;; (message "Rule-grammar: '%s'" rule-grammar)
                ;; (message "Rule-logic: '%s'" rule-logic)

                ;; Iterate all letters of grammar, check if the match and build list of arguments
                (while (and rule-grammar rule-matches)
                  (let ((letter (pop rule-grammar))
                        (head-token nil)
                        (look-ahead-letter (car rule-grammar))
                        (look-ahead-letter-is-recursive-rule nil)
                        (look-ahead-token (car tokens)))
                    (when look-ahead-token
                      (setq look-ahead-token (car look-ahead-token)))
                    (message "Checking letter '%s' with look-ahead token '%s' and look-ahead letter '%s'" letter look-ahead-token look-ahead-letter)
                    (message "Does it match a recursive rule?")

                    ;; Check if letter is a reference to recursive rules or a token
                    (if (gethash letter phps-mode-parser-custom-grammar)
                        (progn
                          (message "Found matching recursive rule '%s'" letter)

                          ;; Is the look-ahead letter another recursive rule?
                          (when (and look-ahead-token
                                     look-ahead-letter
                                     (gethash look-ahead-letter phps-mode-parser-custom-grammar))
                            (setq look-ahead-letter-is-recursive-rule t))

                          (if (and look-ahead-letter
                                   look-ahead-token
                                   (not look-ahead-letter-is-recursive-rule))
                              (if (equal look-ahead-letter look-ahead-token)
                                  (progn
                                    (message "Look-ahead token matches look-ahead letter")
                                    (let ((recursive-match (phps-mode-parser-custom--parse-state letter)))

                                      ;; On match set response as argument and update remaining tokens
                                      (if recursive-match
                                          (progn
                                            (message "Got recursive match, response: '%s'" recursive-match)
                                            (setq tokens (nth 0 recursive-match))
                                            (push (nth 1 recursive-match) arguments))

                                        ;; No recursive match means this rule does not match
                                        (message "Did not get recursive match")
                                        (setq rule-matches nil))))
                                (message "Look-ahead token does not match look-ahead letter"))
                            (let ((recursive-match (phps-mode-parser-custom--parse-state letter)))

                              ;; On match set response as argument and update remaining tokens
                              (if recursive-match
                                  (progn
                                    (message "Got recursive match, response: '%s'" recursive-match)
                                    (setq tokens (nth 0 recursive-match))
                                    (push (nth 1 recursive-match) arguments))

                                ;; No recursive match means this rule does not match
                                (message "Did not get recursive match")
                                (setq rule-matches nil)))))

                      (message "Did not find matching recursive rule")

                      (setq head-token (pop tokens))

                      (message "Comparing letter '%s' with head-token '%s'" letter head-token)
                      (if (equal (car head-token) letter)
                          (progn
                            (message "Letter matches token")
                            (push (buffer-substring-no-properties (car (cdr head-token)) (cdr (cdr head-token))) arguments))
                        (message "Letter does not match token")
                        (setq rule-matches nil)))))

                (when arguments
                  ;; Reverse order of arguments
                  (setq arguments (nreverse arguments)))

                (if rule-matches
                    (progn
                      ;; (message "All letters matched tokens")
                      ;; (message "Arguments: '%s'" arguments)
                      (if rule-logic
                          (progn
                            (setq response (funcall rule-logic arguments))
                            (message "Rule-logic is reduce, response: '%s'" response))
                        (setq response arguments)
                        (message "Rule-logic is shift"))
                      (setq ret (list tokens response))
                      (setq no-matching-rule nil)
                      ;; (message "All letters did not match tokens")
                      )
                  (setq rule (pop block))))))
          ret)
      (signal 'error (list (format "Could not find state '%s' in grammar!" state))))))

(defun phps-mode-parser-custom--parse (&optional tokens buffer state state-history grammar)
  "Parse TOKENS from BUFFER, optionally start at STATE with STATE-HISTORY in GRAMMAR.  Return abstract-syntax-tree and err signal."
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (setq phps-mode-parser-custom--ast nil)
    (setq phps-mode-parser-custom--error nil)
    (when tokens
      (setq phps-mode-parser-custom--tokens tokens))
    (unless phps-mode-parser-custom--tokens
      (signal 'error (list "Missing defined tokens!")))
    (when state
      (setq phps-mode-parser-custom-grammar--state state))
    (when state-history
      (setq phps-mode-parser-custom-grammar--state-history state-history))
    (unless phps-mode-parser-custom-grammar--state
      (setq phps-mode-parser-custom-grammar--state phps-mode-parser-custom-grammar--start-state))
    (when grammar
      (setq phps-mode-parser-custom-grammar grammar))
    (unless phps-mode-parser-custom-grammar
      (signal 'error (list "Missing defined grammar!")))
    (let ((ast))
      (while (and
              phps-mode-parser-custom--tokens
              (not phps-mode-parser-custom--error))
        (let ((_old-state phps-mode-parser-custom-grammar--state))
          (let ((consume (phps-mode-parser-custom--parse-state)))
            (if consume
                (let ((response-tokens (car consume))
                      (response-ast (cdr consume)))
                  ;; (message "Successfully parsed grammar at state: '%s', new-state: '%s', response: '%s'" _old-state phps-mode-parser-custom-grammar--state consume)
                  (setq phps-mode-parser-custom--tokens response-tokens)
                  (push response-ast ast))
              (setq phps-mode-parser-custom--error (list phps-mode-parser-custom-grammar--state phps-mode-parser-custom--tokens))))))
      (when ast
        (setq ast (nreverse ast))
        (setq phps-mode-parser-custom--ast ast)))
    (list phps-mode-parser-custom--ast phps-mode-parser-custom--error)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
