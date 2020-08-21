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

(defun phps-mode-parser-custom--parse (unscanned &optional state leaf-states action-table goto-table)
  "Parse UNSCANNED in LEAF-STATES, use parser ACTION-TABLE and GOTO-TABLE in STATE."
  (unless leaf-states
    (setq leaf-states phps-mode-parser-custom--parser-leaf-states))
  (unless action-table
    (setq action-table phps-mode-parser-custom--parser-action-table))
  (unless goto-table
    (setq goto-table phps-mode-parser-custom--parser-goto-table))
  (let ((parse-stack)
        (step 0)
        (look-ahead)
        (parse-action)
        (continue t))
    (setq look-ahead (car (car unscanned)))
    (while continue
      (unless look-ahead
        (setq continue nil))

      (message "Parse-state: '%s'" state)
      (message "Parse-stack: '%s'" parse-stack)
      (message "Look-ahead: '%s'" look-ahead)
      (message "Unscanned: '%s'" unscanned)
      (setq parse-action nil)

      ;; If we have a parse-stack, match it to state-patterns
      (when parse-stack
        (let ((goto-states))

          (if state
              (setq goto-states (gethash state goto-table))
            (setq goto-states leaf-states))

          (message "Looking for reduction in goto-states: '%s'" goto-states)
          (when goto-states
            (let ((goto-state)
                  (searching-reduction t))
              (setq goto-state (pop goto-states))

              ;; Search all goto-states for reduction
              (while (and
                      goto-state
                      searching-reduction)
                (message "Looking for reductions in goto-state: '%s'" goto-state)
                (when (gethash (list goto-state parse-stack) action-table)
                  (let ((action (gethash (list goto-state parse-stack) action-table)))
                    (setq searching-reduction nil)
                    (setq parse-action 'reduce)
                    (message "Action: reduce '%s' -> '%s'" parse-stack action)
                    (setq parse-stack (list action))
                    (setq state action)))
                (setq goto-state (pop goto-states)))))))

      (when (and (not parse-action) continue)
        (push look-ahead parse-stack)
        (pop unscanned)
        (message "Action: 'shift '%s'" look-ahead))

      (setq look-ahead (car (car unscanned))))
    parse-stack))

(defun phps-mode-parser-custom--generate-parser (&optional grammar start)
  "Generate action-table, goto-table and leaf states for GRAMMAR starting at START."
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
      (message "State: '%s'" state-name)
      (puthash state-name (list state-name) goto-table)
      (let ((is-leaf t))
        (let ((state (gethash state-name grammar)))
          (dolist (state-block state)
            (let ((state-patterns (nreverse (car state-block)))
                  (state-logic (cdr state-block)))
              (message "Reduction: '%s' -> '%s'" state-patterns state-name)
              (puthash (list state-name state-patterns) state-name action-table)
              (message "State-patterns: '%s'" state-patterns)
              (dolist (state-pattern state-patterns)

                ;; Does rule contain a branch?
                (if (gethash state-pattern grammar)
                    (progn
                      (message "Branch-pattern: '%s'" state-pattern)

                      ;; This state is not a leaf
                      (when is-leaf
                        (setq is-leaf nil))

                      (let ((state-connections)
                             (has-link))
                        (when (gethash state-pattern goto-table)
                          (setq state-connections (gethash state-pattern goto-table)))

                        ;; Check if relationship is already saved
                        (dolist (connection state-connections)
                          (when (equal connection state-name)
                            (message "Relation ship already exists '%s'" state-name)
                            (setq has-link t)))

                        ;; Save new relationship
                        (unless has-link
                          (push state-name state-connections)
                          (message "Relationship: '%s' -> '%s'" state-pattern state-name))

                        (puthash state-pattern state-connections goto-table))
                      (when (and (not (equal state-pattern state-name))
                                 (not (gethash state-pattern parsed-states)))
                        (push state-pattern state-queue)))
                  (message "Leaf-pattern: '%s'" state-pattern))))))

        (when is-leaf
          (message "Leaf-state: '%s'" state-name)
          (push state-name leaf-states))

        ;; Mark state as parsed
        (puthash state-name t parsed-states)

        ;; Process next state in queue
        (setq state-name (pop state-queue))))
    (setq phps-mode-parser-custom--parser-leaf-states leaf-states)
    (setq phps-mode-parser-custom--parser-goto-table goto-table)
    (setq phps-mode-parser-custom--parser-action-table action-table)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
