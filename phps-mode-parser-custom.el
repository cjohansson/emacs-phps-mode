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

      (phps-mode-debug-message
       (message "Parse-state: '%s'" state)
       (message "Parse-stack: '%s'" parse-stack)
       (message "Look-ahead: '%s'" look-ahead)
       (message "Unscanned: '%s'" unscanned))
      (setq parse-action nil)

      ;; If we have a parse-stack, match it to state-patterns
      (when parse-stack
        (let ((goto-states))

          (if state
              (setq goto-states (gethash state goto-table))
            (setq goto-states leaf-states))

          (phps-mode-debug-message
           (message "Looking for reduction in goto-states: '%s'" goto-states))

          (when goto-states
            (let ((goto-state)
                  (searching-reduction t))
              (setq goto-state (pop goto-states))

              ;; Search all goto-states for reduction
              (while (and
                      goto-state
                      searching-reduction)
                (phps-mode-debug-message
                 (message "Looking for reductions in goto-state: '%s'" goto-state))
                (let ((reductions (gethash goto-state action-table))
                      (reduction)
                      (reduction-search)
                      (reduction-length))

                  (when reductions
                    (phps-mode-debug-message
                     (message "Found reductions: '%s'" reductions))

                    ;; Iterate all possible reductions in state
                    (setq reduction (pop reductions))
                    (while (and searching-reduction reduction)
                      (setq reduction-length 0)
                      (setq reduction-search t)

                      (if (< (length parse-stack) (length reduction))
                          (progn
                            (phps-mode-debug-message
                             (message "Reduction is longer than parse stack, ignore"))
                            (setq reduction-search nil))
                        (phps-mode-debug-message
                         (message "Comparing parse-stack: '%s' with reduction: '%s'" parse-stack reduction))
                        ;; Iterate all parts of pattern and compare with stack
                        (setq reduction-subpattern (pop reduction))

                        ;; Empty pattern is no match
                        (unless reduction-subpattern
                          (setq reduction-search nil))

                        (while (and
                                reduction-subpattern
                                reduction-search)
                          (setq reduction-length (1+ reduction-length))

                          (if (equal
                               (nth (1- reduction-length) parse-stack)
                               reduction-subpattern)
                              (phps-mode-debug-message
                               (message "Sub-pattern '%s' did match '%s'" reduction-subpattern (nth (1- reduction-length) parse-stack)))
                            (phps-mode-debug-message
                             (message "Sub-pattern '%s' did not match '%s'" reduction-subpattern (nth (1- reduction-length) parse-stack)))
                            (setq reduction-search nil))

                          (setq reduction-subpattern (pop reduction)))

                        ;; When we find a reduction, stop parent loop
                        (when reduction-search
                          (setq searching-reduction nil)))

                      (setq reduction (pop reductions)))

                    (if reduction-search
                        (progn
                          (setq parse-action 'reduce)
                          (phps-mode-debug-message
                           (message "Action: reduction of length: %s -> '%s'" reduction-length goto-state))
                          (let ((popped-parse-stack))
                            (while (> reduction-length 0)
                              (push (pop parse-stack) popped-parse-stack)
                              (setq reduction-length (1- reduction-length)))
                            (push goto-state parse-stack)
                            (setq state goto-state)
                            (phps-mode-debug-message
                             (message "Popped-parse-stack: '%s'" popped-parse-stack)
                             (message "New-parse-stack: '%s'" parse-stack)
                             (message "New-state: '%s'" state))))
                      (phps-mode-debug-message
                       (message "Failed to find reduction.")))

                    (setq goto-state (pop goto-states)))))))))

      (when (and (not parse-action) continue)
        (if look-ahead
            (progn
              (push look-ahead parse-stack)
              (pop unscanned)
              (phps-mode-debug-message
               (message "Action: 'shift '%s'" look-ahead)))
          (setq continue nil)))

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
      (unless (gethash state-name parsed-states)
        (phps-mode-debug-message
         (message "State: '%s'" state-name))

        ;; A state pattern is always reducible to itself
        (puthash state-name (list state-name) goto-table)

        (let ((is-leaf t))
          (let ((state (gethash state-name grammar)))

            ;; Iterate all grammar blocks in state
            (dolist (state-block state)
              (let ((state-patterns (nreverse (car state-block)))
                    (state-logic (cdr state-block)))
                (phps-mode-debug-message
                 (message "Reduction: '%s' -> '%s'" state-patterns state-name))

                (let ((existing-patterns (gethash state-name action-table)))
                  (if existing-patterns
                      (push state-patterns existing-patterns)
                    (setq existing-patterns (list state-patterns)))
                  (puthash state-name existing-patterns action-table)
                  (phps-mode-debug-message
                   (message "State-action-table: '%s'" existing-patterns)))

                ;; Iterate all patterns in grammar block
                (dolist (state-pattern state-patterns)

                  ;; Does rule contain a branch?
                  (if (and
                       (not (equal state-pattern state-name))
                       (gethash state-pattern grammar))
                      (progn
                        (phps-mode-debug-message
                         (message "Branch-pattern: '%s'" state-pattern))

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
                              (phps-mode-debug-message
                               (message "Relation ship already exists '%s'" state-name))
                              (setq has-link t)))

                          ;; Save new relationship
                          (unless has-link
                            (push state-name state-connections)
                            (phps-mode-debug-message
                             (message "Relationship: '%s' -> '%s'" state-pattern state-name)))

                          (puthash state-pattern state-connections goto-table))
                        (when (and (not (equal state-pattern state-name))
                                   (not (gethash state-pattern parsed-states)))
                          (push state-pattern state-queue)))
                    (phps-mode-debug-message
                     (message "Leaf-pattern: '%s'" state-pattern)))))))

          (when is-leaf
            (phps-mode-debug-message
             (message "Leaf-state: '%s'" state-name))
            (push state-name leaf-states))

          ;; Mark state as parsed
          (puthash state-name t parsed-states)))

      ;; Process next state in queue
      (setq state-name (pop state-queue)))
    (setq phps-mode-parser-custom--parser-leaf-states leaf-states)
    (setq phps-mode-parser-custom--parser-goto-table goto-table)
    (setq phps-mode-parser-custom--parser-action-table action-table)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
