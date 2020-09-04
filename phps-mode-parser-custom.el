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

(defun phps-mode-parser-custom--parse (&optional unscanned state leaf-states action-table goto-table)
  "Parse UNSCANNED in LEAF-STATES, use parser ACTION-TABLE and GOTO-TABLE in STATE."
  (unless unscanned
    (setq unscanned phps-mode-lex-analyzer--tokens))
  (unless leaf-states
    (setq leaf-states phps-mode-parser-custom--parser-leaf-states))
  (unless action-table
    (setq action-table phps-mode-parser-custom--parser-action-table))
  (unless goto-table
    (setq goto-table phps-mode-parser-custom--parser-goto-table))
  (let ((parse-stack)
        (parse-tree)
        (step 0)
        (look-ahead)
        (parse-action)
        (continue t)
        (last-reduction-state)
        (shift-stack)
        (potential-shift-stack))
    (setq look-ahead (car (car unscanned)))
    (while continue
      (setq potential-shift-stack parse-stack)
      (push look-ahead potential-shift-stack)
      (setq potential-shift-stack (reverse potential-shift-stack))

      (phps-mode-debug-message
       (message "State: '%s'" state)
       (message "Parse-stack: '%s'" parse-stack)
       (message "Parse-tree: '%s'" parse-tree)
       (message "Look-ahead: '%s'" look-ahead)
       (message "Potential-shift-stack: '%s'" potential-shift-stack)
       (message "Unscanned: '%s'" unscanned))

      (setq parse-action nil)

      (if state
          (let ((found-shift-action nil)
                (found-reduce-action nil)
                (valid-shifts))

            ;; Get state action-table
            (let* ((state-action-table (gethash state action-table))
                   (state-action-status (gethash potential-shift-stack state-action-table)))

              ;; (message "Found-state-action-table: '%s'" state-action-table)
              (if state-action-status
                  (progn
                    (when (equal state-action-status t)
                      (setq found-reduce-action leaf-state))
                    (setq found-shift-action t))
                (when (gethash shift-stack state-action-table)
                  (push (gethash shift-stack state-action-table) valid-shifts))))

            (if found-shift-action
                (let ((popped-token (pop unscanned)))

                  ;; Shift token
                  (push (car popped-token) parse-stack)
                  (push popped-token parse-tree)

                  ;; Did we shift a entire rule? Then reduce stack and change state
                  (when found-reduce-action
                    (error (format "Should do a partial reduce action here '%s' -> '%s'" parse-stack found-reduce-action))
                    (message "Shifted entire entry-point pattern: '%s' -> '%s'" parse-stack found-reduce-action)
                    (setq parse-stack (list found-reduce-action))
                    (setq parse-tree `(,found-reduce-action . ,parse-tree))
                    (setq state found-reduce-action)))

              (let ((gotos (gethash state goto-table))
                    (searching-reduction t)
                    (goto)
                    (goto-lhs)
                    (goto-state)
                    (goto-look-ahead)
                    (goto-rhs)
                    (state-lhs))
                (message "Found no shift action, valid shifts: '%s'" valid-shifts)

                (setq goto (pop gotos))
                (while (and searching-reduction goto)
                  (setq goto-lhs (car goto))
                  (setq goto-state (car (cdr goto)))
                  (setq goto-look-ahead (car (cdr (cdr goto))))
                  (setq goto-rhs (car (cdr (cdr goto))))
                  (message "Goto: LHS: '%s', state: '%s', look-ahead: '%s', RHS: '%s'" goto-lhs goto-state goto-look-ahead goto-rhs)

                  ;; Is pattern look-ahead empty or does it match current look-ahead?
                  (when (or (not goto-look-ahead)
                            (equal look-ahead goto-look-ahead))
                    (message "State look-ahead matches goto look-ahead or goto look-ahead is empty")

                    ;; If GOTO has a left-hand-side build a state left-hand-side if possible
                    (when (and goto-lhs
                               (>= (length parse-stack) (length goto-lhs)))
                      (let ((remaining (length goto-lhs)))
                        (while (> remaining 0)
                          (push (nth remaining parse-stack) state-lhs))))
                    (message "State-LHS: '%s'" state-lhs)

                    ;; Is pattern left-hard side empty or does it match state left-hand-side?
                    (when (or (not goto-lhs)
                              (equal state-lhs goto-lhs))
                      (message "State LHS matches goto LHS or goto LHS is empty")
                      (setq found-reduce-action t)
                      (setq searching-reduction nil)))
                  (setq goto (pop gotos)))

                (if found-reduce-action
                    (progn
                      (message "Moving to new state: '%s'" goto-state)
                      (setq state goto-state))
                  (error (format "Unexpected state! State: '%s' Parse Stack: '%s'" state parse-stack))
                  ))))

        ;; We are at the entry-point
        (let ((found-shift-action nil)
              (found-reduce-action nil)
              (leaf-state)
              (leaf-states-stack leaf-states)
              (valid-shifts))
          (setq leaf-state (pop leaf-states-stack))
          (while (and
                  leaf-state
                  (not found-shift-action))

            ;; Get state action-table
            (let* ((state-action-table (gethash leaf-state action-table))
                   (state-action-status (gethash potential-shift-stack state-action-table)))
              ;; (message "Found-state-action-table: '%s'" state-action-table)
              (if state-action-status
                  (progn
                    (when (equal state-action-status t)
                      (setq found-reduce-action leaf-state))
                    (setq found-shift-action t))
                (when (gethash shift-stack state-action-table)
                  (push (gethash shift-stack state-action-table) valid-shifts))))

            (setq leaf-state (pop leaf-states-stack)))

          (if found-shift-action
              (let ((popped-token (pop unscanned)))

                ;; Shift token
                (push (car popped-token) parse-stack)
                (push popped-token parse-tree)

                ;; Did we shift a entire rule? Then reduce stack and change state
                (when found-reduce-action
                  (message "Shifted entire entry-point pattern: '%s' -> '%s'" parse-stack found-reduce-action)
                  (setq parse-stack (list found-reduce-action))
                  (setq parse-tree `(,found-reduce-action . ,parse-tree))
                  (setq state found-reduce-action)))

            (error (format "Syntax error! Unexpected token '%s'. Expecting any of '%s'" look-ahead valid-shifts)))))

      (message "\n")
      (setq look-ahead (car (car unscanned))))
    (nreverse parse-tree)))

;; TODO Need to re-design entry-point in grammar
(defun phps-mode-parser-custom--generate-parser (&optional grammar start)
  "Generate action-table, goto-table and leaf states for GRAMMAR starting at START."
  (unless grammar
    (setq grammar phps-mode-parser-custom-grammar))
  (unless start
    (setq start phps-mode-parser-custom-grammar--start-state))
  (let ((state-queue (list (list nil start nil)))
        (state-list)
        (shift-table (make-hash-table :test 'equal))
        (state-graph (make-hash-table :test 'equal))
        (parsed-states (make-hash-table :test 'equal))
        (state)
        (state-prefix)
        (state-name)
        (state-look-ahead)
        (leaf-states))

    ;; Build top-down directed acyclic graph (dag) of grammar
    (phps-mode-debug-message
     (message "Building top-down directed acyclic graph (dag) of grammar starting at '%s'..\n" start))
    (setq state (pop state-queue))
    (setq state-prefix (car state))
    (setq state-name (car (cdr state)))
    (setq state-look-ahead (car (cdr (cdr state))))
    (while state-name
      (unless (gethash state-name parsed-states)
        (phps-mode-debug-message (message "State: '%s', prefix: '%s', look-ahead: '%s'" state-name state-prefix state-look-ahead))

        (let ((is-leaf nil))
          (let ((state (gethash state-name grammar))
                (prefix))

            ;; Iterate all grammar blocks in state
            (dolist (state-block state)
              (setq prefix nil)
              (let ((state-patterns (car state-block))
                    (state-logic (cdr state-block))
                    (state-pattern)
                    (look-ahead)
                    (right-hand-side)
                    (remaining)
                    (is-first-letter t))

                ;; Iterate all patterns in grammar block
                (setq state-pattern (pop state-patterns))
                (setq look-ahead (car state-patterns))
                (while state-pattern

                  ;; Does rule contain a branch?
                  (if (and
                       (not (equal state-pattern '%empty))
                       (gethash state-pattern grammar))
                      (progn
                        (setq remaining state-patterns)
                        (setq right-hand-side (reverse prefix))

                        ;; When pattern starts with intermediate which isn't itself it's not a leaf
                        (when (and is-first-letter
                                   (setq is-leaf nil)))

                        ;; If look-ahead is a intermediate set it to nil
                        (when look-ahead
                          (when (gethash look-ahead grammar)
                            (setq look-ahead nil)))

                        (let ((state-connections)
                              (has-link))
                          (when (gethash state-pattern state-graph)
                            (setq state-connections (gethash state-pattern state-graph)))

                          ;; Check if relationship (directed connected nodes with right-hand-side) is already saved
                          (dolist (connection state-connections)
                            (when (equal connection (list right-hand-side state-name look-ahead remaining))
                              (setq has-link t)))

                          ;; Save new relationship
                          (unless has-link
                            (phps-mode-debug-message (message "'%s' -> %s '%s' %s %s" state-name right-hand-side state-pattern look-ahead remaining))
                            (push (list right-hand-side state-name look-ahead remaining) state-connections)
                            (puthash state-pattern state-connections state-graph)))

                        (when (and (not (equal state-pattern state-name))
                                   (not (gethash state-pattern parsed-states)))
                          ;; (phps-mode-debug-message (message "Added-to-state-queue: %s %s" right-hand-side state-pattern))
                          (push (list right-hand-side state-pattern look-ahead) state-queue)))
                    (unless (equal state-pattern '%empty)
                      (setq is-leaf t)))

                  ;; Avoid listing recursive grammar as having prefix
                  (unless (equal state-pattern state-name)
                    (push state-pattern prefix))

                  (setq state-pattern (pop state-patterns))
                  (setq look-ahead (car state-patterns))
                  (setq is-first-letter nil)))))

          (when (and is-leaf
                 (not state-prefix))
            (phps-mode-debug-message
             (message "Leaf-state: '%s'" state-name)
             )
            (push state-name leaf-states))

          ;; Mark state as parsed
          (puthash state-name t parsed-states)
          (push state-name state-list)))

      ;; Process next state in queue
      (setq state (pop state-queue))
      (setq state-prefix (car state))
      (setq state-name (car (cdr state)))
      (setq state-look-ahead (car (cdr (cdr state)))))

    (setq leaf-states (nreverse leaf-states))

    (phps-mode-debug-message
     (message "\nGrammar entry points: '%s'\n" leaf-states))

    (phps-mode-debug-message
     (message "\nState-list: '%s'\n" state-list))

    ;; Iterate all states and create action-table for each state shift in every pattern
    (dolist (state state-list)
      (let ((state-blocks (gethash state grammar))
            (state-blocks-action-table (make-hash-table :test 'equal)))
        (dolist (state-block state-blocks)
          (let* ((state-patterns (car state-block))
                 (state-patterns-ack)
                 (state-patterns-ack-old)
                 (state-logic (cdr state-block))
                 (pattern-index 1)
                 (pattern-count (length state-patterns)))
            (dolist (state-pattern state-patterns)
              (push state-pattern state-patterns-ack)

              ;; Save shift action here
              (unless (gethash state-patterns-ack state-blocks-action-table)
                (puthash state-patterns-ack t state-blocks-action-table))

              (let ((old-pattern-list (gethash state-patterns-ack-old state-blocks-action-table))
                    (already-exists nil))
                (if (equal old-pattern-list t)
                    (setq old-pattern-list nil)
                  (dolist (old-item old-pattern-list)
                    (when (equal old-item state-pattern)
                      (setq already-exists t))))
                (unless already-exists
                  (push state-pattern old-pattern-list)
                  (puthash state-patterns-ack-old old-pattern-list state-blocks-action-table)))

              (setq state-patterns-ack-old state-patterns-ack))))
        (puthash state state-blocks-action-table shift-table)))

    (setq phps-mode-parser-custom--parser-action-table shift-table)
    (setq phps-mode-parser-custom--parser-goto-table state-graph)
    (setq phps-mode-parser-custom--parser-leaf-states leaf-states)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-parser-custom.el ends here
