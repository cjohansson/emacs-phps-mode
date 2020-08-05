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

(defun phps-mode-parser-custom--consume-token (tokens buffer state)
  "Try to consume TOKENS from BUFFER in STATE."
  (let ((consumed nil)
        (rules nil))

    ;; TODO 1. Use macro to add all grammars here. Only apply macro logic if state matches grammar.
    
    consumed))

(defmacro phps-mode-parser--rule-grammar (name &rest body)
  "Generate code that modified consumed if state matches NAME."
  (when (= state name)
    (let ((ret 
    ))

(defun phps-mode-parser-custom--parse-tokens (tokens buffer &optional state)
  "Parse TOKENS from BUFFER, optionally start at STATE.  Return parsed abstract syntax tree and err signal."
  (unless state
    (setq
     state
     phps-mode-parser-custom--start-state))
  (let ((ast)
        (err nil))
    (while (and
            tokens
            (not err))
      (let ((consume (phps-mode-parser-custom--consume-token
                      tokens
                      buffer
                      state)))
        (if consume
            (push consume ast)
          (setq
           err
           (format "Failed to parse at state: %s, tokens: %s" state tokens)))))
    (list ast err)))

(provide 'phps-mode-parser-custom)

;;; phps-mode-lexer.el ends here 
