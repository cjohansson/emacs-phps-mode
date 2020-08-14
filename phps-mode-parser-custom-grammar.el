;;; phps-mode-parser-custom-grammar.el -- Grammar for the parser for PHPs -*- lexical-binding: t -*-

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

;;; Code:

;; Variables and constants

(defvar phps-mode-parser-custom-grammar nil
  "The current grammar.")

(defconst phps-mode-parser-custom-grammar--start-state 'open
  "The default start state for the grammar.")

(defvar-local phps-mode-parser-custom-grammar--state nil
  "The current state of grammar.")

(defvar-local phps-mode-parser-custom-grammar--state-history nil
  "History of state changes.")

;; Functions

(defun phps-mode-parser-custom-grammar--set-state (state)
  "Setup new STATE."
  (when phps-mode-parser-custom-grammar--state
    (push
     (list (point) phps-mode-parser-custom-grammar--state)
     phps-mode-parser-custom-grammar--state-history))
  (setq phps-mode-parser-custom-grammar--state state))

;; Setup grammar
(setq phps-mode-parser-custom-grammar (make-hash-table :test 'equal))




(provide 'phps-mode-parser-custom-grammar)

;;; phps-mode-parser-custom-grammar.el ends here
