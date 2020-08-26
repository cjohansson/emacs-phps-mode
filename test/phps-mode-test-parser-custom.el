;;; phps-mode-test-parser-custom.el --- Tests for custom parser -*- lexical-binding: t -*-

;; Copyright (C) 2017-2020  Free Software Foundation, Inc.

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Run from terminal make test-parser-custom


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(defun phps-mode-tester-parser-custom--generate-parser-table ()
  "Test this."
  (message "\n-- Run tests for generate-parser-table. --\n")

  (phps-mode-parser-custom--generate-parser)

  (message "Generated parser.\n")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10))
     'namespace_name)
    (list '(name  1 . 10))))
  (message "Passed non-recursive parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10) '(T_NS_SEPARATOR 11 . 12) '(T_STRING 13 . 16) '(T_NS_SEPARATOR 17 . 18) '(T_STRING 19 . 29))
     'namespace_name)
    (list '(name  1 . 29))))
  (message "Passed recursive parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_STRING 1 . 10) '(T_NS_SEPARATOR 11 . 12) '(T_STRING 13 . 16) '(T_NS_SEPARATOR 17 . 18))
     'namespace_name)
    (list '(namespace_name 1 . 16) '(T_NS_SEPARATOR 17 . 18))))
  (message "Passed recursive semi-parse of namespace-name")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_ECHO 7 . 11) '(T_CONSTANT_ENCAPSED_STRING 12 . 22) '(";" 22 . 23)))
    (list '(reserved_non_modifiers 7 . 11) '(T_CONSTANT_ENCAPSED_STRING 12 . 22) '(";" 22 . 23))))

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_FUNCTION 7 . 11)))
    (list '(use_type 7 . 11))))

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15) '(";" 16 . 17))
     'top_statement)
    (list '(top_statement 7 . 17))))
  (message "Passed full top_statement")

  (should
   (equal
    (phps-mode-parser-custom--parse
     (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15))
     'top_statement)
    (list '(T_HALT_COMPILER 7 . 11) '("(" 12 . 13) '(")" 14 . 15))))
  (message "Passed incomplete top_statement")

  (message "\n-- Ran tests for generate-parser-table. --"))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-tester-parser-custom--generate-parser-table)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
