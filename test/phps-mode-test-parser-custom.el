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

(defun phps-mode-test-parser-custom--parse ()
  "Test `phps-mode-parser-custom--parse'."
  (message "-- Running tests for parse... --\n")



  (message "\n-- Ran tests for parse. --"))

(defun phps-mode-test-parser-custom--parse-state ()
  "Run test for `phps-mode-parser-custom--parse-state'."
  (message "\n-- Run tests for parse-state. --\n")

  (with-temp-buffer
    (insert "<?php")
    (setq phps-mode-parser-custom--tokens (list '(T_OPEN_TAG 1 . 5)))
    (should (equal (phps-mode-parser-custom--parse-state 'use_type) nil)))
  (message "Passed test - no matching tokens\n")

  (with-temp-buffer
    (insert "function")
    (setq phps-mode-parser-custom--tokens (list '(T_FUNCTION 1 . 9)))
    (should (equal (phps-mode-parser-custom--parse-state 'use_type) (list nil (list 'phps-mode-parser--ZEND_SYMBOL_FUNCTION)))))
  (message "Passed test - all matching tokens\n")

  (with-temp-buffer
    (insert "Random")
    (setq phps-mode-parser-custom--tokens (list '(T_STRING 1 . 7)))
    (setq phps-mode-parser-custom-grammar--state 'name)
    (should (equal (phps-mode-parser-custom--parse-state 'name) (list nil '(attr phps-mode-parser--ZEND_NAME_NOT_FQ "Random")))))
  (message "Passed test - matching all tokens from name state\n")
  
  (with-temp-buffer
    (insert "<?php Random;\n\nRandom\\Stuff\\Here();")
    (setq phps-mode-parser-custom--tokens (list '(T_STRING 16 . 22) '(T_NS_SEPARATOR 22 . 23) '(T_STRING 23 . 28) '(T_NS_SEPARATOR 28 . 29) '(T_STRING 29 . 33)))
    (setq phps-mode-parser-custom-grammar--state 'name)
    (should (equal (phps-mode-parser-custom--parse-state 'name) (list nil '(attr phps-mode-parser--ZEND_NAME_NOT_FQ (("Random")))))))
  (message "Passed test - matching all tokens from recursive state")

  ;; TODO Make more state-based tests here


  (message "\n-- Ran tests for parse-state. --"))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-test-parser-custom--parse-state)
  ;; (phps-mode-test-parser-custom--parse)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
