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

(defun phps-mode-test-parser-custom--open ()
  "Test start entry-point."
  (message "-- Running tests for open entry-point... --\n")

  (let ((ret
         (phps-mode-parser-custom--parse-tokens
          '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 11 . 12) (T_LNUMBER 12 . 13) (";" 13 . 14) (T_EXIT 15 . 19) (T_VARIABLE 20 . 24) (";" 24 . 25) (";" 26 . 28) (T_CLOSE_TAG 26 . 28))
          nil)))
    (should
     (equal
      ret
      (list nil t))))

  (message "\n-- Ran tests for open entry-point. --"))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-test-parser-custom--open)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
