;;; phps-mode-test-parser.el --- Tests for parser -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

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

;; Run from terminal make test-parser


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-lexer)
(require 'phps-mode-parser)

(defun phps-mode-test-parser--buffer-contentes (buffer-contents name logic)
  (generate-new-buffer "*phps-mode-lex-analyzer*")
  (with-current-buffer "*phps-mode-lex-analyzer*"
    (kill-region (point-min) (point-max))
    (insert buffer-contents)
    (message "Testing buffer '%S' with buffer-contents:\n%S\n" name (buffer-substring-no-properties (point-min) (point-max)))

    ;; Reset lexer
    (setq
     phps-mode-lexer--generated-tokens
     nil)
    (setq
     phps-mode-lexer--state
     'ST_INITIAL)
    (setq
     phps-mode-lexer--states
     nil)
    (setq
     phps-mode-lexer--state-stack
     nil)
    (setq
     phps-mode-lexer--heredoc-label
     nil)
    (setq
     phps-mode-lexer--heredoc-label-stack
     nil)
    (setq
     phps-mode-lexer--nest-location-stack
     nil)

    (funcall logic)

    (message "Passed %s" name)))

(defun phps-mode-test-parser()
  "Run test for lexer."
  (message "-- Running all tests for parser... --\n")

  (phps-mode-test-parser--buffer-contentes
   "<?php echo 'hello';"
   "Basic echo test"
   (lambda()
     (message "was here")
     (should
      (equal
       t
       (phps-mode-parser-parse)))))

  (message "\n-- Ran all tests for parser. --"))

(phps-mode-test-parser)

(provide 'phps-mode-test-parser)

;;; phps-mode-test-parser.el ends here
