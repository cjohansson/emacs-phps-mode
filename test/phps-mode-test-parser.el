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

(defun phps-mode-test-parser--buffer-contents (buffer-contents name logic)
  (generate-new-buffer "*PHPs Lexer*")
  (with-current-buffer "*PHPs Lexer*"
    (kill-region (point-min) (point-max))
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))

    ;; Reset lexer
    (setq-local
     phps-mode-lexer--generated-tokens
     nil)
    (setq-local
     phps-mode-lexer--state
     'ST_INITIAL)
    (setq-local
     phps-mode-lexer--states
     nil)
    (setq-local
     phps-mode-lexer--state-stack
     nil)
    (setq-local
     phps-mode-lexer--heredoc-label
     nil)
    (setq-local
     phps-mode-lexer--heredoc-label-stack
     nil)
    (setq-local
     phps-mode-lexer--nest-location-stack
     nil)

    (funcall logic)

    (message "Passed %s" name)
    (kill-buffer)))

(defun phps-mode-test-parser-boundaries ()
  "Run test for lexer."
  (message "-- Running tests for parser boundaries... --\n")

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'hello';"
   "Basic echo test"
   (lambda()

     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation in reverse: %S" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 459 466 411 333 332 154 102 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<? echo 'hello'; ?>"
   "Basic echo test 2 with short open tag and close tag"
   (lambda()
     (should
      (equal
       '(80 459 466 411 333 332 154 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?= 'hello';"
   "Basic echo test 3 with open tag with echo"
   (lambda()
     (should
      (equal
       '(80 459 466 411 333 332 154 102 79)
       (phps-mode-parser-parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'\necho 'here';"
   "Basic echo test 4 with invalid code"
   (lambda()
     (should-error
      (phps-mode-parser-parse))))

  (phps-mode-test-parser--buffer-contents
   "<?php\necho 'blaha'"
   "Basic echo test 5 with invalid code"
   (lambda()
     (should-error
      (phps-mode-parser-parse))))

  (phps-mode-test-parser--buffer-contents
   "<? echo '<!DOCTYPE html>'; ?><html><head><?php echo 'My Title'; ?><body></html>"
   "Advanced echo test with 2 echo sections"
   (lambda()
     (let ((parse (phps-mode-parser-parse)))
       (message "Left-to-right with left-most derivation in reverse: %S" parse)
       (dolist (production-number (reverse parse))
         (let ((production
                (phps-mode-parser--get-grammar-production-by-number
                 production-number)))
           (message
            "%d: %S -> %S"
            production-number
            (car (car production))
            (car (car (cdr production))))))
       (message "\n")
       (should
        (equal
         '(80 459 466 411 333 332 154 102 79 155 102 79 459 466 411 333 332 154 102 79 155 102 79)
         parse)))))

  (phps-mode-test-parser--buffer-contents
   "<?php echo 'hello'; ?>"
   "Basic translation test of echo with open tag and close tag"
   (lambda()
     (should
      (equal
       '(nil ("echo" "'hello'" ";"))
       (phps-mode-parser-translate)))))

  (message "\n-- Ran tests for parser boundaries. --"))

(defun phps-mode-test-parser ()
  "Run test for lexer."
  (message "-- Running all tests for parser... --\n")

  (phps-mode-test-parser-boundaries)

  (message "\n-- Ran all tests for parser. --"))

(phps-mode-test-parser)

(provide 'phps-mode-test-parser)

;; TODO
;; phps-mode-parser.el:65:167: Warning: reference to free variable
;;     ‘phps-mode-lexer--generated-new-tokens-index’
;; phps-mode-parser.el:65:322: Warning: reference to free variable
;;     ‘phps-mode-lexer--generated-new-tokens’
;; phps-mode-parser.el:65:406: Warning: reference to free variable
;;     ‘semantic-lex-end-point’

;; In end of data:
;; phps-mode-parser.el:65:271: Warning: the function ‘phps-mode-lexer--re2c’ is
;;     not known to be defined.


;;; phps-mode-test-parser.el ends here
