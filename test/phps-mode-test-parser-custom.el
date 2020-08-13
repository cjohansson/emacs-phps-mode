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

  (let ((grammar (make-hash-table :test 'equal)))
    (puthash
     'open
     (list
      (list
       (list 'T_OPEN_TAG)
       (lambda(arguments) (list 'OPEN)))
      (list
       (list 'T_OPEN_TAG_WITH_ECHO)
       (lambda(arguments) (list 'OPEN 'ECHO))))
     grammar)
    (puthash
     'close
     (list
      (list
       (list 'T_CLOSE_TAG)
       (lambda(arguments) (list 'CLOSE))))
     grammar)

    (setq
     phps-mode-parser-custom--grammar
     grammar)

    (with-temp-buffer
      (insert "<?php ?>")
      (should
       (equal
        (phps-mode-parser-custom--parse-tokens
         (list '(T_OPEN_TAG 1 . 7) '(";" 7 . 9) '(T_CLOSE_TAG 7 . 9))
         (current-buffer))
        (list 'OPEN 'CLOSE))))

    )(message "\n-- Ran tests for parse. --"))

(defun phps-mode-test-parser-custom--parse-state-2 ()
  "Run test for `phps-mode-parser-custom--parse-state'."
  (message "\n-- Run tests for parse-state. --")

  (let ((grammar (make-hash-table :test 'equal)))

    (puthash
     'close
     (list
      (list
       (list 'T_CLOSE_TAG)
       (lambda(arguments) (list 'CLOSE))))
     grammar)

    ;; Simple evaluation
    (with-temp-buffer
      (insert "?>")
      (setq
       phps-mode-parser-custom--tokens
       (list '(T_CLOSE_TAG 1 . 3)))
      (should
       (equal
        (phps-mode-parser-custom--evaluate-rule
         (list
          (list 'T_CLOSE_TAG)
          (lambda(arguments) (list 'CLOSE)))
         grammar)
        (list 'CLOSE))))

    ;; Recursive evaluation
    (with-temp-buffer
      (insert "<?php ?>")
      (setq
       phps-mode-parser-custom--tokens
       (list '(T_OPEN_TAG 1 . 6) '(T_CLOSE_TAG 7 . 8)))
      (should
       (equal
        (phps-mode-parser-custom--evaluate-rule
         (list
          (list 'T_OPEN_TAG 'close)
          (lambda(arguments)(list 'OPEN 'CLOSE)))
         grammar)
        (list 'OPEN 'CLOSE))))

    (message "\n-- Ran tests for parse-state. --")))

(defun phps-mode-test-parser-custom--parse-state ()
  "Run test for `phps-mode-parser-custom--parse-state'."
  (message "\n-- Run tests for parse-state. --")
  (setq phps-mode-parser-custom--grammar (make-hash-table :test 'equal))

  ;; Setup grammar
  (puthash
   'close
   (list
    (list
     (list 'T_CLOSE_TAG)
     (lambda(a) (list 'CLOSE))))
   phps-mode-parser-custom--grammar)

  (puthash
   'open
   (list
    (list
     (list 'T_OPEN_TAG)
     (lambda(a) (list 'OPEN)))
    (list
     (list 'T_OPEN_TAG_WITH_ECHO)
     (lambda(a) (list 'OPEN 'ECHO))))
   phps-mode-parser-custom--grammar)

  (with-temp-buffer
    (insert "?>")
    (setq phps-mode-parser-custom--tokens (list '(T_OPEN_TAG 1 . 7)))
    (should (equal (phps-mode-parser-custom--parse-state 'close) nil)))
  (message "Passed test 1")

  (with-temp-buffer
    (insert "?>")
    (setq phps-mode-parser-custom--tokens (list '(T_CLOSE_TAG 1 . 3)))
    (should (equal (phps-mode-parser-custom--parse-state 'close) (list nil (list 'CLOSE)))))
  (message "Passed test 2")

  (with-temp-buffer
    (insert "<?php")
    (setq phps-mode-parser-custom--tokens (list '(T_OPEN_TAG 1 . 5)))
    (should (equal (phps-mode-parser-custom--parse-state 'close) nil)))
  (message "Passed test 2")

  (with-temp-buffer
    (insert "<?php")
    (setq phps-mode-parser-custom--tokens (list '(T_OPEN_TAG 1 . 5)))
    (should (equal (phps-mode-parser-custom--parse-state 'open) (list nil (list 'OPEN)))))
  (message "Passed test 3")

  (with-temp-buffer
    (insert "<?= ?>")
    (setq phps-mode-parser-custom--tokens (list '(T_OPEN_TAG_WITH_ECHO 1 . 3)))
    (should (equal (phps-mode-parser-custom--parse-state 'open) (list nil (list 'OPEN 'ECHO)))))
  (message "Passed test 4")

  (with-temp-buffer
    (insert "<?= echo")
    (setq phps-mode-parser-custom--tokens
          (list
           '(T_OPEN_TAG_WITH_ECHO 1 . 7)
           '(T_ECHO 8 . 13)))
    (should
     (equal
      (phps-mode-parser-custom--parse-state 'open)
      (list 'OPEN 'ECHO 'ECHO))))

  (with-temp-buffer
    (insert "<?= {")
    (setq phps-mode-parser-custom--tokens
          (list
           '(T_OPEN_TAG_WITH_ECHO 1 . 7)
           '("{" 8 . 12)))
    (should
     (equal
      (phps-mode-parser-custom--parse-state 'open)
      (list 'OPEN 'ECHO "{"))))

  (with-temp-buffer
    (insert "<?= ?>")
    (setq phps-mode-parser-custom--tokens
          (list
           '(T_OPEN_TAG_WITH_ECHO 1 . 3)
           '(T_CLOSE_TAG 5 . 7)))
    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule 'open)
      (list 'OPEN 'ECHO 'CLOSE))))

  (with-temp-buffer
    (insert "<?= ?>")
    (setq phps-mode-parser-custom--tokens
          (list
           '(T_OPEN_TAG_WITH_ECHO 1 . 7)
           '(T_STRING 10 . 15)))
    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule 'open)
      (list 'OPEN 'ECHO "random"))))

  (with-temp-buffer
    (insert "<?= ?> echo")
    (setq phps-mode-parser-custom--tokens
          (list
           '(T_OPEN_TAG_WITH_ECHO 1 . 7)
           '(T_CLOSE_TAG 8 . 10)
           '(T_ECHO 11 . 14)))
    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule 'open)
      (list 'OPEN 'ECHO 'CLOSE 'ECHO))))

  (message "\n-- Ran tests for parse-state. --"))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-test-parser-custom--parse-state)
  ;; (phps-mode-test-parser-custom--parse)
  ;; (phps-mode-test-parser-custom--open)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
