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

(defmacro phps-mode-test-parser-custom--with-buffer (tokens buffer-string state)
  "Create a buffer containing BUFFER-STRING and run `phps-mode-parser-custom--parse-tokens' with TOKENS and STATE."
  `(let ((test-buffer (generate-new-buffer "*test*")))
     (switch-to-buffer test-buffer)
     (insert ,buffer-string)
     (goto-char 0)
     (let ((ret
            (phps-mode-parser-custom--parse-tokens
             tokens
             test-buffer
             state)))
       (kill-buffer)
       ret)))

(defun phps-mode-test-parser-custom--open ()
  "Test start entry-point."
  (message "-- Running tests for open entry-point... --\n")

  (let ((ret
         (phps-mode-test-parser-custom--with-buffer
          '((T_OPEN_TAG 1 . 7))
          "<?php\t"
          nil)))
    (should
     (equal
      ret
      (list (list '(T_OPEN_TAG 1 . 7)) 0))))

  (let ((ret
         (phps-mode-test-parser-custom--with-buffer
          '((T_FINAL 1 . 5))
          "final"
          nil)))
    (should
     (equal
      ret
      (list nil 1))))

  (message "\n-- Ran tests for open entry-point. --"))

(defun phps-mode-test-parser-custom--tokens-satisfy-rule-p ()
  "Run test for `phps-mode-parser-custom--tokens-satisfy-rule-p'."
  (message "\n-- Run tests for tokens-satisfy-rule-p. --")

  (let ((grammar (make-hash-table :test 'equal)))

    (puthash
     'close
     (list
      (list 'T_CLOSE_TAG)
      (lambda()))
     grammar)

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list '(T_OPEN_TAG 1 . 7))
       (list 'T_OPEN_TAG)
       grammar)
      t))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list '(T_OPEN_TAG 1 . 7))
       (list 'T_OPEN_TAG_WITH_ECHO)
       grammar)
      nil))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '(T_CLOSE_TAG 8 . 10))
       (list 'T_OPEN_TAG_WITH_ECHO 'T_CLOSE_TAG)
       grammar)
      t))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '(T_ECHO 8 . 12))
       (list 'T_OPEN_TAG_WITH_ECHO 'T_CLOSE_TAG)
       grammar)
      nil))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '("{" 8 . 12))
       (list 'T_OPEN_TAG_WITH_ECHO "{")
       grammar)
      t))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '(T_CLOSE_TAG 8 . 10))
       (list 'T_OPEN_TAG_WITH_ECHO 'close)
       grammar)
      t))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '(T_ECHO 8 . 10))
       (list 'T_OPEN_TAG_WITH_ECHO 'close)
       grammar)
      nil))

    (should
     (equal
      (phps-mode-parser-custom--tokens-satisfy-rule-p
       (list
        '(T_OPEN_TAG_WITH_ECHO 1 . 7)
        '(T_CLOSE_TAG 8 . 10)
        '(T_ECHO 11 . 14))
       (list 'T_OPEN_TAG_WITH_ECHO 'close 'T_ECHO)
       grammar)
      t))

    (message "\n-- Ran tests for tokens-satisfy-ruke-p. --")))

(defun phps-mode-test-parser-custom ()
  "Run test for custom parser."
  (message "-- Running all tests for custom parser... --\n")
  ;; (setq debug-on-error t)
  (phps-mode-test-parser-custom--tokens-satisfy-rule-p)
  ;; (phps-mode-test-parser-custom--open)
  (message "\n-- Ran all tests for custom parser. --"))

(phps-mode-test-parser-custom)

(provide 'phps-mode-test-parser-custom)

;;; phps-mode-test-parser-custom.el ends here
