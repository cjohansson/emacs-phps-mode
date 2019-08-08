;;; phps-mode-test.el --- Commons for tests -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

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


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-functions)
(require 'phps-mode-lexer)

(defmacro phps-mode-test-incremental-vs-intial-buffer (source &optional title &rest change)
  "Set up test buffer with SOURCE, TITLE, apply CHANGE and compare incremental values with initial values."
  `(let ((test-buffer-incremental (generate-new-buffer "test-incremental"))
         (incremental-states nil)
         (incremental-tokens nil)
         (incremental-imenu nil)
         (incremental-indent nil)
         (incremental-buffer nil)
         (incremental-overlays nil)
         (test-buffer-initial (generate-new-buffer "test-initial"))
         (initial-states nil)
         (initial-tokens nil)
         (initial-imenu nil)
         (initial-indent nil)
         (initial-buffer nil)
         (initial-overlays nil))

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-incremental)
     (insert ,source)
     (goto-char 0)
     (when (and (boundp 'phps-mode-functions-verbose)
                phps-mode-functions-verbose)
       (message "\nTesting incremental buffer '%s':\n'%s'\n" ,title ,source))
     (phps-mode)
     ,@change
     (phps-mode-lexer-run-incremental)
     (setq incremental-states (phps-mode-lexer-get-states))
     (setq incremental-tokens (phps-mode-lexer-get-tokens))
     (setq incremental-imenu (phps-mode-functions-get-imenu))
     (setq incremental-indent (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent)))
     (setq incremental-buffer (buffer-substring-no-properties (point-min) (point-max)))
     (setq incremental-overlays (overlays-in (point-min) (point-max)))
     (kill-buffer test-buffer-incremental)

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-initial)
     (insert incremental-buffer)
     (goto-char 0)
     (when (and (boundp 'phps-mode-functions-verbose)
                phps-mode-functions-verbose)
       (message "\nTesting initial buffer '%s':\n'%s'\n" ,title incremental-buffer))
     (phps-mode)
     (setq initial-states (phps-mode-lexer-get-states))
     (setq initial-tokens (phps-mode-lexer-get-tokens))
     (setq initial-imenu (phps-mode-functions-get-imenu))
     (setq initial-indent (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent)))
     (setq initial-buffer (buffer-substring-no-properties (point-min) (point-max)))
     (setq initial-overlays (overlays-in (point-min) (point-max)))
     (kill-buffer test-buffer-initial)

     ;; Run tests
     (when (and (boundp 'phps-mode-functions-verbose)
                phps-mode-functions-verbose)
       (message "\nComparing tokens, lines indent, imenu and overlays between buffer:\n\n'%s'\n\nand:\n\n'%s'\n" initial-buffer incremental-buffer))
     (should (equal initial-buffer incremental-buffer))
     ;; (message "Initial tokens: %s\n" initial-tokens)
     ;; (message "Incremental tokens: %s\n" incremental-tokens)
     (should (equal initial-states incremental-states))
     (should (equal initial-tokens incremental-tokens))
     ;; (message "Initial indent: %s\n" initial-indent)
     ;; (message "Incremental indent: %s\n" incremental-indent)
     (should (equal initial-indent incremental-indent))
     (should (equal initial-imenu incremental-imenu))
     ;; (should (equal initial-overlays incremental-overlays))

     (when ,title
       (message "\nPassed incremental tests for '%s'\n" ,title))))

(defmacro phps-mode-test-with-buffer (source &optional title &rest body)
  "Set up test buffer with SOURCE, TITLE and BODY."
  `(let ((test-buffer (generate-new-buffer "test")))
     (switch-to-buffer test-buffer)
     (insert ,source)
     (goto-char 0)
     (when (and (boundp 'phps-mode-functions-verbose)
                phps-mode-functions-verbose)
       (message "\nTesting buffer '%s':\n'%s'\n" ,title ,source))
     (phps-mode)
     ,@body
     (kill-buffer test-buffer)
     (when ,title
       (message "\nPassed tests for '%s'\n" ,title))))

(defun phps-mode-test-hash-to-list (hash-table)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value)."
  (let (result)
    (maphash
     (lambda (k v)
       (push (list k v) result))
     hash-table)
    (sort (nreverse result) (lambda (a b) (< (car a) (car b))))))

(provide 'phps-mode-test)

;;; phps-mode-test.el ends here
