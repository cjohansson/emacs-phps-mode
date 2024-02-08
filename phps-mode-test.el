;;; phps-mode-test.el --- Commons for tests -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-macros)

(defvar
  phps-mode-test--native-tokens
  nil
  "The native tokens of PHP parser (if available).")

(defmacro phps-mode-test--incremental-vs-intial-buffer (source &optional title &rest change)
  "Set up test buffer with SOURCE, TITLE, apply CHANGE and
compare incremental values with initial values."
  `(let ((test-buffer-incremental (generate-new-buffer "test-incremental"))
         (incremental-state)
         (incremental-state-stack)
         (incremental-states)
         (incremental-heredoc-label)
         (incremental-heredoc-label-stack)
         (incremental-parse-trail)
         (incremental-parse-error)
         (incremental-tokens)
         (incremental-imenu)
         (incremental-buffer)
         (incremental-bookkeeping)
         (incremental-nest-location-stack)
         (test-buffer-initial (generate-new-buffer "test-initial"))
         (initial-state)
         (initial-state-stack)
         (initial-states)
         (initial-heredoc-label)
         (initial-heredoc-label-stack)
         (initial-parse-trail)
         (initial-parse-error)
         (initial-tokens)
         (initial-imenu)
         (initial-buffer)
         (initial-bookkeeping)
         (initial-nest-location-stack))

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-incremental)
     (insert ,source)
     (goto-char 0)
     (phps-mode-debug-message
       (message "\nTesting incremental buffer '%s':\n'%s'\n" ,title ,source))
     (phps-mode)

     (phps-mode-debug-message
      (message "\nPerforming changes\n"))
     
     ,@change
     (phps-mode-lex-analyzer--process-changes test-buffer-incremental)
     (setq incremental-state phps-mode-lex-analyzer--state)
     (setq incremental-state-stack phps-mode-lex-analyzer--state-stack)
     (setq incremental-heredoc-label phps-mode-lex-analyzer--heredoc-label)
     (setq incremental-heredoc-label-stack phps-mode-lex-analyzer--heredoc-label-stack)
     (setq incremental-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq incremental-parse-error phps-mode-lex-analyzer--parse-error)
     (setq incremental-states phps-mode-lex-analyzer--states)
     (setq incremental-tokens phps-mode-lex-analyzer--tokens)
     (setq incremental-imenu phps-mode-lex-analyzer--imenu)
     (save-restriction
       (widen)
       (setq incremental-buffer (buffer-substring (point-min) (point-max))))
     (setq incremental-bookkeeping (phps-mode-test--hash-to-list phps-mode-lex-analyzer--bookkeeping t))
     (setq incremental-nest-location-stack phps-mode-lex-analyzer--nest-location-stack)

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-initial)
     (insert incremental-buffer)
     (goto-char 0)
     (phps-mode-debug-message
       (message "\nTesting initial buffer '%s':\n'%s'\n" ,title incremental-buffer))
     (phps-mode)
     (setq initial-state phps-mode-lex-analyzer--state)
     (setq initial-state-stack phps-mode-lex-analyzer--state-stack)
     (setq initial-states phps-mode-lex-analyzer--states)
     (setq initial-heredoc-label phps-mode-lex-analyzer--heredoc-label)
     (setq initial-heredoc-label-stack phps-mode-lex-analyzer--heredoc-label-stack)
     (setq initial-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq initial-parse-error phps-mode-lex-analyzer--parse-error)
     (setq initial-tokens phps-mode-lex-analyzer--tokens)
     (setq initial-imenu phps-mode-lex-analyzer--imenu)
     (setq initial-buffer (buffer-substring (point-min) (point-max)))
     (setq initial-bookkeeping (phps-mode-test--hash-to-list phps-mode-lex-analyzer--bookkeeping t))
     (setq initial-nest-location-stack phps-mode-lex-analyzer--nest-location-stack)

     ;; Run tests
     (phps-mode-debug-message
       (message "\nComparing tokens, bookkeeping and imenu between buffer:\n\n'%s'\n\nand:\n\n'%s'\n" initial-buffer incremental-buffer))
     (should (equal initial-buffer incremental-buffer))
     ;; (message "Initial tokens: %s\n" initial-tokens)
     ;; (message "Incremental tokens: %s\n" incremental-tokens)
     (should (equal initial-tokens incremental-tokens))
     (should (equal initial-state incremental-state))
     (should (equal initial-state-stack incremental-state-stack))
     (should (equal initial-states incremental-states))
     (should (equal initial-heredoc-label incremental-heredoc-label))
     (should (equal initial-heredoc-label-stack incremental-heredoc-label-stack))
     (should (equal initial-parse-trail incremental-parse-trail))
     (should (equal initial-parse-error incremental-parse-error))
     (should (equal initial-nest-location-stack incremental-nest-location-stack))
     (should (equal initial-imenu incremental-imenu))
     (should (equal initial-bookkeeping incremental-bookkeeping))

     (kill-buffer test-buffer-incremental)
     (kill-buffer test-buffer-initial)

     (when ,title
       (message "\nPassed incremental tests for '%s'\n" ,title))))

(defmacro phps-mode-test--with-buffer (source &optional title &rest body)
  "Set up test buffer with SOURCE, TITLE and BODY."
  `(let ((test-buffer (generate-new-buffer "test")))
     (switch-to-buffer test-buffer)
     (insert ,source)
     (goto-char 0)
     (phps-mode-debug-message
      (message
       "\nTesting buffer '%s':\n'%s'\n"
       ,title
       (buffer-substring-no-properties (point-min) (point-max))))
     (phps-mode)
     (when phps-mode-lex-analyzer--parse-error
       (error "PHP Parse Error: " phps-mode-lex-analyzer--parse-error))
     ,@body
     (kill-buffer test-buffer)
     (when ,title
       (message "\nPassed tests for '%s'\n" ,title))))

(defun phps-mode-test--hash-to-list (hash-table &optional un-sorted)
  "Return a list that represent the HASH-TABLE.
Each element is a list: (list key value), optionally UN-SORTED."
  (let (result)
    (if (hash-table-p hash-table)
        (progn
          (maphash
           (lambda (k v)
             (push (list k v) result))
           hash-table)
          (if un-sorted
              (nreverse result)
            (sort
             (nreverse result)
             (lambda (a b)
               (if (listp (car a))
                   (< (car (car a)) (car (car b)))
                 (< (car a) (car b)))))))
      nil)))

(defun phps-mode-test--output-parse-productions (parse)
  "Output productions by PARSE trail."
  (message "Left-to-right with right-most derivation in reverse:\n%S\n" parse)
  (dolist (production-number parse)
    (let ((production
           (phps-mode-parser--get-grammar-production-by-number
            production-number)))
      (message
       "%d: %S -> %S"
       production-number
       (car (car production))
       (car (cdr production)))))
  (message "\n"))

(transient-mark-mode t)
(electric-pair-mode t)
(setq phps-mode-async-process nil)

(provide 'phps-mode-test)

;;; phps-mode-test.el ends here
