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
  `(let ((test-buffer-incremental
          (generate-new-buffer "test-incremental"))
         (incremental-cache)
         (incremental-parse-trail)
         (incremental-parse-error)
         (incremental-tokens)
         (incremental-imenu)
         (incremental-buffer)
         (incremental-bookkeeping)
         (test-buffer-initial
          (generate-new-buffer "test-initial"))
         (initial-cache)
         (initial-parse-trail)
         (initial-parse-error)
         (initial-tokens)
         (initial-imenu)
         (initial-buffer)
         (initial-bookkeeping))

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-incremental)
     (insert ,source)
     (goto-char 0)

     (phps-mode-debug-message
       (message "\nTesting incremental buffer '%s':\n'%s'\n" ,title ,source))

     (phps-mode)

     (phps-mode-debug-message
      (message "\nPerforming changes\n"))

     (switch-to-buffer test-buffer-incremental)
     ,@change

     (phps-mode-lex-analyzer--process-changes
      test-buffer-incremental)

     (setq
      incremental-cache
      (phps-mode-test--hash-to-list
       phps-mode-lex-analyzer--cache
       t))
     (setq incremental-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq incremental-parse-error phps-mode-lex-analyzer--parse-error)
     (setq incremental-tokens phps-mode-lex-analyzer--tokens)
     (setq incremental-imenu phps-mode-lex-analyzer--imenu)
     (save-restriction
       (widen)
       (setq incremental-buffer (buffer-substring (point-min) (point-max))))
     (setq incremental-bookkeeping (phps-mode-test--hash-to-list phps-mode-lex-analyzer--bookkeeping t))

     ;; Setup incremental buffer
     (switch-to-buffer test-buffer-initial)
     (insert incremental-buffer)
     (goto-char 0)
     (phps-mode-debug-message
       (message "\nTesting initial buffer '%s':\n'%s'\n" ,title incremental-buffer))

     (phps-mode)

     (setq
      initial-cache
      (phps-mode-test--hash-to-list
       phps-mode-lex-analyzer--cache
       t))
     (setq initial-parse-trail phps-mode-lex-analyzer--parse-trail)
     (setq initial-parse-error phps-mode-lex-analyzer--parse-error)
     (setq initial-tokens phps-mode-lex-analyzer--tokens)
     (setq initial-imenu phps-mode-lex-analyzer--imenu)
     (setq initial-buffer (buffer-substring (point-min) (point-max)))
     (setq
      initial-bookkeeping
      (phps-mode-test--hash-to-list
       phps-mode-lex-analyzer--bookkeeping
       t))

     ;; Run tests
     (phps-mode-debug-message
       (message "\nComparing tokens, bookkeeping and imenu between buffer:\n\n'%s'\n\nand:\n\n'%s'\n" initial-buffer incremental-buffer))
     (should (equal initial-buffer incremental-buffer))

     ;; (message "Initial tokens: %s\n" initial-tokens)
     ;; (message "Incremental tokens: %s\n" incremental-tokens)

     (should (equal initial-cache incremental-cache))
     (should (equal initial-tokens incremental-tokens))
     (should (equal initial-parse-trail incremental-parse-trail))
     (should (equal initial-parse-error incremental-parse-error))
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
       (error "PHP Parse Error: %s" phps-mode-lex-analyzer--parse-error))
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
