;;; phps-mode.el --- Major mode for PHP with code intelligence -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 3 Mar 2018
;; Modified: 24 Apr 2024
;; Version: 0.4.49
;; Keywords: tools, convenience
;; URL: https://github.com/cjohansson/emacs-phps-mode

;; Package-Requires: ((emacs "26"))

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; A major-mode that uses the original PHP lex-analyzer and parser for syntax coloring, bookkeeping, AST, imenu making it easier to spot errors in syntax.
;;
;; Also includes full support for PSR-1 and PSR-2 indentation and indentaton for HTML/XML contents.
;;
;; For flycheck support run `(phps-mode-flycheck-setup)'.
;;
;; For asynchronous lexer set: `(setq phps-mode-async-process t)'
;;
;; For asynchronous lexer via `async.el' instead of threads set: `(setq phps-mode-async-process-using-async-el t)'
;;
;; Please see README.md from the same repository for extended documentation.



;;; Code:

(require 'phps-mode-flymake)
(require 'phps-mode-indent)
(require 'phps-mode-lex-analyzer)
(require 'phps-mode-syntax-table)

(defvar phps-mode-idle-interval 1
  "Idle seconds before running the incremental parser.")

(defvar phps-mode-use-psr-2 t
  "Whether to use PSR-2 guidelines for white-space or not.")

(defvar phps-mode-use-psr-12 t
  "Whether to use PSR-12 guidelines for white-space or not.")

(defvar phps-mode-async-process t
  "Whether to use asynchronous processes.")

(defvar phps-mode-async-process-using-async-el nil
  "Whether to use async.el for asynchronous processes.")

(defvar phps-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'phps-mode-rescan-buffer)
    (define-key map (kbd "C-c C-f") #'phps-mode-format-buffer)
    map)
  "Keymap for `phps-mode'.")

(defun phps-mode-error-notice ()
  "Display error notices in echo are if any."
  (when (and
         phps-mode-lex-analyzer--error-start
         phps-mode-lex-analyzer--error-message
         (>= (point) phps-mode-lex-analyzer--error-start))
    (let ((display-error))
      (if phps-mode-lex-analyzer--error-end
          (when (<= (point) phps-mode-lex-analyzer--error-end)
            (setq display-error t))
        (setq display-error t))
      (when display-error
        (message
         "PHPs Error: %s"
         phps-mode-lex-analyzer--error-message)))))

;;;###autoload
(defun phps-mode-rescan-buffer ()
  "Re-scan buffer."
  (interactive)
  (phps-mode-lex-analyzer--reset-local-variables t)
  (phps-mode-lex-analyzer--re2c-run
   nil
   (not (buffer-modified-p))
   (not (buffer-modified-p))))

;;;###autoload
(defun phps-mode-flycheck-setup ()
  "Setup `flycheck' for `phps-mode'."
  ;; Add support for flycheck PHP checkers: PHP, PHPMD and PHPCS here
  ;; Do it only if flycheck is available
  (when (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'php 'phps-mode)
    (flycheck-add-mode 'php-phpmd 'phps-mode)
    (flycheck-add-mode 'php-phpcs 'phps-mode)))

(defun phps-mode-add-trailing-newline ()
  "Add a trailing newline to buffer if missing."
  (let ((max (point-max)))
    (when (> max 1)
      (let ((last-character (buffer-substring-no-properties (1- max) max)))
        (unless (string= last-character "\n")
          (save-excursion
            (goto-char (point-max))
            (insert "\n")))))))

;;;###autoload
(defun phps-mode-format-buffer ()
  "Format current buffer according to PHPs mode."
  (interactive)
  (if (derived-mode-p 'phps-mode)
      (progn
        (when phps-mode-use-psr-2
          (untabify (point-min) (point-max)))
        (when phps-mode-use-psr-12

          ;; All PHP files MUST use the Unix LF (linefeed) line ending only.
          (set-buffer-file-coding-system 'utf-8-unix t t)

          ;; There MUST NOT be trailing whitespace at the end of lines.
          (delete-trailing-whitespace (point-min) (point-max))
          (whitespace-cleanup)

          ;; All PHP files MUST end with a non-blank line, terminated with a single LF.
          (phps-mode-add-trailing-newline))

        (phps-mode-lex-analyzer--process-changes nil t)
        (indent-region (point-min) (point-max)))
    (let ((old-buffer-contents
           (buffer-substring-no-properties (point-min) (point-max)))
          (old-buffer (current-buffer))
          (temp-buffer (generate-new-buffer "*PHPs Formatting*"))
          (new-buffer-contents "")
          (phps-mode-async-process nil))
      (save-excursion
        (switch-to-buffer temp-buffer)
        (insert old-buffer-contents)
        (phps-mode)
        (when phps-mode-use-psr-2
          (untabify (point-min) (point-max)))
        (when phps-mode-use-psr-12

          ;; All PHP files MUST use the Unix LF (linefeed) line ending only.
          (set-buffer-file-coding-system 'utf-8-unix t t)

          ;; There MUST NOT be trailing whitespace at the end of lines.
          (delete-trailing-whitespace (point-min) (point-max))
          (whitespace-cleanup)

          ;; All PHP files MUST end with a non-blank line, terminated with a single LF.
          (phps-mode-add-trailing-newline))
        (indent-region (point-min) (point-max))
        (setq
         new-buffer-contents
         (buffer-substring-no-properties
          (point-min)
          (point-max)))
        (kill-buffer)
        (switch-to-buffer old-buffer)
        (delete-region (point-min) (point-max))
        (insert new-buffer-contents)))))

(define-derived-mode phps-mode prog-mode "PHPs"
  "Major mode for PHP with code intelligence."

  ;; Skip comments when navigating via syntax-table
  (setq-local
   parse-sexp-ignore-comments
   t)

  ;; Font lock
  ;; This makes it possible to have full control over syntax coloring from the lexer
  (setq-local
   font-lock-keywords-only
   nil)
  (setq-local
   font-lock-defaults
   '(nil t))

  ;; Flymake TODO?
  ;; (phps-mode-flymake-init)

  ;; Indentation
  ;; Indent-region will call this on each line of selected region
  (setq-local
   indent-line-function
   #'phps-mode-indent-line)

  ;; Custom Imenu
  (setq-local
   imenu-create-index-function
   #'phps-mode-lex-analyzer--imenu-create-index)

  ;; Beginning and ending of defun:s
  (setq-local
   beginning-of-defun-function
   #'phps-mode-lex-analyzer--beginning-of-defun)
  (setq-local
   end-of-defun-function
   #'phps-mode-lex-analyzer--end-of-defun)

  ;; Should we follow PSR-2?
  (when phps-mode-use-psr-2

    ;; Code MUST use an indent of 4 spaces
    (setq-local
     tab-width
     4)

    ;; MUST NOT use tabs for indenting
    (setq-local
     indent-tabs-mode
     nil))

  (when phps-mode-use-psr-12

    ;; All PHP files MUST use the Unix LF (linefeed) line ending only.
    (set-buffer-file-coding-system
     'utf-8-unix
     t
     t)

    ;; TODO There MUST NOT be trailing whitespace at the end of lines.
    
    ;; All PHP files MUST end with a non-blank line, terminated with a single LF.
    (setq-local
     require-final-newline
     t))

  (phps-mode-lex-analyzer--reset-local-variables)

  ;; Make (comment-region) and (uncomment-region) work
  (setq-local
   comment-region-function
   #'phps-mode-lex-analyzer--comment-region)
  (setq-local
   uncomment-region-function
   #'phps-mode-lex-analyzer--uncomment-region)
  (setq-local
   comment-start
   "// ")
  (setq-local
   comment-end
   "")

  ;; Support for change detection
  (add-hook
   'after-change-functions
   #'phps-mode-lex-analyzer--after-change
   0
   t)

  ;; Support for error notices
  (run-with-idle-timer
   phps-mode-idle-interval
   t
   #'phps-mode-error-notice)

  ;; Initial run of lexer
  (phps-mode-lex-analyzer--re2c-run
   nil
   (not (buffer-modified-p))
   (not (buffer-modified-p))))

(provide 'phps-mode)
;;; phps-mode.el ends here
