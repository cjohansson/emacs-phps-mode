;;; phps-mode.el --- Major mode for PHP with Semantic integration -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Christian Johansson <christian@cvj.se>
;; Maintainer: Christian Johansson <christian@cvj.se>
;; Created: 3 Mar 2018
;; Modified: 20 Aug 2019
;; Version: 0.2.5
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

;; A major-mode that uses original PHP lexer tokens for syntax coloring and indentation making it easier to spot errors in syntax.  Also includes full support for PSR-1 and PSR-2 indentation, imenu.  Improved syntax table in comparison with old PHP major-mode.

;; Please see README.md from the same repository for extended documentation.



;;; Code:

;; NOTE use wisent-parse-toggle-verbose-flag and (semantic-debug) to debug parsing

(require 'phps-mode-flymake)
(require 'phps-mode-functions)
(require 'phps-mode-lexer)
(require 'phps-mode-semantic)
(require 'phps-mode-syntax-table)
(require 'phps-mode-tags)
(require 'semantic)

(defvar phps-mode-use-psr-2 t
  "Whether to use PSR-2 guidelines for white-space or not.")

(defvar phps-mode-idle-interval 1.0
  "Idle seconds before running the incremental lexer.")

(defvar phps-mode-flycheck-applied nil "Boolean flag whether flycheck configuration has been applied or not.")

(defvar phps-mode-map-applied nil "Boolean flag whether mode-map has been initialized or not.")

(defvar phps-mode-inline-mmm-submode nil "Symbol declaring what mmm-mode to use as submode in inline areas.")

(define-derived-mode phps-mode prog-mode "PHPs"
  "Major mode for PHP with Semantic integration."

  ;; TODO Check whether PSR-2 requires final newlines or not
  (setq-local require-final-newline nil)

  ;; Verify this setting
  (setq-local parse-sexp-ignore-comments t)

  ;; Key-map
  ;; prog-mode will create the key-map and we just modify it here.
  (when (and phps-mode-map
             (not phps-mode-map-applied))
    (define-key phps-mode-map (kbd "C-c /") #'comment-region)
    (define-key phps-mode-map (kbd "C-c DEL") #'uncomment-region)
    (setq phps-mode-map-applied t))
  (use-local-map phps-mode-map)

  ;; Syntax table
  (set-syntax-table phps-mode-syntax-table)

  ;; NOTE: These are required for wrapping region functionality
  (transient-mark-mode)
  (electric-pair-local-mode)

  ;; Font lock
  ;; This makes it possible to have full control over syntax coloring from the lexer
  (setq-local font-lock-keywords-only nil)
  (setq-local font-lock-defaults '(nil t))

  ;; Flymake TODO
  ;; (phps-mode-flymake-init)

  ;; Flycheck
  ;; Add support for flycheck PHP checkers: PHP, PHPMD and PHPCS here
  ;; Do it once but only if flycheck is available
  (when (and (fboundp 'flycheck-add-mode)
             (not phps-mode-flycheck-applied))
    (flycheck-add-mode 'php 'phps-mode)
    (flycheck-add-mode 'php-phpmd 'phps-mode)
    (flycheck-add-mode 'php-phpcs 'phps-mode)
    (setq phps-mode-flycheck-applied t))

    ;; Custom indentation
  ;; Indent-region will call this on each line of selected region
  (setq-local indent-line-function #'phps-mode-functions-indent-line)

  ;; Custom Imenu
  (setq-local imenu-create-index-function #'phps-mode-functions-imenu-create-index)

  ;; Should we follow PSR-2?
  (when (and (boundp 'phps-mode-use-psr-2)
             phps-mode-use-psr-2)

    ;; Code MUST use an indent of 4 spaces
    (setq-local tab-width 4)

    ;; MUST NOT use tabs for indenting
    (setq-local indent-tabs-mode nil))

  ;; Add support for moving indexes quickly when making newlines
  (advice-add #'newline :around #'phps-mode-functions-around-newline)

  ;; Reset flags
  (set (make-local-variable 'phps-mode-functions-allow-after-change) t)
  (set (make-local-variable 'phps-mode-functions-buffer-changes-start) nil)
  (set (make-local-variable 'phps-mode-functions-lines-indent) nil)
  (set (make-local-variable 'phps-mode-functions-imenu) nil)
  (set (make-local-variable 'phps-mode-functions-processed-buffer) nil)

  ;; Make (comment-region) and (uncomment-region) work
  (setq-local comment-region-function #'phps-mode-functions-comment-region)
  (setq-local uncomment-region-function #'phps-mode-functions-uncomment-region)
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  ;; Support for change detection
  (add-hook 'after-change-functions #'phps-mode-functions-after-change)

  ;; Lexer
  (if (and (boundp 'semantic-lex-syntax-table)
           (boundp 'phps-mode-syntax-table))
      (setq-local semantic-lex-syntax-table phps-mode-syntax-table)
    (signal 'error "Semantic or regular syntax-table for PHPs-mode missing!"))

  ;; Semantic
  (if (boundp 'semantic-lex-analyzer)
      (setq-local semantic-lex-analyzer #'phps-mode-lexer-lex)
    (signal 'error "Semantic semantic-lex-analyzer missing!"))

  ;; Set semantic-lex initializer function
  (add-hook 'semantic-lex-reset-functions #'phps-mode-lexer-setup)

  ;; Reset tokens
  (setq-local phps-mode-lexer-tokens nil)

  ;; Initial run of lexer
  (phps-mode-lexer-run)

  ;; Run semantic functions for new buffer
  (semantic-new-buffer-fcn)

  ;; Wisent LALR parser TODO
  ;; (phps-mode-tags-init)

  ;; Add compatibility for plug-ins here
  (run-hooks 'phps-mode-hook))

(provide 'phps-mode)
;;; phps-mode.el ends here
