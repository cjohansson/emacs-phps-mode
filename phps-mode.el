;;; phps-mode.el --- Major mode for PHP with Semantic integration -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: 4 May 2019
;; Version: 0.2.1
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A major-mode that uses original PHP lexer tokens for syntax coloring and indentation making it easier to spot errors in syntax.  Also includes full support for PSR-1 and PSR-2 indentation, imenu.  Improved syntax table in comparison with old PHP major-mode.

;; Please see README.md from the same repository for extended documentation.



;;; Code:

;; NOTE use wisent-parse-toggle-verbose-flag and (semantic-debug) to debug parsing

(autoload 'phps-mode-flycheck-init "phps-mode-flycheck")
(autoload 'phps-mode-flymake-init "phps-mode-flymake")
(autoload 'phps-mode-font-lock-init "phps-mode-font-lock")
(autoload 'phps-mode-functions-init "phps-mode-functions")
(autoload 'phps-mode-map-init "phps-mode-map")
(autoload 'phps-mode-lexer-init "phps-mode-lexer")
(autoload 'phps-mode-syntax-table-init "phps-mode-syntax-table")
(autoload 'phps-mode-tags-init "phps-mode-tags")
(autoload 'phps-mode-semantic-init "phps-mode-semantic")

(autoload 'semantic-new-buffer-fcn "semantic")

(defvar phps-mode-use-psr-2 t
  "Whether to use PSR-2 guidelines for white-space or not.")

(defvar phps-mode-idle-interval 1.0
  "Idle seconds before running incremental lexer.")

(define-derived-mode phps-mode prog-mode "PHPs"
  "Major mode for PHP with Semantic integration."

  ;; Key-map
  (phps-mode-map-init)

  ;; Syntax table
  (phps-mode-syntax-table-init)

  ;; Font lock
  (phps-mode-font-lock-init)

  ;; Flymake TODO
  ;; (phps-mode-flymake-init)

  ;; Flycheck
  (phps-mode-flycheck-init)

  ;; Override functions
  (phps-mode-functions-init)

  ;; Lexer
  (phps-mode-lexer-init)

  ;; Wisent LALR parser
  ;; (phps-mode-tags-init)

  (run-hooks 'phps-mode-hook)
  (semantic-new-buffer-fcn))

(provide 'phps-mode)
;;; phps-mode.el ends here
