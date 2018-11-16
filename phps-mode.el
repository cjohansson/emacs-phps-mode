;;; phps-mode.el --- Major mode for PHP with Semantic integration -*- lexical-binding: t -*-

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

;; Package-Requires: ((emacs "24"))

;; Copyright (C) 2017 Christian Johansson

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Spathoftware Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; Please see README.md from the same repository for extended documentation.

;; TODO 0. Add tests for semantic like semantic-php project
;; TODO 1. Get semantic working based on zend_language_parser.y
;; TODO 2. Add support for flymake
;; DONE 3. Add support for flycheck
;; TODO 4. Get syntax coloring working based on semantic data (as js2-mode)
;; TODO 5. Get indent-functions working

;; NOTE use wisent-parse-toggle-verbose-flag and (semantic-debug) to debug parsing


;;; Code:


(autoload 'phps-mode/flycheck-init "phps-flycheck")
(autoload 'phps-mode/flymake-init "phps-flymake")
(autoload 'phps-mode/font-lock-init "phps-font-lock")
(autoload 'phps-mode/functions-init "phps-functions")
(autoload 'phps-mode/map-init "phps-map")
(autoload 'phps-mode/lexer-init "phps-lexer")
(autoload 'phps-mode/syntax-table-init "phps-syntax-table")
(autoload 'phps-mode/tags-init "phps-tags")
(autoload 'phps-mode/semantic-init "phps-semantic")

(defvar phps-mode/use-psr-2 t
  "Whether to use PSR-2 guidelines for white-space or not.")

(defvar phps-mode/idle-interval 1
  "Idle seconds before running incremental lexer.")

(define-derived-mode phps-mode prog-mode "PHPs"
  "Major mode for PHP with Semantic integration."

  ;; Key-map
  (phps-mode/map-init)

  ;; Syntax table
  (phps-mode/syntax-table-init)

  ;; Font lock
  (phps-mode/font-lock-init)

  ;; Flymake
  ;; (phps-mode/flymake-init)

  ;; Flycheck
  (phps-mode/flycheck-init)

  ;; Override functions
  (phps-mode/functions-init)

  (setq major-mode 'phps-mode)
  (setq mode-name "PHPs")

  ;; Lexer
  (phps-mode/lexer-init)

  ;; Wisent LALR parser
  ;; (phps-mode/tags-init)

  (run-hooks 'phps-mode-hook)
  (semantic-new-buffer-fcn))

(provide 'phps-mode)
;;; phps-mode.el ends here
