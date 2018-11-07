;;; phps-mode/phps-tags.el --- PHP LALR parser for Emacs

;; Copyright (C) 2001-2006, 2009-2018 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 15 Dec 2001
;; Keywords: syntax

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO Make similar to /Users/christianjohansson/Documents/emacs/nextstep/Emacs.app/Contents/Resources/lisp/cedet/semantic/wisent/java-tags.el.gz

;;; Code:

(require 'semantic/wisent)
(require 'phps-mode/tags-wy "tags-wy")
(require 'phps-mode/phps-semantic "phps-semantic")

;;;;
;;;; Simple parser error reporting function
;;;;

(defun wisent-php-parse-error (msg)
  "Error reporting function called when a parse error occurs.
MSG is the message string to report."
;;   (let ((error-start (nth 2 wisent-input)))
;;     (if (number-or-marker-p error-start)
;;         (goto-char error-start)))
  (message msg)
  ;;(debug)
  )

;;;;
;;;; Local context
;;;;

(define-mode-local-override semantic-get-local-variables
  phps-mode ()
  "Get local values from a specific context.
Parse the current context for `field_declaration' nonterminals to
collect tags, such as local variables or prototypes.
This function override `get-local-variables'."
  (let ((vars nil)
        (ct (semantic-current-tag))
        ;; We want nothing to do with funny syntaxing while doing this.
        (semantic-unmatched-syntax-hook nil))
    (while (not (semantic-up-context (point) 'function))
      (save-excursion
        (forward-char 1)
        (setq vars
              (append (semantic-parse-region
                       (point)
                       (save-excursion (semantic-end-of-context) (point))
                       'field_declaration
                       0 t)
                      vars))))
    ;; Add 'this' if in a fcn
    (when (semantic-tag-of-class-p ct 'function)
      ;; Append a new tag THIS into our space.
      (setq vars (cons (semantic-tag-new-variable
                        "this" (semantic-tag-name (semantic-current-tag-parent))
                        nil)
                       vars)))
    vars))

;;; Analyzer and type cache support
(define-mode-local-override semantic-analyze-split-name phps-mode (name)
  "Split up tag names on colon . boundaries."
  (let ((ans (split-string name "\\->")))
    (if (= (length ans) 1)
        name
      (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name phps-mode (namelist)
  "Assemble the list of names NAMELIST into a namespace name."
  (mapconcat 'identity namelist "->"))



;;;; Semantic integration of the PHP LALR parser

;; In semantic/imenu.el, not part of Emacs.
(defvar semantic-imenu-summary-function)

;;;###autoload
(defun phps-mode/tags-init ()
  "Hook run to setup Semantic in `phps-mode'.
Use the alternate LALR(1) parser."

  (phps-mode-tags--install-parser)

  (require 'semantic/bovine/debug)

  (setq

   ;; Semantic requires this expression for line-comments,
   ;; if lexing without major mode
   semantic-lex-comment-regex "\\s<\\|\\(/\\*\\|//\\)"

   ;; Lexical analysis
   semantic-lex-number-expression semantic-php-number-regexp
   semantic-lex-analyzer 'phps-mode-tags-lexer

   ;; Parsing
   semantic-tag-expand-function 'semantic-php-expand-tag

   ;; Environment
   semantic-imenu-summary-function 'semantic-format-tag-prototype
   imenu-create-index-function 'semantic-create-imenu-index
   semantic-type-relation-separator-character '("::" "->")
   semantic-command-separation-character ";"
   semantic-debug-parser-class 'semantic-bovine-debug-parser

   ;; speedbar and imenu buckets name
   semantic-symbol->name-assoc-list-for-type-parts

   ;; in type parts
   ;; TODO Add constants to this?
   '((package . "Namespaces")
     (type     . "Classes")
     (variable . "Variables")
     (function . "Functions"))
   semantic-symbol->name-assoc-list

   ;; everywhere
   (append semantic-symbol->name-assoc-list-for-type-parts
           '((namespace  . "Namespaces")))

   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable)

   ;; Remove 'recursive from the default semanticdb find throttle
   ;; since php imports never recurse.
   semanticdb-find-default-throttle

   (remq 'recursive (default-value 'semanticdb-find-default-throttle)))

  ;; Setup phpdoc stuff
  (semantic-php-doc-setup))

(provide 'phps-mode/phps-tags)

;;; phps-tags.el ends here
