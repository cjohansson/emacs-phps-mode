;;; phps-mode/phps-font-lock.el --- Font Lock for PHP Semantic

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

;;; Commentary:


;;; Code:


(defconst phps-mode/keywords
  (list
     "__halt_compiler"
     "abstract"
     "and"
     "array"
     "as"
     "break"
     "callable"
     "case"
     "catch"
     "class"
     "clone"
     "const"
     "continue"
     "declare"
     "default"
     "die"
     "do"
     "echo"
     "else"
     "elseif"
     "empty"
     "enddeclare"
     "endfor"
     "endforeach"
     "endif"
     "endswitch"
     "endwhile"
     "eval"
     "exit"
     "extends"
     "final"
     "finally"
     "for"
     "foreach"
     "function"
     "global"
     "goto"
     "if"
     "implements"
     "include"
     "include_once"
     "instanceof"
     "insteadof"
     "interface"
     "isset"
     "list"
     "namespace"
     "new"
     "or"
     "print"
     "private"
     "protected"
     "public"
     "require"
     "require_once"
     "return"
     "static"
     "switch"
     "throw"
     "trait"
     "try"
     "unset"
     "use"
     "var"
     "while"
     "xor"
     "yield"

     ;; Compile-time constants here
     "__CLASS__"
     "__DIR__"
     "__FILE__"
     "__FUNCTION__"
     "__LINE__"
     "__METHOD__"
     "__NAMESPACE__"
     "__TRAIT__"
   )
  "Use list of keywords to build regular expression for syntax highlighting.")

(defconst phps-mode/font-lock-keywords
  (let ((regex (concat "\\<" (regexp-opt phps-mode/keywords t) "\\>")))
    (list
     `(,regex . font-lock-builtin-face)
     '("\\('\\w*'\\)" . font-lock-variable-name-face)))
  "Minimal highlighting expressions for major mode.")

(defun phps-mode/font-lock-init ()
  "Apply font-lock."

  (setq font-lock-keywords-only nil)

  ;; This makes it possible to have full control over syntax coloring from the lexer
  (set (make-local-variable 'font-lock-defaults) '(nil t))

  ;; (set (make-local-variable 'font-lock-defaults) '(phps-mode/font-lock-keywords))
  )

(provide 'phps-mode/font-lock)

;;; phps-font-lock.el ends here
