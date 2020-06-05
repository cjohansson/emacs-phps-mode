;;; phps-mode-grammar-macro.el --- Generated parser support file

;; Copyright (C) 2020 Christian Johansson

;; Author: Christian Johansson <christianjohansson@Christians-MacBook-Air.local>
;; Created: 2020-06-04 15:46:31+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file phps-mode-parser-grammar-macro.el.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(eval-and-compile (defconst phps-mode-grammar-macro--expected-conflicts
                    nil
                    "The number of expected shift/reduce conflicts in this grammar."))

(defconst phps-mode-grammar-macro--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst phps-mode-grammar-macro--token-table
  (semantic-lex-make-type-table 'nil 'nil)
  "Table of lexical tokens.")

