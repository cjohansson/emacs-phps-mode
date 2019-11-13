;;; phps-mode-wy-wy.el --- Generated parser support file

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Christian Johansson <christianjohansson@Christians-MacBook-Air.local>
;; Created: 2019-11-13 07:07:16+0100
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
;; generated from the grammar file phps-mode-wy.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(eval-and-compile (defconst phps-mode-wy-wy--expected-conflicts
                    nil
                    "The number of expected shift/reduce conflicts in this grammar."))

(defconst phps-mode-wy-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst phps-mode-wy-wy--token-table
  (semantic-lex-make-type-table 'nil 'nil)
  "Table of lexical tokens.")

(defconst phps-mode-wy-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '(nil nil)
     'nil))
  "Parser table.")

(defun phps-mode-wy-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table phps-mode-wy-wy--parse-table
        semantic-debug-parser-source "phps-mode-wy.wy"
        semantic-flex-keywords-obarray phps-mode-wy-wy--keyword-table
        semantic-lex-types-obarray phps-mode-wy-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;

;;; Epilogue
;;

(provide 'phps-mode-wy-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; phps-mode-wy-wy.el ends here
