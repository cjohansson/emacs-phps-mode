;;; phps-mode/phps-syntax-table.el --- Major mode for PHP with Semantic integration

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


;;; Code:


(defvar phps-mode/syntax-table
  (let ((phps-mode/syntax-table (make-syntax-table)))

    ;; This is added so entity names with underscores can be more easily parsed as one word
    
    ;; Treat underscore as a punctuation
    (modify-syntax-entry ?_ "." phps-mode/syntax-table)

    ;; Improve parsing of <?php and ?> as words
    ;;(modify-syntax-entry ?? "w" phps-mode/syntax-table)
    ;;(modify-syntax-entry ?< "w" phps-mode/syntax-table)
    ;;(modify-syntax-entry ?> "w" phps-mode/syntax-table)

    ;; (modify-syntax-entry ?_ "w" phps-mode/syntax-table)

    ;; Comment styles are same as C++
    ;; (modify-syntax-entry ?/ ". 124b" phps-mode/syntax-table)
    ;; (modify-syntax-entry ?* ". 23" phps-mode/syntax-table)
    ;; (modify-syntax-entry ?\n "> b" phps-mode/syntax-table)

    ;; From Old PHP-mode, analyse these
    ;; (modify-syntax-entry ?_    "_" php-mode-syntax-table)
    ;; (modify-syntax-entry ?`    "\"" php-mode-syntax-table)
    ;; (modify-syntax-entry ?\"   "\"" php-mode-syntax-table)
    ;; (modify-syntax-entry ?#    "< b" php-mode-syntax-table)
    ;; (modify-syntax-entry ?\n   "> b" php-mode-syntax-table)
    ;; (modify-syntax-entry ?$    "'" php-mode-syntax-table)
    ;; (set (make-local-variable 'syntax-propertize-function) #'php-syntax-propertize-function)

    phps-mode/syntax-table)
  "Syntax table for phps-mode.")

(defun phps-mode/syntax-table-init ()
  "Apply syntax table."
  (set-syntax-table phps-mode/syntax-table))

(provide 'phps-mode/syntax-table)

;;; phps-syntax-table.el ends here
