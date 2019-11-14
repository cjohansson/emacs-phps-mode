;;; phps-automation --- Generate a Wisent Parser file -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

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

;;; Uses a parser to convert LALR Yacc grammar to Wisent grammar

;;; AST should be like this: (root (block (rule (logic))))


;;; Code:

(let ((php-yacc-url "https://raw.githubusercontent.com/php/php-src/master/Zend/zend_language_parser.y")
      (php-yacc-file (expand-file-name "zend_language_parser.y"))
      (wisent-destination (expand-file-name "../phps-mode-wy.wy"))
      (header (expand-file-name "phps-mode-automation-header.wy")))

  ;; Download Yacc if not available
  (unless (file-exists-p php-yacc-file)
    (message "Downloading PHP Yacc grammar..")
    (url-copy-file php-yacc-url php-yacc-file t t)
    (message "Download completed"))

  ;; Generate grammar
  (message "Generating Wisent grammar..")
  (if (fboundp 'emacs-wisent-grammar-converter-generate-grammar-from-filename)
      (emacs-wisent-grammar-converter-generate-grammar-from-filename
       php-yacc-file wisent-destination header)
    (display-warning 'warning "Missing emacs-wisent-grammar-converter!"))
  (message "Automation completed"))

(provide 'phps-mode-automation)
;;; phps-mode-automation.el ends here
