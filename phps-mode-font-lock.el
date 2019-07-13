;;; phps-mode-font-lock.el --- Font Lock for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019  Free Software Foundation, Inc.

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

;; Please see README.md from the same repository for extended documentation.


;;; Code:

(defun phps-mode-font-lock-init ()
  "Apply font-lock."

  (setq font-lock-keywords-only nil)

  ;; This makes it possible to have full control over syntax coloring from the lexer
  (set (make-local-variable 'font-lock-defaults) '(nil t))

  )

(provide 'phps-mode-font-lock)
;;; phps-mode-font-lock.el ends here
