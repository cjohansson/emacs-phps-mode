;;; phps-mode-font-lock.el --- Font Lock for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Christian Johansson

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


(defun phps-mode-font-lock-init ()
  "Apply font-lock."

  (setq font-lock-keywords-only nil)

  ;; This makes it possible to have full control over syntax coloring from the lexer
  (set (make-local-variable 'font-lock-defaults) '(nil t))

  )

(provide 'phps-mode-font-lock)
;;; phps-mode-font-lock.el ends here
