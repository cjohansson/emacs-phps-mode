;;; phps-mode/phps-functions.el --- Mode functions for PHPs

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


;;; Code:


(defun phps-mode/indent-line ()
  "Indent line."
  (let ((data (phps-mode/lexer-get-point-data)))
    (save-excursion
      (beginning-of-line)
      (let ((start (nth 0 data))
            (end (nth 1 data)))

        ;; Are we in scripting?
        (if (nth 0 start)
            (let ((indent-level (* (+ (nth 1 start) (nth 2 start)) 4)))
              (message "inside scripting, start: %s, end: %s, indenting to column %s " start end indent-level)
              (indent-line-to indent-level))
          (progn
            (message "Outside scripting %s" start)
            ;; (indent-relative)
            ))))))

(defun phps-mode/indent-region ()
  "Indent region."
  )

(defun phps-mode/functions-init ()
  "PHP specific init-cleanup routines."

  (set (make-local-variable 'indent-line-function) #'phps-mode/indent-line)
  (set (make-local-variable 'tab-width) 8)
  ;; (set (make-local-variable 'indent-line-function) #'phps-mode/indent-region)
  )


(provide 'phps-mode/functions)

;;; phps-functions.el ends here
