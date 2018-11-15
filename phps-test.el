;;; phps-test.el --- Commons for tests -*- lexical-binding: t -*-

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

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


;;; Code:


(autoload 'phps-mode "phps-mode")

(defmacro phps-mode/with-test-buffer (source &rest body)
  "Set up test buffer with SOURCE and BODY."
  `(let ((test-buffer (generate-new-buffer "test")))
     (switch-to-buffer test-buffer)
     (insert ,source)
     (goto-char 0)
     ;;,(message "\nTesting buffer:\n'%s'\n" source)
     (phps-mode)
     ,@body
     (kill-buffer test-buffer)
     ))


(provide 'phps-mode/test)

;;; phps-test.el ends here
