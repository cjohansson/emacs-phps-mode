;;; phps-mode/phps-flymake.el --- Major mode for PHP with Semantic integration

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


(require 'flymake)

(defun phps-mode/flymake-init ()
  "PHP specific init-cleanup routines.

This is an alternative function of `flymake-php-init'.
Look at the `php-executable' variable instead of the constant \"php\" command."

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.php[345s]?\\'"
                 phps-mode--flymake-init
                 flymake-simple-cleanup
                 flymake-get-real-file-name))

  (add-to-list 'flymake-err-line-patterns
               '("\\(Parse\\|Fatal\\) error: \\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)" 3 4 nil 2))

  (if (boundp 'php-executable)
      (let* ((temp-file
              (funcall
               (eval-when-compile
                 (if (fboundp 'flymake-proc-init-create-temp-buffer-copy)
                     'flymake-proc-init-create-temp-buffer-copy
                   'flymake-init-create-temp-buffer-copy))
               'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list php-executable (list "-f" local-file "-l")))))

(provide 'phps-mode/flymake)

;;; phps-flymake.el ends here
