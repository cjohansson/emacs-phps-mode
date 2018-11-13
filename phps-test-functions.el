;;; phps-test-functions.el --- Tests for functions

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


;; Run from terminal make functions-test


;;; Code:


(autoload 'phps-mode/with-test-buffer "phps-test")
(autoload 'should "ert")

(defun phps-mode/test-indentation ()
  "Test for indentation."

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"
   (goto-char 69)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n} ?></title><body>Bla bla</body></html>"
   
   (goto-char 75)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n        echo $title;\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"

   (goto-char 98)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n    } ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n}\n?>\n</title><body>Bla bla</body></html>"

   (goto-char 110)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n}\n?>\n</title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<?php\n$variable = array(\n'random'\n);\n$variable = true;\n"
   (goto-char 28)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random'\n);\n$variable = true;\n"))))

(phps-mode/with-test-buffer
   "<?php\n$variable = array(\n    'random'\n    );\n$variable = true;\n"
   (goto-char 39)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random'\n);\n$variable = true;\n"))))

  )

(defun phps-mod/test-functions ()
  "Run test for functions."
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq-default c-basic-indent 4)
  (phps-mode/test-indentation))

(phps-mod/test-functions)

(provide 'phps-mod/test-functions)

;;; phps-test-functions.el ends here
