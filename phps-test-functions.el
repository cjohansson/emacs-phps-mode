;;; phps-test-functions.el --- Tests for functions -*- lexical-binding: t -*-

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
(autoload 'phps-mode/indent-line "phps-functions")
(autoload 'phps-mode/get-point-data "phps-functions")
(autoload 'should "ert")

(defun phps-mode/test-indent-line ()
  "Test for indentation."

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"
   (goto-char 69)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title1;\n} ?></title><body>Bla bla</body></html>"
   
   (goto-char 75)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n        echo $title1;\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n} ?></title><body>Bla bla</body></html>"

   (goto-char 98)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n    } ?></title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"

   (goto-char 110)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"))))

  (phps-mode/with-test-buffer
   "<?php\n$variable = array(\n'random3'\n);\n$variable = true;\n"
   (goto-char 28)
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random3'\n);\n$variable = true;\n"))))

  (phps-mode/with-test-buffer
   "<?php\n$variable = array(\n    'random2'\n    );\n$variable = true;\n"
   (goto-char 43)
   ;; (message "Tokens %s point %s" phps-mode/lexer-tokens (point))
   (phps-mode/indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random2'\n);\n$variable = true;\n"))))

  (phps-mode/with-test-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   (goto-char 20)
   (phps-mode/indent-line)
   ;; (message "Tokens %s point %s" phps-mode/lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n * My first line\n* My second line\n**/\n"))))

  (phps-mode/with-test-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   (goto-char 9)
   (phps-mode/indent-line)
   ;; (message "Tokens %s point %s" phps-mode/lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n**/\n"))))

  (phps-mode/with-test-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   (goto-char 46)
   (phps-mode/indent-line)
   ;; (message "Tokens %s point %s" phps-mode/lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n **/\n"))))

  (phps-mode/with-test-buffer
   "<?php\n$variable = array(\n'random4');\n$variable = true;\n"
   (goto-char 29)
   (phps-mode/indent-line)
   ;; (message "Tokens %s point %s" phps-mode/lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"))))

  )

(defun phps-mode/test-functions--get-point-data ()
  "Return information about point in tokens."

  (phps-mode/with-test-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   (goto-char 35)
   (should (equal (list (list t 0 0 0 3 nil) (list t 1 0 0 6 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 15)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 5 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?>\n</title><body>Bla bla</body></html>"
   (goto-char 50)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 nil nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title></title><body>Bla bla</body></html>"
   (goto-char 15)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 nil nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 30)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 5 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 50)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 5 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { \n   if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   ;; (message "Tokens: %s" phps-mode/lexer-tokens)
   (goto-char 48)
   (should (equal (list (list t 1 0 0 5 nil) (list nil 0 0 0 17 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) {\n echo $title;\n} } ?></title><body>Bla bla</body></html>"
   (goto-char 72)
   (should (equal (list (list t 2 0 0 10 nil) (list t 2 0 0 13 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n}\n}\n ?></title><body>Bla bla</body></html>"
   (goto-char 84)
   (should (equal (list (list t 2 0 0 13 nil) (list t 1 0 0 14 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   (goto-char 100)
   (should (equal (list (list nil 0 0 0 nil nil) (list nil 0 0 0 17 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   (goto-char 20)
   (should (equal (list (list t 0 0 0 nil t) (list t 0 0 0 nil t)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   (goto-char 9)
   (should (equal (list (list nil 0 0 0 nil nil) (list t 0 0 0 1 nil)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   (goto-char 50)
   (should (equal (list (list t 0 0 0 nil t) (list t 0 0 0 nil t)) (phps-mode/get-point-data))))

  (phps-mode/with-test-buffer
   "<?php\n$variable = array(\n'random4');\n$variable = true;\n"
   (goto-char 29)
   (should (equal (list (list t 0 1 0 4 nil) (list t 0 0 0 7 nil)) (phps-mode/get-point-data))))
  )

;; TODO Add tests for all examples here: https://www.php-fig.org/psr/psr-2/

(defun phps-mod/test-functions ()
  "Run test for functions."
  (phps-mode/test-functions--get-point-data)
  (phps-mode/test-indent-line))

(phps-mod/test-functions)

(provide 'phps-mod/test-functions)

;;; phps-test-functions.el ends here
