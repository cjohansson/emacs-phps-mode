;;; phps-mode-test-integration.el --- Tests for integration -*- lexical-binding: t -*-

;; Copyright (C) 2019 Christian Johansson

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


;; Run from terminal make test-integration


;;; Code:


(autoload 'phps-mode-test-with-buffer "phps-mode-test")
(autoload 'phps-mode-functions-indent-line "phps-mode-functions")
(autoload 'phps-mode-functions-get-lines-indent "phps-mode-functions")
(autoload 'phps-mode-functions-get-imenu "phps-mode-functions")
(autoload 'phps-mode-functions-get-buffer-changes-start "phps-mode-functions")
(autoload 'phps-mode-lexer-get-tokens "phps-mode-lexer")
(autoload 'phps-mode-lexer-run-incremental "phps-mode-lexer")
(autoload 'phps-mode-test-hash-to-list "phps-mode-test")
(autoload 'should "ert")

(defun phps-mode-test-integration-incremental ()
  "Test for object-oriented PHP file."

    (phps-mode-test-with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test for regular PHP with namespaces, classes and functions"

   ;; Tokens
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (should (equal (phps-mode-lexer-get-tokens) '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) ("{" 29 . 30) (T_CLASS 35 . 40) (T_STRING 41 . 48) ("{" 53 . 54) (T_PUBLIC 63 . 69) (T_FUNCTION 70 . 78) (T_STRING 79 . 89) ("(" 89 . 90) (")" 90 . 91) ("{" 100 . 101) (T_ECHO 114 . 118) (T_CONSTANT_ENCAPSED_STRING 119 . 133) (";" 133 . 134) ("}" 143 . 144) ("}" 149 . 150) ("}" 151 . 152))))

   ;; Indentation
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (2 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent))))

   ;; Imenu
   (should (equal (phps-mode-functions-get-imenu) '(("\\myNamespace" . 17) ("\\myNamespace\\myClass" . 41) ("\\myNamespace\\myClass->myFunction()" . 79))))

   ;; Make changes
   (goto-char 144)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")

   ;; Verify stored point of change
   (should (equal (phps-mode-functions-get-buffer-changes-start) 144))

   ;; Run incremental lexer
   (phps-mode-lexer-run-incremental)

   ;; TODO Should maybe change this to a more more simpler way
   ;; like compare lexer tokens, indent and imenu with same contents in a new buffer

   ;; Tokens
   ;; (message "Tokens %s" (phps-mode-lexer-get-tokens))
   (should (equal (phps-mode-lexer-get-tokens) '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) ("{" 29 . 30) (T_CLASS 35 . 40) (T_STRING 41 . 48) ("{" 53 . 54) (T_PUBLIC 63 . 69) (T_FUNCTION 70 . 78) (T_STRING 79 . 89) ("(" 89 . 90) (")" 90 . 91) ("{" 100 . 101) (T_ECHO 114 . 118) (T_CONSTANT_ENCAPSED_STRING 119 . 133) (";" 133 . 134) (";" 133 . 134) ("}" 143 . 144) (T_PUBLIC 154 . 160) (T_FUNCTION 161 . 169) (T_STRING 170 . 181) ("(" 181 . 182) (")" 182 . 183) ("{" 192 . 193) (T_ECHO 206 . 210) (T_CONSTANT_ENCAPSED_STRING 211 . 232) (";" 232 . 233) ("}" 242 . 243) ("}" 249 . 250) ("}" 251 . 252))))

   ;; Indentation
   ;; (message "indent: %s" (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (2 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (2 0)) (11 (2 0)) (12 (2 0)) (13 (3 0)) (14 (2 0)) (15 (2 0)) (16 (1 0)) (17 (0 0))) (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent))))

   ;; Imenu
   (should (equal (phps-mode-functions-get-imenu) '(("\\myNamespace" . 17) ("\\myNamespace\\myClass" . 41) ("\\myNamespace\\myClass->myFunction()" . 79) ("\\myNamespace\\myClass->myFunctionB()" . 170))))

   ;; Remove first function
   (goto-char 55)
   (push-mark nil t t)
   (goto-char 145)
   (execute-kbd-macro (kbd "<backspace>"))

   ;; Verify stored point of change
   (should (equal (phps-mode-functions-get-buffer-changes-start) 55))

   ;; Run incremental lexer
   (phps-mode-lexer-run-incremental)

   ;; Tokens
   ;; (message "Tokens %s" (phps-mode-lexer-get-tokens))
   (should (equal (phps-mode-lexer-get-tokens) '((T_OPEN_TAG 1 . 7) (T_NAMESPACE 7 . 16) (T_STRING 17 . 28) ("{" 29 . 30) (T_CLASS 35 . 40) (T_STRING 41 . 48) ("{" 53 . 54) ("{" 53 . 54) (T_PUBLIC 64 . 70) (T_FUNCTION 71 . 79) (T_STRING 80 . 91) ("(" 91 . 92) (")" 92 . 93) ("{" 102 . 103) (T_ECHO 116 . 120) (T_CONSTANT_ENCAPSED_STRING 121 . 142) (";" 142 . 143) ("}" 152 . 153) ("}" 159 . 160) ("}" 161 . 162))))

   ;; Imenu
   (should (equal (phps-mode-functions-get-imenu) '(("\\myNamespace" . 17) ("\\myNamespace\\myClass" . 41) ("\\myNamespace\\myClass->myFunctionB()" . 80) )))

   ;; Indentation
   (message "indent: %s" (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (2 0)) (7 (2 0)) (8 (2 0)) (9 (3 0)) (10 (2 0)) (11 (2 0)) (12 (1 0)) (13 (0 0))) (phps-mode-test-hash-to-list (phps-mode-functions-get-lines-indent))))


   )

)

(defun phps-mode-test-integration ()
  "Run test for integration."
  ;; (setq debug-on-error t)
  ;; (setq phps-mode-functions-verbose t)

  (phps-mode-test-integration-incremental)

)

(phps-mode-test-integration)

(provide 'phps-mode-test-integration)

;;; phps-mode-test-integration.el ends here
