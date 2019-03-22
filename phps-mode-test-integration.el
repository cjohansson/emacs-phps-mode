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
(autoload 'phps-mode-test-incremental-vs-intial-buffer "phps-mode-test")
(autoload 'phps-mode-functions-get-lines-indent "phps-mode-functions")
(autoload 'phps-mode-functions-get-imenu "phps-mode-functions")
(autoload 'phps-mode-functions-get-buffer-changes-start "phps-mode-functions")
(autoload 'phps-mode-lexer-get-tokens "phps-mode-lexer")
(autoload 'phps-mode-lexer-run-incremental "phps-mode-lexer")
(autoload 'phps-mode-test-hash-to-list "phps-mode-test")
(autoload 'should "ert")

;; TODO Add test for making changes inside tokens that is (and (> token-start) (< token-end))
(defun phps-mode-test-integration-incremental ()
  "Test for object-oriented PHP file."

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 1 for regular PHP with namespaces, classes and functions"

   ;; Make changes
   (goto-char 144)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")
   (should (equal (phps-mode-functions-get-buffer-changes-start) 144)))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 2 for regular PHP with namespaces, classes and functions"

   ;; Make changes
   (goto-char 144)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")
   (should (equal (phps-mode-functions-get-buffer-changes-start) 144))
   (phps-mode-lexer-run-incremental)

   ;; Remove first function
   (goto-char 55)
   (push-mark nil t t)
   (goto-char 145)
   (execute-kbd-macro (kbd "<backspace>"))
   (should (equal (phps-mode-functions-get-buffer-changes-start) 55)))

  (phps-mode-test-incremental-vs-intial-buffer
   ""
   "Integration-test 3 for function-oriented PHP"

   ;; Make changes
   (goto-char 1)
   (insert "<?php\nfunction myFunctionA()\n{\n    echo 'my second statement';\n}\n")
   (should (equal (phps-mode-functions-get-buffer-changes-start) 1)))

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
