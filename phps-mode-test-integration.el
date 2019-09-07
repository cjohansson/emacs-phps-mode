;;; phps-mode-test-integration.el --- Tests for integration -*- lexical-binding: t -*-

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

;; Run from terminal make test-integration


;;; Code:

(require 'ert)
(require 'phps-mode-functions)
(require 'phps-mode-lexer)
(require 'phps-mode-test)

(eval-when-compile
  (require 'phps-mode-macros))

(defun phps-mode-test-integration-incremental ()
  "Test for object-oriented PHP file."

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 1 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 144)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")
   (should (equal (phps-mode-functions-get-buffer-changes-start) 144)))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 2 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 144)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")
   (should (equal (phps-mode-functions-get-buffer-changes-start) 144))

   ;; Run incremental lexer
   (phps-mode-lexer-run-incremental)

   ;; Make changes - remove first function
   (goto-char 55)
   (push-mark nil t t)
   (goto-char 145)
   (execute-kbd-macro (kbd "<backspace>"))

   ;; Test
   (should (equal (phps-mode-functions-get-buffer-changes-start) 55)))

  (phps-mode-test-incremental-vs-intial-buffer
   ""
   "Integration-test 3 for function-oriented PHP"

   ;; Make changes
   (goto-char 1)
   (insert "<?php\nfunction myFunctionA()\n{\n    echo 'my second statement';\n}\n")

   ;; Test
   (should (equal (phps-mode-functions-get-buffer-changes-start) 1)))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 4 for regular PHP with namespaces, classes and functions, minor insert"

   ;; Make changes
   (goto-char 132)
   (insert " is a complex one")

   ;; Test
   (should (equal (phps-mode-functions-get-buffer-changes-start) 132)))

  
  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 5 for regular PHP with namespaces, classes and functions, single deletion"

   ;; Make changes - insert a echo
   (goto-char 132)
   (backward-delete-char-untabify 1)
   (should (equal (phps-mode-functions-get-buffer-changes-start) 131)))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n        echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 6 for regular PHP with namespaces, classes and functions, single indent line"

   ;; Make changes
   (goto-char 110)
   (indent-according-to-mode)

   ;; Test
   (should (equal (phps-mode-functions-get-buffer-changes-start) nil)))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 7 for regular PHP with namespaces, classes and functions, with multiple, isolated edits"

   ;; Make changes
   (goto-char 110)
   (indent-according-to-mode)

   ;; Make changes
   (goto-char 28)
   (insert "One")

   ;; Test
   (should (equal (phps-mode-functions-get-buffer-changes-start) 28)))

  )

(defun phps-mode-test-integration ()
  "Run test for integration."
  (setq debug-on-error t)
  (setq phps-mode-runtime-debug t)
  (setq phps-mode-lazy-process-buffer t)
  (phps-mode-test-integration-incremental))

(phps-mode-test-integration)

(provide 'phps-mode-test-integration)

;;; phps-mode-test-integration.el ends here
