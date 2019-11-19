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
(require 'phps-mode)
(require 'phps-mode-test)

(eval-when-compile
  (require 'phps-mode-macros))

(defun phps-mode-test-integration-incremental-vs-initial-buffers ()
  "Test for object-oriented PHP file."

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 1 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 145)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n"))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 2 for regular PHP with namespaces, classes and functions"

   ;; Make changes - insert a new function
   (goto-char 145)
   (insert "\n\n        public function myFunctionB()\n        {\n            echo 'my second statement';\n        }\n")

   ;; Make changes - remove first function
   (goto-char 55)
   (push-mark nil t t)
   (goto-char 145)
   (execute-kbd-macro (kbd "<backspace>")))

  (phps-mode-test-incremental-vs-intial-buffer
   ""
   "Integration-test 3 for function-oriented PHP"

   ;; Make changes
   (goto-char 1)
   (insert "<?php\nfunction myFunctionA()\n{\n    echo 'my second statement';\n}\n"))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 4 for regular PHP with namespaces, classes and functions, minor insert"

   ;; Make changes
   (goto-char 132)
   (insert " is a complex one"))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 5 for regular PHP with namespaces, classes and functions, single deletion"

   ;; Make changes - insert a echo
   (goto-char 132)
   (backward-delete-char-untabify 1))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n        echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 6 for regular PHP with namespaces, classes and functions, single indent line"

   ;; Make changes
   (goto-char 110)
   (insert "    "))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n        echo 'my statement';\n        }\n    }\n}\n"
   "Integration-test 7 for regular PHP with namespaces, classes and functions, with multiple, isolated edits"

   ;; Make changes
   (goto-char 110)
   (insert "    ")

   ;; Make changes
   (goto-char 28)
   (insert "One"))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\nif ($myCondition) {}\n"
   "Integration-test 8 for regular PHP with newline between curly brackets"

   ;; Make changes
   (goto-char 26)
   (execute-kbd-macro (kbd "RET")))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\n/**\n * @see Something\n * @see here\n *\n */\n"
   "Integration-test 9 for regular PHP with newline in doc comment block"

   ;; Make changes
   (goto-char 41)
   (execute-kbd-macro (kbd "RET")))

  (phps-mode-test-incremental-vs-intial-buffer
   "<?php\necho 'my comment';\n"
   "Integration-test 10 insert code at end of buffer"

   ;; Make changes
   (goto-char (point-max))

   (insert "\necho 'my comments';\n"))

  ;; TODO T_ENCAPSED_AND_WHITESPACE 72 should be removed by deletion (that is implicitly triggered by electric-pair-mode)
  (phps-mode-test-incremental-vs-intial-buffer
   ""
   "Integration-test 11 insert code in empty buffer using macro, use several passes"

   ;; Make changes - Pass 1
   (goto-char (point-max))
   (execute-kbd-macro "<?php")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro "echo 'was here';")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro (kbd "RET"))
   (phps-mode-analyzer-process-changes)

   ;; Pass 2
   (execute-kbd-macro "if ($myCondition) {")
   (execute-kbd-macro (kbd "RET"))
   (execute-kbd-macro "echo 'my special condition';")
   (phps-mode-analyzer-process-changes)

   ;; Pass 3
   (execute-kbd-macro (kbd "TAB"))
   (execute-kbd-macro (kbd "RET")))

  )

;; TODO Add tests for (delete-backward-char) as well
(defun phps-mode-test-integration-whitespace-modifications ()
  "Test white-space modifications functions."

  (phps-mode-test-with-buffer
   "<?php\n$var = 'abc';\n\n$var2 = '123';\n"
   "Add newline between two assignments and inspect moved tokens and states"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)

   ;; Initial state

   ;; Tokens
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 19) (";" 19 . 20) (T_VARIABLE 22 . 27) ("=" 28 . 29) (T_CONSTANT_ENCAPSED_STRING 30 . 35) (";" 35 . 36))))

   ;; States
   (should (equal phps-mode-lexer-states
                  '((35 36 1 nil) (30 35 1 nil) (28 29 1 nil) (22 27 1 nil) (19 20 1 nil) (14 19 1 nil) (12 13 1 nil) (7 11 1 nil) (1 7 1 nil))))
   
   ;; Insert newline
   (goto-char 21)
   (newline)

   ;; Final state
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)
   (phps-mode-analyzer-process-changes)
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)

   ;; Tokens
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 19) (";" 19 . 20) (T_VARIABLE 23 . 28) ("=" 29 . 30) (T_CONSTANT_ENCAPSED_STRING 31 . 36) (";" 36 . 37))))

   ;; States
   (should (equal phps-mode-lexer-states
                  '((36 37 1 nil) (31 36 1 nil) (29 30 1 nil) (23 28 1 nil) (19 20 1 nil) (14 19 1 nil) (12 13 1 nil) (7 11 1 nil) (1 7 1 nil)))))

  (phps-mode-test-with-buffer
   "<?php\n$var = 'abc';\n\n$var2 = '123';\n"
   "Delete backward char between two assignments and inspect moved tokens and states"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)

   ;; Initial state

   ;; Tokens
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 19) (";" 19 . 20) (T_VARIABLE 22 . 27) ("=" 28 . 29) (T_CONSTANT_ENCAPSED_STRING 30 . 35) (";" 35 . 36))))

   ;; States
   (should (equal phps-mode-lexer-states
                  '((35 36 1 nil) (30 35 1 nil) (28 29 1 nil) (22 27 1 nil) (19 20 1 nil) (14 19 1 nil) (12 13 1 nil) (7 11 1 nil) (1 7 1 nil))))

   ;; Insert newline
   (goto-char 21)
   (delete-char 1)

   (phps-mode-analyzer-process-changes)

   ;; Final state
   ;; (message "Modified buffer: '%s'" (buffer-substring-no-properties (point-min) (point-max)))
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)

   ;; Tokens
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_VARIABLE 7 . 11) ("=" 12 . 13) (T_CONSTANT_ENCAPSED_STRING 14 . 19) (";" 19 . 20) (T_VARIABLE 21 . 26) ("=" 27 . 28) (T_CONSTANT_ENCAPSED_STRING 29 . 34) (";" 34 . 35))))

   ;; States
   (should (equal phps-mode-lexer-states
                  '((34 35 1 nil) (29 34 1 nil) (27 28 1 nil) (21 26 1 nil) (19 20 1 nil) (14 19 1 nil) (12 13 1 nil) (7 11 1 nil) (1 7 1 nil)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    $var = 'abc';\n    $var2 = '123';\nendif;\n"
   "Add newline inside if body after two assignments and inspect moved tokens and states"

   ;; Initial state
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_STRING 11 . 15) (")" 15 . 16) (":" 16 . 17) (T_VARIABLE 22 . 26) ("=" 27 . 28) (T_CONSTANT_ENCAPSED_STRING 29 . 34) (";" 34 . 35) (T_VARIABLE 40 . 45) ("=" 46 . 47) (T_CONSTANT_ENCAPSED_STRING 48 . 53) (";" 53 . 54) (T_ENDIF 55 . 60) (";" 60 . 61))))

   (should (equal phps-mode-lexer-states
                  '((60 61 1 nil) (55 60 1 nil) (53 54 1 nil) (48 53 1 nil) (46 47 1 nil) (40 45 1 nil) (34 35 1 nil) (29 34 1 nil) (27 28 1 nil) (22 26 1 nil) (16 17 1 nil) (15 16 1 nil) (11 15 1 nil) (10 11 1 nil) (7 9 1 nil) (1 7 1 nil))))

   ;; Insert newline and then indent
   (goto-char 54)
   (newline-and-indent)

   (phps-mode-analyzer-process-changes)

   ;; Final state
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   ;; (message "States: %s" phps-mode-lexer-states)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_STRING 11 . 15) (")" 15 . 16) (":" 16 . 17) (T_VARIABLE 22 . 26) ("=" 27 . 28) (T_CONSTANT_ENCAPSED_STRING 29 . 34) (";" 34 . 35) (T_VARIABLE 40 . 45) ("=" 46 . 47) (T_CONSTANT_ENCAPSED_STRING 48 . 53) (";" 53 . 54) (T_ENDIF 60 . 65) (";" 65 . 66))))

   (should (equal phps-mode-lexer-states
                  '((65 66 1 nil) (60 65 1 nil) (53 54 1 nil) (48 53 1 nil) (46 47 1 nil) (40 45 1 nil) (34 35 1 nil) (29 34 1 nil) (27 28 1 nil) (22 26 1 nil) (16 17 1 nil) (15 16 1 nil) (11 15 1 nil) (10 11 1 nil) (7 9 1 nil) (1 7 1 nil)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    $var = \"abc\nanother line here\nmore text here\";\n    $var2 = '123';\nendif;"
   "Add test for inserting newlines inside token"

   ;; (message "Before Tokens %s" phps-mode-lexer-tokens)
   ;; (message "Before States: %s" phps-mode-lexer-states)

   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_STRING 11 . 15) (")" 15 . 16) (":" 16 . 17) (T_VARIABLE 22 . 26) ("=" 27 . 28) (T_CONSTANT_ENCAPSED_STRING 29 . 67) (";" 67 . 68) (T_VARIABLE 73 . 78) ("=" 79 . 80) (T_CONSTANT_ENCAPSED_STRING 81 . 86) (";" 86 . 87) (T_ENDIF 88 . 93) (";" 93 . 94))))
   (should (equal phps-mode-lexer-states
                  '((93 94 1 nil) (88 93 1 nil) (86 87 1 nil) (81 86 1 nil) (79 80 1 nil) (73 78 1 nil) (67 68 1 nil) (29 67 1 nil) (27 28 1 nil) (22 26 1 nil) (16 17 1 nil) (15 16 1 nil) (11 15 1 nil) (10 11 1 nil) (7 9 1 nil) (1 7 1 nil))))

   ;; Insert newline and then indent
   (goto-char 51)
   (newline-and-indent)

   (phps-mode-analyzer-process-changes)

   ;; (message "After Tokens %s" phps-mode-lexer-tokens)
   ;; (message "After States: %s" phps-mode-lexer-states)
   (should (equal phps-mode-lexer-tokens
                  '((T_OPEN_TAG 1 . 7) (T_IF 7 . 9) ("(" 10 . 11) (T_STRING 11 . 15) (")" 15 . 16) (":" 16 . 17) (T_VARIABLE 22 . 26) ("=" 27 . 28) (T_CONSTANT_ENCAPSED_STRING 29 . 76) (";" 76 . 77) (T_VARIABLE 82 . 87) ("=" 88 . 89) (T_CONSTANT_ENCAPSED_STRING 90 . 95) (";" 95 . 96) (T_ENDIF 97 . 102) (";" 102 . 103))))
   (should (equal phps-mode-lexer-states
                  '((102 103 1 nil) (97 102 1 nil) (95 96 1 nil) (90 95 1 nil) (88 89 1 nil) (82 87 1 nil) (76 77 1 nil) (29 76 1 nil) (27 28 1 nil) (22 26 1 nil) (16 17 1 nil) (15 16 1 nil) (11 15 1 nil) (10 11 1 nil) (7 9 1 nil) (1 7 1 nil)))))

  (phps-mode-test-with-buffer
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n"
   "White-space changes in imenu function-oriented file"

   (should (equal (phps-mode-functions-get-imenu) '(("myFunctionA" . 16) ("myFunctionB" . 42))))

   (goto-char 32)
   (newline-and-indent)

   (phps-mode-analyzer-process-changes)

   (should (equal (phps-mode-functions-get-imenu) '(("myFunctionA" . 16) ("myFunctionB" . 43)))))

  )

(defun phps-mode-test-integration ()
  "Run test for integration."
  (setq debug-on-error t)
  ;; (setq phps-mode-analyzer-process-on-indent-and-imenu t)
  (phps-mode-test-integration-incremental-vs-initial-buffers)
  ;; (phps-mode-test-integration-whitespace-modifications)
  )

(phps-mode-test-integration)

(provide 'phps-mode-test-integration)

;;; phps-mode-test-integration.el ends here
