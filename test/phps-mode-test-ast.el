;;; phps-mode-test-ast.el --- Tests for AST -*- lexical-binding: t -*-

;; Copyright (C) 2017-2021  Free Software Foundation, Inc.

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

;; Run from terminal make test-parser


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-ast)

(defun phps-mode-test-ast--buffer-contents (buffer-contents name logic)
  (with-temp-buffer
    ;; Setup buffer
    (insert buffer-contents)
    (message
     "Testing buffer %S with buffer-contents:\n%S\n"
     name
     (buffer-substring-no-properties (point-min) (point-max)))
    
    ;; Setup lexer
    (setq
     phps-mode-lexer--generated-tokens
     nil)
    (setq
     phps-mode-lexer--state
     'ST_INITIAL)
    (setq
     phps-mode-lexer--states
     nil)
    (setq
     phps-mode-lexer--state-stack
     nil)
    (setq
     phps-mode-lexer--heredoc-label
     nil)
    (setq
     phps-mode-lexer--heredoc-label-stack
     nil)
    (setq
     phps-mode-lexer--nest-location-stack
     nil)

    ;; Run lexer
    (setq
     semantic-lex-analyzer
     #'phps-mode-lex-analyzer--re2c-lex)
    (setq
     semantic-lex-syntax-table
     phps-mode-syntax-table)
    (semantic-lex-buffer)
    (setq
     phps-mode-parser-tokens
     (phps-mode-lex-analyzer--generate-parser-tokens
      phps-mode-lexer--generated-tokens))

    ;; Run test
    (funcall logic)
    (message "Passed test for %S\n" name)))

(defun phps-mode-test-ast-imenu ()
  "Run test for imenu generation."
  (message "-- Running tests for imenu generation... --\n")

  (phps-mode-test-ast--buffer-contents
   "<?php\nclass myClass\n{\n\n    public function myFunction1()\n    {\n        echo \"my string with variable {$variable} inside it\";\n    }\n\n    public function myFunction2()\n    {\n    }\n\n}"
   "Imenu generated via parser SDT for simple class"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myClass" ("myFunction1" . 44) ("myFunction2" . 153)))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\ninterface myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu generated via parser SDT for interface"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myInterface" . (("myFunctionA" . 51) ("myFunctionB" . 91))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n$var = function () {\n    echo 'here';\n};"
   "Imenu generated via parser SDT for function-oriented file without namespace"
   (lambda()
     (phps-mode-ast-generate)
     (should (equal
              phps-mode-ast--imenu
              '(("myFunctionA" . 16) ("myFunctionB" . 42))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace MyNamespace;\n\nfunction aFunction() {\n    /**\n     * With some contents\n     */\n}\n\nclass MyClass\n{\n\n    /**\n     *\n     */\n    public function __construct()\n    {\n        if ($test) {\n        }\n    }\n\n    /**\n     *\n     */\n    public function myFunction1()\n    {\n        $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n    }\n    \n    /**\n     *\n     */\n    public function myFunction2()\n    {\n    }\n\n    /**\n     * It's good\n     */\n    public function myFunction3()\n    {\n    }\n\n    /**\n     *\n     */\n    public function myFunction4()\n    {\n    }\n}\n"
   "Passed imenu-generation via parser AST for basic object oriented file"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("MyNamespace" ("aFunction" . 41) ("MyClass" ("__construct" . 160) ("myFunction1" . 261) ("myFunction2" . 433) ("myFunction3" . 513) ("myFunction4" . 583))))))))

  (phps-mode-test-ast--buffer-contents
   "<?php\n\nnamespace MyNamespaceA\n{\n    function aFunctionA() {\n        /**\n         * With some contents\n         */\n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}\nnamespace {\n    function aFunctionB()\n    {\n        \n    }\n    class MyClass\n    {\n\n        /**\n         *\n         */\n        public function __construct()\n        {\n            if ($test) {\n            }\n        }\n\n        /**\n         *\n         */\n        public function myFunction1()\n        {\n            $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n        }\n        \n        /**\n         *\n         */\n        public function myFunction2()\n        {\n        }\n\n        /**\n         * It's good\n         */\n        public function myFunction3()\n        {\n        }\n\n        /**\n         *\n         */\n        public function myFunction4()\n        {\n        }\n    }\n}"
   "Passed imenu-generation via parser AST for advanced object oriented file"
   (lambda()
     (phps-mode-ast-generate)
     (should
      (equal
       phps-mode-ast--imenu
       '(("MyNamespaceA" ("aFunctionA" . 46) ("MyClass" ("__construct" . 205) ("myFunction1" . 338) ("myFunction2" . 542) ("myFunction3" . 646) ("myFunction4" . 740))) ("aFunctionB" . 807) ("MyClass" ("__construct" . 925) ("myFunction1" . 1058) ("myFunction2" . 1262) ("myFunction3" . 1366) ("myFunction4" . 1460)))))))

  (message "\n-- Ran tests for imenu generation. --"))

(defun phps-mode-test-ast ()
  "Run test for ast."
  (message "-- Running all tests for ast... --\n")

  (phps-mode-test-ast-imenu)

  (message "\n-- Ran all tests for ast. --"))

(phps-mode-test-ast)

(provide 'phps-mode-test-ast)


;;; phps-mode-test-ast.el ends here
