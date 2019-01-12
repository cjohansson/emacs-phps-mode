;;; phps-mode-test-functions.el --- Tests for functions -*- lexical-binding: t -*-

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


(autoload 'phps-mode-test-with-buffer "phps-mode-test")
(autoload 'phps-mode-functions-verbose "phps-mode-functions")
(autoload 'phps-mode-functions-indent-line "phps-mode-functions")
(autoload 'phps-mode-functions-get-lines-indent "phps-mode-functions")
(autoload 'should "ert")

(defun phps-mode-test-functions--hash-to-list (hash-table)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value)."
  (let (result)
    (maphash
     (lambda (k v)
       (push (list k v) result))
     hash-table)
    (sort (nreverse result) (lambda (a b) (< (car a) (car b))))))

(defun phps-mode-test-functions-get-lines-indent ()
  "Test `phps-mode-functions-get-lines-indent' function."
  
  (phps-mode-test-with-buffer
   "<?php\n/**\n * Bla\n */"
   "DOC-COMMENT"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nmyFunction(array(\n    23,\n    [\n        25\n    ]\n    )\n);"
   "Round and square bracket expressions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (2 0)) (6 (1 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

    (phps-mode-test-with-buffer
   "<?php\nvar_dump(array(<<<EOD\nfoobar!\nEOD\n));\n?>"
   "HEREDOC in arguments example"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))


  (phps-mode-test-with-buffer
   "<?php\n$str = <<<'EOD'\nExample of string\nspanning multiple lines\nusing nowdoc syntax.\nEOD;\n"
   "Multi-line NOWDOC string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$var = \"A line\nmore text here\nlast line here\";"
   "Multi-line double-quoted string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$var = 'A line\nmore text here\nlast line here';"
   "Multi-line single-quoted string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\necho \"A line\" .\n    \"more text here\" .\n    \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\necho 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\necho <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string outside assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n * @var string\n */\necho 'was here';\n"
   "Statement after doc-comment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1)) (5 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n/** @define _SYSTEM_START_TIME_     Startup time for system */\ndefine('_SYSTEM_START_TIME_', microtime(true));\necho 'statement';\n"
   "Statement after a define() with a doc-comment"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nfunction myFunction($parameters = null)\n{\n    echo 'statement';\n}\n"
   "Statement after one-lined function declaration with optional argument"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php if (true) { ?>\n    <?php echo 'here'; ?>\n<?php } ?>"
   "Regular if-expression but inside scripting tags"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (1 0)) (3 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-functions-get-lines-indent-multi-line-assignments ()
  "Test for multi-line assignments."

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n    'random4'\n);\n$variable = true;\n"
   "Array assignment on three lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"
   "Array assignment on two lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) ) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$var = 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string multiple-lines in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n$var =\n    500 .\n    \"200\" .\n    100.0 .\n    '200' .\n    $this->getTail()\n    ->getBottom();"
   "Multi-line assignments"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (1 0)) (6 (1 0)) (7 (1 0)) (8 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-functions-get-lines-indent-inline-if ()
  "Test for inline if indentations."

  (phps-mode-test-with-buffer
   "<?php\nif (true)\n    echo 'Something';\nelse\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true)\n    echo 'Something';\nelse if (true)\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else if"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nwhile (true)\n    echo 'Something';"
   "Inline control structures while"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-functions-get-lines-indent-alternative-if ()
  "Test for alternative if indentations."

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (1 0)) (9 (0 0)) (10 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true\n):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 1"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (0 0)) (9 (1 0)) (10 (1 0)) (11 (0 0)) (12 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (1 0)) (7 (0 0)) (8 (1 0)) (9 (1 0)) (10 (0 0)) (11 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-function-get-lines-indent-classes ()
  "Test for class indent."

  (phps-mode-test-with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (2 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (2 0)) (6 (2 0)) (7 (3 0)) (8 (2 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nclass MyClass extends MyAbstract implements\n    myInterface,\n    myInterface2\n{\n}\n"
   "Class multi-line implements"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nclass MyClass\n    extends MyAbstract\n    implements myInterface, myInterface2\n{\n}\n"
   "Class multi-line extends and implements"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))


  (phps-mode-test-with-buffer
   "<?php\n/**\n *\n */\nnamespace Aomebo\n{\n    /**\n     *\n     */\n    class Base\n    {\n    }\n}\n"
   "Namespace and class with doc-comments"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1)) (5 (0 0)) (6 (0 0)) (7 (1 0)) (8 (1 1)) (9 (1 1)) (10 (1 0)) (11 (1 0)) (12 (1 0)) (13 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

)

(defun phps-mode-test-functions-get-lines-lindent-if ()
  "Test for multi-line if expressions."

  (phps-mode-test-with-buffer
   "<?php\nif (\n    true\n    && true\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 1"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n// Can we load configuration?\nif ($configuration::load(\n    self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME)\n)) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (1 0)) (7 (1 0)) (8 (0 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

    (phps-mode-test-with-buffer
   "<?php\nif (true) {\n    if ($configuration::load(\n        self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME))\n    ) {\n        echo 'was here';\n    }\n}\n"
   "If expression spanning multiple lines 3"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (2 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFunction(true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 4"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFunction(\n    true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 5"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true) {\n    if (myFunction(\n        true)\n    ) {\n        echo 'was here';\n    }\n}\n"
   "Nested if expression spanning multiple lines 6"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (2 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\n        echo $title2;\n\n    } ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression and token-less lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (1 0)) (3 (2 0)) (4 (2 0)) (5 (1 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php\nif ($myCondition) {\n    if ($mySecondCondition) {\n        echo $title;\n    } else if ($mySecondCondition) {\n        echo $title4;\n    } else {\n        echo $title2;\n        echo $title3;\n    }\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (2 0)) (7 (1 0)) (8 (2 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression indent calculation"
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-functions-get-lines-indent-switch-case ()
  "Test for switch-case indentation."

  (phps-mode-test-with-buffer
   "<?php\nswitch ($condition) {\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\n}\n"
   "Switch, case, default"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nswitch ($condition):\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\nendswitch;\n"
   "Switch, case, default with alternative control structure"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  )

(defun phps-mode-test-functions-indent-line ()
  "Test for indentation."

  ;; Curly bracket tests
  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test"
   (goto-char 69)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title1;\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test 2"
   (goto-char 75)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n        echo $title1;\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test 3"
   (goto-char 98)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n    } ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"
   "Curly bracket test 4"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (goto-char 110)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n'random3'\n);\n$variable = true;\n"
   "Assignment test 1"
   (goto-char 28)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random3'\n);\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n    'random2'\n    );\n$variable = true;\n"
   "Assignment test 2"
   (goto-char 43)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random2'\n);\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 1"
   (goto-char 20)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n * My first line\n* My second line\n**/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 2"
   (goto-char 9)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n**/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 3"
   (goto-char 46)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n **/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"
   "Round bracket test 1"
   (goto-char 30)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n'random4');\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nadd_filter(\n\"views_{$screen->id}\",'__return_empty_array'\n);"
   "Round bracket test 2"
   (goto-char 25)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nadd_filter(\n    \"views_{$screen->id}\",'__return_empty_array'\n);"))))

  (phps-mode-test-with-buffer
   "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"
   "Round bracket test 3"
   (goto-char 36)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"
   "Nested if-expression"
   (goto-char 54)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression"
   (goto-char 68)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"
   "Regular else if test"
   (goto-char 68)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"))))

  ;; Square bracket
  (phps-mode-test-with-buffer
   "<?php\n$var = [\n    'random' => [\n        'hello',\n],\n];\n"
   "Square bracket test 1"
   (goto-char 51)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = [\n    'random' => [\n        'hello',\n    ],\n];\n"))))
  
  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    else:\n    echo 'Something else here 8';\nendif;\n"
   "Alternative else test"
   (goto-char 62)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\necho 'Something here';\nelse:\n    echo 'Something else here 8';\nendif;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case indentation test"
   (goto-char 45)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()) {\n    case 'Something here':\necho 'Something else here';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nswitch (myRandomCondition()): \ncase 'Something here':\necho 'Something else here';\nendswitch;\n"
   "Alternative switch case indentation test 2"
   (goto-char 70)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()): \ncase 'Something here':\n        echo 'Something else here';\nendswitch;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition())\necho 'Something here';\necho 'Something else here';\n"
   "Inline control structure indentation"
   (goto-char 40)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition())\n    echo 'Something here';\n    echo 'Something else here';\n"
   "Inline control structure indentation 2"
   (goto-char 60)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    echo 'Something else here';\nendif;\n"
   "Alternative control structure indentation 1"
   (goto-char 40)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\n    echo 'Something here';\n    echo 'Something else here';\nendif;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n        ),\n    $var5\n);\n"
   "Function arguments with associate array indentation"
   (goto-char 65)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n    ),\n    $var5\n);\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$var = $var2->getHead()\n->getTail();\n"
   "Multi-line assignment indentation test 1"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (goto-char 35)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = $var2->getHead()\n    ->getTail();\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$var =\n'random string';\n"
   "Single-line assignment indentation test"
   (goto-char 20)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var =\n    'random string';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   "Alternative control structure if expression"
   (goto-char 60)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\nendif;"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   "Alternative control structure test"
   (goto-char 35)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n    $this->var = 'abc123';\n    endif;"))))

  )

;; TODO Add tests for all examples here: https://www.php-fig.org/psr/psr-2/

(defun phps-mode-test-functions ()
  "Run test for functions."
  ;; (setq debug-on-error t)
  (setq phps-mode-functions-verbose t)
  (phps-mode-test-functions-get-lines-lindent-if)
  (phps-mode-test-function-get-lines-indent-classes)
  (phps-mode-test-functions-get-lines-indent-inline-if)
  (phps-mode-test-functions-get-lines-indent-alternative-if)
  (phps-mode-test-functions-get-lines-indent-multi-line-assignments)
  (phps-mode-test-functions-get-lines-indent)
  (phps-mode-test-functions-get-lines-indent-switch-case)
  (phps-mode-test-functions-indent-line))

(phps-mode-test-functions)

(provide 'phps-mode-test-functions)

;;; phps-mode-test-functions.el ends here
