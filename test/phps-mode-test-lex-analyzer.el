;;; phps-mode-test-lex-analyzer.el --- Tests for functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

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

;; Run from terminal make functions-test


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

(defun phps-mode-test-lex-analyzer--process-changes ()
  "Test `phps-mode-lex-analyzer--process-changes'."

  (phps-mode-test--with-buffer
   "\n<html>\n<?php\n/**\n * Bla\n */"
   "Process changes before current tokens"
   (goto-char (point-min))
   (insert "<?php echo 'test';\n?>")
   (should (equal
            (phps-mode-lex-analyzer--process-changes)
            '((RUN-FULL-LEXER) (FOUND-NO-HEAD-TOKENS 1)))))

  (phps-mode-test--with-buffer
   "\n<html>\n<?php\n/**\n * Bla\n */"
   "Process changes without changes"
   (should (equal
            (phps-mode-lex-analyzer--process-changes)
            '((RUN-FULL-LEXER) (FOUND-NO-CHANGE-POINT-MINIMUM)))))

  (phps-mode-test--with-buffer
   "\n<html>\n<?php\n/**\n * Bla\n */"
   "Process changes after existing tokens"
   (goto-char (point-max))
   (insert "\necho 'I was here';\n")
   (should (equal
            (phps-mode-lex-analyzer--process-changes)
            '((INCREMENTAL-LEX 15)))))

  )

(defun phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer ()
  "Use alternative indentation of every line of buffer."
  (goto-char (point-min))
  (phps-mode-lex-analyzer--alternative-indentation)
  (while (search-forward "\n" nil t nil)
    (phps-mode-lex-analyzer--alternative-indentation)))

(defun phps-mode-test-lex-analyzer--alternative-indentation ()
  "Test `phps-mode-lex-analyzer--alternative-indentation'."

  (phps-mode-test--with-buffer
   "<?php\nif ($myCondition) {\necho 'I was here';\n}"
   "Alternative indentation inside if block"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($myCondition) {\n    echo 'I was here';\n}"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($myCondition) {\necho 'I was here';\necho 'I was here again';\n}"
   "Alternative indentation on closing if block"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($myCondition) {\n    echo 'I was here';\n    echo 'I was here again';\n}"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($test) {\nif ($test2) {\n\n}\n}"
   "Alternative indentation on nested if block with empty contents"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($test) {\n    if ($test2) {\n        \n    }\n}"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($test) {\nif ($test2) {\n\n}\n\n}"
   "Alternative indentation on multiple closing brackets"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($test) {\n    if ($test2) {\n        \n    }\n    \n}"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($test) {\n\n} else if ($test) {\n\n}\n"
   "Alternative indentation on elseif block"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($test) {\n    \n} else if ($test) {\n    \n}\n"))))

  (phps-mode-test--with-buffer
   "if ($true) {\nif ($true) {\n}\n}"
   "Alternative indentation on closing bracket inside parent bracket"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "if ($true) {\n    if ($true) {\n    }\n}"))))

  (phps-mode-test--with-buffer
   "/**\n*\n*/"
   "Alternative indentation on last line of doc comment block"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "/**\n *\n */"))))

  (phps-mode-test--with-buffer
   "    $var = 'abc';\n        // Comment"
   "Alternative indentation on single-line assignment"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "$var = 'abc';\n// Comment"))))

  (phps-mode-test--with-buffer
   "$var =\n'abc';\n$var =\n'abc'\n. 'def';\n// Comment\n"
   "Alternative indentation on multi-line assignment"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "$var =\n    'abc';\n$var =\n    'abc'\n    . 'def';\n// Comment\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($here) {\nif ($wasHere)\n{\n\n}\n}\n\n"
   "Alternative indentation on line after condition"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($here) {\n    if ($wasHere)\n    {\n        \n    }\n}\n\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($myCondition)\n{\n$var = array(\n'was here'\n);\n// Was here\n}\n"
   "Alternative indentation on line after array declaration"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($myCondition)\n{\n    $var = array(\n        'was here'\n    );\n    // Was here\n}\n"
              ))))

  (phps-mode-test--with-buffer
   "<?php\nif ($myCondition == 2) {\necho 'store_vars: <pre>' . print_r($store_vars, true) . '</pre>';\necho 'search_ids: <pre>' . print_r($search_ids, true) . '</pre>';\n}"
   "Alternative indentation on line echo"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($myCondition == 2) {\n    echo 'store_vars: <pre>' . print_r($store_vars, true) . '</pre>';\n    echo 'search_ids: <pre>' . print_r($search_ids, true) . '</pre>';\n}"
              ))))

  (phps-mode-test--with-buffer
   "<?php\nif (is_array(\n$array\n)) {\necho 'was here';\n}"
   "Alternative indentation after trailing opening bracket while closing two earlier on line"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif (is_array(\n    $array\n)) {\n    echo 'was here';\n}"
              ))))

  (phps-mode-test--with-buffer
   "<?php\n\n$var = array(\n'123' =>\n'def',\n);"
   "Alternative indentation on lines after lines ending with T_DOUBLE_ARROW"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\n\n$var = array(\n    '123' =>\n        'def',\n);"
              ))))

  (phps-mode-test--with-buffer
   "<?php\n$var = array(\n'123' => true,\n\n);"
   "Alternative indentation after comma ended double arrow assignment"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\n$var = array(\n    '123' => true,\n    \n);"
              ))))

  (phps-mode-test--with-buffer
   "<?php\nfunction myFunction(\n$arg = true,\n$arg2 = false\n) {\n\n}"
   "Line after function argument with default value"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nfunction myFunction(\n    $arg = true,\n    $arg2 = false\n) {\n    \n}"
              ))))

  (phps-mode-test--with-buffer
   "$random = get_post_meta(\n                $postId,\n            '_random', // TODO Here\n            true // TODO Here\n        );"
   "Line in multi-line function call"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "$random = get_post_meta(\n    $postId,\n    '_random', // TODO Here\n    true // TODO Here\n);"
              ))))

  (phps-mode-test--with-buffer
   "$cartPrice = round(\n    $cartPrice,\n2 // TODO Here\n);"
   "Assignment with multi-line function call"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "$cartPrice = round(\n    $cartPrice,\n    2 // TODO Here\n);"
              ))))

  (phps-mode-test--with-buffer
   "$applications =\n    $transaction->getResponseBodyDecoded();\n    // TODO Here\n"
   "Line after multi-line assignment with object-operator"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "$applications =\n    $transaction->getResponseBodyDecoded();\n// TODO Here\n"
              ))))

  (phps-mode-test--with-buffer
   "<?php\necho '<dl><dt>' . __('Data', 'something')\n    . ':</dt><dd><pre>' . print_r($decodedData, true) . '</pre></dd></dl>';\necho '<div class=\"meta actions\">';\n"
   "Two echo statements, one spans two lines"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\necho '<dl><dt>' . __('Data', 'something')\n    . ':</dt><dd><pre>' . print_r($decodedData, true) . '</pre></dd></dl>';\necho '<div class=\"meta actions\">';\n    "
              ))))

  (phps-mode-test--with-buffer
   "<?php\nif ($shippingMethod->id ===\n        \\MyClass::METHOD_ID\n    ) {\n"
   "Multi-linte if statement testing equality in two lines"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($shippingMethod->id ===\n    \\MyClass::METHOD_ID\n) {\n    "
              ))))

  )

(defun phps-mode-test-lex-analyzer--move-lines-indent ()
  "Test `phps-mode-functions-move-lines-indent'."

  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "Move line-indents zero lines down"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-moved-lines-indent (phps-mode-lex-analyzer--get-lines-indent) 2 0)))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "Move line-indents one line down"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 1)) (5 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-moved-lines-indent (phps-mode-lex-analyzer--get-lines-indent) 2 1)))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "Move line-indents two lines down"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 1)) (6 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-moved-lines-indent (phps-mode-lex-analyzer--get-lines-indent) 2 2)))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "Move line-indents one line up"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))
   (should (equal '((1 (0 0)) (2 (0 1)) (3 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-moved-lines-indent (phps-mode-lex-analyzer--get-lines-indent) 3 -1)))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "Move line-indents two lines up"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))
   (should (equal '((1 (0 1)) (2 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-moved-lines-indent (phps-mode-lex-analyzer--get-lines-indent) 3 -2)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent ()
  "Test `phps-mode-lex-analyzer--get-lines-indent' function."
  
  (phps-mode-test--with-buffer
   "<?php\n/**\n * Bla\n */"
   "DOC-COMMENT"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nmyFunction(array(\n    23,\n    [\n        25\n    ]\n    )\n);"
   "Round and square bracket expressions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (2 0)) (6 (1 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nvar_dump(array(<<<EOD\nfoobar!\nEOD\n));\n?>"
   "HEREDOC in arguments example"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))


  (phps-mode-test--with-buffer
   "<?php\n$str = <<<'EOD'\nExample of string\nspanning multiple lines\nusing nowdoc syntax.\nEOD;\n"
   "Multi-line NOWDOC string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var = \"A line\nmore text here\nlast line here\";"
   "Multi-line double-quoted string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var = 'A line\nmore text here\nlast line here';"
   "Multi-line single-quoted string"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho \"A line\" .\n    \"more text here\" .\n    \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho \"A line\"\n    . \"more text here\"\n    . \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho myFunction('A line' .\n    'more text here' .\n    'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho 'A line'\n    . 'more text here'\n    . 'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho myFunction('A line'\n    . 'more text here'\n    . 'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string outside assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * @var string\n */\necho 'was here';\n"
   "Statement after doc-comment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n/** @define _SYSTEM_START_TIME_     Startup time for system */\ndefine('_SYSTEM_START_TIME_', microtime(true));\necho 'statement';\n"
   "Statement after a define() with a doc-comment"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nfunction myFunction($parameters = null)\n{\n    echo 'statement';\n}\n"
   "Statement after one-lined function declaration with optional argument"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php if (true) { ?>\n    <?php echo 'here'; ?>\n<?php } ?>"
   "Regular if-expression but inside scripting tags"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (1 0)) (3 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated assignment string with function call"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated assignment string with function call"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated echo string with function call"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated echo string with function call"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$options = [\n    0 => [\n        'label' => __('No'),\n        'value' => 0,\n    ],\n];"
   "Assignment with square bracketed array"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$options = array(\n    'blaha' .\n        'blaha',\n    123,\n    'blaha'\n);"
   "Assignment with square bracketed array"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nreturn $variable\n    && $variable;"
   "Multi-line return statement"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$options = myFunction(\n    array(array(\n        'options' => 123\n    ))\n);"
   "Assignment with double-dimensional array with double arrow assignment inside function call"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nswitch ($condition) {\n    case 34:\n        if ($item['Random'] % 10 == 0) {\n            $attributes['item'] = ($item['IntegerValue'] / 10);\n        } else {\n            $attributes['item'] =\n                number_format(($item['IntegerValue'] / 10), 1, '.', '');\n        }\n        break;\n}\n"
   "Switch case with conditional modulo expression"
   ;; (message "indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (3 0)) (6 (2 0)) (7 (3 0)) (8 (4 0)) (9 (2 0)) (10 (2 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$options = array(\n    'options' => array(array(\n        'errorTo'\n    ))\n);"
   "Assignment with three-dimensional array with double arrow assignment"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif ($myCondition) {\n    $myObject->myMethod(myClass::class)\n        ->myMethod2($myArgument2);\n    }"
   "Object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$myObj->myFunction()\n    ->mySecondaryFunction();"
   "Indentation of chained class method calls outside of assignments and conditionals"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$myVar = $myClass->meMethod()\n    ->mySecondMethod()\n    ->myThirdMethod()\n->myFourthFunction(\n    $myVariable\n);"
   "Indentation for chained object operators in assignment with method call with arguments"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (1 0)) (7 (2 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$myResult = !empty($myVar->myMethod3)\n    && $myVar->myMethod\n        && $myVar->myMethod2;\n"
   "Indentation for chained object operators in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$array = [\n    'second' => [\n        'hello' => true\n        ]\n];\n\n$array = array(\n    'second' => array(\n        'third' => true\n        )\n);"
   "Indent multi-dimensional arrays without trailing commas"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (0 0)) (7 (0 0)) (8 (0 0)) (9 (1 0)) (10 (2 0)) (11 (1 0)) (12 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent))))

   (phps-mode-test--with-buffer
    "<html>\n    <head>\n        <?php echo $title; ?>\n    </head>\n    <body>\n    <?php\n\n    if ($myTest) {\n        doSomething();\n    }\n\n    ?>\n    </body>\n</html>"
    "A mixed HTML and PHP file."
    ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
    (should (equal '((1 (0 0)) (2 (1 0)) (3 (2 0)) (4 (1 0)) (5 (1 0)) (6 (0 0)) (7 (0 0)) (8 (0 0)) (9 (1 0)) (10 (0 0)) (11 (0 0)) (12 (0 0)) (13 (1 0)) (14 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))
   )

  (phps-mode-test--with-buffer
   "<?php\n\n    if ($fullInfo) $fullInfo = unserialize ($fullInfo);\n    else array();\n\n"
   "Indentation for single-line inline control structures."
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n\nif (true) {\n    // Was here\n}"
   "If condition after a mixed newline encoded file"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-psr-2 ()
  "Test PSR-2 examples from: https://www.php-fig.org/psr/psr-2/."

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nuse FooInterface;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass Foo extends Bar implements FooInterface\n{\n    public function sampleMethod($a, $b = null)\n    {\n        if ($a === $b) {\n            bar();\n        } elseif ($a > $b) {\n            $foo->bar($arg1);\n        } else {\n            BazClass::bar($arg2, $arg3);\n        }\n    }\n\n    final public static function bar()\n    {\n        // method body\n    }\n}\n"
   "PSR-2 : 1.1. Example"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0)) (7 (0 0)) (8 (0 0)) (9 (0 0)) (10 (1 0)) (11 (1 0)) (12 (2 0)) (13 (3 0)) (14 (2 0)) (15 (3 0)) (16 (2 0)) (17 (3 0)) (18 (2 0)) (19 (1 0)) (20 (1 0)) (21 (1 0)) (22 (1 0)) (23 (2 0)) (24 (1 0)) (25 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\n// ... additional PHP code ..."
   "PSR-2 : 3. Namespace and Use Declarations"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0)) (7 (0 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements \\ArrayAccess, \\Countable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 1"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0)) (7 (0 0)) (8 (0 0)) (9 (0 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements\n    \\ArrayAccess,\n    \\Countable,\n    \\Serializable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 2"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0)) (7 (0 0)) (8 (0 0)) (9 (1 0)) (10 (1 0)) (11 (1 0)) (12 (0 0)) (13 (1 0)) (14 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public $foo = null;\n}"
   "PSR-2 : 4.2. Properties"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function fooBarBaz($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.3. Methods"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (1 0)) (7 (1 0)) (8 (2 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function foo($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 1"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (1 0)) (7 (1 0)) (8 (2 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function aVeryLongMethodName(\n        ClassTypeHint $arg1,\n        &$arg2,\n        array $arg3 = []\n    ) {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 2"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (1 0)) (7 (2 0)) (8 (2 0)) (9 (2 0)) (10 (1 0)) (11 (2 0)) (12 (1 0)) (13 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace Vendor\\Package;\n\nabstract class ClassName\n{\n    protected static $foo;\n\n    abstract protected function zim();\n\n    final public static function bar()\n    {\n        // method body\n    }\n}"
   "PSR-2 ; 4.5. abstract, final, and static"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (1 0)) (7 (1 0)) (8 (1 0)) (9 (1 0)) (10 (1 0)) (11 (1 0)) (12 (2 0)) (13 (1 0)) (14 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nbar();\n$foo->bar($arg1);\nFoo::bar($arg2, $arg3);"
   "PSR-2 : 4.6. Method and Function Calls : Example 1"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$foo->bar(\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n);"
   "PSR-2 : 4.6. Method and Function Calls : Example 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif ($expr1) {\n    // if body\n} elseif ($expr2) {\n    // elseif body\n} else {\n    // else body;\n}"
   "PSR-2 : 5.1. if, elseif, else"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nswitch ($expr) {\n    case 0:\n        echo 'First case, with a break';\n        break;\n    case 1:\n        echo 'Second case, which falls through';\n        // no break\n    case 2:\n    case 3:\n    case 4:\n        echo 'Third case, return instead of break';\n        return;\n    default:\n        echo 'Default case';\n        break;\n}"
   "PSR-2 : 5.2. switch, case"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (2 0)) (8 (2 0)) (9 (1 0)) (10 (1 0)) (11 (1 0)) (12 (2 0)) (13 (2 0)) (14 (1 0)) (15 (2 0)) (16 (2 0)) (17 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nwhile ($expr) {\n    // structure body\n}"
   "PSR-2 : 5.3. while, do while : Example 1"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\ndo {\n    // structure body;\n} while ($expr);"
   "PSR-2 : 5.3. while, do while : Example 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nfor ($i = 0; $i < 10; $i++) {\n    // for body\n}"
   "PSR-2 : 5.4. for"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))
  
  (phps-mode-test--with-buffer
   "<?php\nforeach ($iterable as $key => $value) {\n    // foreach body\n}"
   "PSR-2 : 5.5. foreach"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\ntry {\n    // try body\n} catch (FirstExceptionType $e) {\n    // catch body\n} catch (OtherExceptionType $e) {\n    // catch body\n}"
   "PSR-2 : 5.6. try, catch"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$closureWithArgs = function ($arg1, $arg2) {\n    // body\n};\n\n$closureWithArgsAndVars = function ($arg1, $arg2) use ($var1, $var2) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 1"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (0 0)) (6 (0 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$longArgs_noVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) {\n    // body\n};\n\n$noArgs_longVars = function () use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_longVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_shortVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use ($var1) {\n    // body\n};\n\n$shortArgs_longVars = function ($arg) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 2"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (0 0)) (9 (0 0)) (10 (0 0)) (11 (1 0)) (12 (1 0)) (13 (1 0)) (14 (0 0)) (15 (1 0)) (16 (0 0)) (17 (0 0)) (18 (0 0)) (19 (1 0)) (20 (1 0)) (21 (1 0)) (22 (0 0)) (23 (1 0)) (24 (1 0)) (25 (1 0)) (26 (0 0)) (27 (1 0)) (28 (0 0)) (29 (0 0)) (30 (0 0)) (31 (1 0)) (32 (1 0)) (33 (1 0)) (34 (0 0)) (35 (1 0)) (36 (0 0)) (37 (0 0)) (38 (0 0)) (39 (1 0)) (40 (1 0)) (41 (1 0)) (42 (0 0)) (43 (1 0)) (44 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$foo->bar(\n    $arg1,\n    function ($arg2) use ($var1) {\n        // body\n    },\n    $arg3\n);"
   "PSR-2 : 6. Closures : Example 3"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (2 0)) (6 (1 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-multi-line-assignments ()
  "Test for multi-line assignments."

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n    'random4'\n);\n$variable = true;\n"
   "Array assignment on three lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n    'random4' =>\n        'hello'\n);"
   "Array assignment with double arrow elements on four lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"
   "Array assignment on two lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) ) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var = 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string multiple-lines in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var .=\n    'A line';"
   "Concatenated equal single-quoted-string on multiple-lines in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var *=\n    25;"
   "Multiplication equal assignment on multiple-lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string in assignment"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (0 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n$var =\n    500 .\n    \"200\" .\n    100.0 .\n    '200' .\n    $this->getTail()\n    ->getBottom();"
   "Multi-line assignments"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (2 0)) (7 (2 0)) (8 (3 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-inline-if ()
  "Test for inline if indentations."

  (phps-mode-test--with-buffer
   "<?php\nif (true)\n    echo 'Something';\nelse\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true)\n    echo 'Something';\nelse if (true)\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else if"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nwhile (true)\n    echo 'Something';"
   "Inline control structures while"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-alternative-if ()
  "Test for alternative if indentations."

  (phps-mode-test--with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (1 0)) (9 (0 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true\n):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 1"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (1 0)) (8 (0 0)) (9 (1 0)) (10 (1 0)) (11 (0 0)) (12 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (1 0)) (7 (0 0)) (8 (1 0)) (9 (1 0)) (10 (0 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-classes ()
  "Test for class indent."

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (2 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (2 0)) (6 (2 0)) (7 (3 0)) (8 (2 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nclass MyClass extends MyAbstract implements\n    myInterface,\n    myInterface2\n{\n}\n"
   "Class multi-line implements"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nclass MyClass\n    extends MyAbstract\n    implements myInterface, myInterface2\n{\n}\n"
   "Class multi-line extends and implements"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))


  (phps-mode-test--with-buffer
   "<?php\n/**\n *\n */\nnamespace Aomebo\n{\n    /**\n     *\n     */\n    class Base\n    {\n    }\n}\n"
   "Namespace and class with doc-comments"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1)) (4 (0 1)) (5 (0 0)) (6 (0 0)) (7 (1 0)) (8 (1 1)) (9 (1 1)) (10 (1 0)) (11 (1 0)) (12 (1 0)) (13 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-if ()
  "Test for multi-line if expressions."

  (phps-mode-test--with-buffer
   "<?php\nif (\n    true\n    && true\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 1"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (1 0)) (5 (0 0)) (6 (1 0)) (7 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\n// Can we load configuration?\nif ($configuration::load(\n    self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME)\n)) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 2"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (1 0)) (6 (1 0)) (7 (1 0)) (8 (0 0)) (9 (1 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true) {\n    if ($configuration::load(\n        self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME))\n    ) {\n        echo 'was here';\n    }\n}\n"
   "If expression spanning multiple lines 3"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (2 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (myFunction(true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 4"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 0)) (4 (1 0)) (5 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (myFunction(\n    true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 5"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true) {\n    if (myFunction(\n        true)\n    ) {\n        echo 'was here';\n    }\n}\n"
   "Nested if expression spanning multiple lines 6"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (2 0)) (7 (1 0)) (8 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\n        echo $title2;\n\n    } ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression and token-less lines"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (1 0)) (3 (2 0)) (4 (2 0)) (5 (1 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<html><head><title><?php\nif ($myCondition) {\n    if ($mySecondCondition) {\n        echo $title;\n    } else if ($mySecondCondition) {\n        echo $title4;\n    } else {\n        echo $title2;\n        echo $title3;\n    }\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression 2"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (2 0)) (7 (1 0)) (8 (2 0)) (9 (2 0)) (10 (1 0)) (11 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression indent calculation"
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-switch-case ()
  "Test for switch-case indentation."

  (phps-mode-test--with-buffer
   "<?php\nswitch ($condition) {\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\n}\n"
   "Switch, case, default"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nswitch ($condition):\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\nendswitch;\n"
   "Switch, case, default with alternative control structure"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (2 0)) (6 (1 0)) (7 (2 0)) (8 (1 0)) (9 (2 0)) (10 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true) {\n    switch ($condition):\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    endswitch;\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Alternative switch, case, default with exception after it"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (3 0)) (6 (3 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (3 0)) (11 (1 0)) (12 (1 0)) (13 (2 0)) (14 (2 0)) (15 (1 0)) (16 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  (phps-mode-test--with-buffer
   "<?php\nif (true) {\n    switch ($condition) {\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    }\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Curly switch, case, default with exception after it"
   ;; (message "Indent: %s" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (3 0)) (6 (3 0)) (7 (2 0)) (8 (3 0)) (9 (2 0)) (10 (3 0)) (11 (1 0)) (12 (1 0)) (13 (2 0)) (14 (2 0)) (15 (1 0)) (16 (0 0))) (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-lines-indent)))))

  )

(defun phps-mode-test-lex-analyzer--indent-line ()
  "Test for indentation."

  ;; Curly bracket tests
  (phps-mode-test--with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test"
   (goto-char 69)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test--with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title1;\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test 2"
   (goto-char 75)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n        echo $title1;\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test--with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test 3"
   (goto-char 98)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n    } ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test--with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"
   "Curly bracket test 4"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (goto-char 110)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n    </title><body>Bla bla</body></html>"))))

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n'random3'\n);\n$variable = true;\n"
   "Assignment test 1"
   (goto-char 28)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random3'\n);\n$variable = true;\n"))))

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n    'random2'\n    );\n$variable = true;\n"
   "Assignment test 2"
   (goto-char 43)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random2'\n);\n$variable = true;\n"))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 1"
   (goto-char 20)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n * My first line\n* My second line\n**/\n"))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 2"
   (goto-char 9)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n**/\n"))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 3"
   (goto-char 46)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n **/\n"))))

  (phps-mode-test--with-buffer
   "<?php\n$variable = array(\n  'random4');\n$variable = true;\n"
   "Round bracket test 1"
   (goto-char 30)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"))))

  (phps-mode-test--with-buffer
   "<?php\nadd_filter(\n\"views_{$screen->id}\",'__return_empty_array'\n);"
   "Round bracket test 2"
   (goto-char 25)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nadd_filter(\n    \"views_{$screen->id}\",'__return_empty_array'\n);"))))

  (phps-mode-test--with-buffer
   "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"
   "Round bracket test 3"
   (goto-char 36)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"))))

  (phps-mode-test--with-buffer
   "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"
   "Nested if-expression"
   (goto-char 54)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression"
   (goto-char 68)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"
   "Regular else if test"
   (goto-char 68)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"))))

  ;; Square bracket
  (phps-mode-test--with-buffer
   "<?php\n$var = [\n    'random' => [\n        'hello',\n],\n];\n"
   "Square bracket test 1"
   (goto-char 51)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = [\n    'random' => [\n        'hello',\n    ],\n];\n"))))
  
  (phps-mode-test--with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    else:\n    echo 'Something else here 8';\nendif;\n"
   "Alternative else test"
   (goto-char 62)
   (phps-mode-lex-analyzer--indent-line)
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\necho 'Something here';\nelse:\n    echo 'Something else here 8';\nendif;\n"))))

  (phps-mode-test--with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case indentation test"
   (goto-char 45)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()) {\n    case 'Something here':\necho 'Something else here';\n}\n"))))

  (phps-mode-test--with-buffer
   "<?php\nswitch (myRandomCondition()): \ncase 'Something here':\necho 'Something else here';\nendswitch;\n"
   "Alternative switch case indentation test 2"
   (goto-char 70)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()): \ncase 'Something here':\n        echo 'Something else here';\nendswitch;\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (myRandomCondition())\necho 'Something here';\necho 'Something else here';\n"
   "Inline control structure indentation"
   (goto-char 40)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (myRandomCondition())\n    echo 'Something here';\n    echo 'Something else here';\n"
   "Inline control structure indentation 2"
   (goto-char 60)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    echo 'Something else here';\nendif;\n"
   "Alternative control structure indentation 1"
   (goto-char 40)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\n    echo 'Something here';\n    echo 'Something else here';\nendif;\n"))))

  (phps-mode-test--with-buffer
   "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n        ),\n    $var5\n);\n"
   "Function arguments with associate array indentation"
   (goto-char 65)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n    ),\n    $var5\n);\n"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = $var2->getHead()\n->getTail();\n"
   "Multi-line assignment indentation test 1"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (goto-char 35)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = $var2->getHead()\n    ->getTail();\n"))))

  (phps-mode-test--with-buffer
   "<?php\n$var =\n'random string';\n"
   "Single-line assignment indentation test"
   (goto-char 20)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var =\n    'random string';\n"))))

  (phps-mode-test--with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   "Alternative control structure if expression"
   (goto-char 60)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\nendif;"))))

  (phps-mode-test--with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\nendif;"
   "Alternative control structure test"
   (goto-char 35)
   (phps-mode-lex-analyzer--indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n    $this->var = 'abc123';\nendif;"))))

  (phps-mode-test--with-buffer
   "<html>\n<head>\n<title><?php echo $title; ?></title>\n</head>\n<body>\n<div class=\"contents\"><?php echo $body; ?></div>\n</body>\n</html>"
   "A mixed HTML and PHP file, each PHP command is inside HTML markup"
   (indent-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<html>\n    <head>\n        <title><?php echo $title; ?></title>\n    </head>\n    <body>\n        <div class=\"contents\"><?php echo $body; ?></div>\n    </body>\n</html>"))))

  (phps-mode-test--with-buffer
   "<html>\n<head>\n<title><?php echo $title; ?></title>\n</head>\n<body class=\"<?php echo $class; ?>\">\n<div class=\"contents\"><?php echo $body; ?></div>\n</body>\n</html>"
   "A mixed HTML and PHP file, each PHP command is inside HTML markup, one PHP inside markup tag"
   (indent-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<html>\n    <head>\n        <title><?php echo $title; ?></title>\n    </head>\n    <body class=\"<?php echo $class; ?>\">\n        <div class=\"contents\"><?php echo $body; ?></div>\n    </body>\n</html>"))))

  (phps-mode-test--with-buffer
   "<html>\n    <head>\n        <title><?php $myTitle; ?></title>\n    </head>\n    <body>\n        <?php echo 'test'; ?>\n        <h1>My title</h1>\n        <?php if ($myTest): ?>\n        <div>\n            A lot of other stuff.\n        </div>\n        <?php endif; ?>\n    </body>\n</html>"
   "Indent mixed HTML and one-line PHP lines."
   (indent-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<html>\n    <head>\n        <title><?php $myTitle; ?></title>\n    </head>\n    <body>\n        <?php echo 'test'; ?>\n        <h1>My title</h1>\n        <?php if ($myTest): ?>\n        <div>\n            A lot of other stuff.\n        </div>\n        <?php endif; ?>\n    </body>\n</html>"))))

  (phps-mode-test--with-buffer
   "<?php\nif ($here) {\n    $var = \"abc $b[abc] def\";\n// Was here\n}\n\n"
   "Indentation after line with square brackets inside double quoted string"
   (indent-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($here) {\n    $var = \"abc $b[abc] def\";\n    // Was here\n}\n\n"))))

  (phps-mode-test--with-buffer
   "<?php\n\n// Adjust days to delivery accorind to document\nswitch ($dayOfWeek)\n{\n    case 1: // Monday\n    case 2: // Tuesday\n    case 3: // Wednesday\n    case 7: // Sunday\n        $daysToDelivery = 3;\n        break;\n    case 4: // Thursday\n    case 5: // Friday\n        $daysToDelivery = 5;\n        break;\n    case 6: // Saturday\n        $daysToDelivery = 4;\n        break;\n    default:\n        throw new \Exception(sprintf(\n            'day of week above interval (1-7): %d',\n            $dayOfWeek\n        ));\n}\n"
   "Switch case with default case and trailing comments"
   (indent-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n\n// Adjust days to delivery accorind to document\nswitch ($dayOfWeek)\n{\n    case 1: // Monday\n    case 2: // Tuesday\n    case 3: // Wednesday\n    case 7: // Sunday\n        $daysToDelivery = 3;\n        break;\n    case 4: // Thursday\n    case 5: // Friday\n        $daysToDelivery = 5;\n        break;\n    case 6: // Saturday\n        $daysToDelivery = 4;\n        break;\n    default:\n        throw new \Exception(sprintf(\n            'day of week above interval (1-7): %d',\n            $dayOfWeek\n        ));\n}\n"))))

  )

(defun phps-mode-test-lex-analyzer--imenu ()
  "Test for imenu."

  (phps-mode-test--with-buffer
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n$var = function () {\n    echo 'here';\n};"
   "Imenu function-oriented file with anonymous function"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myFunctionA" . 16) ("myFunctionB" . 42)))))

  (phps-mode-test--with-buffer
   "<?php\nfunction myFunctionA() {}\nfunction myFunctionB() {}\n"
   "Imenu function-oriented file"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myFunctionA" . 16) ("myFunctionB" . 42)))))

  (phps-mode-test--with-buffer
   "<?php\nclass myClass {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myClass" . (("myFunctionA" . 43) ("myFunctionB" . 83)))))))

  (phps-mode-test--with-buffer
   "<?php\ninterface myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file with interface"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myInterface" . (("myFunctionA" . 51) ("myFunctionB" . 91)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace {\n    class myClass {\n        public function myFunctionA() {}\n        protected function myFunctionB() {}\n    }\n}\n"
   "Imenu object-oriented file with namespace, class and function"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace" ("myClass" ("myFunctionA" . 75) ("myFunctionB" . 119)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class and function"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace" ("myClass" ("myFunctionA" . 66) ("myFunctionB" . 106)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace {\n    class myClass extends myAbstract {\n        public function myFunctionA() {}\n        protected function myFunctionB() {}\n    }\n}\n"
   "Imenu object-oriented file with namespace, class that extends and functions"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace" ("myClass" ("myFunctionA" . 94) ("myFunctionB" . 138)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA() {}\n    protected function myFunctionB() {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 148)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace\\myNamespace2;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Imenu object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myNamespace\\myNamespace2" ("myClass" ("myFunctionA" . 121) ("myFunctionB" . 174)))))))

  (phps-mode-test--with-buffer
   "<?php\nclass myClass\n{\n\n    public function myFunction1()\n    {\n        echo \"my string with variable {$variable} inside it\";\n    }\n\n    public function myFunction2()\n    {\n    }\n\n}"
   "Imenu with double quoted string with variable inside it"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("myClass" ("myFunction1" . 44) ("myFunction2" . 153))))))

  (phps-mode-test--with-buffer
   "<?php\n\nnamespace MyNamespace;\n\nclass MyClass\n{\n\n    /**\n     *\n     */\n    public function __construct()\n    {\n        if ($test) {\n        }\n    }\n\n    /**\n     *\n     */\n    public function myFunction1()\n    {\n        $this->addMessage(\"My random {$message} here\" . ($random > 1 ? \"A\" : \"\") . \" was here.\");\n    }\n    \n    /**\n     *\n     */\n    public function myFunction2()\n    {\n    }\n\n    /**\n     * It's good\n     */\n    public function myFunction3()\n    {\n    }\n\n    /**\n     *\n     */\n    public function myFunction4()\n    {\n    }\n}\n"
   "Imenu with double quoted string with variable inside it and concatenated string"
   (should (equal (phps-mode-lex-analyzer--get-imenu) '(("MyNamespace" ("MyClass" ("__construct" . 92) ("myFunction1" . 193) ("myFunction2" . 365) ("myFunction3" . 445) ("myFunction4" . 515)))))))

  (phps-mode-test--with-buffer
   "<?php\nclass myClass {}"
   "Imenu empty class"
   (should (equal (phps-mode-lex-analyzer--get-imenu) nil)))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace {}"
   "Imenu empty bracketed namespace"
   (should (equal (phps-mode-lex-analyzer--get-imenu) nil)))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;"
   "Imenu empty namespace without brackets"
   (should (equal (phps-mode-lex-analyzer--get-imenu) nil)))

  )

(defun phps-mode-test-lex-analyzer--get-moved-imenu ()
  "Test for moving imenu index."

  ;; (message "Moved imenu %s" (phps-mode-lex-analyzer--get-moved-imenu '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))) 0 2))

  (should (equal
           '(("myNamespace" ("myClass" ("myFunctionA" . 110) ("myFunctionB" . 163))))
           (phps-mode-lex-analyzer--get-moved-imenu '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))) 0 2)))

  (should (equal
           '(("myNamespace" ("myClass" ("myFunctionA" . 106) ("myFunctionB" . 159))))
           (phps-mode-lex-analyzer--get-moved-imenu '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))) 0 -2)))

  (should (equal
           '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 171))))
           (phps-mode-lex-analyzer--get-moved-imenu '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))) 110 10)))

  (should (equal
           '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161))))
           (phps-mode-lex-analyzer--get-moved-imenu '(("myNamespace" ("myClass" ("myFunctionA" . 108) ("myFunctionB" . 161)))) 180 10)))

  )

(defun phps-mode-test-lex-analyzer--comment-uncomment-region ()
  "Test (comment-region) and (uncomment-region)."

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Comment object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (comment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n/* namespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n} */\n"))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Comment part of object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (comment-region 62 86)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract /* implements myInterface */ {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"))))

  (phps-mode-test--with-buffer
   "// <?php\n// namespace myNamespace;\n// class myClass extends myAbstract implements myInterface {\n//    public function myFunctionA($myArg = null) {}\n//    protected function myFunctionB($myArg = 'abc') {}\n//}\n"
   "Uncomment object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (uncomment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "// <?php\n namespace myNamespace;\n class myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract/*  implements myInterface  */{\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"
   "Uncomment part of object-oriented file with bracket-less namespace, class that extends and implements and functions with optional arguments"
   (uncomment-region 62 92)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}\n"))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract/*  implements myInterface  */{\n    // public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}"
   "Comment region were some of the region is already commented-out"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (comment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n/* namespace myNamespace;\nclass myClass extends myAbstract *//*  implements myInterface  *//* { */\n    // public function myFunctionA($myArg = null) {}\n    /* protected function myFunctionB($myArg = 'abc') {}\n} */"))))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract/*  implements myInterface  */{\n    public function myFunctionA($myArg = null) {}\n    /* protected function myFunctionB($myArg = 'abc') {} */\n}"
   "Un-comment region were some of the region is already un-commented"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (uncomment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nnamespace myNamespace;\nclass myClass extends myAbstract implements myInterface {\n    public function myFunctionA($myArg = null) {}\n    protected function myFunctionB($myArg = 'abc') {}\n}"))))

  (phps-mode-test--with-buffer
   "<?php\n/**\n * My doc comment\n */\n$var = 'abc';\n"
   "Comment region were some of the region is in doc comment"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (comment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n/**\n * My doc comment\n */\n/* $var = 'abc'; */\n"))))

  (phps-mode-test--with-buffer
   "<?php\n/** $var = '123'; */\n$var = 'abc';\n"
   "Un-comment region were some of the region is already un-commented"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (uncomment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = '123';\n$var = 'abc';\n"))))

  )

(defun phps-mode-test-lex-analyzer--get-inline-html-indentation ()
  "Test function."

  (should (equal
           '(0 1 2 1 1 2 1 0)
           (nth 0 (phps-mode-lex-analyzer--get-inline-html-indentation
                   "<html>\n<head>\n<title>MyTitle</title>\n</head>\n<body>\n<p>My paragraph</p>\n</body>\n</html>"
                   0
                   0
                   0
                   0
                   0
                   ))))

  (should (equal
           '(2 2 1 0)
           (nth 0 (phps-mode-lex-analyzer--get-inline-html-indentation
                   "\n<p>My paragraph</p>\n</body>\n</html>"
                   2
                   2
                   0
                   0
                   0
                   ))))

  (should (equal
           '(0)
           (nth 0 (phps-mode-lex-analyzer--get-inline-html-indentation
                   "<html>"
                   0
                   0
                   0
                   0
                   0
                   ))))

  (should (equal
           '(0 1 2 1 0)
           (nth 0 (phps-mode-lex-analyzer--get-inline-html-indentation
                   "<script type=\"text/javascript\">\n    if (something()) {\n        alert('Something here');\n    }\n</script>\n"
                   0
                   0
                   0
                   0
                   0
                   ))))

  )


(defun phps-mode-test-lex-analyzer--bookkeeping ()
  "Test the bookkeeping."

  (phps-mode-test--with-buffer
   "<?php\n\n$var = 'abc';\n\nif ($var2) {\n    echo 'This never happens';\n}\nif ($var) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #1."
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list "*test* id $var" t) (list (list 8 12) t) (list (list 27 32) nil) (list (list 73 77) t)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$var = 'abc';\n\nif ($var) {\n    echo 'This never happens';\n}\nif ($var2) {\n    echo 'This happens';\n}"
   "Bookkeeping in root level variable assignments #2."
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list "*test* id $var" t) (list (list 8 12) t) (list (list 27 31) t) (list (list 72 77) nil)))))

  (phps-mode-test--with-buffer
   "<?php\n\n$var2 = 4;\n\nfunction myFunction($var)\n{\n    $var3 = 3\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Hit';\n    }\n}\n\nfunction myFunction2($abc)\n{\n    if ($var) {\n        echo 'Miss';\n    }\n    if ($abc) {\n        echo 'Hit';\n    }\n}\n\nif ($var) {\n    echo 'Miss';\n}\nif ($var2) {\n    echo 'Hit';\n}"
   "Bookkeeping in function level with variable assignments."
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list "*test* id $var2" t) (list (list 8 13) t) (list "*test* function myFunction id $var" t) (list (list 40 44) t) (list "*test* function myFunction id $var3" t) (list (list 52 57) t) (list (list 70 74) t) (list (list 112 117) nil) (list (list 156 161) t) (list "*test* function myFunction2 id $abc" t) (list (list 215 219) t) (list (list 231 235) nil) (list (list 274 278) t) (list (list 315 319) nil) (list (list 346 351) t)))))

  (phps-mode-test--with-buffer
   "<?php\n\nnamespace myNamespaceA {\n    $var = 123;\n    class myClassA {\n        private $var2 = 123;\n        function myFunctionA($var3) {\n            $var4 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Hit';\n            }\n            if ($var4) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var5)\n        {\n            $var6 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n}\n\nnamespace myNamespaceB {\n    $var7 = 123;\n    class myClassB {\n        private $var8 = 123;\n        function myFunctionA($var10) {\n            $var9 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Miss';\n            }\n            if ($var6) {\n                echo 'Miss';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Hit';\n            }\n            if ($var10) {\n                echo 'Hit';\n            }\n        }\n\n        function myFunctionB($var12)\n        {\n            $var11 = 123;\n            if ($var) {\n                echo 'Miss';\n            }\n            if ($var2) {\n                echo 'Miss';\n            }\n            if ($var3) {\n                echo 'Miss';\n            }\n            if ($var4) {\n                echo 'Miss';\n            }\n            if ($var5) {\n                echo 'Hit';\n            }\n            if ($var6) {\n                echo 'Hit';\n            }\n            if ($var7) {\n                echo 'Miss';\n            }\n            if ($var8) {\n                echo 'Miss';\n            }\n            if ($var9) {\n                echo 'Miss';\n            }\n            if ($var10) {\n                echo 'Miss';\n            }\n            if ($var11) {\n                echo 'Hit';\n            }\n            if ($var12) {\n                echo 'Hit';\n            }\n        }\n    }\n\n    if ($var) {\n        echo 'Hit';\n    }\n    if ($var2) {\n        echo 'Miss';\n    }\n    if ($var3) {\n        echo 'Miss';\n    }\n    if ($var4) {\n        echo 'Miss';\n    }\n    if ($var5) {\n        echo 'Miss';\n    }\n    if ($var6) {\n        echo 'Miss';\n    }\n    if ($var7) {\n        echo 'Hit';\n    }\n}\n"
   "Bookkeeping in maximum level with namespaces, classes and functions."
   ;; (message "Bookkeeping: '%s'" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t))
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list "*test* id $var" t) (list (list 37 41) t) (list (list 86 91) nil) (list "*test* namespace myNamespaceA class myClassA function myFunctionA id $var3" t) (list (list 128 133) t) (list "*test* namespace myNamespaceA class myClassA function myFunctionA id $var4" t) (list (list 149 154) t) (list (list 178 182) nil) (list (list 245 250) nil) (list (list 313 318) t) (list (list 380 385) t) (list "*test* namespace myNamespaceA class myClassA function myFunctionB id $var5" t) (list (list 471 476) t) (list "*test* namespace myNamespaceA class myClassA function myFunctionB id $var6" t) (list (list 500 505) t) (list (list 529 533) nil) (list (list 596 601) nil) (list (list 664 669) nil) (list (list 732 737) nil) (list (list 800 805) t) (list (list 867 872) t) (list (list 943 947) t) (list (list 985 990) nil) (list (list 1029 1034) nil) (list (list 1073 1078) nil) (list (list 1117 1122) nil) (list (list 1161 1166) nil) (list "*test* id $var7" t) (list (list 1229 1234) t) (list (list 1279 1284) nil) (list "*test* namespace myNamespaceB class myClassB function myFunctionA id $var10" t) (list (list 1321 1327) t) (list "*test* namespace myNamespaceB class myClassB function myFunctionA id $var9" t) (list (list 1343 1348) t) (list (list 1372 1376) nil) (list (list 1439 1444) nil) (list (list 1507 1512) nil) (list (list 1575 1580) nil) (list (list 1643 1648) nil) (list (list 1711 1716) nil) (list (list 1779 1784) nil) (list (list 1847 1852) nil) (list (list 1915 1920) t) (list (list 1982 1988) t) (list "*test* namespace myNamespaceB class myClassB function myFunctionB id $var12" t) (list (list 2074 2080) t) (list "*test* namespace myNamespaceB class myClassB function myFunctionB id $var11" t) (list (list 2104 2110) t) (list (list 2134 2138) nil) (list (list 2201 2206) nil) (list (list 2269 2274) nil) (list (list 2337 2342) nil) (list (list 2405 2410) nil) (list (list 2472 2477) nil) (list (list 2539 2544) nil) (list (list 2607 2612) nil) (list (list 2675 2680) nil) (list (list 2743 2749) nil) (list (list 2812 2818) t) (list (list 2880 2886) t) (list (list 2957 2961) t) (list (list 2999 3004) nil) (list (list 3043 3048) nil) (list (list 3087 3092) nil) (list (list 3131 3136) nil) (list (list 3175 3180) nil) (list (list 3219 3224) t)))))

  (phps-mode-test--with-buffer
   "<?php\n\n// Super-globals\n\nif ($_GET) {\n    echo 'Hit';\n}\nif ($_POST) {\n    echo 'Hit';\n}\nif ($_COOKIE) {\n    echo 'Hit';\n}\nif ($_SESSION) {\n    echo 'Hit';\n}\nif ($_REQUEST) {\n    echo 'Hit';\n}\nif ($_GLOBALS) {\n    echo 'Hit';\n}\nif ($_SERVER) {\n    echo 'Hit';\n}\n"
   "Super-globals"
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list (list 30 35) t) (list (list 61 67) t) (list (list 93 101) t) (list (list 127 136) t) (list (list 162 171) t) (list (list 197 206) t) (list (list 232 240) t)))))

  (phps-mode-test--with-buffer
   "<?php\n\n// Class properties\n\nclass myParent {}\n\nclass myClass extends myParent {\n    private $var1 = 123;\n    protected static $var2;\n    public $var3;\n    var $var4;\n    function __construct() {\n        if ($this) {\n            echo 'Hit';\n        }\n        if ($this->var1) {\n            echo 'Hit';\n        }\n        if (self::$var2) {\n            echo 'Hit';\n        }\n        if ($this->var3) {\n            echo 'Hit';\n        }\n        if ($this->var4) {\n            echo 'Hit';\n        }\n        if ($this->var5) {\n            echo 'Miss';\n        }\n        if (parent) {\n            echo 'Hit';\n        }\n    }\n}\n\nif ($this) {\n    echo 'Miss';\n}\nif (self) {\n    echo 'Miss';\n}\nif (parent) {\n    echo 'Miss';\n}"
   "Class properties"
   ;; (message "Bookkeeping: '%s'" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t))
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list (list "*test* class myClass id $var1" t) (list 93 98) t) (list "*test* class myClass id $var2" t) (list (list 127 132) t) (list "*test* class myClass id $var3" t) (list (list 145 150) t) (list "*test* class myClass id $var4" t) (list (list 160 165) t) (list (list 208 213) t) (list (list 263 268) t) (list (list 330 335) t) (list (list 385 390) t) (list (list 446 451) t) (list (list 507 512) t) (list (list 626 631) nil)))))

  (phps-mode-test--with-buffer
   "<?php\n\n// Conditional assignments\n\n$items = array(1, 2, 3);\nforeach ($items as $item) {\n    if ($item) {\n        echo 'Hit';\n    }\n}\nforeach ($items as $key => $value) {\n    if ($key || $value) {\n        echo 'Hit';\n    }\n}\nfor ($i = 0; $i < count($items); $i++) {\n    if ($i) {\n        echo 'Hit';\n    }\n}\nif ($a = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\nwhile ($b = 123) {\n    if ($a) {\n        echo 'Hit';\n    }\n}\ndo {\n    echo 'Hit';\n} while ($c = 456);\n"
   "Conditional assignments"
   (message "Bookkeeping: '%s'" (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t))
   (should (equal
            (phps-mode-test--hash-to-list (phps-mode-lex-analyzer--get-bookkeeping) t)
            (list (list "$_COOKIE" t) (list "$_GET" t) (list "$_GLOBALS" t) (list "$_POST" t) (list "$_REQUEST" t) (list "$_SERVER" t) (list "$_SESSION" t) (list "*test* id $items" t) (list (list 36 42) t) (list (list 70 76) t) (list "*test* id $item" t) (list (list 80 85) t) (list (list 97 102) t) (list (list 143 149) t) (list "*test* id $key" t) (list (list 153 157) t) (list "*test* id $value" t) (list (list 161 167) t) (list (list 179 183) t) (list (list 187 193) t) (list "*test* id $i" t) (list (list 230 232) t) (list (list 238 240) t) (list (list 249 255) t) (list (list 258 260) nil) (list (list 274 276) t) (list "*test* id $a" t) (list (list 312 314) t) (list (list 332 334) t) (list "*test* id $b" t) (list (list 373 375) t) (list (list 393 395) t) (list "*test* id $c" t) (list (list 457 459) t)))))

  )

(defun phps-mode-test-lex-analyzer ()
  "Run test for functions."
  ;; (setq debug-on-error t)
  (phps-mode-test-lex-analyzer--bookkeeping)
  (phps-mode-test-lex-analyzer--process-changes)
  (phps-mode-test-lex-analyzer--alternative-indentation)
  (phps-mode-test-lex-analyzer--move-lines-indent)
  (phps-mode-test-lex-analyzer--get-inline-html-indentation)
  (phps-mode-test-lex-analyzer--get-lines-indent-if)
  (phps-mode-test-lex-analyzer--get-lines-indent-classes)
  (phps-mode-test-lex-analyzer--get-lines-indent-inline-if)
  (phps-mode-test-lex-analyzer--get-lines-indent-alternative-if)
  (phps-mode-test-lex-analyzer--get-lines-indent-multi-line-assignments)
  (phps-mode-test-lex-analyzer--get-lines-indent-switch-case)
  (phps-mode-test-lex-analyzer--get-lines-indent-psr-2)
  (phps-mode-test-lex-analyzer--get-lines-indent)
  (phps-mode-test-lex-analyzer--indent-line)
  (phps-mode-test-lex-analyzer--imenu)
  (phps-mode-test-lex-analyzer--get-moved-imenu)
  (phps-mode-test-lex-analyzer--comment-uncomment-region)
  (phps-mode-test-lex-analyzer--move-lines-indent))

(phps-mode-test-lex-analyzer)

(provide 'phps-mode-test-lex-analyzer)

;;; phps-mode-test-lex-analyzer.el ends here
