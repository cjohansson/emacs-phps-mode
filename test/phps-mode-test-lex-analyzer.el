;;; phps-mode-test-lex-analyzer.el --- Tests for functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.

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
   "Multi-line if statement testing equality in two lines"
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif ($shippingMethod->id ===\n    \\MyClass::METHOD_ID\n) {\n    "
              ))))

  (phps-mode-test--with-buffer
   ""
   "Multi-line if block after opening parenthesis"
   (execute-kbd-macro "<?php")
   (execute-kbd-macro (kbd "<return>"))
   (execute-kbd-macro "if (true) {")
   (execute-kbd-macro (kbd "<return>"))
   (execute-kbd-macro "if (")
   (execute-kbd-macro (kbd "<return>"))
   (let ((buffer-contents
          (buffer-substring-no-properties
           (point-min)
           (point-max))))
     (should (equal
              buffer-contents
              "<?php\nif (true) {\n    if (\n        \n    )\n}"
              ))))

  )

(defun phps-mode-test-lex-analyzer--indent-should-equal (string name)
  "Test indent of whole buffer containing STRING with NAME."
  (phps-mode-test--with-buffer
   string
   name
   (phps-mode-test-lex-analyzer--alternative-indentation-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal
              buffer-contents
              string)))))

(defun phps-mode-test-lex-analyzer--get-lines-indent ()
  "Test indent function."
  
  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n * Bla\n */"
   "DOC-COMMENT")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nmyFunction(array(\n    23,\n    [\n        25\n    ]\n    )\n);"
   "Round and square bracket expressions")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nvar_dump(array(<<<EOD\nfoobar!\nEOD\n));\n?>"
   "HEREDOC in arguments example")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$str = <<<'EOD'\nExample of string\nspanning multiple lines\nusing nowdoc syntax.\nEOD;\n"
   "Multi-line NOWDOC string")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var = \"A line\nmore text here\nlast line here\";"
   "Multi-line double-quoted string")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var = 'A line\nmore text here\nlast line here';"
   "Multi-line single-quoted string")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho \"A line\" .\n    \"more text here\" .\n    \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho \"A line\"\n    . \"more text here\"\n    . \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho myFunction('A line' .\n    'more text here' .\n    'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho 'A line'\n    . 'more text here'\n    . 'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho myFunction('A line'\n    . 'more text here'\n    . 'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string outside assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n * @var string\n */\necho 'was here';\n"
   "Statement after doc-comment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/** @define _SYSTEM_START_TIME_     Startup time for system */\ndefine('_SYSTEM_START_TIME_', microtime(true));\necho 'statement';\n"
   "Statement after a define() with a doc-comment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nfunction myFunction($parameters = null)\n{\n    echo 'statement';\n}\n"
   "Statement after one-lined function declaration with optional argument")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php if (true) { ?>\n    <?php echo 'here'; ?>\n<?php } ?>"
   "Regular if-expression but inside scripting tags")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated assignment string with function call")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated assignment string with function call")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated echo string with function call")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated echo string with function call")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$options = [\n    0 => [\n        'label' => __('No'),\n        'value' => 0,\n    ],\n];"
   "Assignment with square bracketed array")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$options = array(\n    'blaha' .\n        'blaha',\n    123,\n    'blaha'\n);"
   "Assignment with square bracketed array")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nreturn $variable\n    && $variable;"
   "Multi-line return statement")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$options = myFunction(\n    array(array(\n        'options' => 123\n    ))\n);"
   "Assignment with double-dimensional array with double arrow assignment inside function call")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch ($condition) {\n    case 34:\n        if ($item['Random'] % 10 == 0) {\n            $attributes['item'] = ($item['IntegerValue'] / 10);\n        } else {\n            $attributes['item'] =\n                number_format(($item['IntegerValue'] / 10), 1, '.', '');\n        }\n        break;\n}\n"
   "Switch case with conditional modulo expression")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$options = array(\n    'options' => array(array(\n        'errorTo'\n    ))\n);"
   "Assignment with three-dimensional array with double arrow assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif ($myCondition) {\n    $myObject->myMethod(myClass::class)\n        ->myMethod2($myArgument2);\n    }"
   "Object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$myObj->myFunction()\n    ->mySecondaryFunction();"
   "Indentation of chained class method calls outside of assignments and conditionals")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n\n$myVar = $myClass->meMethod()\n    ->mySecondMethod()\n    ->myThirdMethod()\n->myFourthFunction(\n    $myVariable\n);"
   "Indentation for chained object operators in assignment with method call with arguments")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n\n$myResult = !empty($myVar->myMethod3)\n    && $myVar->myMethod\n        && $myVar->myMethod2;\n"
   "Indentation for chained object operators in assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$array = [\n    'second' => [\n        'hello' => true\n        ]\n];\n\n$array = array(\n    'second' => array(\n        'third' => true\n        )\n);"
   "Indent multi-dimensional arrays without trailing commas")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html>\n    <head>\n        <?php echo $title; ?>\n    </head>\n    <body>\n    <?php\n\n    if ($myTest) {\n        doSomething();\n    }\n\n    ?>\n    </body>\n</html>"
   "A mixed HTML and PHP file.")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n\n    if ($fullInfo) $fullInfo = unserialize ($fullInfo);\n    else array();\n\n"
   "Indentation for single-line inline control structures.")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n\nif (true) {\n    // Was here\n}"
   "If condition after a mixed newline encoded file")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-psr-2 ()
  "Test PSR-2 examples from: https://www.php-fig.org/psr/psr-2/."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooInterface;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass Foo extends Bar implements FooInterface\n{\n    public function sampleMethod($a, $b = null)\n    {\n        if ($a === $b) {\n            bar();\n        } elseif ($a > $b) {\n            $foo->bar($arg1);\n        } else {\n            BazClass::bar($arg2, $arg3);\n        }\n    }\n\n    final public static function bar()\n    {\n        // method body\n    }\n}\n"
   "PSR-2 : 1.1. Example")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\n// ... additional PHP code ..."
   "PSR-2 : 3. Namespace and Use Declarations")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements \\ArrayAccess, \\Countable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements\n    \\ArrayAccess,\n    \\Countable,\n    \\Serializable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public $foo = null;\n}"
   "PSR-2 : 4.2. Properties")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function fooBarBaz($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.3. Methods")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function foo($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function aVeryLongMethodName(\n        ClassTypeHint $arg1,\n        &$arg2,\n        array $arg3 = []\n    ) {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace Vendor\\Package;\n\nabstract class ClassName\n{\n    protected static $foo;\n\n    abstract protected function zim();\n\n    final public static function bar()\n    {\n        // method body\n    }\n}"
   "PSR-2 ; 4.5. abstract, final, and static")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nbar();\n$foo->bar($arg1);\nFoo::bar($arg2, $arg3);"
   "PSR-2 : 4.6. Method and Function Calls : Example 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$foo->bar(\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n);"
   "PSR-2 : 4.6. Method and Function Calls : Example 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif ($expr1) {\n    // if body\n} elseif ($expr2) {\n    // elseif body\n} else {\n    // else body;\n}"
   "PSR-2 : 5.1. if, elseif, else")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch ($expr) {\n    case 0:\n        echo 'First case, with a break';\n        break;\n    case 1:\n        echo 'Second case, which falls through';\n        // no break\n    case 2:\n    case 3:\n    case 4:\n        echo 'Third case, return instead of break';\n        return;\n    default:\n        echo 'Default case';\n        break;\n}"
   "PSR-2 : 5.2. switch, case")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nwhile ($expr) {\n    // structure body\n}"
   "PSR-2 : 5.3. while, do while : Example 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\ndo {\n    // structure body;\n} while ($expr);"
   "PSR-2 : 5.3. while, do while : Example 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nfor ($i = 0; $i < 10; $i++) {\n    // for body\n}"
   "PSR-2 : 5.4. for")
  
  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nforeach ($iterable as $key => $value) {\n    // foreach body\n}"
   "PSR-2 : 5.5. foreach")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\ntry {\n    // try body\n} catch (FirstExceptionType $e) {\n    // catch body\n} catch (OtherExceptionType $e) {\n    // catch body\n}"
   "PSR-2 : 5.6. try, catch")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$closureWithArgs = function ($arg1, $arg2) {\n    // body\n};\n\n$closureWithArgsAndVars = function ($arg1, $arg2) use ($var1, $var2) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$longArgs_noVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) {\n    // body\n};\n\n$noArgs_longVars = function () use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_longVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_shortVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use ($var1) {\n    // body\n};\n\n$shortArgs_longVars = function ($arg) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$foo->bar(\n    $arg1,\n    function ($arg2) use ($var1) {\n        // body\n    },\n    $arg3\n);"
   "PSR-2 : 6. Closures : Example 3")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-multi-line-assignments ()
  "Test for multi-line assignments."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n    'random4'\n);\n$variable = true;\n"
   "Array assignment on three lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n    'random4' =>\n        'hello'\n);"
   "Array assignment with double arrow elements on four lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"
   "Array assignment on two lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var = 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string multiple-lines in assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var .=\n    'A line';"
   "Concatenated equal single-quoted-string on multiple-lines in assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var *=\n    25;"
   "Multiplication equal assignment on multiple-lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax.\nEOD;\n"
   "Multi-line HEREDOC string in assignment")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var =\n    500 .\n    \"200\" .\n    100.0 .\n    '200' .\n    $this->getTail()\n    ->getBottom();"
   "Multi-line assignments")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-inline-if ()
  "Test for inline if indentations."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true)\n    echo 'Something';\nelse\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true)\n    echo 'Something';\nelse if (true)\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else if")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nwhile (true)\n    echo 'Something';"
   "Inline control structures while")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-alternative-if ()
  "Test for alternative if indentations."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true):\n    echo 'Something';\nelseif (true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true\n):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true):\n    echo 'Something';\nelseif (true\n    && true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures with multi-line elseif 2")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-classes ()
  "Test for class indent."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nnamespace myNamespace\n{\n    class myClass {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nclass MyClass extends MyAbstract implements\n    myInterface,\n    myInterface2\n{\n}\n"
   "Class multi-line implements")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nclass MyClass\n    extends MyAbstract\n    implements myInterface, myInterface2\n{\n}\n"
   "Class multi-line extends and implements")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n *\n */\nnamespace Aomebo\n{\n    /**\n     *\n     */\n    class Base\n    {\n    }\n}\n"
   "Namespace and class with doc-comments")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-if ()
  "Test for multi-line if expressions."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (\n    true\n    && true\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n// Can we load configuration?\nif ($configuration::load(\n    self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME)\n)) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true) {\n    if ($configuration::load(\n        self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME))\n    ) {\n        echo 'was here';\n    }\n}\n"
   "If expression spanning multiple lines 3")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myFunction(true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 4")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myFunction(\n    true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 5")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true) {\n    if (myFunction(\n        true)\n    ) {\n        echo 'was here';\n    }\n}\n"
   "Nested if expression spanning multiple lines 6")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\n        echo $title2;\n\n    } ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression and token-less lines")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html><head><title><?php\nif ($myCondition) {\n    if ($mySecondCondition) {\n        echo $title;\n    } else if ($mySecondCondition) {\n        echo $title4;\n    } else {\n        echo $title2;\n        echo $title3;\n    }\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression indent calculation")

  )

(defun phps-mode-test-lex-analyzer--get-lines-indent-switch-case ()
  "Test for switch-case indentation."

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch ($condition) {\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\n}\n"
   "Switch, case, default")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch ($condition):\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\nendswitch;\n"
   "Switch, case, default with alternative control structure")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true) {\n    switch ($condition):\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    endswitch;\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Alternative switch, case, default with exception after it")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (true) {\n    switch ($condition) {\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    }\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Curly switch, case, default with exception after it")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$product_path = \"${filename[0]}/${filename[1]}/\";\necho 'here';\n"
   "Double-quoted string with multiple indexed variables in it")

  )

(defun phps-mode-test-lex-analyzer--indent-line ()
  "Test for indentation."

  ;; Curly bracket tests
  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n    echo $title;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n} ?></title><body>Bla bla</body></html>"
   "Curly bracket test 3")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"
   "Curly bracket test 4")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n'random3'\n);\n$variable = true;\n"
   "Assignment test 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n    'random2'\n    );\n$variable = true;\n"
   "Assignment test 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   "Doc-comment test 3")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$variable = array(\n  'random4');\n$variable = true;\n"
   "Round bracket test 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nadd_filter(\n\"views_{$screen->id}\",'__return_empty_array'\n);"
   "Round bracket test 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"
   "Round bracket test 3")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"
   "Nested if-expression")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"
   "Regular else if test")

  ;; Square bracket
  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var = [\n    'random' => [\n        'hello',\n],\n];\n"
   "Square bracket test 1")
  
  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    else:\n    echo 'Something else here 8';\nendif;\n"
   "Alternative else test")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   "Switch case indentation test")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nswitch (myRandomCondition()): \ncase 'Something here':\necho 'Something else here';\nendswitch;\n"
   "Alternative switch case indentation test 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myRandomCondition())\necho 'Something here';\necho 'Something else here';\n"
   "Inline control structure indentation")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myRandomCondition())\n    echo 'Something here';\n    echo 'Something else here';\n"
   "Inline control structure indentation 2")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    echo 'Something else here';\nendif;\n"
   "Alternative control structure indentation 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n        ),\n    $var5\n);\n"
   "Function arguments with associate array indentation")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var = $var2->getHead()\n->getTail();\n"
   "Multi-line assignment indentation test 1")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n$var =\n'random string';\n"
   "Single-line assignment indentation test")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   "Alternative control structure if expression")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\nendif;"
   "Alternative control structure test")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html>\n<head>\n<title><?php echo $title; ?></title>\n</head>\n<body>\n<div class=\"contents\"><?php echo $body; ?></div>\n</body>\n</html>"
   "A mixed HTML and PHP file, each PHP command is inside HTML markup")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html>\n<head>\n<title><?php echo $title; ?></title>\n</head>\n<body class=\"<?php echo $class; ?>\">\n<div class=\"contents\"><?php echo $body; ?></div>\n</body>\n</html>"
   "A mixed HTML and PHP file, each PHP command is inside HTML markup, one PHP inside markup tag")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<html>\n    <head>\n        <title><?php $myTitle; ?></title>\n    </head>\n    <body>\n        <?php echo 'test'; ?>\n        <h1>My title</h1>\n        <?php if ($myTest): ?>\n        <div>\n            A lot of other stuff.\n        </div>\n        <?php endif; ?>\n    </body>\n</html>"
   "Indent mixed HTML and one-line PHP lines.")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\nif ($here) {\n    $var = \"abc $b[abc] def\";\n// Was here\n}\n\n"
   "Indentation after line with square brackets inside double quoted string")

  (phps-mode-test-lex-analyzer--indent-should-equal
   "<?php\n\n// Adjust days to delivery accorind to document\nswitch ($dayOfWeek)\n{\n    case 1: // Monday\n    case 2: // Tuesday\n    case 3: // Wednesday\n    case 7: // Sunday\n        $daysToDelivery = 3;\n        break;\n    case 4: // Thursday\n    case 5: // Friday\n        $daysToDelivery = 5;\n        break;\n    case 6: // Saturday\n        $daysToDelivery = 4;\n        break;\n    default:\n        throw new \Exception(sprintf(\n            'day of week above interval (1-7): %d',\n            $dayOfWeek\n        ));\n}\n"
   "Switch case with default case and trailing comments")

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
   "Un-comment region were some of the region is already un-commented 1"
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
   "Un-comment region were some of the region is already un-commented 2"
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (uncomment-region (point-min) (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = '123';\n$var = 'abc';\n"))))

  (phps-mode-test--with-buffer
   "<?php\n$var1 = '123';"
   "Comment region after changes has been made to buffer"
   (goto-char 19)
   (insert " def")
   (comment-region 7 (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n/* $var1 = '123 def'; */"))))

  (phps-mode-test--with-buffer
   "<?php\n/* $var1 = '123'; */"
   "Un-comment region after changes has been made to buffer"
   (goto-char 22)
   (insert " def")
   (uncomment-region 7 (point-max))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var1 = '123 def';"))))

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

(defun phps-mode-test-lex-analyzer--parse-string ()
  "Test the parser."

  (should
   (equal
    '(80 459 466 411 333 332 154 102 79)
    (car (car (cdr (phps-mode-lex-analyzer--lex-string
                    "<?php echo 'abc';"))))))
  (message "Passed valid parse test")

  (should-error
   (phps-mode-lex-analyzer--lex-string
    "<?php echo 'abc'"))
  (message "Passed error parse test")
  )

(defun phps-mode-test-lex-analyzer ()
  "Run test for functions."
  ;; (setq debug-on-error t)
  (phps-mode-test-lex-analyzer--process-changes)
  (phps-mode-test-lex-analyzer--alternative-indentation)
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
  (phps-mode-test-lex-analyzer--get-moved-imenu)
  (phps-mode-test-lex-analyzer--comment-uncomment-region))

(phps-mode-test-lex-analyzer)

(provide 'phps-mode-test-lex-analyzer)

;;; phps-mode-test-lex-analyzer.el ends here
