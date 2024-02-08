;;; phps-mode-test-indent.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:


;; Run from terminal make test-indent


;;; Code:


(require 'ert)
(require 'phps-mode)
(require 'phps-mode-indent)
(require 'phps-mode-test)

(defun phps-mode-test-indent--indent-whole-buffer ()
  "Use alternative indentation of every line of buffer."
  (goto-char (point-min))
  (execute-kbd-macro (kbd "TAB"))
  (while (search-forward "\n" nil t nil)
    ;; Go to random point on line
    (let* ((line-min-position (line-beginning-position))
           (line-max-position (line-end-position))
           (line-diff (- line-max-position line-min-position)))
      (if (> line-diff 0)
        (goto-char
         (+
          line-min-position
          (random line-diff)))
        (goto-char line-min-position)))
    (execute-kbd-macro (kbd "TAB"))))

(defun phps-mode-test-indent--should-equal (string name &optional new-string)
  "Test indent of whole buffer containing
STRING with NAME with optional NEW-STRING."
  (phps-mode-test--with-buffer
   string
   name
   (message "Initial buffer:\n%S" string)
   (phps-mode-test-indent--indent-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
         (test-string string))
     (when new-string
       (setq
        test-string
        new-string))
     (message "\nIndented buffer:\n%S" buffer-contents)
     (should (equal
              buffer-contents
              test-string)))))

(defun phps-mode-test-indent--helpers ()
  "Test helper functions."

  (should
   (string=
    (phps-mode-indent--string-ends-with-regexp "	  a" "a")
    "a"))

  (should
   (equal
    (phps-mode-indent--string-ends-with-regexp "	  a" "b")
    nil))

  (should
   (string=
    (phps-mode-indent--string-starts-with-regexp "if (something) {" "if ")
    "if "))

  (should
   (equal
    (phps-mode-indent--string-starts-with-regexp "if (something) {" "if  ")
    nil))

  (should
   (equal
    (phps-mode-indent--string-starts-with-closing-bracket "<?php   }")
    "}"))

  (should
   (equal
    (phps-mode-indent--string-starts-with-closing-bracket "    ]")
    "]"))

  (should
   (equal
    (phps-mode-indent--string-starts-with-closing-bracket "    )")
    ")"))

  (should
   (equal
    (phps-mode-indent--string-starts-with-closing-bracket "    ,)")
    nil))

  (with-temp-buffer
    (insert "<?php\nmyFunction(\n    'element 1',\n    'element 2',\n);")
    (goto-char 45)
    (should
     (string=
      (phps-mode-indent--get-previous-reference-index-line)
      "    'element 1',")))
  (message "Passed reference index line 1")

  (with-temp-buffer
    (insert "<?php\nthrow new Exception(\n    sprintf(\n        self::systemTranslate(\n            'Invalid parameters for Application. Parameters: \"%s\".'\n        ),\n        print_r(self::$_parameters, true)\n    )\n);\n")
    (goto-char 144)
    (should
     (string=
      (phps-mode-indent--get-previous-reference-index-line)
      "        self::systemTranslate(")))

  (with-temp-buffer
    (insert "<?php\necho 'command 1';\necho 'command 2';")
    (goto-char (1- (point-max)))
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "echo 'command 1';")))

  (with-temp-buffer
    (insert "<?php\n$var = 'command'\n    . '1';\necho 'command 2';")
    (goto-char (1- (point-max)))
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "$var = 'command'")))

  (with-temp-buffer
    (insert "<?php\nif(true):\n    echo 'here';\nelse:\n    echo 'Something else';\n    echo 'Something else again';")
    (goto-char (1- (point-max)))
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "    echo 'Something else';")))

  (with-temp-buffer
    (insert "<?php\nif(true):\n    echo 'here';\nelse:\n    echo 'Something else';\n    echo 'Something else again';")
    (goto-char (1- (point-max)))
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "    echo 'Something else';")))

  (with-temp-buffer
    (insert "<?php\n\nif (true) {\n    switch ($var):\n        case true:\n            echo 'here';\n    endswitch;\n    echo 'was here';\n}")
    (goto-char 105)
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "    endswitch;")))

  (with-temp-buffer
    (insert "<?php\nif (!defined('VARIABLE')) {\n    exit;\n}\n\nrequire_once(CONSTANT . 'path');\n$variable = myFunction1(\n    'argument1',\n    'argument2'\n);\n")
    (goto-char 92)
    (should
     (string=
      (phps-mode-indent--get-previous-reference-command-line)
      "require_once(CONSTANT . 'path');")))

    (with-temp-buffer
      (insert "<?php\n    $variable =\n        Object::\n        method($variable2, true);\n        // Line comment")
      (goto-char (point-max))
      (should
       (string=
        (phps-mode-indent--get-previous-reference-command-line)
        "    $variable =")))

    (with-temp-buffer
      (insert "<?php\n\nif (true) {\n    $valid = true;\n    $variable = myFunction();\n    switch ($variable) {\n        case Object::CASE1:\n            throw new Exception(\n                'MyException'\n            );\n        case Object::Case2:\n            throw new \\Exception(\n                'MyException2',\n            );\n        case Object::Case3:\n            $valid = false;\n            break;\n        case Object::Case4:\n            if (!Object2::validate($variable)) {\n                $valid = false;\n            }\n            break;\n        case Object::Case5:\n            $valid = false;\n            break;\n        case Object::Case6:\n            $valid = true;\n        break;\n    }\n}\n")
      (goto-char 380)
      (should
       (string=
        (phps-mode-indent--get-previous-reference-command-line)
        "            $valid = false;")))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    array(\n        8,\n    );")
    (goto-char (point-max))
    (should
     (string=
      (phps-mode-indent--get-previous-start-of-bracket-line)
      "    array(")))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    array(\n        [array(\n            8,\n            16,\n        )]\n    );")
    (goto-char (point-max))
    (should
     (string=
      (phps-mode-indent--get-previous-start-of-bracket-line)
      "    array(")))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    array(\n        [array(\n            8,\n            16,\n        )]\n    );")
    (goto-char 87)
    (should
     (string=
      (phps-mode-indent--get-previous-start-of-bracket-line)
      "        [array(")))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    array(\n        [array(\n            8,\n            16,\n            32)]\n    );")
    (goto-char 89)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-bracket-line)
      nil)))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    array(\n        [array(\n            8,\n            16,\n            32)]\n    );")
    (goto-char 89)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-bracket-line t)
      "        [array(")))

  (with-temp-buffer
    (insert "<?php\narray(2, 3)")
    (goto-char (point-max))
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-bracket-line t)
      nil)))

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $table = $installer->getConnection()\n        ->newTable($installer->getTable('my_table'))\n        ->addColumn();\n}\n")
    (goto-char 127)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "        ->newTable($installer->getTable('my_table'))")))

  (message "Passed chaining test #1")

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $table = $installer->getConnection()\n        ->newTable($installer->getTable('my_table'))\n        ->addColumn();\n}\n")
    (goto-char 73)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      nil)))

  (message "Passed chaining test #2")

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $criteria = $this->searchCriteriaBuilder\n        ->addFilter('status', $status)\n        ->addFilter(method', 'my_method_' . $object->getId())\n        ->create();\n}\n")
    (goto-char 177)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "        ->addFilter('status', $status)")))

  (message "Passed chaining test #3")

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $criteria = $this->searchCriteriaBuilder\n        ->addFilter('status', $status)\n        ->addFilter(method', 'my_method_' . $object->getId())\n        ->create();\n}\n")
    (goto-char 135)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "        ->addFilter('status', $status)")))

  (message "Passed chaining test #4")

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $criteria = $this->searchCriteriaBuilder\n        ->addFilter('status', $status)\n        ->addFilter(method', 'my_method_' . $object->getId())\n        ->create();\n}\n")
    (goto-char 96)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      nil)))

  (message "Passed chaining test #5")

  (with-temp-buffer
    (insert "<?php\nif (true) {\n    $criteria = $this->searchCriteriaBuilder\n        ->addFilter('status', $status)\n        ->addFilter(method', 'my_method_' . $object->getId())\n        ->create();\n}\n")
    (goto-char 51)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      nil)))

  (message "Passed chaining test #6")

  (with-temp-buffer
    (insert "<?php\n$var = 500 .\n    \"200\" .\n    100.0 .\n    '200' .\n    $this->getTail()\n        ->getBottom();")
    (goto-char 90)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "    $this->getTail()")))

  (message "Passed chaining test #7")

  (with-temp-buffer
    (insert "<?php\nif ($myCondition) {\n    $myObject->myMethod(myClass::class)\n        ->myMethod2($myArgument2)\n        ->myMethod3($myArgument3);\n}")
    (goto-char 121)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "        ->myMethod2($myArgument2)")))

  (message "Passed chaining test #8")

  (with-temp-buffer
    (insert "<?php\nif ($myCondition) {\n    $myObject->myMethod(myClass::class)\n        ->myMethod2($myArgument2)\n        ->myMethod3($myArgument3);\n}")
    (goto-char 85)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      nil)))

  (message "Passed chaining test #9")

  (with-temp-buffer
    (insert "<?php\n$myObject->myMethod(myClass::class)\n    ->myMethod2($myArgument2)\n    ->myMethod3($myArgument3);\n")
    (goto-char 55)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      nil)))

  (message "Passed chaining test #10")

  (with-temp-buffer
    (insert "<?php\n\nif ($you) {\n    if ($here) {\n        if ($true) {\n            if ($a = 23) {\n                $myObject->build(array(\n                    'name' => 'Trueman',\n                    'sku' => 25.0\n                ))\n                ->test()\n                    ->validate()\n                        ->give();\n            }\n        }\n    }\n}\n")
    (goto-char 272)
    (should
     (equal
      (phps-mode-indent--get-previous-start-of-chaining)
      "                ->test()")))

  (message "Passed chaining test #11")

  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<!DOCTYPE html>")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<html>")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "</html>")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<body>")
    1))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "</body>")
    -1))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<meta>")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<meta />")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<button>")
    1))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "<button />")
    nil))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "</button>")
    -1))
  (should
   (equal
    (phps-mode-indent--get-html-string-bracket-level "</p>")
    -1))

  (message "Passed tests for indentation helper functions"))

(defun phps-mode-test-indent--get-lines-indent ()
  "Test indent function."
  
  (phps-mode-test-indent--should-equal
   "<?php\n/**\n * Bla\n */"
   "DOC-COMMENT")

  (phps-mode-test-indent--should-equal
   "<?php\nmyFunction(array(\n    23,\n    [\n        25\n    ]\n));"
   "Round and square bracket expressions")

  (phps-mode-test-indent--should-equal
   "<?php\nvar_dump(array(<<<EOD\nfoobar!\nEOD\n));\n?>"
   "HEREDOC in arguments example")

  (phps-mode-test-indent--should-equal
   "<?php\n$str = <<<'EOD'\nExample of string\nspanning multiple lines\nusing nowdoc syntax\nEOD;\n"
   "Multi-line NOWDOC string")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = \"A line\n    more text here\n    last line here\";"
   "Multi-line double-quoted string")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line\n    more text here\n    last line here';"
   "Multi-line single-quoted string")

  (phps-mode-test-indent--should-equal
   "<?php\necho \"A line\" .\n    \"more text here\" .\n    \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines")

  (phps-mode-test-indent--should-equal
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function")

  (phps-mode-test-indent--should-equal
   "<?php\necho \"A line\"\n    . \"more text here\"\n    . \"last line here\";"
   "Concatenated double-quoted-string spanning multiple-lines 2")

  (phps-mode-test-indent--should-equal
   "<?php\necho myFunction(\"A line\" .\n    \"more text here\" .\n    \"last line here\");"
   "Concatenated double-quoted-string spanning multiple-lines inside function 2")

  (phps-mode-test-indent--should-equal
   "<?php\necho 'A line' .\n    'more text here' .\n    'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines")

  (phps-mode-test-indent--should-equal
   "<?php\necho myFunction('A line' .\n    'more text here' .\n    'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function")

  (phps-mode-test-indent--should-equal
   "<?php\necho 'A line'\n    . 'more text here'\n    . 'last line here';"
   "Concatenated single-quoted-string spanning multiple-lines 2")

  (phps-mode-test-indent--should-equal
   "<?php\necho myFunction('A line'\n    . 'more text here'\n    . 'last line here');"
   "Concatenated single-quoted-string spanning multiple-lines inside function 2")

  (phps-mode-test-indent--should-equal
   "<?php\necho <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax\nEOD;\n"
   "Multi-line HEREDOC string outside assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n/**\n * @var string\n */\necho 'was here';\n"
   "Statement after doc-comment")

  (phps-mode-test-indent--should-equal
   "<?php\n/** @define _SYSTEM_START_TIME_     Startup time for system */\ndefine('_SYSTEM_START_TIME_', microtime(true));\necho 'statement';\n"
   "Statement after a define() with a doc-comment")

  (phps-mode-test-indent--should-equal
   "<?php\nfunction myFunction($parameters = null)\n{\n    echo 'statement';\n}\n"
   "Statement after one-lined function declaration with optional argument")

  (phps-mode-test-indent--should-equal
   "<?php if (true) { ?>\n    <?php echo 'here'; ?>\n<?php } ?>"
   "Regular if-expression but inside scripting tags")

  (phps-mode-test-indent--should-equal
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition")

  (phps-mode-test-indent--should-equal
   "<?php\ndo {\n    echo 'true';\n} while ($number > 0\n    && $letter > 0\n);"
   "Do while loop with multi-line condition")

  (phps-mode-test-indent--should-equal
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated assignment string with function call")

  (phps-mode-test-indent--should-equal
   "<?php\n$myVar = 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated assignment string with function call")

  (phps-mode-test-indent--should-equal
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    ) . 'okeoke';\n?>"
   "Concatenated echo string with function call")

  (phps-mode-test-indent--should-equal
   "<?php\necho 'blaha'\n    . 'ijeije' . __(\n        'okeoke'\n    )\n    . 'okeoke';\n?>"
   "Concatenated echo string with function call")

  (phps-mode-test-indent--should-equal
   "<?php\n$options = [\n    0 => [\n        'label' => __('No'),\n        'value' => 0,\n    ],\n];"
   "Assignment with square bracketed array")

  (phps-mode-test-indent--should-equal
   "<?php\n$options = array(\n    'blaha' .\n    'blaha',\n    123,\n    'blaha'\n);"
   "Assignment with square bracketed array")

  (phps-mode-test-indent--should-equal
   "<?php\nreturn $variable\n    && $variable;"
   "Multi-line return statement")

  (phps-mode-test-indent--should-equal
   "<?php\n$options = myFunction(\n    array(array(\n        'options' => 123\n    ))\n);"
   "Assignment with double-dimensional array with double arrow assignment inside function call")

  (phps-mode-test-indent--should-equal
   "<?php\n// die('debug: ' . $debug\nif ($debug) {\n    \n}\n"
   "Line after commented out opening bracket line")

  (phps-mode-test-indent--should-equal
   "<?php\nswitch ($condition) {\n    case 34:\n        if ($item['Random'] % 10 == 0) {\n            $attributes['item'] = ($item['IntegerValue'] / 10);\n        } else {\n            $attributes['item'] =\n                number_format(($item['IntegerValue'] / 10), 1, '.', '');\n        }\n        break;\n}\n"
   "Switch case with conditional modulo expression")

  (phps-mode-test-indent--should-equal
   "<?php\n$options = array(\n    'options' => array(array(\n        'errorTo'\n    ))\n);"
   "Assignment with three-dimensional array with double arrow assignment")

  (phps-mode-test-indent--should-equal
   "<?php\nif ($myCondition) {\n    $myObject->myMethod(myClass::class)\n        ->myMethod2($myArgument2);\n}"
   "Object-oriented file with bracket-less namespace with multiple levels, class that extends and implements and functions with optional arguments")

  (phps-mode-test-indent--should-equal
   "<?php\n$myObj->myFunction()\n    ->mySecondaryFunction();"
   "Indentation of chained class method calls outside of assignments and conditionals")

  (phps-mode-test-indent--should-equal
   "<?php\n\n$myVar = $myClass->meMethod()\n    ->mySecondMethod()\n    ->myThirdMethod()\n    ->myFourthFunction(\n        $myVariable\n    );"
   "Indentation for chained object operators in assignment with method call with arguments")

  (phps-mode-test-indent--should-equal
   "<?php\n\n$myResult = !empty($myVar->myMethod3)\n    && $myVar->myMethod\n    && $myVar->myMethod2;\necho 'here';"
   "Indentation for chained object operators in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n$array = [\n    'second' => [\n        'hello' => true\n    ]\n];\n\n$array = array(\n    'second' => array(\n        'third' => true\n    )\n);"
   "Indent multi-dimensional arrays without trailing commas")

  (phps-mode-test-indent--should-equal
   "<html>\n<head>\n<?php echo $title; ?>\n</head>\n<body>\n<?php\n\nif ($myTest) {\n    doSomething();\n}\n\n?>\n</body>\n</html>"
   "A mixed HTML and PHP file.")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif ($fullInfo) $fullInfo = unserialize ($fullInfo);\nelse array();\n\n"
   "Indentation for single-line inline control structures.")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (true) {\n    // Was here\n}"
   "If condition after a mixed newline encoded file")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif ($you) {\n    if ($here) {\n        if ($true) {\n            if ($a = 23) {\n                $myObject->build(array(\n                    'name' => 'Trueman',\n                    'sku' => 25.0\n                ))\n                ->test()\n                ->validate()\n                ->give();\n            }\n        }\n    }\n}\n"
   "Tested chaining of object")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif ($there) {\n    if ($something) {\n        $var = [\n            [\n                [\n                    'abc' => 1,\n                    'def' => 2,\n                ]\n            ]\n        ];\n    }\n}\n"
   "Nested array with square bracket syntax")

  (phps-mode-test-indent--should-equal
   "<?php\n\nclass MyClass\n{\n    function myFunction1()\n    {\n        return tester(\n            '123');\n    }\n    function myFunction2()\n    {\n        return (count(self::$stuff) > 0 ?\n            self::$stuff : false);\n    }\n}\n"
   "Return statements in class")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = myFunction(\n    'setting');\necho 'here';\n"
   "Multi-line assignment from function ending without opening bracket")

  (phps-mode-test-indent--should-equal
   "<?php\n\nclass MyClass\n{\n    function myFunction()\n    {\n        return 'Text'\n            . 'here';\n    }\n}\n"
   "Multi-line return statement of concatenated string")

  (phps-mode-test-indent--should-equal
   "<?php\n\nclass MyClass\n{\n    function myFunction()\n    {\n        return aFunction(\n            'values');\n    }\n}\n"
   "Multi-line return expression ending at line with starting close bracket")

  (phps-mode-test-indent--should-equal
   "<?php\n$packageItems[] = [\n    'customs_value' =>\n        $orderItem->getPrice(),\n    'name' =>\n        $orderItem->getName(),\n    'order_item_id' =>\n        $orderItem->getItemId(),\n    'price' =>\n        $orderItem->getPrice(),\n    'product_id' =>\n        $orderItem->getProductId(),\n    'qty' =>\n        $orderItem->getQtyToShip(),\n    'weight' =>\n        $orderItem->getRowWeight(),\n];"
   "Assignment of square bracket array with element values from object methods")

  (phps-mode-test-indent--should-equal
   "<?php\n$productId1 = $this->generateProduct([\n    'country_of_origin' =>\n        'SE',\n    'hs_code' =>\n        '0000 1234 5679 0000',\n    'name' =>\n        'Simple Product # 1',\n    'price' =>\n        7,\n    'weight' =>\n        1.2,\n]);\n"
   "Assignment from object method values with square bracket argument on multiple lines")

  (phps-mode-test-indent--should-equal
   "<?php\nforeach ($order->getAllItems() as $orderItem) {\n    // Check if order item has qty to ship or is virtual\n    if (\n        !$orderItem->getQtyToShip()\n        || $orderItem->getIsVirtual()\n    ) {\n        continue;\n    }\n}"
   "Multi-line if expression checking object method values")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    throw new Exception(\n        sprintf(\n            self::systemTranslate(\n                'Invalid parameters for Application. Parameters: \"%s\".'\n            ),\n            print_r(self::$_parameters, true)\n        )\n    );\n}\n"
   "Throwing exception on multiple lines using sprintf")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (false) {\n    if (true) {\n        $conversion = myFunction(\n            $weight,\n            $weightUnit,\n            $toWeightUnit\n        );\n    }\n}"
   "Nested multi-line assignment from function call")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = [\n    1,\n    2,\n    3\n];\n"
   "Square bracket array definition in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n[\n    1,\n    [\n        2,\n        [\n            3,\n            4\n        ]\n    ],\n    [\n        5,\n        6,\n    ],\n    7,\n];\n"
   "Multi-dimensional square bracket array definition")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = [\n    1,\n    [\n        2,\n        [\n            3,\n            4\n        ]\n    ],\n    [\n        5,\n        6,\n    ],\n    7,\n];\n"
   "Multi-dimensional square bracket array definition in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\narray(\n    1,\n    array(\n        2,\n        array(\n            3,\n            4\n        )\n    ),\n    array(\n        5,\n        6,\n    ),\n    7,\n);\n"
   "Multi-dimensional round bracket array definition")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = array(\n    1,\n    array(\n        2,\n        array(\n            3,\n            4\n        )\n    ),\n    array(\n        5,\n        6,\n    ),\n    7,\n);\n"
   "Multi-dimensional round bracket array definition in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\nmyFunctionA(\n    1,\n    myFunctionB(\n        2,\n        myFunctionC(\n            3,\n            4\n        )\n    ),\n    myFunctionD(\n        5,\n        6\n    ),\n    7\n);\n"
   "Multi-dimensional function calls")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = myFunctionA(\n    1,\n    myFunctionB(\n        2,\n        myFunctionC(\n            3,\n            4\n        )\n    ),\n    myFunctionD(\n        5,\n        6\n    ),\n    7\n);\n"
   "Multi-dimensional function calls in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n/** @define string _PRIVATE_ROOT_           Absolute root to private */\ndefine('_PRIVATE_ROOT_',\n    dirname($parameters[self::PARAMETER_SITE_PATH]) . DIRECTORY_SEPARATOR);\n"
   "Mutiline define statement")

  (phps-mode-test-indent--should-equal
   "<?php\nif ($useRuntimeCache\n    && self::isWritingnabled()\n    && \\Aomebo\\Cache\\System::cacheExists(\n        $cacheParameters,\n        $cacheKey,\n        \\Aomebo\\Cache\\System::CACHE_STORAGE_LOCATION_FILESYSTEM)\n) {    \n}"
   "Multiline if condition")

  (phps-mode-test-indent--should-equal
   "<?php\nif ($data = \\Aomebo\\Cache\\System::loadCache(\n    $cacheParameters,\n    $cacheKey,\n    \\Aomebo\\Cache\\System::FORMAT_SERIALIZE,\n    \\Aomebo\\Cache\\System::CACHE_STORAGE_LOCATION_FILESYSTEM)\n) {    \n}"
   "Multiline if-condition with assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n\nThrow new \\Exception(\n    sprintf(\n        self::systemTranslate(\n            'Something went wrong when including file \"%s\", error: \"%s\".'\n        ),\n        $tryPath,\n        $e->getMessage()\n    )\n);\n"
   "Multiline exception throwing")

  (phps-mode-test-indent--should-equal
   "<?php\n/**\n * @param string $key\n * @param bool [$reloadFromFilesystem = false]\n * @return mixed\n */\nfunction getApplicationData($key,\n    $reloadFromFilesystem = false)\n{\n    if (!empty($key)) {\n        if (!empty($reloadFromFilesystem)) {\n            self::loadApplicationData();\n        }\n        if (isset(self::$_applicationData[$key])) {\n            return self::$_applicationData[$key];\n        }\n    }\n    return null;\n}\n"
   "Function definition not in PSR-2 style")

  (phps-mode-test-indent--should-equal
   "<?php\nself::addAutoLoadPaths(array(\n    _SYSTEM_ROOT_,\n    _PRIVATE_ROOT_,\n    _PUBLIC_ROOT_,\n    _SITE_ROOT_\n));\n"
   "Multiline function call with multiline array as argument")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $random = self::getData();\n    // My random comment\n}\n"
   "Comment after assignment from method call on same line")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    // My comment\n    $cacheKey = sprintf(\n        'key_%s',\n        md5(json_encode($key))\n    );\n    $cache =\n        Cache::getInstance();\n}"
   "Line after assignment from multi-line function-call")

  (phps-mode-test-indent--should-equal
   "<?php\nif (\n    $responseStatusCode === 200\n    || $responseStatusCode === 400\n) {\n    \n}\n"
   "Multi-line conditions with OR operators")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    foreach ($explode as $exploded) {\n        if ($i == 0) {\n            $lastName = trim($exploded);\n        } elseif ($i == $lastIndex) {\n            $lastQuantity = (int) $exploded;\n            $matches[] = array(\n                'name' => $lastName,\n                'quantity' => $lastQuantity\n            );\n        }\n    }\n}\n"
   "Closing bracket on new line after ending statement / expression with closing bracket")

  (phps-mode-test-indent--should-equal
   "<?php\nclass MyClass\n{\n    public function log(\n        $message,\n        $level = _INFO_\n    ) {\n    }\n}"
   "Opening method body after argument with default value")

  (phps-mode-test-indent--should-equal
   "<?php\nclass MyClass\n{\n    public function log(\n        $message,\n        $level = _INFO_,\n        $rate = _BASE_RATE_\n    ) {\n    }\n}"
   "Opening method body after 2 arguments with default values")

  (phps-mode-test-indent--should-equal
   "<?php\nclass MyClass\n{\n    public function getOperators()\n    {\n        return array(\n            '' => __(\n                'None',\n                'domain'\n            ),\n            '-' => __(\n                'Subtraction',\n                'domain'\n            ),\n            '+' => __(\n                'Addition',\n                'domain'\n            ),\n        );\n    }\n}\n"
   "Method that returns multi-line array")

  (phps-mode-test-indent--should-equal
   "<?php\nfunction myFunction()\n{\n    if (!isset($randomize)) {\n        if (true) {\n            throw new \Exception(sprintf(\n                __(\n                    'Library not found at %s',\n                    'domain'\n                ),\n                $path\n            ));\n        }\n    }\n    return false;\n}\n"
   "Multi-line throw statement")

  (phps-mode-test-indent--should-equal
   "<?php\n\nforeach ($datas as $data) {\n    if (\n        stripos(\n            $data,\n            ' , '\n        ) !== false\n    ) {\n        $explode = explode(\n            ' , ',\n            $data\n        );\n        if (\n            !empty($explode)\n            && is_array($explode)\n            && isset(\n                $explode[0],\n                $explode[1]\n            )\n        ) {\n            $name = trim($explode[1]);\n        }\n    }\n}\n"
   "Line after multi-line function call inside if condition list")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (\n    (!isset(Meta()->login)\n        || !$name = Meta()->login->name())\n    && $override\n) {\n    echo 'here';\n}\n"
   "Line after multi-line condition in parenthesis")

  (phps-mode-test-indent--should-equal
   "<?php\nif (\n    $first != false\n    || $second != false\n    || $third != false\n) {\n}\n"
   "Line after variable not equal condition")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (\n    (is_array($data)\n        && !empty($data['index'])\n        && (is_a($data['index'], 'Index')\n            || is_a($data['Index'], 'Index2')))\n    || is_a($data, 'WC_Index')\n) {\n    \n}\n"
   "Line after multi-line condition in parenthesis 2")

  (phps-mode-test-indent--should-equal
   "<?php\nfunction myFunction()\n{\n    return self::getOption('username')\n        && self::getOption('password');\n}\n"
   "Line after line that started a multi-line return expression")

  (phps-mode-test-indent--should-equal
   "<?php\n$debug = ($debug1\n    && ($debug2 || $debug3)\n    && ($debug4 || $debug5));\n\n// die('debug: ' . $debug);\nif ($debug) {\n}\n"
   "Line after a commented die statement after a multi-line logical assignment to variable")

  (phps-mode-test-indent--should-equal
   "<?php\n$debug = ($debug1\n    && ($debug2 || $debug3)\n    && ($debug4 || $debug5));\n\ndie('debug: ' . $debug);\nif ($debug) {\n}\n"
   "Line after a die statement after a multi-line logical assignment to variable")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $copies = method_exists($object, 'get_copies')\n        ? $object->get_copies()\n        : $object->copies;\n    $vol = ($object->get_vol()\n        ? $object->get_vol()\n        : $object->vol);\n}\n"
   "Line after ending a ternary assignment block")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $myFlag = $this->settings['my_flag']\n        && $this->settings['my_flag'] == 'yes';\n}\n"
   "Multi-line boolean assignment")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $replacements = array(\n        '$object_address_1' =>\n            $object->get_shipping_address_1(),\n        '$object_address_2' =>\n            $object->get_shipping_address_2(),\n        '$object_name' =>\n            $object->get_shipping_first_name()\n            . ' ' . $object->get_shipping_last_name(),\n        '$object_city' =>\n            $object->get_shipping_city(),\n        '$object_company' =>\n            $object->get_shipping_company(),\n        '$object_country' =>\n            $object->get_shipping_country(),\n        '$object_postcode' =>\n            $object->get_shipping_postcode(),\n        '$object_state' =>\n            $object->get_shipping_state(),\n        '$object_email' =>\n            $object->get_billing_email(),\n        '$object_mobile' =>\n            $object->get_billing_phone(),\n        '$object_id' =>\n            $object->ID,\n        '$object_number' =>\n            $object->get_order_number(),\n        '$object_subtotal' =>\n            $object->get_subtotal()\n    );\n}\n"
   "Multi-dimensional array with assignment from object methods")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $true = (true\n        && (true || false)\n        && (true || false));\n    echo 'here';\n}"
   "Line after multi-line parenthesized logical expression")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    echo 'here';\n/* something */\n    echo 'there';\n}\n"
   "Line after commented out lines with wrong indentation"
   "<?php\nif (true) {\n    echo 'here';\n    /* something */\n    echo 'there';\n}\n")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $variable1 = (true\n        ? true\n        : false);\n    \n    $variable2 = (true\n        ? true\n        : false);\n    \n    $variable3 = myFunction(true);\n    echo 'here';\n    \n}\n"
   "Line after multi-line parenthesized logical expression in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    return [\n        'data' => Object\n            ::myMethod($data)\n            ->myMethod2($data),\n    ];\n}\n"
   "Chaining object operators and double-colon on multiple lines")

  (phps-mode-test-indent--should-equal
   "<?php\nif ($random) {\n    if ($random) {\n        if ($random) {\n        }\n    }\n    apply_filters(\n        'my_filter',\n        $random\n    );\n    return $random;\n}\n"
   "Line after line that ends a multi-line function call")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (!defined('VARIABLE')) {\n    exit;\n}\n\nrequire_once(CONSTANT . 'path');\n$variable = myFunction1(\n    'argument1',\n    'argument2'\n);\n\nif (is_logged_in()) {\n    $variable = myFunction2(\n        'argument1',\n        'argument2'\n    );\n    require_once(CONSTANT . 'string');\n}\n"
   "Mixed expressions and statements")

  (phps-mode-test-indent--should-equal
   "<?php\n\n$array = [\n    'enabled' => (true\n        && false)\n        || true\n        || false,\n];\n"
   "Multi-line logical expression in associative array")

  (phps-mode-test-indent--should-equal
   "<?php\n\n$array = [\n    'pointers' => (!empty($data['point1'])\n        && $data['point2'] === 22)\n        || (!empty($data['point3'])\n            && $data['point4'] === 33)\n        || (!empty($data['point4'])\n            && $data['point4'] === 44),\n    'arrows' =>\n        $data['arrows'],\n];\n"
   "Another multi-line logical expression inside associative array")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $variable =\n        Object::\n        method($variable2, true);\n    // Line comment\n    $variable['index'] = $variabl2->method2();\n}"
   "Mix of various types of statements and expressions")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $html .= '<dt>'\n        . __(\n            'Text',\n            'namespace'\n        )\n        . ':</dt><dd>'\n        . '<input type=\"hidden\" name=\"my_name['\n        . $variable . ']\" value=\"' . esc($myName) . '\" />'\n        . '<select class=\"my-class\" name=\"my_name['\n        . $variable2 . ']\">';\n    echo 'here';\n}\n"
   "Multi-line echo statement with HTML markup 1")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    echo '<script type=\"text/javascript\">'\n        . 'jQuery(document).ready(function() {'\n        . 'jQuery(\"<option>\").val(\"my_value\").text(\"'\n        . __(\"Was here\", 'namespace')\n        . '\").appendTo(\"select[name=\\'key\\']\");'\n        . 'jQuery(\"<option>\").val(\"action\").text(\"'\n        . __(\"My action\", 'namespace')\n        . '\").appendTo(\"select[name=\\'anotherAction\\']\");'\n        . 'jQuery(\"<option>\").val(\"my_value2\").text(\"'\n        . __(\"My other action\", 'namespace')\n        . '\").appendTo(\"select[name=\\'anotherAction2\\']\");'\n        . 'jQuery(\"<option>\").val(\"my_value3\").text(\"'\n        . __(\"My third action\", 'namespace')\n        . '\").appendTo(\"select[name=\\'anotherAction3\\']\");'\n        . '\"\"});</script>';\n    echo 'here';\n}"
   "Multi-line echo statement with HTML markup 2")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    echo '<script type=\"text/javascript\">'\n        . 'jQuery(document).ready(function() { '\n        . 'window.open(\"'\n        . $url . '\", \"_blank\");'\n        . ' });</script>';\n}"
   "Multi-line echo statement with HTML markup 3")

  (phps-mode-test-indent--should-equal
   "<!DOCTYPE html>\n<html>\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Was here</title>\n    <meta charset=\"ISO-8559-1\" />\n</head>\n<body>\n    <div>\n        <p>\n            My mixed content\n            <br>\n            Was here\n        </p>\n    </div>\n</body>\n</html>"
   "Plain HTML markup")

  (phps-mode-test-indent--should-equal
   "<?php\n\nif (true) {\n    if (\n        true\n    ) {\n        return false;\n    }\n    \n    /**\n     * My first line,\n     * my second line.\n     *\n     * @since Module 1.0.0\n     */\n    echo 'here';\n    \n}\n"
   "Doc-comment ending with comma and dot.")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    if (\n        (!isset(myClass()->property)\n            || !$variable = myClass()->property2->method())\n        && false\n    ) {\n    }\n}\n"
   "Parenthesized condition on multi-rows")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    require_once(myClass()->method1()\n        . '/hard-coded-file.php');\n}\n"
   "Require once expression on multiple rows")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $html .= 'My text'\n        . 'and more text';\n    foreach ($data as $key => $fields) {\n        $html .= '<strong>' . $key . '</strong>';\n        foreach ($fields as $key => $value) {\n            $html .= '<span>' . $value . '</span>';\n        }\n        $html .= '</ul></li>';\n    }\n    $html .= '</dd>';\n}\n"
   "Two nested foreach loops containing string concatenation assignments")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 23; /* 23 = Company */\necho 'was here';"
   "Line after line that have an assignment doc-commented out")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 23; // 23 = Company\necho 'was here';"
   "Line after line that have an assignment commented out")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = Object->myMethod()\n    ->myMethod2();\necho 'here';"
   "Line after assignment were an chaining of object methods started")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $table = $installer->getConnection()\n        ->newTable($installer->getTable('my_table'))\n        ->addColumn();\n}\n"
   "Variable assignment with chained method calls on multiple lines #1")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    $criteria = $this->searchCriteriaBuilder\n        ->addFilter('status', $status)\n        ->addFilter('method', 'my_method_' . $object->getId())\n        ->create();\n}\n"
   "Variable assignment with chained method calls on multiple lines #2")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    /*\n    was here\n    */\n    echo 'there';\n}"
   "Line after closing multi-row comment")

  (phps-mode-test-indent--should-equal
   "<?php\nenum Suit\n{\n    case Hearts;\n    case Diamonds;\n    case Clubs;\n    case Spades;\n}"
   "Basic Enumeration")

  (phps-mode-test-indent--should-equal
   "<?php\nswitch($case) {\n    case 1;\n        echo 'here';\n}\n"
   "Switch case statement with semicolon")

  (phps-mode-test-indent--should-equal
   "<?php\nfunction myFunction(\n    $arg1,\n    $arg2\n) {\n}\nmyFunction(\n    arg1: $var1,\n    arg2: $var2,\n);\nmyFunction(\n    arg1:\n        $var1,\n    arg2:\n        $var2,\n);\n"
   "Multi-line function call with named arguments")

  )

(defun phps-mode-test-indent--get-lines-indent-psr-2 ()
  "Test PSR-2 examples from: https://www.php-fig.org/psr/psr-2/."

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooInterface;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass Foo extends Bar implements FooInterface\n{\n    public function sampleMethod($a, $b = null)\n    {\n        if ($a === $b) {\n            bar();\n        } elseif ($a > $b) {\n            $foo->bar($arg1);\n        } else {\n            BazClass::bar($arg2, $arg3);\n        }\n    }\n    \n    final public static function bar()\n    {\n        // method body\n    }\n}\n"
   "PSR-2 : 1.1. Example")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\n// ... additional PHP code ..."
   "PSR-2 : 3. Namespace and Use Declarations")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements \\ArrayAccess, \\Countable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 1")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nuse FooClass;\nuse BarClass as Bar;\nuse OtherVendor\\OtherPackage\\BazClass;\n\nclass ClassName extends ParentClass implements\n    \\ArrayAccess,\n    \\Countable,\n    \\Serializable\n{\n    // constants, properties, methods\n}"
   "PSR-2 : 4.1. Extends and Implements : Example 2")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public $foo = null;\n}"
   "PSR-2 : 4.2. Properties")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function fooBarBaz($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.3. Methods")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function foo($arg1, &$arg2, $arg3 = [])\n    {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 1")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nclass ClassName\n{\n    public function aVeryLongMethodName(\n        ClassTypeHint $arg1,\n        &$arg2,\n        array $arg3 = []\n    ) {\n        // method body\n    }\n}"
   "PSR-2 : 4.4. Method Arguments : Example 2")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace Vendor\\Package;\n\nabstract class ClassName\n{\n    protected static $foo;\n    \n    abstract protected function zim();\n    \n    final public static function bar()\n    {\n        // method body\n    }\n}"
   "PSR-2 ; 4.5. abstract, final, and static")

  (phps-mode-test-indent--should-equal
   "<?php\nbar();\n$foo->bar($arg1);\nFoo::bar($arg2, $arg3);"
   "PSR-2 : 4.6. Method and Function Calls : Example 1")

  (phps-mode-test-indent--should-equal
   "<?php\n$foo->bar(\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n);"
   "PSR-2 : 4.6. Method and Function Calls : Example 2")

  (phps-mode-test-indent--should-equal
   "<?php\nif ($expr1) {\n    // if body\n} elseif ($expr2) {\n    // elseif body\n} else {\n    // else body;\n}"
   "PSR-2 : 5.1. if, elseif, else")

  (phps-mode-test-indent--should-equal
   "<?php\nswitch ($expr) {\n    case 0:\n        echo 'First case, with a break';\n        break;\n    case 1:\n        echo 'Second case, which falls through';\n        // no break\n    case 2:\n    case 3:\n    case 4:\n        echo 'Third case, return instead of break';\n        return;\n    default:\n        echo 'Default case';\n        break;\n}"
   "PSR-2 : 5.2. switch, case")

  (phps-mode-test-indent--should-equal
   "<?php\nwhile ($expr) {\n    // structure body\n}"
   "PSR-2 : 5.3. while, do while : Example 1")

  (phps-mode-test-indent--should-equal
   "<?php\ndo {\n    // structure body;\n} while ($expr);"
   "PSR-2 : 5.3. while, do while : Example 2")

  (phps-mode-test-indent--should-equal
   "<?php\nfor ($i = 0; $i < 10; $i++) {\n    // for body\n}"
   "PSR-2 : 5.4. for")
  
  (phps-mode-test-indent--should-equal
   "<?php\nforeach ($iterable as $key => $value) {\n    // foreach body\n}"
   "PSR-2 : 5.5. foreach")

  (phps-mode-test-indent--should-equal
   "<?php\ntry {\n    // try body\n} catch (FirstExceptionType $e) {\n    // catch body\n} catch (OtherExceptionType $e) {\n    // catch body\n}"
   "PSR-2 : 5.6. try, catch")

  (phps-mode-test-indent--should-equal
   "<?php\n$closureWithArgs = function ($arg1, $arg2) {\n    // body\n};\n\n$closureWithArgsAndVars = function ($arg1, $arg2) use ($var1, $var2) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 1")

  (phps-mode-test-indent--should-equal
   "<?php\n$longArgs_noVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) {\n    // body\n};\n\n$noArgs_longVars = function () use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_longVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};\n\n$longArgs_shortVars = function (\n    $longArgument,\n    $longerArgument,\n    $muchLongerArgument\n) use ($var1) {\n    // body\n};\n\n$shortArgs_longVars = function ($arg) use (\n    $longVar1,\n    $longerVar2,\n    $muchLongerVar3\n) {\n    // body\n};"
   "PSR-2 : 6. Closures : Example 2")

  (phps-mode-test-indent--should-equal
   "<?php\n$foo->bar(\n    $arg1,\n    function ($arg2) use ($var1) {\n        // body\n    },\n    $arg3\n);"
   "PSR-2 : 6. Closures : Example 3")

  )

(defun phps-mode-test-indent--get-lines-indent-multi-line-assignments ()
  "Test for multi-line assignments."

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = array(\n    'random4'\n);\n$variable = true;\n"
   "Array assignment on three lines without trailing comma")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = array(\n    'random4',\n);\n$variable = true;\n"
   "Array assignment on three lines with trailing comma")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = [\n    'random4'\n];\n$variable = true;\n"
   "Array assignment on three lines without trailing comma #2")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = [\n    'random4',\n];\n$variable = true;\n"
   "Array assignment on three lines with trailing comma #2")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = array(\n    'random4' =>\n        'hello'\n);"
   "Array assignment with double arrow elements on four lines without trailing comma")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = array(\n    'random4' =>\n        'hello',\n);"
   "Array assignment with double arrow elements on four lines with trailing comma")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = [\n    'random4' =>\n        'hello'\n];"
   "Array assignment with double arrow elements on four lines without trailing comma #2")

  (phps-mode-test-indent--should-equal
   "<?php\n$variable = [\n    'random4' =>\n        'hello',\n];"
   "Array assignment with double arrow elements on four lines with trailing comma #2")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' \n    . 'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' .\n    'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment #2")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' \n    . 'more text here'\n    . 'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment #3")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' .\n    'more text here' .\n    'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment #4")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' \n    . 'more text here'\n    . 'even more text'\n    . 'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment #5")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 'A line' .\n    'more text here' .\n    'even more text' .\n    'last line here';\necho 'was here';"
   "Concatenated single-quoted-string multiple-lines in assignment #6")

  (phps-mode-test-indent--should-equal
   "<?php\n$var .=\n    'A line';\necho 'was here';"
   "Concatenated equal single-quoted-string on multiple-lines in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n$var *=\n    25;\necho 'was here';"
   "Multiplication equal assignment on two-lines")

  (phps-mode-test-indent--should-equal
   "<?php\n$var =\n    25\n    * 35;\necho 'was here';"
   "Multiplication assignment on three-lines")

  (phps-mode-test-indent--should-equal
   "<?php\n$str = <<<EOD\nExample of string\nspanning multiple lines\nusing heredoc syntax\nEOD;\n"
   "Multi-line HEREDOC string in assignment")

  (phps-mode-test-indent--should-equal
   "<?php\n$var = 500 .\n    \"200\" .\n    100.0 .\n    '200' .\n    $this->getTail()\n    ->getBottom();"
   "Multi-line assignments with string concatenation and object-method chaining")

  )

(defun phps-mode-test-indent--get-lines-indent-inline-control-structures ()
  "Test for inline control structures."

  (phps-mode-test-indent--should-equal
   "<?php\nif (true)\n    echo 'Something';\necho 'Something after';\n"
   "Inline control structures if")
  
  (phps-mode-test-indent--should-equal
   "<?php\nif (true)\n    echo 'Something';\nelse\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true)\n    echo 'Something';\nelse if (true)\n    echo 'Something else';\necho true;\n"
   "Inline control structures if else if")

  (phps-mode-test-indent--should-equal
   "<?php\nwhile (true)\n    echo 'Something';"
   "Inline control structures while")

  (phps-mode-test-indent--should-equal
   "<?php\nwhile (true)\n    echo 'Something';\necho 'Afterwards';"
   "Inline control structures while")

  )

(defun phps-mode-test-indent--get-lines-indent-alternative-control-structures ()
  "Test for alternative control structures."

  (phps-mode-test-indent--should-equal
   "<?php\nif (true):\n    echo 'Something';\nelseif (true):\n    echo 'Something';\nelse:\n    echo 'Something else';\n    echo 'Something else again';\nendif;\necho true;\n"
   "Alternative control structures basic if-elseif-else flow")

  (phps-mode-test-indent--should-equal
   "<?php\nwhile (true):\n    echo 'Something';\n    echo 'Something';\nendwhile;\necho 'Something else';\n"
   "Alternative control structures basic while-endwhile flow")

  (phps-mode-test-indent--should-equal
   "<?php\nfor ($i = 0; $i < 10; $i++):\n    echo 'Something';\n    echo 'Something';\nendfor;\necho 'Something else';\n"
   "Alternative control structures basic for-endfor flow")

  (phps-mode-test-indent--should-equal
   "<?php\nforeach ($array as $value):\n    echo 'Something';\n    echo 'Something';\nendforeach;\necho 'Something else';\n"
   "Alternative control structures basic foreach-endforeach flow")

  )

(defun phps-mode-test-indent--get-lines-indent-classes ()
  "Test for class indent."

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions")

  (phps-mode-test-indent--should-equal
   "<?php\nnamespace myNamespace\n{\n    class myClass {\n        public function myFunction()\n        {\n            echo 'my statement';\n        }\n    }\n}\n"
   "Regular PHP with namespaces, classes and functions")

  (phps-mode-test-indent--should-equal
   "<?php\nclass MyClass extends MyAbstract implements\n    myInterface,\n    myInterface2\n{\n}\n"
   "Class multi-line implements")

  (phps-mode-test-indent--should-equal
   "<?php\nclass MyClass\n    extends MyAbstract\n    implements myInterface, myInterface2\n{\n}\n"
   "Class multi-line extends and implements")

  (phps-mode-test-indent--should-equal
   "<?php\n/**\n *\n */\nnamespace Aomebo\n{\n    /**\n     *\n     */\n    class Base\n    {\n    }\n}\n"
   "Namespace and class with doc-comments")

  )

(defun phps-mode-test-indent--get-lines-indent-if ()
  "Test for multi-line if expressions."

  (phps-mode-test-indent--should-equal
   "<?php\nif (\n    true\n    && true\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 1")

  (phps-mode-test-indent--should-equal
   "<?php\n// Can we load configuration?\nif ($configuration::load(\n    self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n    self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME)\n)) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 2")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    if ($configuration::load(\n        self::getParameter(self::PARAMETER_CONFIGURATION_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_CONFIGURATION_EXTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_INTERNAL_FILENAME),\n        self::getParameter(self::PARAMETER_STRUCTURE_EXTERNAL_FILENAME))\n    ) {\n        echo 'was here';\n    }\n}\n"
   "If expression spanning multiple lines 3")

  (phps-mode-test-indent--should-equal
   "<?php\nif (myFunction(\n    true)\n) {\n    echo 'was here';\n}\n"
   "If expression spanning multiple lines 5")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    if (myFunction(\n        true)\n    ) {\n        echo 'was here';\n    }\n}\n"
   "Nested if expression spanning multiple lines 6")

  (phps-mode-test-indent--should-equal
   "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\n        echo $title2;\n        \n    }\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression and token-less lines")

  (phps-mode-test-indent--should-equal
   "<html><head><title><?php\nif ($myCondition) {\n    if ($mySecondCondition) {\n        echo $title;\n    } else if ($mySecondCondition) {\n        echo $title4;\n    } else {\n        echo $title2;\n        echo $title3;\n    }\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP with if expression 2")

  (phps-mode-test-indent--should-equal
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"
   "Regular else expression indent calculation")

  )

(defun phps-mode-test-indent--get-lines-indent-switch-case ()
  "Test for switch-case indentation."

  (phps-mode-test-indent--should-equal
   "<?php\nswitch ($condition) {\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\n}\n"
   "Switch, case, default")

  (phps-mode-test-indent--should-equal
   "<?php\nswitch ($condition):\n    case true:\n        echo 'here';\n        echo 'here 2';\n    case false:\n        echo 'here 4';\n    default:\n        echo 'here 3';\nendswitch;\n"
   "Switch, case, default with alternative control structure")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    switch ($condition) {\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    }\n}\n"
   "Curly switch, case, default inside if expression body")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    switch ($condition) {\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    }\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Curly switch, case, default with expression after it")

  (phps-mode-test-indent--should-equal
   "<?php\nif (true) {\n    switch ($condition):\n        case true:\n            echo 'here';\n            echo 'here 2';\n        case false:\n            echo 'here 4';\n        default:\n            echo 'here 3';\n    endswitch;\n    sprintf(__(\n        'Error: %s',\n        $error\n    ));\n}\n"
   "Alternative switch, case, default with expression after it")

  (phps-mode-test-indent--should-equal
   "<?php\n$product_path = \"${filename[0]}/${filename[1]}/\";\necho 'here';\n"
   "Double-quoted string with multiple indexed variables in it")

  )

(defun phps-mode-test-indent ()
  "Run test for functions."
  ;; (setq debug-on-error t)
  (phps-mode-test-indent--helpers)
  (phps-mode-test-indent--get-lines-indent-if)
  (phps-mode-test-indent--get-lines-indent-classes)
  (phps-mode-test-indent--get-lines-indent-inline-control-structures)
  (phps-mode-test-indent--get-lines-indent-alternative-control-structures)
  (phps-mode-test-indent--get-lines-indent-multi-line-assignments)
  (phps-mode-test-indent--get-lines-indent-switch-case)
  (phps-mode-test-indent--get-lines-indent-psr-2)
  (phps-mode-test-indent--get-lines-indent))

(phps-mode-test-indent)

(provide 'phps-mode-test-indent)

;;; phps-mode-test-indent.el ends here
