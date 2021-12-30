;;; phps-mode-test-indent.el --- Tests for indentation -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.


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
    (let ((line-min-position (line-beginning-position))
          (line-max-position (line-end-position)))
      (goto-char
       (+
        line-min-position
        (random (- line-max-position line-min-position)))))
    (execute-kbd-macro (kbd "TAB"))))

(defun phps-mode-test-indent--should-equal (string name)
  "Test indent of whole buffer containing STRING with NAME."
  (phps-mode-test--with-buffer
   string
   name
   (message "Initial buffer:\n%S" string)
   (phps-mode-test-indent--indent-whole-buffer)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (message "\nIndented buffer:\n%S" buffer-contents)
     (should (equal
              buffer-contents
              string)))))

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

  )

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
     "$var = myFunction(\n    'setting');\necho 'here';\n"
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
   "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\n        echo $title2;\n        \n    } ?></title><body>Bla bla</body></html>"
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
