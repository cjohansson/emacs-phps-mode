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
(autoload 'phps-mode-functions-indent-line "phps-mode-functions")
(autoload 'phps-mode-functions-get-lines-indent "phps-mode-functions")
(autoload 'phps-mode-functions-get-current-line-data "phps-mode-functions")
(autoload 'hash-table-values "subr-x")
(autoload 'should "ert")

(defun phps-mode-test-functions--hash-to-list (hash-table)
  "Return a list that represent the HASH-TABLE.  Each element is a list: (list key value)."
  (let (result)
    (maphash
     (lambda (k v)
       (push (list k v) result))
     hash-table)
    (nreverse result)))

(defun phps-mode-test-functions-get-lines-indent ()
  "Test `phps-mode-functions-get-lines-indent' function."

  (phps-mode-test-with-buffer
   "<html><head><title><?php\nif ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n}\n} ?></title><body>Bla bla</body></html>"
   "Mixed HTML/PHP"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (2 0)) (5 (1 0)) (6 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true):\n    echo 'Something';\nelse:\n    echo 'Something else';\nendif;\necho true;\n"
   "Alternative control structures"
   ;; (message "Tokens: %s" phps-mode-lexer-tokens)
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 0)) (7 (0 0))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\nif (true)\n    echo 'Something';\nelse\n    echo 'Something else';\necho true;\n"
   "Inline control structures"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (1 0)) (4 (0 0)) (5 (1 0)) (6 (0 1))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* Bla\n*/"
   "DOC-COMMENT"
   (should (equal '((1 (0 0)) (2 (0 0)) (3 (0 1))) (phps-mode-test-functions--hash-to-list (phps-mode-functions-get-lines-indent)))))

  ;; TODO CASE, DEFAULT

  ;; TODO NOWDOC, HEREDOC

  ;; TODO Multi-line assignments

  )

;; TODO Add unit tests for HEREDOC and NOWDOC regions as well

(defun phps-mode-test-functions-indent-line ()
  "Test for indentation."

  ;; Curly bracket tests
  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 69)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\n    if ($mySeconCondition) {\necho $title;\n\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title1;\n} ?></title><body>Bla bla</body></html>"
   (goto-char 75)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\n        echo $title1;\n} ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n} ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 98)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title2;\n\n    } ?></title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"
   (goto-char 110)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title3;\n\n}\n?>\n</title><body>Bla bla</body></html>"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n'random3'\n);\n$variable = true;\n"
   nil
   (goto-char 28)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random3'\n);\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n    'random2'\n    );\n$variable = true;\n"
   (goto-char 43)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random2'\n);\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   nil
   (goto-char 20)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n * My first line\n* My second line\n**/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   (goto-char 9)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n**/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n/**\n* My first line\n* My second line\n**/\n"
   nil
   (goto-char 46)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n/**\n* My first line\n* My second line\n **/\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n'random4');\n$variable = true;\n"
   (goto-char 29)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\n$variable = array(\n    'random4');\n$variable = true;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nadd_filter(\n\"views_{$screen->id}\",'__return_empty_array'\n);"
   nil
   (goto-char 25)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nadd_filter(\n    \"views_{$screen->id}\",'__return_empty_array'\n);"))))

  (phps-mode-test-with-buffer
   "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"
   (goto-char 36)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (random_expression(\ntrue\n)) {\nsome_logic_here();\n}"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"
   nil
   (goto-char 54)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents  "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else {\n    $this->var = 'def456';\n}\n"
   (goto-char 68)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else {\n    $this->var = 'def456';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n    } else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"
   nil
   (goto-char 68)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s point %s" phps-mode-lexer-tokens (point))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myFirstCondition()) {\n    $this->var = 'abc123';\n} else if (mySeconCondition()) {\n    $this->var = 'def456';\n}\n"))))

  ;; Square bracket
  (phps-mode-test-with-buffer
   "<?php\n$var = [\n    'random' => [\n        'hello',\n],\n];\n"
   (goto-char 51)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = [\n    'random' => [\n        'hello',\n    ],\n];\n"))))
  
  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    else:\n    echo 'Something else here 8';\nendif;\n"
   nil
   (goto-char 62)
   (phps-mode-functions-indent-line)
   ;; (message "Tokens %s" phps-mode-lexer-tokens)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\necho 'Something here';\nelse:\n    echo 'Something else here 8';\nendif;\n"))))

  ;; switch case
  
  (phps-mode-test-with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   (goto-char 45)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()) {\n    case 'Something here':\necho 'Something else here';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\necho 'Something else here';\n}\n"
   nil
   (goto-char 65)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nswitch (myRandomCondition()) {\ncase 'Something here':\n        echo 'Something else here';\n}\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition())\necho 'Something here';\necho 'Something else here';\n"
   (goto-char 40)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition())\n    echo 'Something here';\n    echo 'Something else here';\n"
   nil
   (goto-char 60)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition())\n    echo 'Something here';\necho 'Something else here';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (myRandomCondition()):\necho 'Something here';\n    echo 'Something else here';\nendif;\n"
   (goto-char 40)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (myRandomCondition()):\n    echo 'Something here';\n    echo 'Something else here';\nendif;\n"))))

  (phps-mode-test-with-buffer
   "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n        ),\n    $var5\n);\n"
   nil
   (goto-char 65)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nmyFunction(\n    array(\n        'random' => 'abc',\n    ),\n    $var5\n);\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$var = $var2->getHead()\n->getTail();\n"
   (goto-char 35)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = $var2->getHead()\n    ->getTail();\n"))))

  (phps-mode-test-with-buffer
   "<?php\n$var =\n'random string';\n"
   nil
   (goto-char 20)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var =\n    'random string';\n"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   (goto-char 60)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\nendif;"))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty($this->var)):\n$this->var = 'abc123';\n    endif;"
   nil
   (goto-char 30)
   (phps-mode-functions-indent-line)
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\nif (empty($this->var)):\n    $this->var = 'abc123';\n    endif;"))))

  )

;; TODO Remove this functions
(defun phps-mode-test-functions-get-current-line-data ()
  "Return information about point in tokens."

  (phps-mode-test-with-buffer
   "<?php\nNAMESPACE MyNameSpace;\nCLASS MyClass {\n\tpublic function __construct() {\n\t\texit;\n\t}\n}\n"
   (goto-char 35)
   (should (equal (list (list t 0 0 0 0 0 3 nil) (list t 1 0 0 0 0 6 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 15)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 5 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php echo $title; ?>\n</title><body>Bla bla</body></html>"
   (goto-char 50)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 nil nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title></title><body>Bla bla</body></html>"
   nil
   (goto-char 15)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 nil nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   (goto-char 30)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 5 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php echo $title; ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 50)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 5 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) { \n   if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   (goto-char 48)
   (should (equal (list (list t 1 0 0 0 0 5 nil) (list nil 0 0 0 0 0 17 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) {\n echo $title;\n} } ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 72)
   (should (equal (list (list t 2 0 0 0 0 10 nil) (list t 2 0 0 0 0 13 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) {\nif ($mySeconCondition) {\necho $title;\n}\n}\n ?></title><body>Bla bla</body></html>"
   (goto-char 84)
   (should (equal (list (list t 2 0 0 0 0 13 nil) (list t 1 0 0 0 0 14 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<html><head><title><?php if ($myCondition) { if ($mySeconCondition) { echo $title; } } ?></title><body>Bla bla</body></html>"
   nil
   (goto-char 100)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list nil 0 0 0 0 0 17 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   (goto-char 20)
   (should (equal (list (list t 0 0 0 0 0 nil t) (list t 0 0 0 0 0 nil t)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   nil
   (goto-char 9)
   (should (equal (list (list nil 0 0 0 0 0 nil nil) (list t 0 0 0 0 0 1 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php /**\n * My first line\n * My second line\n **/"
   (goto-char 50)
   (should (equal (list (list t 0 0 0 0 0 nil t) (list t 0 0 0 0 0 nil t)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\n$variable = array(\n'random4');\n$variable = true;\n"
   nil
   (goto-char 29)
   (should (equal (list (list t 0 1 0 0 0 4 nil) (list t 0 0 0 0 0 7 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif (empty(\n$this->var\n) && !empty($this->var)\n) {\n$this->var = 'abc123';\n}\n"
   (goto-char 54)
   (should (equal (list (list t 0 1 0 0 0 16 nil) (list t 1 0 0 0 0 18 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\n$var = [\n    'random' => [\n        'hello',\n    ],\n];\n"
   nil
   (goto-char 46)
   (should (equal (list (list t 0 0 2 0 0 6 nil) (list t 0 0 2 0 0 8 nil)) (phps-mode-functions-get-current-line-data))))

  ;; INLINE SYNTAX FOR CONTROL STRUCTURES

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition)\n    echo 'was here';\necho 'was here 2';\n"
   (goto-char 41)
   (should (equal (list (list t 0 0 0 1 0 4 nil) (list t 0 0 0 0 0 7 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition)\n    echo 'was here';\necho 'was here 2';\n"
   nil
   (goto-char 60)
   (should (equal (list (list t 0 0 0 0 0 7 nil) (list t 0 0 0 0 0 10 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition) echo 'was here'; echo 'was here 2';\n"
   (goto-char 32)
   (should (equal (list (list t 0 0 0 0 0 0 nil) (list t 0 0 0 0 0 10 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition) echo 'was here'; echo 'was here 2';\n"
   nil
   (goto-char 55)
   (should (equal (list (list t 0 0 0 0 0 0 nil) (list t 0 0 0 0 0 10 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition)\n    echo 'was here';\nelse\n    echo 'was here 2';\n"
   (goto-char 47)
   (should (equal (list (list t 0 0 0 0 0 7 nil) (list t 0 0 0 0 0 8 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition)\n    echo 'was here';\nelse\n    echo 'was here 2';\n"
   nil
   (goto-char 57)
   (should (equal (list (list t 0 0 0 1 0 8 nil) (list t 0 0 0 0 0 11 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition)\n    echo 'was here';\nelse\n    echo 'was here 2';\n"
   (goto-char 55)
   (should (equal (list (list t 0 0 0 1 0 8 nil) (list t 0 0 0 0 0 11 nil)) (phps-mode-functions-get-current-line-data))))

  ;; ALTERNATIVE SYNTAX FOR CONTROL STRUCTURES

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):\n    echo 'was here';\nendif;\necho 'was here 2';\n"
   nil
   (goto-char 41)
   (should (equal (list (list t 0 0 0 0 1 5 nil) (list t 0 0 0 0 1 8 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):\n    echo 'was here';\nendif;\necho 'was here 3';\n"
   (goto-char 52)
   (should (equal (list (list t 0 0 0 0 0 8 nil) (list t 0 0 0 0 0 10 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):    echo 'was here';\nendif;\necho 'was here 4';\n"
   nil
   (goto-char 32)
   (should (equal (list (list t 0 0 0 0 0 0 nil) (list t 0 0 0 0 1 8 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition): echo 'was here'; endif; echo 'was here 5';\n"
   (goto-char 35)
   (should (equal (list (list t 0 0 0 0 0 0 nil) (list t 0 0 0 0 0 13 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):\n    echo 'was here';\nelse:\n    echo 'was here 2';\nendif;\n"
   nil
   (goto-char 44)
   (should (equal (list (list t 0 0 0 0 1 5 nil) (list t 0 0 0 0 1 8 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):\n    echo 'was here';\nelse:\n    echo 'was here 2';\nendif;\n"
   (goto-char 64)
   (should (equal (list (list t 0 0 0 0 1 10 nil) (list t 0 0 0 0 1 13 nil)) (phps-mode-functions-get-current-line-data))))

  (phps-mode-test-with-buffer
   "<?php\nif ($myCondition):\n    echo 'was here';\nelse:\n    echo 'was here 2';\nendif;\n"
   nil
   (goto-char 79)
   (should (equal (list (list t 0 0 0 0 0 10 nil) (list t 0 0 0 0 0 15 nil)) (phps-mode-functions-get-current-line-data))))

  ;; TODO SWITCH, CASE, DEFAULT AS WELL
  
  )

;; TODO Add tests for all examples here: https://www.php-fig.org/psr/psr-2/

(defun phps-mode-test-functions ()
  "Run test for functions."
  (phps-mode-test-functions-get-lines-indent)
  (phps-mode-test-functions-get-current-line-data)
  (phps-mode-test-functions-indent-line))

(phps-mode-test-functions)

(provide 'phps-mode-test-functions)

;;; phps-mode-test-functions.el ends here
