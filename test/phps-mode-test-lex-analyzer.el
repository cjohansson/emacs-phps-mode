;;; phps-mode-test-lex-analyzer.el --- Tests for lex-analyzer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:


;; Run from terminal make test-lex-analyzer


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
            '((INCREMENTAL-LEX 1)))))

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
            '((INCREMENTAL-LEX 29)))))

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

(defun phps-mode-test-lex-analyzer--narrow-to-defun ()
  "Test (narrow-to-defun)."

  (phps-mode-test--with-buffer
   "<?php\nfunction test($a) {\n    return $a + 1;\n}\necho 'here';\n"
   "Test `beginning-of-defun', `end-of-defun' and `narrow-to-defun' basic example"
   (goto-char 27)
   (should (equal (phps-mode-lex-analyzer--beginning-of-defun) t))
   (should (equal (point) 7))
   (should (equal (phps-mode-lex-analyzer--end-of-defun) t))
   (should (equal (point) 47))
   (goto-char 27)
   (narrow-to-defun)
   (should (equal (point-min) 7))
   (should (equal (point-max) 48)))

  (phps-mode-test--with-buffer
   "<?php\nfunction test2($a) {\n    echo 'was there }';\n    echo \"was here \\\"}\\\" or there\";\n    return $a + 1;\n}"
   "Test `beginning-of-defun', `end-of-defun' and `narrow-to-defun' advanced example"
   (goto-char 41)
   (should (equal (phps-mode-lex-analyzer--beginning-of-defun) t))
   (should (equal (point) 7))
   (should (equal (phps-mode-lex-analyzer--end-of-defun) t))
   (should (equal (point) 108))
   (goto-char 65)
   (narrow-to-defun)
   (should (equal (point-min) 7))
   (should (equal (point-max) 108)))

  (phps-mode-test--with-buffer
   "<?php\necho 'here';\n$var = function() {\n    echo 'here';\n};\necho 'there';"
   "Test `beginning-of-defun', `end-of-defun' and `narrow-to-defun' for a anonymous function."
   (goto-char 53)
   (should (equal (phps-mode-lex-analyzer--beginning-of-defun) t))
   (should (equal (point) 20))
   (should (equal (phps-mode-lex-analyzer--end-of-defun) t))
   (should (equal (point) 58))
   (goto-char 47)
   (narrow-to-defun)
   (should (equal (point-min) 20))
   (should (equal (point-max) 58)))

  (phps-mode-test--with-buffer
   "<?php\nnamespace myNamespace\n{\n    class myClass\n    {\n        function myFunction($arg)\n        {\n            /**\n             * if ($arg) { return true; }\n             */\n            if ($arg) {\n                // }}\n                # }}\n            }\n            return false;\n        }\n    }\n}"
   "Test `beginning-of-defun', `end-of-defun' and `narrow-to-defun' with commented-out code."
   (goto-char 148)
   (should (equal (phps-mode-lex-analyzer--beginning-of-defun) t))
   (should (equal (point) 55))
   (should (equal (phps-mode-lex-analyzer--end-of-defun) t))
   (should (equal (point) 289))
   (goto-char 253)
   (narrow-to-defun)
   (should (equal (point-min) 55))
   (should (equal (point-max) 290)))

  )

(defun phps-mode-test-lex-analyzer ()
  "Run test for functions."
  ;; (setq debug-on-error t)
  (phps-mode-test-lex-analyzer--narrow-to-defun)
  (phps-mode-test-lex-analyzer--process-changes)
  (phps-mode-test-lex-analyzer--comment-uncomment-region))

(phps-mode-test-lex-analyzer)

(provide 'phps-mode-test-lex-analyzer)

;;; phps-mode-test-lex-analyzer.el ends here
