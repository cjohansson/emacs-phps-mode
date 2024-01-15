;;; phps-mode-test-syntax-table.el --- Tests for syntax-table -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:

;; Run from terminal make functions-test


;;; Code:

(require 'ert)
(require 'phps-mode)
(require 'phps-mode-test)

;; TODO Should test `backward-sexp', `forward-sexp', `backward-word', `forward-word', `backward-list', `forward-list' as well

(defun phps-mode-test-syntax-table--quote-region ()
  "Test double quotes, single quotes, curly bracket,
square bracket, round bracket, back-quotes on regions."

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Double quotes around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "\""))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = \"abc\";"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Single-quotes brackets around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "'"))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = 'abc';"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Round brackets around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "("))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = (abc);"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Square brackets around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "["))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = [abc];"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Curly brackets around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "{"))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = {abc};"))))

  (phps-mode-test--with-buffer
   "<?php\n$var = abc;"
   "Backquotes brackets around region"
   (goto-char 14)
   (push-mark nil t t)
   (goto-char 17)
   (execute-kbd-macro (kbd "`"))
   (let ((buffer-contents (buffer-substring-no-properties (point-min) (point-max))))
     (should (equal buffer-contents "<?php\n$var = `abc`;"))))

  )

(defun phps-mode-test-syntax-table ()
  "Run test."
  (phps-mode-test-syntax-table--quote-region))

(phps-mode-test-syntax-table)


(provide 'phps-mode-test-syntax-table)

;;; phps-mode-test-syntax-table.el ends here
