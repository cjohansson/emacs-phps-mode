;;; phps-mode-test-cache.el --- Tests for cache -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:


;; Run from terminal make test-cache


;;; Code:


(require 'phps-mode-cache)

(require 'ert)

(defun phps-mode-test-cache ()
  "Run test."

  (phps-mode-cache-delete "abc")

  (should
   (equal
    (phps-mode-cache-test-p "abc")
    nil))
  (message "Passed cache test function 1")

  (phps-mode-cache-save '(0 1 2) "abc")

  (should
   (equal
    (phps-mode-cache-test-p "abc")
    t))

  (message "Passed cache test function 2")

  (should
   (equal
    (phps-mode-cache-load "abc")
    '(0 1 2)))

  (message "Passed cache load function")

  (message "Passed tests for cache"))

(phps-mode-test-cache)

(provide 'phps-mode-test-cache)

;;; phps-mode-test-cache.el ends here
