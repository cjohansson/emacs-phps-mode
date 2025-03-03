;;; phps-mode-macros.el --- Macros for major mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.

;;; Commentary:


;;; Code:

(defconst
  phps-mode-macrotime-debug
  nil
  "Debug messages during macro expansion time, default nil.")

(defmacro phps-mode-debug-message (&rest code)
  "Run CODE only when debug flag is on."
  `(when ,phps-mode-macrotime-debug
    ,@code))

(provide 'phps-mode-macros)
;;; phps-mode-macros.el ends here
