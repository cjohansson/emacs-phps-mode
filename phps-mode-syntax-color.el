;;; phps-mode-syntax-color.el --- Syntax coloring for major mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-syntax-color-generator)

(defvar
  phps-mode-syntax-color--token-font-face
  (eval-when-compile (phps-mode-syntax-color-generator--token-colors))
  "Syntax color table for tokens.")

(defvar
  phps-mode-syntax-color--token-for-bookkeeping-p
  (eval-when-compile (phps-mode-syntax-color-generator--bookkeeping-tokens))
  "Flags whether token is used for bookkeeping or not.")


(provide 'phps-mode-syntax-color)
;;; phps-mode-syntax-color.el ends here
