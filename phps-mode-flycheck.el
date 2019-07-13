;;; phps-mode-flycheck.el --- Flycheck support for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2017-2019  Free Software Foundation, Inc.

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

;; Please see README.md from the same repository for extended documentation.


;;; Code:

(defun phps-mode-flycheck-init ()
  "Add flycheck support for phps-mode."

  (when (fboundp 'flycheck-add-mode)
    (flycheck-add-mode 'php 'phps-mode)
    (flycheck-add-mode 'php-phpmd 'phps-mode)
    (flycheck-add-mode 'php-phpcs 'phps-mode)))


(provide 'phps-mode-flycheck)

;;; phps-mode-flycheck.el ends here
