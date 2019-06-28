;;; phps-mode-flycheck.el --- Flycheck support for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Christian Johansson

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
