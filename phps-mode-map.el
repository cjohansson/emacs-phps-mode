;; phps-mode-map.el --- Map for PHPs -*- lexical-binding: t -*-

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


;; Please see README.md from the same repository for extended documentation.


;;; Code:


(defvar phps-mode-map
  (let ((map (make-keymap)))
    ;; TODO keys here
    map)
  "Key-map for major mode.")

(defun phps-mode-map-init ()
  "Apply map to mode."
  (use-local-map phps-mode-map))

(provide 'phps-mode-map)
;;; phps-mode-map.el ends here
