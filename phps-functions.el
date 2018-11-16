;;; phps-mode/phps-functions.el --- Mode functions for PHPs -*- lexical-binding: t -*-

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

;; Package-Requires: ((emacs "24"))

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


;;; Code:


(defvar phps-mode/buffer-changes--start nil
  "Start of buffer changes, nil if none.")

(autoload 'phps-mode/lexer-get-point-data "phps-lexer")

;; TODO Should also format white-space inside the line, i.e. after function declarations
(defun phps-mode/indent-line ()
  "Indent line."
  (let ((data (phps-mode/lexer-get-point-data)))
    (save-excursion
      (beginning-of-line)
      (let* ((start (nth 0 data))
             (end (nth 1 data))
             (in-scripting (nth 0 start)))

        ;; Are we in scripting?
        (when in-scripting
          (let ((start-bracket-level (nth 1 start))
                (start-parenthesis-level (nth 2 start))
                (start-token-number (nth 4 start))
                (end-bracket-level (nth 1 end))
                (end-parenthesis-level (nth 2 end))
                (end-token-number (nth 4 end)))
            (let* ((indent-start (+ start-bracket-level start-parenthesis-level))
                   (indent-end (+ end-bracket-level end-parenthesis-level))
                   (indent-level indent-start))
              ;; (message "indent-start %s, indent-end %s" indent-start indent-end)
              (when (and
                     (boundp 'phps-mode/lexer-tokens)
                     (> indent-start indent-end))
                (let ((token-number start-token-number)
                      (valid-tokens t))
                  ;; (message "token start %s, token end %s" start-token-number end-token-number)
                  (while (and valid-tokens
                              (<= token-number end-token-number))
                    (let ((token (car (nth token-number phps-mode/lexer-tokens)))
                          (token-start (car (cdr (nth token-number phps-mode/lexer-tokens)))))
                      (when (and valid-tokens
                                 (>= token-start (point))
                                 (not (or
                                       (string= token "{")
                                       (string= token "}")
                                       (string= token "(")
                                       (string= token ")")
                                       (string= token "[")
                                       (string= token "]")
                                       (string= token ";")
                                       (eq token 'T_CLOSE_TAG))))
                        ;; (message "Token %s - %s in %s was invalid" token token-number phps-mode/lexer-tokens)
                        (setq valid-tokens nil)))
                    (setq token-number (+ token-number 1)))
                  (when valid-tokens
                    ;; (message "Tokens was valid, decreasing indent %s - %s" (line-beginning-position) (line-end-position))
                    (setq indent-level (- indent-level (- indent-start indent-end))))))
              ;; (message "inside scripting, start: %s, end: %s, indenting to column %s " start end indent-level)
              (indent-line-to (* indent-level tab-width))
              (phps-mode/run-incremental-lex))))))))

;; TODO Implement this
(defun phps-mode/indent-region ()
  "Indent region."
  )

;; TODO This function should track between what min and max region a specific buffer has been modified and then re-run lexer for that region when editor is idle, maybe use (buffer-name))
;; maybe use 'auto-save-hook for this
(defun phps-mode/after-change-functions (start stop length)
  "Track buffer change from START to STOP with length LENGTH."
  (when (string= major-mode "phps-mode")
    (when (and (not phps-mode/buffer-changes--start)
               (boundp 'phps-mode/idle-interval))
      (run-with-idle-timer phps-mode/idle-interval nil #'phps-mode/run-incremental-lex))
    (setq phps-mode/buffer-changes--start start)
    (message "phps-mode/after-change-functions %s %s %s" start stop length)
  ))

(defun phps-mode/functions-init ()
  "PHP specific init-cleanup routines."

  ;; indent-region will call this on each line of region
  (set (make-local-variable 'indent-line-function) #'phps-mode/indent-line)

  (when (and (boundp 'phps-mode/use-psr-2)
             phps-mode/use-psr-2)

    ;; PSR-2 : Code MUST use an indent of 4 spaces
    (set (make-local-variable 'tab-width) 4)

    ;; PSR-2 : MUST NOT use tabs for indenting
    (set (make-local-variable 'indent-tabs-mode) nil)

    )

  (set (make-local-variable 'phps-mode/buffer-changes--start) nil)

  (add-hook 'after-change-functions #'phps-mode/after-change-functions)

  ;; (set (make-local-variable 'indent-line-function) #'phps-mode/indent-region)
  )


(provide 'phps-mode/functions)

;;; phps-functions.el ends here
