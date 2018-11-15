;;; phps-mode/phps-flycheck.el --- Flycheck support for PHP with Semantic integration -*- lexical-binding: t -*-

;; Author: Christian Johansson <github.com/cjohansson>
;; Maintainer: Christian Johansson <github.com/cjohansson>
;; Created: 3 Mar 2018
;; Modified: .
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: -

;; Package-Requires: ((emacs "24"))

;; Copyright (C) 2017 Christian Johansson

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

;;; Commentary:


;;; Code:


(defun phps-mode/flycheck-init ()
  "Add flycheck support for PHP Semantic mode."

  (when (fboundp 'flycheck-define-checker)
    (flycheck-define-checker php
      "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
      :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
                "-d" "log_errors=0" source)
      :error-patterns
      ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
              (message) " in " (file-name) " on line " line line-end))
      :modes (php-mode php+-mode phps-mode)
      :next-checkers ((warning . php-phpmd)
                      (warning . php-phpcs)))

    (flycheck-define-checker php-phpmd
      "A PHP style checker using PHP Mess Detector.

See URL `https://phpmd.org/'."
      :command ("phpmd" source "xml"
                (eval (flycheck-option-comma-separated-list
                       flycheck-phpmd-rulesets)))
      :error-parser flycheck-parse-phpmd
      :modes (php-mode php+-mode phps-mode)
      :next-checkers (php-phpcs))

    (flycheck-define-checker php-phpcs
      "A PHP style checker using PHP Code Sniffer.

Needs PHP Code Sniffer 2.6 or newer.

See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
      :command ("phpcs" "--report=checkstyle"
                ;; Use -q flag to force quiet mode
                ;; Quiet mode prevents errors from extra output when phpcs has
                ;; been configured with show_progress enabled
                "-q"
                (option "--standard=" flycheck-phpcs-standard concat)
                ;; Pass original file name to phpcs.  We need to concat explicitly
                ;; here, because phpcs really insists to get option and argument as
                ;; a single command line argument :|
                (eval (when (buffer-file-name)
                        (concat "--stdin-path=" (buffer-file-name))))
                ;; Read from standard input
                "-")
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "STDIN" errors)))
      :modes (php-mode php+-mode phps-mode)
      ;; phpcs seems to choke on empty standard input, hence skip phpcs if the
      ;; buffer is empty, see https://github.com/flycheck/flycheck/issues/907
      :predicate (lambda () (not (flycheck-buffer-empty-p))))
    )
  )


(provide 'phps-mode/flycheck)

;;; phps-flycheck.el ends here
