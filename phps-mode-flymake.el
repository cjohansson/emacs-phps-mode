;;; phps-mode-flymake.el --- Flymake support for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:

;; Please see README.md from the same repository for extended documentation.


;;; Code:

(require 'flymake)

(defun phps-mode-flymake-init ()
  "PHP specific init-cleanup routines.

This is an alternative function of `flymake-php-init'.
Look at the `php-executable' variable instead of the constant \"php\" command."

  (when (boundp 'flymake-allowed-file-name-masks)
    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.php[345s]?\\'"
                   phps-mode--flymake-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)))

  (when (boundp 'flymake-err-line-patterns)
    (add-to-list 'flymake-err-line-patterns
                 '("\\(Parse\\|Fatal\\) error: \\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)" 3 4 nil 2)))

  (if (boundp 'php-executable)
      (let* ((temp-file
              (funcall
               (eval-when-compile
                 (if (fboundp 'flymake-proc-init-create-temp-buffer-copy)
                     'flymake-proc-init-create-temp-buffer-copy
                   (if (fboundp 'flymake-init-create-temp-buffer-copy)
                   'flymake-init-create-temp-buffer-copy)))
               'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list php-executable (list "-f" local-file "-l")))))

(provide 'phps-mode-flymake)

;;; phps-mode-flymake.el ends here
