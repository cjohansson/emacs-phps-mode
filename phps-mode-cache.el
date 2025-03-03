;;; phps-mode-cache.el -- Cache for phps-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(defvar
  phps-mode-cache--use-p
  t
  "Whether to use cache or not.")

(defvar
  phps-mode-cache--base-filename
  "~/.phps-mode-cache/"
  "Base filename for cache files.")

(defun phps-mode-cache--get-filename-for-key (key)
  "Get filename for KEY."
  (let ((directory-filename
         (expand-file-name phps-mode-cache--base-filename)))
    (unless (file-exists-p directory-filename)
      (make-directory directory-filename))
    (let ((filename
           (expand-file-name
            (replace-regexp-in-string
             "\\(/\\|@\\|:\\|\\.\\)" "_"
             key)
            directory-filename)))
      filename)))

(defun phps-mode-cache-test-p (key &optional source-file)
  "Test whether KEY exists in cache and that it is
optionally not older than SOURCE-FILE."
  (let ((cache-filename (phps-mode-cache--get-filename-for-key key))
        (exists))
    (when (file-exists-p cache-filename)
      (if (and
           source-file
           (file-exists-p source-file))
          (unless
              (file-newer-than-file-p
               source-file
               cache-filename)
            (setq
             exists
             t))
        (setq
         exists
         t)))
    exists))

(defun phps-mode-cache-delete (key)
  "Delete cache for KEY."
  (let ((cache-filename (phps-mode-cache--get-filename-for-key key)))
    (when (file-exists-p cache-filename)
      (delete-file cache-filename nil))))

(defun phps-mode-cache-save (data key)
  "Save DATA in cache for KEY."
  (let ((cache-filename (phps-mode-cache--get-filename-for-key key))
        (save-silently t))
    (with-temp-file cache-filename
      (insert (format "'%S" data)))))

(defun phps-mode-cache-load (key)
  "Load DATA in cache for KEY."
  (with-temp-buffer
    (insert-file-contents
     (phps-mode-cache--get-filename-for-key
      key))
    (let ((data
           (eval
            (car
             (read-from-string
              (buffer-substring-no-properties
               (point-min)
               (point-max)))))))
      data)))


(provide 'phps-mode-cache)

;;; phps-mode-cache.el ends here
