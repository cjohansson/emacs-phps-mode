;;; phps-mode-ast.el --- Abstract Syntax Tree functions -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-parser-sdt)


;;; Variables:


(defvar-local
  phps-mode-ast--parse-trail
  nil
  "Parse trail for current buffer.")

(defvar-local
  phps-mode-ast--tree
  nil
  "Tree for current buffer.")


;; Functions:


(defun phps-mode-ast--generate ()
  "Generate AST for current buffer."
  (setq
   phps-mode-parser-sdt-bookkeeping
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-table-index
   0)
  (setq
   phps-mode-parser-sdt-symbol-imenu--table
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-imenu--namespace
   nil)
  (setq
   phps-mode-parser-sdt-symbol-imenu--stack
   nil)
  (setq
   phps-mode-parser-sdt-symbol-table
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt-symbol-table-by-uri
   (make-hash-table :test 'equal))
  (setq
   phps-mode-parser-sdt--bookkeeping-namespace
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-symbol-stack
   nil)
  (setq
   phps-mode-parser-sdt--bookkeeping-anonymous-function-count
   0)
  (setq
   phps-mode-parser-sdt--bookkeeping-arrow-function-count
   0)
  (let* ((result (phps-mode-parser--parse t))
         (parse-trail (nth 0 result))
         (translation (nth 1 result)))
    (setq
     phps-mode-ast--parse-trail
     parse-trail)

    (phps-mode-debug-message
     (message "\nTranslation:\n%S\n\n" translation))

    ;; Convert imenu index from hash-table to list structure here
    (let ((imenu-index))
      (maphash
       (lambda (k v)
         (if (hash-table-p v)
             (let ((v-list)
                   (v-index))
               (maphash
                (lambda (k2 v2)
                  (if (hash-table-p v2)
                      (let ((v2-list)
                            (v2-index))
                        (maphash
                         (lambda (k3 v3)
                           (if (hash-table-p v3)
                               (let ((v3-list)
                                     (v3-index))
                                 (maphash
                                  (lambda (k4 v4)
                                    (if (symbolp k4)
                                        (setq v3-index v4)
                                      (push `(,k4 . ,v4) v3-list)))
                                  v3)

                                 ;; Sort level 4
                                 (setq
                                  v3-list
                                  (sort
                                   v3-list
                                   (lambda (a b)
                                     (cond
                                      ((and
                                        (listp (cdr a))
                                        (listp (cdr b)))
                                       (< (cdr (car (cdr a))) (cdr (car (cdr b)))))
                                      ((listp (cdr a))
                                       (< (cdr (car (cdr a))) (cdr b)))
                                      ((listp (cdr b))
                                       (< (cdr a) (cdr (car (cdr b)))))
                                      (t
                                       (< (cdr a) (cdr b)))))))
                                 (push `("declaration" . ,v3-index) v3-list)
                                 (push (append (list k3) v3-list) v2-list))
                             (if (symbolp k3)
                                 (setq v2-index v3)
                             (push `(,k3 . ,v3) v2-list))))
                         v2)

                        ;; Sort level 3
                        (setq
                         v2-list
                         (sort
                          v2-list
                          (lambda (a b)
                            (cond
                             ((and
                               (listp (cdr a))
                               (listp (cdr b)))
                              (< (cdr (car (cdr a))) (cdr (car (cdr b)))))
                             ((listp (cdr a))
                              (< (cdr (car (cdr a))) (cdr b)))
                             ((listp (cdr b))
                              (< (cdr a) (cdr (car (cdr b)))))
                             (t
                              (< (cdr a) (cdr b)))))))
                        (push `("declaration" . ,v2-index) v2-list)
                        (push (append (list k2) v2-list) v-list))
                    (if (symbolp k2)
                        (setq v-index v2)
                      (push `(,k2 . ,v2) v-list))))
                v)

               ;; Sort level 2
               (setq
                v-list
                (sort
                 v-list
                 (lambda (a b)
                   (cond
                    ((and
                      (listp (cdr a))
                      (listp (cdr b)))
                     (< (cdr (car (cdr a))) (cdr (car (cdr b)))))
                    ((listp (cdr a))
                     (< (cdr (car (cdr a))) (cdr b)))
                    ((listp (cdr b))
                     (< (cdr a) (cdr (car (cdr b)))))
                    (t
                     (< (cdr a) (cdr b)))))))
               (push `("declaration" . ,v-index) v-list)
               (push (append (list k) v-list) imenu-index))
           (push `(,k . ,v) imenu-index)))
       phps-mode-parser-sdt-symbol-imenu--table)

      ;; Sort level 1
      (setq
       imenu-index
       (sort
        imenu-index
        (lambda (a b)
          (cond
           ((and
             (listp (cdr a))
             (listp (cdr b)))
            (< (cdr (car (cdr a))) (cdr (car (cdr b)))))
           ((listp (cdr a))
            (< (cdr (car (cdr a))) (cdr b)))
           ((listp (cdr b))
            (< (cdr a) (cdr (car (cdr b)))))
           (t
            (< (cdr a) (cdr b)))))))

      (setq
       phps-mode-parser-sdt-symbol-imenu
       imenu-index))

    (setq
     phps-mode-ast--tree
     translation)
    phps-mode-ast--tree))


(provide 'phps-mode-ast)
;;; phps-mode-ast.el ends here
