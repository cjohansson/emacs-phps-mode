;;; phps-mode/phps-semantic.el --- Semantic functions for PHP

;;; Copyright (C) 1999-2018 Free Software Foundation, Inc.

;; Author: David Ponce <david@dponce.com>

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Make similar to /Users/christianjohansson/Documents/emacs/nextstep/Emacs.app/Contents/Resources/lisp/cedet/semantic/java.el.gz
;;
;; Common function for PHP parsers.

;;; Code:
(require 'semantic)
(require 'semantic/ctxt)
(require 'semantic/doc)
(require 'semantic/format)

(eval-when-compile
  (require 'semantic/find)
  (require 'semantic/dep))

;;; Lexical analysis
;; TODO Verify this syntax
(defconst semantic-php-number-regexp
  (eval-when-compile
    (concat "\\("
            "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[.][fFdD]\\>"
            "\\|"
            "\\<[0-9]+[.]"
            "\\|"
            "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
            "\\|"
            "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
            "\\|"
            "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
            "\\|"
            "\\<[0-9]+[lLfFdD]?\\>"
            "\\)"
            ))
  "Lexer regexp to match PHP number terminals.
Following is the specification of PHP number literals.

DECIMAL_LITERAL:
    [1-9][0-9]*
  ;
HEX_LITERAL:
    0[xX][0-9a-fA-F]+
  ;
OCTAL_LITERAL:
    0[0-7]*
  ;
INTEGER_LITERAL:
    <DECIMAL_LITERAL>[lL]?
  | <HEX_LITERAL>[lL]?
  | <OCTAL_LITERAL>[lL]?
  ;
EXPONENT:
    [eE][+-]?[09]+
  ;
FLOATING_POINT_LITERAL:
    [0-9]+[.][0-9]*<EXPONENT>?[fFdD]?
  | [.][0-9]+<EXPONENT>?[fFdD]?
  | [0-9]+<EXPONENT>[fFdD]?
  | [0-9]+<EXPONENT>?[fFdD]
  ;")

;;; Parsing

;; TODO Verify this
(defsubst semantic-php-dim (id)
  "Split ID string into a pair (NAME . DIM).
NAME is ID without trailing brackets: \"[]\".
DIM is the dimension of NAME deduced from the number of trailing
brackets, or 0 if there is no trailing brackets."
  (let ((dim (string-match "\\(\\[]\\)+\\'" id)))
    (if dim
        (cons (substring id 0 dim)
              (/ (length (match-string 0 id)) 2))
      (cons id 0))))

;; TODO Verify this
(defsubst semantic-php-type (tag)
  "Return the type of TAG, taking care of array notation."
  (let ((type (semantic-tag-type tag))
        (dim  (semantic-tag-get-attribute tag :dereference)))
    (when dim
      (while (> dim 0)
        (setq type (concat type "[]")
              dim (1- dim))))
    type))

;; TODO Verify this, how about ::?
(defun semantic-php-expand-tag (tag)
  "Expand compound declarations found in TAG into separate tags.
TAG contains compound declarations when its class is `variable', and
its name is a list of elements (NAME START . END), where NAME is a
compound variable name, and START/END are the bounds of the
corresponding compound declaration."
  (let* ((class (semantic-tag-class tag))
         (elts (semantic-tag-name tag))
         dim type dim0 elt clone start end xpand)
    (cond
     ((and (eq class 'function)
           (> (cdr (setq dim (semantic-php-dim elts))) 0))
      (setq clone (semantic-tag-clone tag (car dim))
            xpand (cons clone xpand))
      (semantic-tag-put-attribute clone :dereference (cdr dim)))

     ((eq class 'variable)
      (or (consp elts) (setq elts (list (list elts))))
      (setq dim  (semantic-php-dim (semantic-tag-get-attribute tag :type))
            type (car dim)
            dim0 (cdr dim))
      (while elts
        ;; For each compound element, clone the initial tag with the
        ;; name and bounds of the compound variable declaration.
        (setq elt   (car elts)
              elts  (cdr elts)
              start (if elts  (cadr elt) (semantic-tag-start tag))
              end   (if xpand (cddr elt) (semantic-tag-end   tag))
              dim   (semantic-php-dim (car elt))
              clone (semantic-tag-clone tag (car dim))
              xpand (cons clone xpand))
        (semantic-tag-put-attribute clone :type type)
        (semantic-tag-put-attribute clone :dereference (+ dim0 (cdr dim)))
        (semantic-tag-set-bounds clone start end)))

     ((and (eq class 'type) (string-match "\\->" (semantic-tag-name tag)))
      ;; phpp outputs files where the package name is stuck onto the class or interface
      ;; name.  To make this more regular, we extract the package name into a package statement,
      ;; then make the class name regular.
      (let* ((name (semantic-tag-name tag))
             (rsplit (nreverse (split-string name "\\->" t)))
             (newclassname (car rsplit))
             (newpkg (mapconcat 'identity (reverse (cdr rsplit)) "->")))
        (semantic-tag-set-name tag newclassname)
        (setq xpand
              (list tag
                    (semantic-tag-new-package newpkg nil))))
      ))
    xpand))

;;; Environment
;; TODO Verify this
(defcustom-mode-local-semantic-dependency-system-include-path
  phps-mode semantic-php-dependency-system-include-path
  ;; @todo - Use JDEE to get at the include path, or something else?
  nil
  "The system include path used by PHP language.")

;; Local context
;; TODO Verify this
(define-mode-local-override semantic-ctxt-scoped-types
  phps-mode (&optional point)
  "Return a list of type names currently in scope at POINT."
  (mapcar 'semantic-tag-name
          (semantic-find-tags-by-class
           'type (semantic-find-tag-by-overlay point))))

;; Tag Protection
;; TODO Verify this
(define-mode-local-override semantic-tag-protection
  phps-mode (tag &optional parent)
  "Return the protection of TAG in PARENT.
Override function for `semantic-tag-protection'."
  (let ((prot (semantic-tag-protection-default tag parent)))
    (or prot 'package)))

;; Prototype handler
;;
(defun semantic-php-prototype-function (tag &optional parent color)
  "Return a function (method) prototype for TAG.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-format-tag-prototype'."
  (let ((name (semantic-tag-name tag))
        (type (semantic-php-type tag))
        (tmpl (semantic-tag-get-attribute tag :template-specifier))
        (args (semantic-tag-function-arguments tag))
        (argp "")
        arg argt)
    (while args
      (setq arg  (car args)
            args (cdr args))
      (if (semantic-tag-p arg)
          (setq argt (if color
                         (semantic--format-colorize-text
                          (semantic-php-type arg) 'type)
                       (semantic-php-type arg))
                argp (concat argp argt (if args "," "")))))
    (when color
      (when type
        (setq type (semantic--format-colorize-text type 'type)))
      (setq name (semantic--format-colorize-text name 'function)))
    (concat (or tmpl "") (if tmpl " " "")
            (or type "") (if type " " "")
            name "(" argp ")")))

(defun semantic-php-prototype-variable (tag &optional parent color)
  "Return a variable (field) prototype for TAG.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-format-tag-prototype'."
  (let ((name (semantic-tag-name tag))
        (type (semantic-php-type tag)))
    (concat (if color
                (semantic--format-colorize-text type 'type)
              type)
            " "
            (if color
                (semantic--format-colorize-text name 'variable)
              name))))

(defun semantic-php-prototype-type (tag &optional parent color)
  "Return a type (class/interface) prototype for TAG.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in.
See also `semantic-format-tag-prototype'."
  (let ((name (semantic-tag-name tag))
        (type (semantic-tag-type tag))
        (tmpl (semantic-tag-get-attribute tag :template-specifier)))
    (concat type " "
            (if color
                (semantic--format-colorize-text name 'type)
              name)
            (or tmpl ""))))

;; TODO Verify this
(define-mode-local-override semantic-format-tag-prototype
  phps-mode (tag &optional parent color)
  "Return a prototype for TOKEN.
Optional argument PARENT is a parent (containing) item.
Optional argument COLOR indicates that color should be mixed in."
  (let ((f (intern-soft (format "semantic-php-prototype-%s"
                                (semantic-tag-class tag)))))
    (funcall (if (fboundp f)
                 f
               'semantic-format-tag-prototype-default)
             tag parent color)))

(semantic-alias-obsolete 'semantic-php-prototype-nonterminal
                         'semantic-format-tag-prototype-phps-mode "23.2")

;; Include Tag Name
;;

;; Thanks Bruce Stephens
(define-mode-local-override semantic-tag-include-filename phps-mode (tag)
  "Return a suitable path for (some) PHP imports."
  (let ((name (semantic-tag-name tag)))
    (concat (mapconcat 'identity (split-string name "\\.") "/") ".php")))

;; Documentation handler

;; TODO Verify this
(defsubst semantic-php-skip-spaces-backward ()
  "Move point backward, skipping PHP whitespaces."
  (skip-chars-backward " \n\r\t"))

;; TODO Verify this
(defsubst semantic-php-skip-spaces-forward ()
  "Move point forward, skipping PHP whitespaces."
  (skip-chars-forward " \n\r\t"))

;; TODO Verify this
(define-mode-local-override semantic-documentation-for-tag
  phps-mode (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.
Php have documentation set in a comment preceding TAG's definition.
Attempt to strip out comment syntactic sugar, unless optional argument
NOSNARF is non-nil.
If NOSNARF is 'lex, then return the semantic lex token."
  (when (or tag (setq tag (semantic-current-tag)))
    (with-current-buffer (semantic-tag-buffer tag)
      (save-excursion
        ;; Move the point at token start
        (goto-char (semantic-tag-start tag))
        (semantic-php-skip-spaces-forward)
        ;; If the point already at "/**" (this occurs after a doc fix)
        (if (looking-at "/\\*\\*")
            nil
          ;; Skip previous spaces
          (semantic-php-skip-spaces-backward)
          ;; Ensure point is after "*/" (phpdoc block comment end)
          (condition-case nil
              (backward-char 2)
            (error nil))
          (when (looking-at "\\*/")
            ;; Move the point backward across the comment
            (forward-char 2)              ; return just after "*/"
            (forward-comment -1)          ; to skip the entire block
            ))
        ;; Verify the point is at "/**" (phpdoc block comment start)
        (if (looking-at "/\\*\\*")
            (let ((p (point))
                  (c (semantic-doc-snarf-comment-for-tag 'lex)))
              (when c
                ;; Verify that the token just following the doc
                ;; comment is the current one!
                (goto-char (semantic-lex-token-end c))
                (semantic-php-skip-spaces-forward)
                (when (eq tag (semantic-current-tag))
                  (goto-char p)
                  (semantic-doc-snarf-comment-for-tag nosnarf)))))
        ))))

;;; Phpdoc facilities

;; Phpdoc elements
;;
(defvar semantic-php-doc-line-tags nil
  "Valid phpdoc line tags.
Ordered following Sun's Tag Convention at
<http://php.sun.com/products/jdk/phpdoc/writingdoccomments/index.html>")

(defvar semantic-php-doc-with-name-tags nil
  "Phpdoc tags which have a name.")

(defvar semantic-php-doc-with-ref-tags nil
  "Phpdoc tags which have a reference.")

;; Optional phpdoc tags by classes of semantic tag
;;
(defvar semantic-php-doc-extra-type-tags nil
  "Optional tags used in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-php-doc-extra-function-tags nil
  "Optional tags used in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-php-doc-extra-variable-tags nil
  "Optional tags used in field documentation.
Ordered following Sun's Tag Convention.")

;; All phpdoc tags by classes of semantic tag
;;
(defvar semantic-php-doc-type-tags nil
  "Tags allowed in class/interface documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-php-doc-function-tags nil
  "Tags allowed in method/constructor documentation.
Ordered following Sun's Tag Convention.")

(defvar semantic-php-doc-variable-tags nil
  "Tags allowed in field documentation.
Ordered following Sun's Tag Convention.")

;; Access to Phpdoc elements
;;
(defmacro semantic-php-doc-tag (name)
  "Return doc tag from NAME.
That is @NAME."
  `(concat "@" ,name))

(defsubst semantic-php-doc-tag-name (tag)
  "Return name of the doc TAG symbol.
That is TAG `symbol-name' without the leading `@'."
  (substring (symbol-name tag) 1))

(defun semantic-php-doc-keyword-before-p (k1 k2)
  "Return non-nil if phpdoc keyword K1 is before K2."
  (let* ((t1   (semantic-php-doc-tag k1))
         (t2   (semantic-php-doc-tag k2))
         (seq1 (and (semantic-lex-keyword-p t1)
                    (plist-get (semantic-lex-keyword-get t1 'phpdoc)
                               'seq)))
         (seq2 (and (semantic-lex-keyword-p t2)
                    (plist-get (semantic-lex-keyword-get t2 'phpdoc)
                               'seq))))
    (if (and (numberp seq1) (numberp seq2))
        (<= seq1 seq2)
      ;; Unknown tags (probably custom ones) are always after official
      ;; ones and are not themselves ordered.
      (or (numberp seq1)
          (and (not seq1) (not seq2))))))

(defun semantic-php-doc-keywords-map (fun &optional property)
  "Run function FUN for each phpdoc keyword.
Return the list of FUN results.  If optional PROPERTY is non nil only
call FUN for phpdoc keywords which have a value for PROPERTY.  FUN
receives two arguments: the phpdoc keyword and its associated
'phpdoc property list.  It can return any value.  All nil values are
removed from the result list."
  (delq nil
        (mapcar
         #'(lambda (k)
             (let* ((tag   (semantic-php-doc-tag k))
                    (plist (semantic-lex-keyword-get tag 'phpdoc)))
               (if (or (not property) (plist-get plist property))
                   (funcall fun k plist))))
         semantic-php-doc-line-tags)))

;;; Mode setup
;;

(defun semantic-php-doc-setup ()
  "Lazy initialization of phpdoc elements."
  (or semantic-php-doc-line-tags
      (setq semantic-php-doc-line-tags
            (sort (mapcar #'semantic-php-doc-tag-name
                          (semantic-lex-keywords 'phpdoc))
                  #'semantic-php-doc-keyword-before-p)))

  (or semantic-php-doc-with-name-tags
      (setq semantic-php-doc-with-name-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-name)))

  (or semantic-php-doc-with-ref-tags
      (setq semantic-php-doc-with-ref-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 k)
             'with-ref)))

  (or semantic-php-doc-extra-type-tags
      (setq semantic-php-doc-extra-type-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-php-doc-extra-function-tags
      (setq semantic-php-doc-extra-function-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-php-doc-extra-variable-tags
      (setq semantic-php-doc-extra-variable-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k))
             'opt)))

  (or semantic-php-doc-type-tags
      (setq semantic-php-doc-type-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'type (plist-get p 'usage))
                     k)))))

  (or semantic-php-doc-function-tags
      (setq semantic-php-doc-function-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'function (plist-get p 'usage))
                     k)))))

  (or semantic-php-doc-variable-tags
      (setq semantic-php-doc-variable-tags
            (semantic-php-doc-keywords-map
             #'(lambda (k p)
                 (if (memq 'variable (plist-get p 'usage))
                     k)))))

  )

(provide 'phps-mode/semantic)

;;; phps-semantic.el ends here
