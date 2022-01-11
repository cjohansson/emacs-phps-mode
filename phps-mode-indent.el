;;; phps-mode-indent.el -- Indentation for phps-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.



;;; Commentary:


;;; Code:


(require 'phps-mode-macros)


;; General helper functions


(defun phps-mode-indent--string-indentation (string)
  "Count indentation of STRING."
  (if (string-match "\\(^[\t ]+\\)" string)
      (length (substring string (match-beginning 0) (match-end 0)))
    0))

(defun phps-mode-indent--backwards-looking-at (regexp)
  "Backward string if point is backwards looking at REGEXP, otherwise nil."
  (let ((point (point))
        (limit 100))
    (when (< point limit)
      (setq limit (1- point)))
    (let* ((start (- point limit))
           (backward-string
            (replace-regexp-in-string
             "\n"
             "\r"
             (buffer-substring-no-properties
              start
              (1+ point)))))
      (if (string-match regexp backward-string)
          backward-string
        nil))))

(defun phps-mode-indent--string-starts-with-regexp (string regexp &optional match-index)
  "If STRING start with REGEXP, return it, otherwise nil.  With optional MATCH-INDEX."
  (phps-mode-indent--string-match-regexp
   string
   (concat "^" regexp)
   match-index))

(defun phps-mode-indent--string-ends-with-regexp (string regexp &optional match-index)
  "If STRING end with REGEXP, return it, otherwise nil.  With optional MATCH-INDEX."
  (phps-mode-indent--string-match-regexp
   string
   (concat regexp "$")
   match-index))

(defun phps-mode-indent--string-match-regexp (string regexp &optional match-index)
  "If STRING match REGEXP, return it, otherwise nil.  With optional MATCH-INDEX."
  (unless match-index
    (setq match-index 0))
  (if
      (string-match regexp string)
      (match-string match-index string)
    nil))


;; Specific helper functions


(defun phps-mode-indent--string-starts-with-closing-bracket (string)
  "If STRING start with closing bracket, return it, otherwise return nil."
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*\\(<\\?php[\t\n ]*\\)?\\([\]})]\\)"
   2))

(defun phps-mode-indent--string-starts-with-opening-bracket (string)
  "If STRING start with opening bracket return it otherwise nil."
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*\\(<\\?php[\t\n ]*\\)?\\([\[{(]\\)"
   2))

(defun phps-mode-indent--string-starts-with-opening-doc-comment (string)
  "Does STRING start with opening doc comment?"
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*\\(<\\?php[\t\n ]*\\)?\\(/\\*\\*\\)"
   2))

(defun phps-mode-indent--string-ends-with-assignment (string)
  "If STRING end with terminus, return it, otherwise return nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\(=>?\\)[\t ]*\\(\\?>[\t\n ]*\\)?"
   1))

(defun phps-mode-indent--string-ends-with-closing-bracket (string)
  "If STRING end with closing bracket, return it, otherwise nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\([\]})]\\)[\t ]*\\(\\?>[\t\n ]*\\)?"
   1))

(defun phps-mode-indent--string-ends-with-closing-doc-comment (string)
  "If STRING end with closing doc comment, return it, otherwise nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\(\\*/\\)[\t ]*\\(\\?>[\t\n ]*\\)?"
   1))

(defun phps-mode-indent--string-ends-with-opening-bracket (string)
  "If STRING end with opening bracket, return it, otherwise nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\([\[{(]\\)[\t ]*\\(\\?>[\t\n ]*\\)?"
   1))

(defun phps-mode-indent--string-ends-with-terminus (string)
  "If STRING end with terminus, return it, otherwise return nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\(;\\|,\\)[\t ]*\\(\\?>[\t\n ]*\\)?"
   1))


;; Main functions


(defun phps-mode-indent-line (&optional initial-point)
  "Apply alternative indentation at INITIAL-POINT here."
  (let ((point))
    (if initial-point
        (setq point initial-point)
      (setq point (point)))

    (let ((new-indentation 0)
          (point-at-end-of-line (equal point (line-end-position))))
      (save-excursion
        (let ((move-length 0)
              (move-length1 0)
              (current-line-string "")
              (previous-line-string "")
              (previous-line-is-empty-p)
              (previous2-line-string ""))

          (when initial-point
            (goto-char point))
          (move-beginning-of-line nil)
          (setq point (point))

          ;; Current line is line at initial point
          (setq
           current-line-string
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position)))

          ;; (message "\nCurrent line: %S" current-line-string)

          ;; Try to find previous 2 non-empty lines
          (let ((line-is-empty-p t)
                (line-is-comment-p t)
                (searching-previous-lines 2))
            (while (and
                    (= (forward-line -1) 0)
                    (> searching-previous-lines 0))
              (beginning-of-line)
              (let ((line-string
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
                (setq
                 line-is-empty-p
                 (string-match-p
                  "^[ \t\f\r\n]*$"
                  line-string))
                (setq
                 line-is-comment-p
                 (string-match-p
                 "^[\t ]*\\(//\\|#\\)"
                 line-string))
                (unless (or
                         line-is-empty-p
                         line-is-comment-p)
                  (cond
                   ((= searching-previous-lines 2)
                    (setq
                     move-length1
                     (1+ move-length))
                    (setq
                     previous-line-string
                     line-string))
                   ((= searching-previous-lines 1)
                    (setq
                     previous2-line-string
                     line-string)))
                  (setq
                   searching-previous-lines
                   (1- searching-previous-lines))
                  (when (= searching-previous-lines 2)
                    (setq
                     previous-line-is-empty-p
                     line-is-empty-p)))
                (setq
                 move-length
                 (1+ move-length)))))
          (goto-char point)

          (if previous-line-is-empty-p
              (indent-line-to
               (phps-mode-indent--string-indentation
                previous-line-string))
            (let* ((previous-indentation
                    (phps-mode-indent--string-indentation
                     previous-line-string))
                   (current-line-starts-with-closing-bracket
                    (phps-mode-indent--string-starts-with-closing-bracket
                     current-line-string))
                   (current-line-starts-with-opening-bracket
                    (phps-mode-indent--string-starts-with-opening-bracket
                     current-line-string))
                   (current-line-ends-with-terminus
                    (phps-mode-indent--string-ends-with-terminus
                     current-line-string))
                   (previous-line-starts-with-closing-bracket
                    (phps-mode-indent--string-starts-with-closing-bracket
                     previous-line-string))
                   (previous-line-ends-with-closing-bracket
                    (phps-mode-indent--string-ends-with-closing-bracket
                     previous-line-string))
                   (previous-line-starts-with-opening-doc-comment
                    (phps-mode-indent--string-starts-with-opening-doc-comment
                     previous-line-string))
                   (previous-line-ends-with-closing-doc-comment
                    (phps-mode-indent--string-ends-with-closing-doc-comment
                     previous-line-string))
                   (previous-line-ends-with-assignment
                    (phps-mode-indent--string-ends-with-assignment
                     previous-line-string))
                   (previous-line-ends-with-opening-bracket
                    (phps-mode-indent--string-ends-with-opening-bracket
                     previous-line-string))
                   (previous-line-ends-with-terminus
                    (phps-mode-indent--string-ends-with-terminus
                     previous-line-string))
                   (previous-bracket-level
                    (phps-mode-indent--get-string-brackets-count
                     previous-line-string))
                   (match-type 'none))
              ;; (message "Previous non-empty line: %S with indentation: %S" previous-line-string old-indentation)
              ;; (message "previous-line-ends-with-terminus: %S" previous-line-ends-with-terminus)

              (setq
               new-indentation
               previous-indentation)

              ;; debug stuff
              (phps-mode-debug-message
               (message "\ncurrent-line-string: %S" current-line-string)
               (message "previous-line-string: %S" previous-line-string))

              ;; (message "current-line-starts-with-closing-bracket: %S" current-line-starts-with-closing-bracket)
              ;; (message "current-line-starts-with-opening-bracket: %S" current-line-starts-with-opening-bracket)
              ;; (message "previous-line-ends-with-closing-bracket: %S" previous-line-ends-with-closing-bracket)
              ;; (message "previous-line-ends-with-opening-bracket: %S" previous-line-ends-with-opening-bracket)
              ;; (message "previous-line-ends-with-terminus: %S" previous-line-ends-with-terminus)
              ;; (message "previous-bracket-level: %S" previous-bracket-level)
              ;; (message "previous-indentation: %S" previous-indentation)


              ;; Case by case logic below - most specific to most general

              (cond

               ;; LINE AFTER EXTENDS / IMPLEMENTS
               ;; class MyClass implements
               ;;     myInterface
               ;; or
               ;; class MyClass extends
               ;;     myParent
               ((string-match-p
                 "[\t ]+\\(extends\\|implements\\)$"
                 previous-line-string)
                (setq
                 match-type
                 'line-after-extends-or-implements)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER EXTENDS / IMPLEMENTS that starts on new line
               ;; class MyClass
               ;;     implements myInterface
               ;; or
               ;; class MyClass
               ;;     extends myParent
               ;; or
               ;; class MyClass
               ;;     extends myParent
               ;;     implements MyInterface
               ((string-match-p
                 "^[\t ]*\\(extends\\|implements\\)"
                 current-line-string)
                (setq
                 match-type
                 'line-after-extends-or-implements2)
                (when-let ((backwards-string
                            (phps-mode-indent--backwards-looking-at
                             "\r+\\([\t ]*\\)class[\r\t ]+[a-zA-Z0-9_]+[\r\t ]+\\(extends[\r\t ]+[a-zA-Z0-9_]+\\)?[\r\t ]*\\(implements[\r\t ]+[a-zA-Z0-9_]+\\)?$")))
                  (let ((old-indentation
                         (length
                          (match-string 1 backwards-string))))
                    (setq
                     new-indentation
                     (+ old-indentation tab-width)))))

               ;; CLASS BODY AFTER implements and extends
               ;; class MyClass implements
               ;;     myInterface,
               ;;     myInterface2
               ;; {
               ;; ignore case
               ;; class MyClass implements myInterface, myInterface2
               ;; {
               ((and
                 current-line-starts-with-opening-bracket
                 (string= current-line-starts-with-opening-bracket "{")
                 (phps-mode-indent--backwards-looking-at
                  "[\r\t ]*implements[\r\t ]+\\([\r\t ]*[\\a-zA-Z_0-9_]+,?\\)+[\r\t ]*{$")
                 (not
                  (string-match-p
                   "[\t ]*\\(class\\|interface\\)[\t ]+"
                   previous-line-string)))
                (setq
                 match-type
                 'class-body-after-extends-or-implements)
                (setq
                 new-indentation
                 (- new-indentation tab-width)))

               ;; LINE AFTER OPENING INLINE OR ALTERNATIVE CONTROL STRUCTURE
               ;; if (true)
               ;;     echo 'Something';
               ;; or
               ;; while (true)
               ;;     echo 'Something';
               ;; or
               ;; if (true):
               ;;     echo 'Something';
               ;; or
               ;; while (true):
               ;;     echo 'Something';
               ;; or
               ;; for ($i = 0; $i < 10; $i++):
               ;;     echo 'Something';
               ;; or
               ;; foreach ($array as $value):
               ;;     echo 'Something';
               ((and
                 current-line-ends-with-terminus
                 (string= current-line-ends-with-terminus ";")
                 (string-match-p
                  "^[\t ]*\\(if\\|while\\|for\\|foreach\\)[\t ]*(.+):?$"
                  previous-line-string))
                (setq
                 match-type
                 'line-after-inline-or-alternative-control-structure)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER INLINE OR ALTERNATIVE ELSE / ELSEIF CONTROL STRUCTURE
               ;; else
               ;;     echo 'Something';
               ;; or
               ;; else if (true)
               ;;     echo 'Something';
               ;; or
               ;; elseif (true)
               ;;     echo 'Something';
               ;; or
               ;; else:
               ;;     echo 'Something';
               ;; or
               ;; else if (true):
               ;;     echo 'Something';
               ;; or
               ;; elseif (true):
               ;;     echo 'Something';
               ((and
                 current-line-ends-with-terminus
                 (string=
                  current-line-ends-with-terminus
                  ";")
                 (string-match-p
                  "^[\t ]*else\\([\t ]*$\\|.*\\()\\|:\\)$\\)"
                  previous-line-string))
                (setq
                 match-type
                 'line-after-inline-or-alternative-else)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER LINE INSIDE INLINE OR ALTERNATIVE CONTROL STRUCTURE
               ;; if (true)
               ;;     echo 'Something';
               ;; else
               ;; or
               ;; if (true):
               ;;     echo 'Something';
               ;; else:
               ;; or
               ;; if (true)
               ;;     echo 'Something';
               ;; elseif (false)
               ;; or
               ;; if (true):
               ;;     echo 'Something';
               ;; elseif (false):
               ;; or
               ;; if (true):
               ;;     echo 'Something';
               ;; endif;
               ;; or
               ;; while (true):
               ;;     echo 'Something';
               ;; endwhile;
               ;; or
               ;; for ($i = 0; $i < 10; $i++):
               ;;     echo 'Something';
               ;; endfor;
               ;; or
               ;; foreach ($array as $value):
               ;;     echo 'Something';
               ;; endforeach;
               ((and
                 previous-line-ends-with-terminus
                 (string=
                  previous-line-ends-with-terminus
                  ";")
                 (string-match-p
                  "^[\t ]*\\(else:?[\t ]*$\\|else.*):?$\\|endif;[\t ]*$\\|endfor;[\t ]*$\\|endforeach;[\t ]*$\\|endwhile;[\t ]*$\\)"
                  current-line-string))
                (setq
                 match-type
                 'line-after-line-inside-inline-or-alternative-control-structure)
                (setq
                 new-indentation
                 (-
                  new-indentation
                  tab-width)))

               ;; LINE AFTER LINE INSIDE INLINE CONTROL STRUCTURE
               ;; if (true)
               ;;     echo 'Something';
               ;; else
               ;;     echo 'Something else';
               ;; echo true;
               ;; or
               ;; if (true)
               ;;     echo 'Something';
               ;; echo 'Something else';
               ;; or
               ;; when (true)
               ;;     echo 'Something';
               ;; echo 'Afterwards';
               ;; or
               ;; elseif (true)
               ;;     echo 'Something';
               ;; echo 'Afterwards';
               ((and
                 previous-line-ends-with-terminus
                 (string=
                  previous-line-ends-with-terminus
                  ";")
                 (string-match-p
                  "^[\t ]*\\(else[\t ]*$\\|else.*)[\t ]*$\\|if.*)$\\|while.*)$\\)"
                  previous2-line-string))
                (setq
                 match-type
                 'line-after-line-inside-inline-control-structure)
                (setq
                 new-indentation
                 (-
                  new-indentation
                  tab-width)))

               ;; TODO Add :: as well
               ;; LINE CONTINUING CHAINING OBJECT OPERATORS
               ;; $myObject->myFunction()
               ;;     ->myFunction2()
               ;; but ignore
               ;; $myObject->test(
               ;;     'here'
               ((and
                 (not previous-line-ends-with-opening-bracket)
                 (string-match-p
                  "->"
                  previous-line-string)
                 (string-match-p
                  "^[\t ]*->"
                  current-line-string))
                (setq
                 match-type
                 'line-continuing-object-operators)
                (let ((not-found t)
                      (started-chaining-on-this-line t)
                      (is-assignment)
                      (is-string-concatenation)
                      (is-bracket-less-command)
                      (is-same-line-p t))
                  (while
                      (and
                       not-found
                       (search-backward-regexp
                        "\\(;\\|{\\|(\\|)\\|=\\|->\\|echo[\t ]+\\|print[\t ]+\\|\n\\|^[\t ]*\\.\\|\\.[\t ]*$\\)"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((string=
                         "->"
                         match)
                        (setq
                         started-chaining-on-this-line
                         is-same-line-p))

                       ((string=
                         "\n"
                         match)
                        (setq
                         is-same-line-p
                         nil))

                       ((or
                         (string=
                          "echo"
                          match)
                         (string=
                          "print"
                          match))
                        (setq
                         is-bracket-less-command
                         t)
                        (setq
                         not-found
                         nil))

                       ((or
                         (string=
                          ";"
                          match)
                         (string=
                          "}"
                          match))
                        (setq
                         not-found
                         nil))

                       ((string=
                         "="
                         match)
                        (setq
                         is-assignment
                         t)
                        (setq
                         not-found
                         nil))

                       ((string-match-p
                         "\\(^[\t ]*\\.\\|\\.[\t ]*\\)$"
                         match)
                        (setq
                         is-string-concatenation
                         t)
                        (setq
                         not-found
                         nil))

                       )))

                  (when (and
                         (not is-assignment)
                         (not is-string-concatenation)
                         (not started-chaining-on-this-line)
                         (not is-bracket-less-command))
                    (setq
                     new-indentation
                     (+ new-indentation tab-width))))
                (goto-char point))

               ;; LINE AFTER OPENING HEREDOC/NOWDOC
               ;; echo <<<VAR
               ;; abc
               ;; or
               ;; echo <<<'VAR'
               ;; abc
               ;; or
               ;; echo <<<"VAR"
               ;; abc
               ((string-match-p
                 "<<<'?\"?[a-zA-Z0-9_]+'?\"?$"
                 previous-line-string)
                (setq
                 match-type
                 'line-after-opening-heredoc-nowdoc)
                (setq
                 new-indentation
                 0))

               ;; LINE AFTER STARTING MULTI-LINE CONCATENATING COMMAND
               ;; echo 'Something' .
               ;;     'something';
               ;; but ignore
               ;; print_r($object)
               ((and
                 (string-match-p
                  "^[\t ]*\\(echo\\|print$\\|print[\t ]+\\|return\\|die\\)"
                  previous-line-string)
                 (not
                  (string-match-p
                   ";[\t ]*$"
                   previous-line-string)))
                (setq
                 match-type
                 'line-after-starting-multiline-concatenating-command)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE THAT ENDS BRACKET
               ;; switch ($condition) {
               ;;     case true:
               ;;         echo 'here';
               ;; }
               (current-line-starts-with-closing-bracket
                (setq
                 match-type
                 'line-that-ends-bracket)
                (let ((old-point (point))
                      (still-looking t)
                      (bracket-start-line)
                      (bracket-level -1))

                  ;; Should keep track of brackets
                  ;; and stop when we reach the correct bracket
                  (while (and
                          still-looking
                          (search-backward-regexp "[][{}()]" nil t))
                    (let ((match (match-string-no-properties 0)))
                      (cond
                       ((or
                         (string= "{" match)
                         (string= "(" match)
                         (string= "[" match))
                        (setq
                         bracket-level
                         (1+ bracket-level)))
                       (t
                        (setq
                         bracket-level
                         (1- bracket-level))))

                      (when (= bracket-level 0)
                        (setq
                         still-looking
                         nil)
                        (setq
                         bracket-start-line
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))))

                  (goto-char old-point)

                  (unless still-looking
                    (setq
                     new-indentation
                     (phps-mode-indent--string-indentation
                      bracket-start-line)))))

               ;; LINE THAT ENDS ALTERNATIVE SWITCH BLOCK
               ;; switch (blala):
               ;;     case bla:
               ;;         echo 'bla';
               ;; endswitch;
               ((and
                 (string-match-p
                  "^[\t ]*endswitch[\t ]*;[\t ]*$"
                  current-line-string)
                 (not
                  (string-match-p
                   "^[\t ]*switch"
                   previous-line-string)))
                (setq
                 match-type
                 'line-that-ends-alternative-switch-block)
                (setq
                 new-indentation
                 (- new-indentation tab-width tab-width)))

               ;; NEW CASE AFTER CASE DEFINITION
               ;; case true:
               ;;     echo 'here';
               ;; case false:
               ;; or
               ;; case true:
               ;;     echo 'here';
               ;; default:
               ((and
                 (not previous-line-ends-with-opening-bracket)
                 (not (string-match-p ":[\t ]*$" previous-line-string))
                 (or
                  (string-match-p
                   "^[\t ]*case[\t ]+.*\\(;\\|:\\)[\t ]*$"
                   current-line-string)
                  (string-match-p
                   "^[\t ]*default.*\\(;\\|:\\)[\t ]*$"
                   current-line-string)))
                (setq
                 match-type
                 'line-after-case-definition)
                (setq
                 new-indentation
                 (- new-indentation tab-width)))

               ;; LINE AFTER LINE ENDING WITH COMMA
               ;; return array(
               ;;     '' => __(
               ;;         'None',
               ;;         'domain'
               ;;     ),
               ;;     '-' =>
               ;; or
               ;; return [
               ;;     [
               ;;         '25'
               ;;     ],
               ;;     25
               ;; or
               ;; if (myFunction(
               ;;     random(),
               ;;     heredom(),
               ;; or
               ;; 'name' =>
               ;;     $myObject->getName(),
               ;; 'age' =>
               ;; or
               ;; myFunction(
               ;;     'my Argument',
               ;;     'my second argument'
               ;; or
               ;; myFunction(
               ;;     [
               ;;         2,
               ;;         3,
               ((string-match-p
                 ",[\t ]*\\(\\?>[\t\n ]*\\)?$"
                 previous-line-string)
                (setq
                 match-type
                 'line-after-line-ending-with-comma)

                ;; Backtrack first to line were bracket started
                ;; and use indentation from that line from that line
                (forward-line (* -1 move-length1))
                (end-of-line)
                (search-backward-regexp "," nil t) ;; Skip trailing comma
                (let ((not-found-bracket-start t)
                      (reference-line)
                      (found-colon)
                      (reference-line-started-bracket)
                      (parenthesis-level 0))
                  (while
                      (and
                       not-found-bracket-start
                       (search-backward-regexp
                        "\\([][(),]\\|=>\\)"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((or
                         (string= "(" match)
                         (string= "[" match))
                        (setq
                         parenthesis-level
                         (1+ parenthesis-level))
                        (when (= parenthesis-level 1)
                          (unless found-colon
                            (setq
                             reference-line-started-bracket
                             t)
                            (setq
                             reference-line
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
                          (setq
                           not-found-bracket-start
                           nil)))

                       ((or
                         (string= ")" match)
                         (string= "]" match))
                        (setq
                         parenthesis-level
                         (1- parenthesis-level)))

                       ;; The second occurence of a colon
                       ;; is a significant marker of
                       ;; a starting bracket row
                       ((string= "," match)
                        (when (= parenthesis-level 0)
                          (if found-colon
                              (setq
                               not-found-bracket-start
                               nil)
                            (setq
                             found-colon
                             t)
                            (setq
                             reference-line
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))))

                       ;; The first occurrence of a =>
                       ;; is a significant marker of
                       ;; a starting bracket row
                       ((string= "=>" match)
                        (when (= parenthesis-level 0)
                          (setq
                           reference-line
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                          (setq
                           not-found-bracket-start
                           nil)))

                       )))

                  (when reference-line
                    ;; (message "reference-line-2: %S" reference-line)
                    (setq
                     new-indentation
                     (phps-mode-indent--string-indentation
                      reference-line))
                    (when reference-line-started-bracket
                      (let ((reference-bracket-level
                             (phps-mode-indent--get-string-brackets-count
                              reference-line)))
                        ;; (message "reference-bracket-level: %S" reference-bracket-level)
                        ;; define('_PRIVATE_ROOT',
                        ;;     'here');
                        ;; or
                        ;; ['abc',
                        ;;     'def'];
                        (when (> reference-bracket-level 0)
                          (setq
                           new-indentation
                           (+ new-indentation tab-width)))))
                    )

                  (when current-line-starts-with-closing-bracket
                    (setq
                     new-indentation
                     (- new-indentation tab-width))))

                (goto-char point))

               ;; LINE AFTER LINE THATS ENDS WITH SEMICOLON
               ;; $var .=
               ;;     'hello';
               ;; echo 'here';
               ;; or
               ;; $var =
               ;;     25;
               ;; echo 'here';
               ;; or
               ;; $var = array(
               ;;     'here'
               ;; );
               ;; echo 'here';
               ;; or
               ;;     $var = <<<EOD
               ;; OKASDOKASD
               ;; EOD;
               ;;     echo 'here';
               ;; or
               ;; $var = myFunction(
               ;;     'expression');
               ;; echo 'here';
               ;; or
               ;; return myFunction(
               ;;     'expression');
               ;; echo 'here';
               ;; or
               ;; define('_PRIVATE_ROOT_',
               ;;     'here');
               ;; echo 'here';
               ((and
                 previous-line-ends-with-terminus
                 (string= previous-line-ends-with-terminus ";"))
                (setq
                 match-type
                 'line-after-line-that-ends-with-semicolon)

                ;; Back-trace buffer from previous line semi-colon
                ;; find line where command started
                ;; use that lines indentation for this line
                (forward-line (* -1 move-length1))
                (end-of-line)
                (search-backward-regexp ";" nil t) ;; Skip the semi-colon

                (let ((not-found t)
                      (reference-line
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                      (reference-indentation))
                  (while
                      (and
                       not-found
                       (search-backward-regexp
                        "^[\t ]*[^\t ]+.*$"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ;; Commented out line
                       ((string-match-p
                         "^[\t ]*//"
                         match))

                       ;; A separate command
                       ((or
                         (string-match-p
                          "{[\t ]*$"
                          match)
                         (string-match-p
                          "\\(;\\|:\\)[\t ]*$"
                          match)
                         (string-match-p
                          "[\t ]*<\\?"
                          match))
                        (setq
                         not-found
                         nil))

                       (t
                        (setq
                         reference-line
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))

                       )))

                  (goto-char point)

                  (unless not-found
                    ;; (message "reference-line: %S" reference-line)
                    (setq
                     reference-indentation
                     (phps-mode-indent--string-indentation
                      reference-line))
                    (setq
                     new-indentation
                     reference-indentation))))

               ;; LINE AFTER ALTERNATIVE CASE DEFINITION
               ;; switch ($array):
               ;;     case 'Something';
               ((and
                 (string-match-p
                  "^[\t ]*\\(case.+\\|default\\)\\(;\\|:\\)[\t ]*$"
                  current-line-string)
                 (string-match-p
                  "^[\t ]*\\(switch\\)[\t ]*(.+):$"
                  previous-line-string))
                (setq
                 match-type
                 'line-after-alternative-case-definition)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER ENDING OF DOC-COMMENT
               ;; /**
               ;;  *
               ;;  */
               ;; echo 'here';
               ((= previous-bracket-level -1)
                (setq
                 match-type
                 'line-after-ending-of-doc-comment)
                (setq
                 new-indentation
                 (1- new-indentation)))

               ;; LINE AFTER STARTING DOC-COMMENT
               ;; /**
               ;;  *
               ((= previous-bracket-level 1)
                (setq
                 match-type
                 'line-after-opening-doc-comment)
                (setq
                 new-indentation
                 (+ new-indentation 1)))

               ;; LINE AFTER INCREASE IN BRACKETS
               ;; array(
               ;;     'here'
               ;; or
               ;; [[
               ;;     'here'
               ;; or
               ;; if (something) {
               ;;     echo 'here';
               ((>= previous-bracket-level tab-width)
                (setq
                 match-type
                 'line-after-increase-in-brackets)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER LINE THAT ENDS AND STARTS A BRACKET BLOCK
               ;; ) {
               ;;     echo 'here'
               ;; or
               ;; ][
               ;;     25
               ((and (= previous-bracket-level 0)
                     previous-line-starts-with-closing-bracket)
                (setq
                 match-type
                 'line-after-line-that-ends-and-starts-a-bracket-block)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE THAT STARTS WITH CLOSING BRACKET
               ;; [
               ;;     'hello'
               ;; ]
               ;; or
               ;; array(
               ;;     'hello'
               ;; )
               ;; but ignore
               ;; [
               ;; ]
               ;; or
               ;; array(
               ;; )
               ;; or
               ;; if (
               ;;     myFunction(
               ;;         'random')
               ;; ) {
               ;; but ignore
               ;; var_dump(array(<<<EOD
               ;; ölöas
               ;; EOD
               ;; ));
               ((and
                 current-line-starts-with-closing-bracket
                 (not previous-line-ends-with-opening-bracket))
                (setq
                 match-type
                 'line-that-starts-with-closing-bracket)

                ;; Backtrack to line were bracket started
                ;; and use indentation from that line for this line
                (forward-line (* -1 move-length1))
                (end-of-line)
                (let ((not-found t)
                      (reference-line)
                      (reference-indentation)
                      (parenthesis-level -1))
                  (while
                      (and
                       not-found
                       (search-backward-regexp
                        "[][(){}]"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((or
                         (string= "(" match)
                         (string= "[" match)
                         (string= "{" match))
                        (setq
                         parenthesis-level
                         (1+ parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found
                           nil)))

                       ((or
                         (string= ")" match)
                         (string= "]" match)
                         (string= "}" match))
                        (setq
                         parenthesis-level
                         (1- parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found
                           nil)))

                       )))
                  (unless not-found
                    (setq
                     reference-line
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
                    (setq
                     reference-indentation
                     (phps-mode-indent--string-indentation
                      reference-line)))

                  (goto-char point)

                  (when reference-indentation
                    (setq
                     new-indentation
                     reference-indentation))))

               ;; LINE AFTER LINE INSIDE DOC-COMMENT
               ;; /**
               ;;  * here
               ;; but ignore
               ;; /** */
               ;; here
               ((and
                 previous-line-starts-with-opening-doc-comment
                 (not previous-line-ends-with-closing-doc-comment))
                (setq
                 match-type
                 'line-after-line-inside-doc-comment)
                (setq
                 new-indentation
                 (+ new-indentation 1)))

               ;; LINE AFTER LINE THAT ENDS WITH ASSIGNMENT
               ;; $var =
               ((and
                 previous-line-ends-with-assignment
                 (<= previous-bracket-level 0))
                (setq
                 match-type
                 'line-after-line-that-ends-with-assignment)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER LINE THAT ENDS WITH OPENING BRACKET
               ;; )) {
               ;;     echo 'here';
               ;; or
               ;; ]][
               ;;     25
               ((and
                 previous-line-ends-with-opening-bracket
                 (< previous-bracket-level 0))
                (setq
                 match-type
                 'line-after-line-that-ends-with-opening-bracket)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER LINE THAT ENDS WITH CLOSING BRACKET
               ;; function myFunction($key,
               ;;     $value)
               ;; {
               ;; or
               ;; (is_array($data)
               ;;     && !empty($data['index'])
               ;;     && (is_a($data['index'], 'Index')
               ;;     || is_a($data['Index'], 'Index2')))
               ;; || is_a($data, 'WC_Index')
               (previous-line-ends-with-closing-bracket
                (setq
                 match-type
                 'line-after-line-that-ends-with-closing-bracket)
                ;; Backtrack to line were bracket started
                ;; and use indentation from that line for this line
                (forward-line (* -1 move-length1))
                (end-of-line)
                (let ((not-found t)
                      (reference-line)
                      (reference-line2)
                      (reference-indentation)
                      (parenthesis-level 0))
                  (while
                      (and
                       not-found
                       (search-backward-regexp
                        "[][(){}]"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((or
                         (string= "(" match)
                         (string= "[" match)
                         (string= "{" match))
                        (setq
                         parenthesis-level
                         (1+ parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found
                           nil)))

                       ((or
                         (string= ")" match)
                         (string= "]" match)
                         (string= "}" match))
                        (setq
                         parenthesis-level
                         (1- parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found
                           nil)))

                       )))
                  (unless not-found
                    (setq
                     reference-line
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
                    (setq
                     reference-line2
                     (buffer-substring-no-properties
                      (point)
                      (line-end-position)))
                    (setq
                     reference-indentation
                     (phps-mode-indent--string-indentation
                      reference-line))
                    (setq
                     new-indentation
                     reference-indentation)
                    (let ((reference-bracket-level
                           (phps-mode-indent--get-string-brackets-count
                            reference-line))
                          (reference-bracket-level2
                           (phps-mode-indent--get-string-brackets-count
                            reference-line2))
                          (reference-contains-assignment
                           (string-match-p
                            "^[\t ]*$[a-zA-Z0-9_]+[\t ]*[^=!]*=\\($\\|[\t ]+.*[^,;]$\\)"
                            reference-line)))
                      ;; if (
                      ;;     (is_array($data)
                      ;;     && !empty($data['index'])
                      ;;         && (is_a($data['index'], 'Index')
                      ;;         || is_a($data['Index'], 'Index2')))
                      ;;     || is_a($data, 'WC_Index')
                      ;; or
                      ;; $copies = method_exists($object, 'get_copies')
                      ;;     ? true
                      ;; (message "reference-bracket-level: %S" reference-bracket-level)
                      ;; (message "reference-bracket-level2: %S" reference-bracket-level2)
                      
                      (when (or
                             reference-contains-assignment
                             (and
                              (> reference-bracket-level 0)
                              (> reference-bracket-level reference-bracket-level2)))
                        (setq
                         new-indentation
                         (+ new-indentation tab-width))))

                    (when current-line-starts-with-closing-bracket
                      (setq
                       new-indentation
                       (- new-indentation tab-width)))

                    )

                  (goto-char point))

                )

               ;; LINE AFTER OPENING MULTI-LINE ASSIGNMENT
               ;; $var = 'A line' .
               ;;     'something';
               ;; or
               ;; $var .= 'A line' .
               ;;     'something'
               ;; or
               ;; $var += 35 +
               ;;     77
               ;; but ignore
               ;; $var === true
               ;; or
               ;; $var == 3
               ;; or
               ;; $argument1 = 3,
               ;; $argument2 = 4
               ;; or
               ;; function myFunction(
               ;;     $abc = 3
               ;; ) {
               ;; or
               ;; $abc != 3
               ((and
                 (string-match-p
                  "^[\t ]*$[a-zA-Z0-9_]+[\t ]*[^=!]*=\\($\\|[\t ]+.*[^,;]$\\)"
                  previous-line-string)
                 (not
                  current-line-starts-with-closing-bracket))
                (setq
                 match-type
                 'line-after-opening-multiline-assignment)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE THAT ENDS BRACKET AND COMMAND
               ;; $variable = array(
               ;;     'random' =>
               ;;         'hello'
               ;; );
               ;; or
               ;; $variable = [
               ;;     'random' =>
               ;;         'hello'
               ;; ];
               ((string-match-p
                 "^[\t ]*\\()\\|]\\);[\t ]*$"
                 current-line-string)
                (setq
                 match-type
                 'line-that-ends-bracket-and-command)
                (let ((old-point (point))
                      (still-looking t)
                      (bracket-count -1))

                  ;; Try to backtrack buffer until we reach start of bracket
                  (while
                      (and
                       still-looking
                       (search-backward-regexp
                        "\\((\\|]\\|\\[\\|)\\)" nil t))
                    (let ((match-string (match-string-no-properties 0)))
                      (cond
                       ((or
                         (string= match-string "(")
                         (string= match-string "["))
                        (setq bracket-count (1+ bracket-count)))
                       ((or
                         (string= match-string ")")
                         (string= match-string "]"))
                        (setq bracket-count (1- bracket-count)))))
                    (when (= bracket-count 0)
                      (setq still-looking nil)))

                  ;; Did we find bracket start line?
                  (unless still-looking
                    (let ((bracket-start-indentation
                           (phps-mode-indent--string-indentation
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
                      ;; Use its indentation for this line as well
                      (setq
                       new-indentation
                       bracket-start-indentation)))

                  ;; Reset point
                  (goto-char old-point)))

               ;; LINE THAT CONTINUES MULTI-LINE CONCATENATION
               ;; echo 'Something'
               ;;     . 'more';
               ;; or
               ;; echo
               ;;     'Something'
               ;;     . 'more';
               ;; or
               ;; echo 'Something' .
               ;;     'more';
               ;; or
               ;; echo
               ;;     'Something' .
               ;;     'more';
               ((or
                 (string-match-p
                  "^[\t ]*\\."
                  current-line-string)
                 (string-match-p
                  "\\.[\t ]*$"
                  previous-line-string))
                (setq
                 match-type
                 'line-that-continues-multi-line-concatenation)

                ;; If previous line matched ending .
                ;; we must backtrack at least two lines
                ;; to find a good reference indentation
                (let ((old-point (point))
                      (match-string)
                      (previous-concatenation)
                      (keep-searching 1)
                      (concat-was-trailing-p
                       (string-match-p
                        "\\.[\t ]*$"
                        previous-line-string))
                      (previous-concatenation2))
                  (when concat-was-trailing-p
                    (setq
                     keep-searching
                     2))
                  (while keep-searching
                    (let ((previous-expression
                           (search-backward-regexp
                            "\\(^[\t ]*\\.\\|\\.[\t ]*$\\|[{}=;]\\)" nil t)))
                      (if previous-expression
                          (progn
                            (setq
                             match-string
                             (match-string-no-properties 0))
                            (if (string-match-p
                                 "[{}=;]"
                                 match-string)
                                (setq
                                 keep-searching
                                 nil)
                              (setq
                               keep-searching
                               (1- keep-searching))
                              (when (= keep-searching 0)
                                (setq
                                 keep-searching
                                 nil)
                                (when concat-was-trailing-p
                                  (goto-char previous-concatenation2))
                                (setq
                                 previous-concatenation
                                 match-string))
                              (setq
                               previous-concatenation2
                               (point))))
                        (setq
                         keep-searching
                         nil))))

                  (if previous-concatenation
                      (let ((first-concatenated-line-indent
                             (phps-mode-indent--string-indentation
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))
                        ;; We use previous concatenated lines indent
                        (setq
                         new-indentation
                         first-concatenated-line-indent)))

                  ;; Reset point
                  (goto-char old-point)))

               ;; LINE AFTER CASE DEFINITION
               ;; case true:
               ;;     echo 'here';
               ;; or
               ;; case true;
               ;;     echo 'here';
               ;; or
               ;; default:
               ;;     echo 'here';
               ;; or
               ;; default;
               ;;     echo 'here';
               ((and
                 (not
                  (string-match-p
                   "^[\t ]*\\(case[\t ]+\\|default\\)"
                   current-line-string))
                 (or
                  (string-match-p
                   "^[\t ]*case[\t ]+.*\\(;\\|:\\)[\t ]*$"
                   previous-line-string)
                  (string-match-p
                   "^[\t ]*default.*\\(;\\|:\\)[\t ]*$"
                   previous-line-string)))
                (setq
                 match-type
                 'line-after-case-definition)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; LINE AFTER ENDING OF BRACKET AND COMMAND
               ;; if (true) {
               ;;     $cacheKey = sprintf(
               ;;         'key_%s',
               ;;         md5(json_encode($key))
               ;;     );
               ;;     $cache =
               ;; or
               ;; if (true) {
               ;;     $cache =
               ;;         Cache::getInstance();
               ;;     echo 'here';
               ((string-match-p
                 "[])][\t ]*;[\t ]*\\(\\?>[\t\n ]*\\)?$"
                 previous-line-string)
                (setq
                 match-type
                 'line-after-ending-of-bracket-and-command)

                ;; Backtrack first to line were bracket started
                ;; and then backwards until the line were statement / expression
                ;; started and use indentation from that line from that line
                (forward-line (* -1 move-length1))
                (end-of-line)
                (search-backward-regexp ";" nil t) ;; Skip trailing comma
                (let ((not-found-bracket-start t)
                      (reference-line)
                      (parenthesis-level 0))
                  (while
                      (and
                       not-found-bracket-start
                       (search-backward-regexp
                        "[][()]"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((or
                         (string= "(" match)
                         (string= "[" match))
                        (setq
                         parenthesis-level
                         (1+ parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found-bracket-start
                           nil)))

                       ((or
                         (string= ")" match)
                         (string= "]" match))
                        (setq
                         parenthesis-level
                         (1- parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           not-found-bracket-start
                           nil)))

                       )))

                  ;; Found line were bracket started?
                  (unless not-found-bracket-start
                    (setq
                     reference-line
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
                    ;; (message "reference-line-1: %S" reference-line)

                    ;; Search for first line of statement / expression here
                    (let ((not-found-command-start t))
                      (while
                          (and
                           not-found-command-start
                           (search-backward-regexp
                            "\\(;\\|}\\|{\\|^[\t ]*[^\t\n ]+.*$\\)"
                            nil
                            t))
                        (let ((match (match-string-no-properties 1)))
                          ;; (message "match: %S" match)
                          (cond

                           ;; End of expression / statement
                           ((or
                             (string= ";" match)
                             (string= "}" match)
                             (string= "{" match))
                            (setq
                             not-found-command-start
                             nil))

                           ;; Non-empty line
                           (t
                            (setq
                             reference-line
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))

                           )))))

                  (when reference-line
                    ;; (message "reference-line-2: %S" reference-line)
                    (setq
                     new-indentation
                     (phps-mode-indent--string-indentation
                      reference-line)))

                  (when
                      (and
                       current-line-starts-with-closing-bracket
                       (string= current-line-starts-with-closing-bracket "}"))
                    (setq
                     new-indentation
                     (- new-indentation tab-width))))

                (goto-char point))

               )

              (phps-mode-debug-message
               (message "new-indentation: %S" new-indentation)
               (message "match-type: %S" match-type))

              (when (< new-indentation 0)
                (setq
                 new-indentation
                 0))

              (indent-line-to new-indentation)))))
      ;; Only move to end of line if point is the current point and is at end of line
      (when (equal point (point))
        (if point-at-end-of-line
            (end-of-line)
          (back-to-indentation)))
      new-indentation)))

(defun phps-mode-indent--get-string-brackets-count
    (string &optional html-mode)
  "Get bracket count for STRING optionally in HTML-MODE."
  (let ((bracket-level 0)
        (start 0)
        (line-is-empty
         (string-match-p "^[ \t\f\r\n]*$" string))
        (test-string "\\([\]{}()[]\\|^[\t ]/\\*\\*\\|^[\t\\* ]*\\*/\\)"))
    (when html-mode
      (setq
       test-string
       "\\([\]{}()[]\\|<[a-zA-Z]+\\|</[a-zA-Z]+\\|/>\\|^[\t ]/\\*\\*\\|^[\t\\* ]*\\*/\\)"))
    (unless line-is-empty
      ;; (message "string: %S" string)
      (while
          (string-match
           test-string
           string
           start)
        (setq
         start
         (match-end 0))
        (let ((bracket (substring string (match-beginning 0) (match-end 0))))
          ;; (message "bracket: %S from %S" bracket string)
          (cond
           ((or
             (string= bracket "{")
             (string= bracket "[")
             (string= bracket "(")
             (string= bracket "<"))
            (setq bracket-level (+ bracket-level tab-width)))
           ((string-match "^[\t\\* ]*\\*/" bracket)
            (setq bracket-level (- bracket-level 1)))
           ((string-match "^[\t ]/\\*\\*" bracket)
            (setq bracket-level (+ bracket-level 1)))
           (t
            (setq bracket-level (- bracket-level tab-width)))))))
    bracket-level))


(provide 'phps-mode-indent)

;;; phps-mode-indent.el ends here
