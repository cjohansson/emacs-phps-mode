;;; phps-mode-indent.el -- Indentation for phps-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.



;;; Commentary:


;;; Code:


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
                     previous-line-string)))
              ;; (message "Previous non-empty line: %S with indentation: %S" previous-line-string old-indentation)
              ;; (message "previous-line-ends-with-terminus: %S" previous-line-ends-with-terminus)

              (setq
               new-indentation
               previous-indentation)

              ;; debug stuff
              ;; (message "\ncurrent-line-string: %S" current-line-string)
              ;; (message "previous-line-string: %S" previous-line-string)

              ;; (message "current-line-starts-with-closing-bracket: %S" current-line-starts-with-closing-bracket)
              ;; (message "current-line-starts-with-opening-bracket: %S" current-line-starts-with-opening-bracket)
              ;; (message "previous-line-ends-with-closing-bracket: %S" previous-line-ends-with-closing-bracket)
              ;; (message "previous-line-ends-with-opening-bracket: %S" previous-line-ends-with-opening-bracket)
              ;; (message "previous-line-ends-with-terminus: %S" previous-line-ends-with-terminus)
              ;; (message "previous-bracket-level: %S" previous-bracket-level)
              ;; (message "previous-indentation: %S" previous-indentation)


              ;; Case by case logic below - most specific to most general

              (cond

               ;; class MyClass implements
               ;;     myInterface
               ;; or
               ;; class MyClass extends
               ;;     myParent
               ((string-match-p
                 "[\t ]+\\(extends\\|implements\\)$"
                 previous-line-string)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

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
                (when-let ((backwards-string
                            (phps-mode-indent--backwards-looking-at
                             "\r+\\([\t ]*\\)class[\r\t ]+[a-zA-Z0-9_]+[\r\t ]+\\(extends[\r\t ]+[a-zA-Z0-9_]+\\)?[\r\t ]*\\(implements[\r\t ]+[a-zA-Z0-9_]+\\)?$")))
                  (let ((old-indentation
                         (length
                          (match-string 1 backwards-string))))
                    (setq
                     new-indentation
                     (+ old-indentation tab-width)))))

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
                 new-indentation
                 (- new-indentation tab-width)))

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
                (setq new-indentation
                      (+ new-indentation tab-width)))

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
                 new-indentation
                 (+ new-indentation tab-width)))

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
                 new-indentation
                 (-
                  new-indentation
                  tab-width)))

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
                 new-indentation
                 (-
                  new-indentation
                  tab-width)))

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
                 new-indentation
                 0))

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
                 new-indentation
                 (+ new-indentation tab-width)))

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
                            reference-line2)))
                      ;; if (
                      ;;     (is_array($data)
                      ;;     && !empty($data['index'])
                      ;;         && (is_a($data['index'], 'Index')
                      ;;         || is_a($data['Index'], 'Index2')))
                      ;;     || is_a($data, 'WC_Index')
                      ;; (message "reference-bracket-level: %S" reference-bracket-level)
                      ;; (message "reference-bracket-level2: %S" reference-bracket-level2)
                      (when (and
                             (> reference-bracket-level 0)
                             (> reference-bracket-level reference-bracket-level2))
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
                 new-indentation
                 (+ new-indentation tab-width)))

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
                 new-indentation
                 (+ new-indentation tab-width)))

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
                 new-indentation
                 (- new-indentation tab-width)))

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
                            "\\(;\\|}\\|{\\|^[\t ]*[^\t\n ]+[\t ]*$\\)"
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

               ;; switch ($condition) {
               ;;     case true:
               ;;         echo 'here';
               ;; }
               ((and
                 current-line-starts-with-closing-bracket
                 (string= current-line-starts-with-closing-bracket "}"))
                (let ((old-point (point))
                      (end-of-switch-statement)
                      (still-looking t)
                      (curly-bracket-balance -1))

                  ;; Should keep track of brackets
                  ;; and stop when we reach the correct bracket
                  (while (and
                          still-looking
                          (search-backward-regexp "[{}]" nil t))
                    (cond
                     ((looking-at-p "{")
                      (setq
                       curly-bracket-balance
                       (1+ curly-bracket-balance)))
                     ((looking-at-p "}")
                      (setq
                       curly-bracket-balance
                       (1- curly-bracket-balance))))

                    (when (= curly-bracket-balance 0)
                      (setq
                       still-looking
                       nil)
                      (let ((bracket-start-line
                             (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))
                        (when (string-match-p
                               "^[\t ]*switch[\t ]*("
                               bracket-start-line)
                          (setq
                           end-of-switch-statement
                           t)))))

                  (goto-char old-point)

                  ;; Ignore cases like
                  ;; if (true) {
                  ;; }
                  ;; or
                  ;; switch($var) {
                  ;; }
                  (unless previous-line-ends-with-opening-bracket

                    ;; if (true) {
                    ;;     echo 'here';
                    (setq
                     new-indentation
                     (- new-indentation tab-width))

                    ;; switch($match) {
                    ;;     case 'here':
                    ;;         echo 'there';
                    ;; }
                    (when end-of-switch-statement
                      (setq
                       new-indentation
                       (- new-indentation tab-width)))

                    ;; should indent double if previous
                    ;; line ended a multi-line assignment:
                    ;; if (true) {
                    ;;     $var =
                    ;;         'abc';
                    ;; }
                    (when (and
                           previous-line-ends-with-terminus
                           (string= previous-line-ends-with-terminus ";")
                           (not
                            (string-match-p
                             "^[\t ]*\\(echo[\t ]+\\|print[\t ]+\\)"
                             previous-line-string)))
                      ;; Back-trace buffer from previous line
                      ;; Determine if semi-colon ended an multi-line assignment or bracket-less command or not
                      ;; If it's on the same line we ignore it
                      (forward-line (* -1 move-length1))
                      (end-of-line)
                      (search-backward-regexp ";" nil t) ;; Skip the semi-colon

                      (let ((not-found t)
                            (is-assignment nil)
                            (parenthesis-level 0)
                            (is-same-line-p t)
                            (is-object-chaining)
                            (is-object-chaining-on-same-line))
                        (while
                            (and
                             not-found
                             (search-backward-regexp
                              "\\(;\\|{\\|(\\|)\\|=\\|echo[\t ]+\\|print[\t ]+\\|\n\\|<<<'?\"?[a-zA-Z0-9_]+'?\"?\\|->\\)"
                              nil
                              t))
                          (let ((match (match-string-no-properties 0)))
                            (cond
                             ((string= match "\n")
                              (setq is-same-line-p nil))
                             ((string-match-p
                               "<<<'?\"?[a-zA-Z0-9_]+'?\"?"
                               match)
                              (setq
                               not-found
                               nil))
                             ((string= match "(")
                              (setq
                               parenthesis-level
                               (1+ parenthesis-level)))
                             ((string= match ")")
                              (setq
                               parenthesis-level
                               (1- parenthesis-level)))
                             ((string= match "->")
                              (when (= parenthesis-level 0)
                                (setq
                                 is-object-chaining
                                 t)
                                (setq
                                 is-object-chaining-on-same-line
                                 is-same-line-p)))
                             ((= parenthesis-level 0)
                              (setq is-assignment (string= match "="))
                              (setq not-found nil)))))

                        (when (or
                               (and
                                (not is-same-line-p)
                                is-assignment)
                               (and
                                (not is-object-chaining-on-same-line)
                                is-object-chaining))
                          (setq
                           new-indentation
                           (- new-indentation tab-width)))

                        (goto-char point)))

                    )))

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
                 new-indentation
                 (- new-indentation tab-width tab-width)))

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
                 (string= previous-line-ends-with-terminus ";")
                 (not
                  (string-match-p
                   "^[\t ]*\\(echo[\t ]+\\|print[\t ]+\\)"
                   previous-line-string)))

                ;; Back-trace buffer from previous line
                ;; Determine if semi-colon ended an multi-line assignment or bracket-less command or not
                ;; If it's on the same line we ignore it
                (forward-line (* -1 move-length1))
                (end-of-line)
                (search-backward-regexp ";" nil t) ;; Skip the semi-colon

                (let ((not-found t)
                      (is-assignment nil)
                      (is-string-doc)
                      (is-function-call)
                      (parenthesis-level 0)
                      (is-bracket-less-command nil)
                      (is-same-line-p t)
                      (bracket-opened-on-first-line))
                  (while
                      (and
                       not-found
                       (search-backward-regexp
                        "\\(;\\|{\\|[a-zA-Z_]+[a-zA-Z0-9_]*[\t ]*(\\|)\\|=$\\|=[^>]\\|return\\|echo[\t ]+\\|print[\t ]+\\|\n\\|<<<'?\"?[a-zA-Z0-9_]+'?\"?\\)"
                        nil
                        t))
                    (let ((match (match-string-no-properties 0)))
                      (cond

                       ((string= match "\n")
                        (setq is-same-line-p nil))

                       ((string-match-p
                         "<<<'?\"?[a-zA-Z0-9_]+'?\"?"
                         match)
                        (setq
                         is-string-doc
                         t)
                        (setq
                         not-found
                         nil))

                       ((string-match-p
                         "[a-zA-Z_]+[a-zA-Z0-9_]*[\t ]*("
                         match)
                        (setq
                         parenthesis-level
                         (1+ parenthesis-level))
                        (when (= parenthesis-level 0)
                          (setq
                           is-function-call
                           t)
                          (setq
                           not-found
                           nil)))

                       ((string= match ")")
                        (setq
                         parenthesis-level
                         (1- parenthesis-level)))

                       ((= parenthesis-level 0)
                        (setq is-assignment (string-match-p "=" match))
                        (setq is-bracket-less-command
                              (string-match-p
                               "\\(echo[\t ]+\\|print[\t ]+\\|return[\t ]+\\)"
                               match))
                        (setq not-found nil)))))

                  ;;     $var = <<<EOD
                  ;; OKASDOKASD
                  ;; EOD;
                  ;;     echo 'here';
                  (when is-string-doc
                    (setq
                     new-indentation
                     (phps-mode-indent--string-indentation
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position)))))

                  ;; When we have an assignment
                  ;; keep track if bracket was opened on first
                  ;; line
                  (when is-assignment
                    (let ((start-bracket-count
                           (phps-mode-indent--get-string-brackets-count
                            (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position)))))
                      ;; (message "start-bracket-count: %S from %S" start-bracket-count (buffer-substring-no-properties
                      ;;        (line-beginning-position)
                      ;;        (line-end-position)))
                      (setq
                       bracket-opened-on-first-line
                       (> start-bracket-count 0))))

                  ;; (message "is-assignment: %S" is-assignment)
                  ;; (message "bracket-opened-on-first-line: %S" bracket-opened-on-first-line)

                  ;; echo 'there' .
                  ;;     'here';
                  ;; echo 'here';
                  ;; or
                  ;; print 'there' .
                  ;;     'here';
                  ;; echo 'here';
                  ;; or
                  ;; $var =
                  ;;     'opkeokoek';
                  ;; echo 'here'

                  ;; ignore cases like
                  ;; $var = array(
                  ;;     'here'
                  ;; );
                  ;; echo 'here';
                  ;; but not cases like
                  ;; $var = 'abc'
                  ;;     . 'def' . __(
                  ;;         'okeoke'
                  ;;     ) . 'ere';
                  ;; echo 'here';
                  ;; NOTE stuff like $var = array(\n    4\n);\n
                  ;; will end assignment but also decrease previous-bracket-level
                  ;; NOTE but cases like $var = array(\n    4);\n should pass
                  (when (and
                         (not is-same-line-p)
                         (or
                          (and
                           is-assignment
                           (or
                            (not bracket-opened-on-first-line)
                            (not previous-line-starts-with-closing-bracket)))
                          is-bracket-less-command))
                    (setq
                     new-indentation
                     (- new-indentation tab-width)))

                  ;; define('_PRIVATE_ROOT',
                  ;;     'here');
                  ;; echo 'here';
                  ;; but ignore
                  ;; if (true) {
                  ;;     define('_PRIVATE_ROOT', 'here');
                  ;;     echo 'here';
                  (when (and
                         is-function-call
                         (not is-same-line-p))
                    (setq
                     new-indentation
                     (- new-indentation tab-width)))

                  )

                (goto-char point))

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
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; /**
               ;;  *
               ;;  */
               ;; echo 'here';
               ((= previous-bracket-level -1)
                (setq
                 new-indentation
                 (1- new-indentation)))

               ;; /**
               ;;  *
               ((= previous-bracket-level 1)
                (setq
                 new-indentation
                 (+ new-indentation 1)))

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
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; ) {
               ;;     echo 'here'
               ;; or
               ;; ][
               ;;     25
               ((and (= previous-bracket-level 0)
                     previous-line-starts-with-closing-bracket)
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

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

               ;; /**
               ;;  * here
               ;; but ignore
               ;; /** */
               ;; here
               ((and
                 previous-line-starts-with-opening-doc-comment
                 (not previous-line-ends-with-closing-doc-comment))
                (setq
                 new-indentation
                 (+ new-indentation 1)))

               ;; $var =
               ((and
                 previous-line-ends-with-assignment
                 (<= previous-bracket-level 0))
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               ;; )) {
               ;;     echo 'here';
               ;; or
               ;; ]][
               ;;     25
               ((and
                 previous-line-ends-with-opening-bracket
                 (< previous-bracket-level 0))
                (setq
                 new-indentation
                 (+ new-indentation tab-width)))

               )

              ;; (message "new-indentation: %S" new-indentation)

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
