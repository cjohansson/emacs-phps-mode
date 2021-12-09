;;; phps-mode-indent.el -- Indentation for phps-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.



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
            (buffer-substring-no-properties
             start
             (1+ point))))
      (if (string-match regexp backward-string)
          backward-string
        nil))))

(defun phps-mode-indent--string-starts-with-regexp (string regexp)
  "If STRING start with REGEXP, return it, otherwise nil."
  (phps-mode-indent--string-match-regexp
   string
   (concat "^" regexp)))

(defun phps-mode-indent--string-ends-with-regexp (string regexp)
  "If STRING end with REGEXP, return it, otherwise nil."
  (phps-mode-indent--string-match-regexp
   string
   (concat regexp "$")))

(defun phps-mode-indent--string-match-regexp (string regexp)
  "If STRING match REGEXP, return it, otherwise nil."
  (if
      (string-match regexp string)
      (match-string 0 string)
    nil))


;; Specific helper functions


(defun phps-mode-indent--string-starts-with-closing-bracket (string)
  "If STRING start with closing bracket, return it, otherwise return nil."
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*\\([\]})[]\\)"))

(defun phps-mode-indent--string-starts-with-opening-bracket (string)
  "If STRING start with opening bracket return it otherwise nil."
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*\\([\[{(]\\)"))

(defun phps-mode-indent--string-starts-with-opening-doc-comment (string)
  "Does STRING start with opening doc comment?"
  (phps-mode-indent--string-starts-with-regexp
   string
   "[\t ]*/\\*\\*"))

(defun phps-mode-indent--string-ends-with-assignment (string)
  "If STRING end with terminus, return it, otherwise return nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "=>?[\t ]*"))

(defun phps-mode-indent--string-ends-with-closing-bracket (string)
  "If STRING end with closing bracket, return it, otherwise nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\([\]})[]\\)[\t ]*"))

(defun phps-mode-indent--string-ends-with-opening-bracket (string)
  "If STRING end with opening bracket, return it, otherwise nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\([\[{(]\\)[\t ]*"))

(defun phps-mode-indent--string-ends-with-terminus (string)
  "If STRING end with terminus, return it, otherwise return nil."
  (phps-mode-indent--string-ends-with-regexp
   string
   "\\(;\\|,\\)[\t ]*"))


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
              (current-line-string "")
              (previous-line-string "")
              (previous-line-is-empty-p)
              (previous2-line-string ""))

          (when initial-point
            (goto-char point))

          ;; Current line is line at initial point
          (setq
           current-line-string
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position)))

          ;; (message "\nCurrent line: %S" current-line-string)

          ;; TODO Try to find previous 2 non-empty lines
          (let ((line-is-empty-p t)
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
                (unless line-is-empty-p
                  (cond
                   ((= searching-previous-lines 2)
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
                  (when (= searching-previous-lines 1)
                    (setq
                     previous-line-is-empty-p
                     line-is-empty-p)))
                (setq
                 move-length
                 (1+ move-length)))))
          ;; (message "previous-line-string: %S" previous-line-string)
          ;; (message "previous2-line-string: %S" previous2-line-string)

          (if previous-line-is-empty-p
              (indent-line-to 0)
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

              (setq new-indentation previous-indentation)
              (goto-char point)

              ;; class MyClass implements
              ;;     myInterface
              ;; or
              ;; class MyClass extends
              ;;     myParent
              (when (string-match-p "[\t ]+\\(extends\\|implements\\)$" previous-line-string)
                (setq previous-bracket-level (+ tab-width)))

              ;; class MyClass
              ;;     implements myInterface
              ;; or
              ;; class MyClass
              ;;     extends myParent
              ;; or
              ;; class MyClass
              ;;     extends myParent
              ;;     implements MyInterface
              (when
                  (string-match "^[\t ]*\\(extends\\|implements\\)" current-line-string)
                (when-let ((backwards-string
                            (phps-mode-indent--backwards-looking-at
                             "\\([\t ]*\\)class[\t ]+[a-zA-Z0-9_]+[\n\t ]+\\(extends[\t ]+[a-zA-Z0-9_]+\\)?[\n\t ]*\\(implements[\t ]+[a-zA-Z0-9_]+\\)?")))
                  (let ((old-indentation (length (match-string 1 backwards-string))))
                    (setq new-indentation (+ old-indentation tab-width)))))

              ;; class MyClass implements
              ;;     myInterface,
              ;;     myInterface2
              ;; {
              (when (and
                     current-line-starts-with-opening-bracket
                     (string= current-line-starts-with-opening-bracket "{")
                     (phps-mode-indent--backwards-looking-at
                      "[\t ]*implements[\n\t ]+\\([\n\t ]*[a-zA-Z_0-9]+,?\\)+[\n\t ]*{$"))
                (setq new-indentation (- new-indentation tab-width)))

              ;; if (true)
              ;;     echo 'Something';
              (when (and
                     (not current-line-starts-with-closing-bracket)
                     previous-line-ends-with-closing-bracket
                     (string= previous-line-ends-with-closing-bracket ")")
                     (string-match-p "^[\t ]*\\(if\\|while\\)[\t ]*(" previous-line-string))
                (setq new-indentation (+ new-indentation tab-width)))

              ;; else
              ;;     echo 'Something';
              ;; or
              ;; else if (true)
              ;;     echo 'Something';
              (when (and
                     (phps-mode-indent--string-starts-with-regexp
                      previous-line-string
                      "[\t ]*else")
                     (not
                      (phps-mode-indent--string-ends-with-regexp
                       previous-line-string
                       "{[\t ]*")))
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     previous-line-ends-with-terminus
                     (string= previous-line-ends-with-terminus ";"))

                ;; if (true)
                ;;     echo 'Something';
                ;; else
                (when (phps-mode-indent--string-starts-with-regexp
                       current-line-string "[\t ]*else")
                  (setq new-indentation (- new-indentation tab-width)))

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
                (when (and
                       (not
                        (phps-mode-indent--string-ends-with-regexp
                         previous2-line-string "{[\t ]*"))
                       (or
                        (phps-mode-indent--string-starts-with-regexp
                         previous2-line-string "[\t ]*else")
                        (phps-mode-indent--string-starts-with-regexp
                         previous2-line-string "[\t ]*if[\t ]*(")
                        (phps-mode-indent--string-starts-with-regexp
                         previous2-line-string "[\t ]*while[\t ]*(")))
                  (setq new-indentation (- new-indentation tab-width)))

                )

              (when (> previous-bracket-level 0)
                (if (< previous-bracket-level tab-width)
                    (setq new-indentation (+ new-indentation 1))
                  (setq new-indentation (+ new-indentation tab-width))))

              (when (= previous-bracket-level -1)
                (setq new-indentation (1- new-indentation)))

              (when (and (= previous-bracket-level 0)
                         previous-line-starts-with-closing-bracket)
                (setq new-indentation (+ new-indentation tab-width)))

              (when current-line-starts-with-closing-bracket
                (setq new-indentation (- new-indentation tab-width)))

              (when previous-line-starts-with-opening-doc-comment
                (setq new-indentation (+ new-indentation 1)))

              (when (and
                     previous-line-ends-with-assignment
                     (<= previous-bracket-level 0))
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     previous-line-ends-with-opening-bracket
                     (< previous-bracket-level 0))
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     previous-line-ends-with-terminus
                     (not (string-match-p "^[\t ]*\\(echo[\t ]+\\|print[\t ]+\\)" previous-line-string)))
                ;; Back-trace buffer from previous line
                ;; Determine if semi-colon ended an multi-line assignment or bracket-less command or not
                ;; If it's on the same line we ignore it
                (forward-line (* -1 move-length))
                (end-of-line)
                (forward-char -1)
                (let ((not-found t)
                      (is-assignment nil)
                      (parenthesis-level 0)
                      (is-bracket-less-command nil))
                  (while (and
                          not-found
                          (search-backward-regexp "\\(;\\|{\\|(\\|)\\|=\\|echo[\t ]+\\|print[\t ]+\\)" nil t))
                    (let ((match (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
                      (when (string= match ")")
                        (setq parenthesis-level (1- parenthesis-level)))
                      (when (= parenthesis-level 0)
                        (setq is-assignment (string= match "="))
                        (setq is-bracket-less-command
                              (string-match-p
                               "\\(echo[\t ]+\\|print[\t ]+\\)"
                               match))
                        (setq not-found nil))

                      (when (string= match "(")
                        (setq parenthesis-level (1+ parenthesis-level)))))

                  ;; If it ended an assignment on a previous line, decrease indentation
                  (when
                      (and
                       (or
                        (and
                         is-assignment
                         (> previous-bracket-level -1))
                        is-bracket-less-command))

                    ;; NOTE stuff like $var = array(\n    4\n);\n
                    ;; will end assignment but also decrease previous-bracket-level
                    (setq new-indentation (- new-indentation tab-width))))

                (goto-char point))

              ;; Decrease indentation if current line decreases in bracket level
              (when (< new-indentation 0)
                (setq new-indentation 0))
              ;; (message "new-indentation: %S previous-bracket-level: %S old-indentation: %S" new-indentation previous-bracket-level old-indentation)

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
