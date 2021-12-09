;;; phps-mode-indent.el -- Indentation for phps-mode -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021  Free Software Foundation, Inc.



;;; Commentary:


;;; Code:


(defun phps-mode-indent--string-indentation (string)
  "Count indentation of STRING."
  (if (string-match "\\(^[\t ]+\\)" string)
      (length (substring string (match-beginning 0) (match-end 0)))
    0))

(defun phps-mode-indent--backwards-looking-at-p (regexp)
  "Non-nil if point is backwards looking at REGEXP."
  (let ((point (point))
        (limit 100))
    (when (< point limit)
      (setq limit (1- point)))
    (let* ((start (- point limit))
           (backward-string
            (buffer-substring-no-properties
             start
             (1+ point))))
      (string-match-p regexp backward-string))))

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
              (line-is-empty-p t)
              (line-beginning-position)
              (line-end-position)
              (line-string)
              (current-line-string))

          (when initial-point
            (goto-char point))

          ;; Current line is line at initial point
          (setq
           current-line-string
           (buffer-substring-no-properties
            (line-beginning-position)
            (line-end-position)))

          (message "\nCurrent line: %S" current-line-string)

          ;; Try to find previous non-empty line
          (while (and
                  (= (forward-line -1) 0)
                  line-is-empty-p)
            (beginning-of-line)
            (setq
             line-beginning-position (line-beginning-position))
            (setq
             line-end-position (line-end-position))
            (setq
             line-string
             (buffer-substring-no-properties line-beginning-position line-end-position))
            (setq
             line-is-empty-p (string-match-p "^[ \t\f\r\n]*$" line-string))
            (setq
             move-length (1+ move-length)))

          (if line-is-empty-p
              (indent-line-to 0)
            (let* ((old-indentation (phps-mode-indent--string-indentation line-string))
                   (current-line-starts-with-closing-bracket (phps-mode-indent--string-starts-with-closing-bracket-p current-line-string))
                   (current-line-starts-with-opening-bracket (phps-mode-indent--string-starts-with-opening-bracket current-line-string))
                   (line-starts-with-closing-bracket (phps-mode-indent--string-starts-with-closing-bracket-p line-string))
                   (line-starts-with-opening-doc-comment (phps-mode-indent--string-starts-with-opening-doc-comment-p line-string))
                   (line-ends-with-assignment (phps-mode-indent--string-ends-with-assignment-p line-string))
                   (line-ends-with-opening-bracket (phps-mode-indent--string-ends-with-opening-bracket-p line-string))
                   (line-ends-with-terminus (phps-mode-indent--string-ends-with-terminus-p line-string))
                   (bracket-level (phps-mode-indent--get-string-brackets-count line-string))
                   (line-ends-with-implements-p (string-match-p "[\t ]+implements$" line-string)))
              (message "Previous non-empty line: %S with indentation: %S" line-string old-indentation)

              (setq new-indentation old-indentation)
              (goto-char point)

              (when line-ends-with-implements-p
                (setq bracket-level (+ tab-width)))

              (when (> bracket-level 0)
                (if (< bracket-level tab-width)
                    (setq new-indentation (+ new-indentation 1))
                  (setq new-indentation (+ new-indentation tab-width))))

              (when (= bracket-level -1)
                (setq new-indentation (1- new-indentation)))

              (when (and (= bracket-level 0)
                         line-starts-with-closing-bracket)
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     current-line-starts-with-opening-bracket
                     (string= current-line-starts-with-opening-bracket "{")
                     (phps-mode-indent--backwards-looking-at-p
                      "[\t ]*implements[\n\t ]+\\([\n\t ]*[a-zA-Z_0-9]+,?\\)+[\n\t ]*{$"))
                (setq new-indentation (- new-indentation tab-width)))

              (when current-line-starts-with-closing-bracket
                (setq new-indentation (- new-indentation tab-width)))

              (when line-starts-with-opening-doc-comment
                (setq new-indentation (+ new-indentation 1)))

              (when (and
                     line-ends-with-assignment
                     (<= bracket-level 0))
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     line-ends-with-opening-bracket
                     (< bracket-level 0))
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and
                     line-ends-with-terminus
                     (not (string-match-p "^[\t ]*\\(echo[\t ]+\\|print[\t ]+\\)" line-string)))
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
                         (> bracket-level -1))
                        is-bracket-less-command))

                    ;; NOTE stuff like $var = array(\n    4\n);\n
                    ;; will end assignment but also decrease bracket-level
                    (setq new-indentation (- new-indentation tab-width))))

                (goto-char point))

              ;; Decrease indentation if current line decreases in bracket level
              (when (< new-indentation 0)
                (setq new-indentation 0))
              (message "new-indentation: %S bracket-level: %S old-indentation: %S" new-indentation bracket-level old-indentation)

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

(defun phps-mode-indent--string-starts-with-closing-bracket-p (string)
  "Get bracket count for STRING."
  (string-match-p "^[\t ]*\\([\]})[]\\|</[a-zA-Z]+\\|/>\\)" string))

(defun phps-mode-indent--string-starts-with-opening-doc-comment-p (string)
  "Does STRING start with opening doc comment?"
  (string-match-p "^[\t ]*/\\*\\*" string))

(defun phps-mode-indent--string-starts-with-opening-bracket (string)
  "If STRING start with opening bracket return it otherwise nil."
  (if
      (string-match "^[\t ]*\\([\[{(]\\)" string)
      (match-string 0 string)
    nil))

(defun phps-mode-indent--string-ends-with-opening-bracket-p (string)
  "Get bracket count for STRING."
  (string-match-p "\\([\[{(]\\|<[a-zA-Z]+\\|[\t ]+implements\\)[\t ]*$" string))

(defun phps-mode-indent--string-ends-with-assignment-p (string)
  "Get bracket count for STRING."
  (string-match-p "=>?[\t ]*$" string))

(defun phps-mode-indent--string-ends-with-terminus-p (string)
  "Get bracket count for STRING."
  (string-match-p "\\(;\\|,\\)[\t ]*$" string))


(provide 'phps-mode-indent)

;;; phps-mode-indent.el ends here
