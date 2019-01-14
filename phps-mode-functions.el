;;; phps-mode-functions.el --- Mode functions for PHPs -*- lexical-binding: t -*-

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

(autoload 'phps-mode-lexer-run-incremental "phps-mode-lexer")
(autoload 'phps-mode-lexer-move-tokens "phps-mode-lexer")
(autoload 'phps-mode-lexer-move-states "phps-mode-lexer")

(defvar phps-mode-functions-buffer-changes-start nil
  "Start point of buffer changes, nil if none.")

(defvar phps-mode-functions-lines-indent nil
  "The indentation of each line in buffer, nil if none.")

(defvar phps-mode-functions-verbose nil
  "Verbose messaging, default nil.")


;; NOTE Also format white-space inside the line, i.e. after function declarations?

;; TODO Add support for automatic parenthesis, bracket, square-bracket, single-quote and double-quote encapsulations

;; Set indent for white-space lines as well
(defun phps-mode-functions-get-lines-indent ()
  "Get the column and tuning indentation-numbers for each line in buffer that contain tokens."
  (if (boundp 'phps-mode-lexer-tokens)
      (save-excursion
        (goto-char (point-min))
        (when phps-mode-functions-verbose
          (message "\nCalculation indentation for all lines in buffer:\n\n%s" (buffer-substring-no-properties (point-min) (point-max))))
        (let ((in-scripting nil)
              (in-heredoc nil)
              (in-heredoc-started-this-line nil)
              (in-heredoc-ended-this-line nil)
              (in-inline-control-structure nil)
              (after-special-control-structure nil)
              (after-special-control-structure-token nil)
              (after-special-control-structure-first-on-line nil)
              (after-extra-special-control-structure nil)
              (after-extra-special-control-structure-first-on-line nil)
              (switch-curly-stack nil)
              (switch-alternative-stack nil)
              (switch-case-alternative-stack nil)
              (curly-bracket-level 0)
              (round-bracket-level 0)
              (square-bracket-level 0)
              (alternative-control-structure-level 0)
              (concatenation-level 0)
              (column-level 0)
              (column-level-start 0)
              (tuning-level 0)
              (nesting-start 0)
              (nesting-end 0)
              (last-line-number 0)
              (first-token-on-line nil)
              (line-indents (make-hash-table :test 'equal))
              (first-token-is-nesting-decrease nil)
              (first-token-is-nesting-increase nil)
              (line-contained-nesting-decrease nil)
              (line-contained-nesting-increase nil)
              (token-number 1)
              (allow-custom-column-increment nil)
              (allow-custom-column-decrement nil)
              (in-assignment nil)
              (in-assignment-level 0)
              (in-assignment-started-this-line nil)
              (in-class-declaration nil)
              (in-class-declaration-level 0)
              (token nil)
              (token-start-line-number 0)
              (token-end-line-number)
              (tokens (nreverse phps-mode-lexer-tokens))
              (nesting-stack nil)
              (changed-nesting-stack-in-line nil)
              (after-class-declaration nil)
              (class-declaration-started-this-line nil)
              (special-control-structure-started-this-line nil)
              (temp-pre-indent nil)
              (temp-post-indent nil))

          (push `(END_PARSE ,(point-max) . ,(point-max)) tokens)

          ;; Iterate through all buffer tokens from beginning to end
          (dolist (item (nreverse tokens))
            ;; (message "Items: %s %s" item phps-mode-lexer-tokens)
            (let ((next-token (car item))
                   (next-token-start (car (cdr item)))
                   (next-token-end (cdr (cdr item)))
                   (next-token-start-line-number nil)
                   (next-token-end-line-number nil))

              ;; Handle the pseudo-token for last-line
              (if (equal next-token 'END_PARSE)
                  (progn
                    (setq next-token-start-line-number (1+ token-start-line-number))
                    (setq next-token-end-line-number (1+ token-end-line-number)))
                (setq next-token-start-line-number (line-number-at-pos next-token-start t))
                (setq next-token-end-line-number (line-number-at-pos next-token-end t)))

              ;; Token logic
              (when token

                ;; Keep track of round bracket level
                (when (string= token "(")
                  (setq round-bracket-level (1+ round-bracket-level))
                  (setq line-contained-nesting-increase t)
                  (when first-token-on-line
                    (setq first-token-is-nesting-increase t)))
                (when (string= token ")")
                  (setq round-bracket-level (1- round-bracket-level))
                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Keep track of square bracket level
                (when (string= token "[")
                  (setq square-bracket-level (1+ square-bracket-level))
                  (setq line-contained-nesting-increase t)
                  (when first-token-on-line
                    (setq first-token-is-nesting-increase t)))
                (when (string= token "]")
                  (setq square-bracket-level (1- square-bracket-level))
                  (setq line-contained-nesting-decrease t)
                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Keep track of when we are inside a class definition
                (if in-class-declaration
                    (if (string= token "{")
                        (progn
                          (setq in-class-declaration nil)
                          (setq in-class-declaration-level 0)

                          (when (not class-declaration-started-this-line)
                            (setq column-level (1- column-level))
                            (pop nesting-stack))

                          (when first-token-on-line
                            (setq after-class-declaration t)
                            (setq first-token-is-nesting-increase nil)
                            (setq first-token-is-nesting-decrease t))

                          )
                      (when first-token-on-line
                        (setq in-class-declaration-level 1)))
                  (when (equal token 'T_CLASS)
                    (setq in-class-declaration t)
                    (setq in-class-declaration-level 1)
                    (setq class-declaration-started-this-line t)))

                ;; Keep track of curly bracket level
                (when (or (equal token 'T_CURLY_OPEN)
                          (equal token 'T_DOLLAR_OPEN_CURLY_BRACES)
                          (string= token "{"))
                  (setq curly-bracket-level (1+ curly-bracket-level))
                  (setq line-contained-nesting-increase t)
                  (when first-token-on-line
                    (setq first-token-is-nesting-increase t)))
                (when (string= token "}")
                  (setq line-contained-nesting-decrease t)
                  (setq curly-bracket-level (1- curly-bracket-level))

                  (when (and switch-curly-stack
                             (= (1+ curly-bracket-level) (car switch-curly-stack)))

                    (when phps-mode-functions-verbose
                      (message "Ended switch curly stack at %s" curly-bracket-level))

                    (setq allow-custom-column-decrement t)
                    (setq alternative-control-structure-level (1- alternative-control-structure-level))
                    (pop switch-curly-stack))
                  
                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Keep track of in scripting
                (when (or (equal token 'T_OPEN_TAG)
                          (equal token 'T_OPEN_TAG_WITH_ECHO))
                  (setq in-scripting t))
                (when (equal token 'T_CLOSE_TAG)
                  (setq in-scripting nil))

                ;; Keep track of ending alternative control structure level
                (when (or (equal token 'T_ENDIF)
                          (equal token 'T_ENDWHILE)
                          (equal token 'T_ENDFOR)
                          (equal token 'T_ENDFOREACH)
                          (equal token 'T_ENDSWITCH))
                  (setq alternative-control-structure-level (1- alternative-control-structure-level))
                  ;; (message "Found ending alternative token %s %s" token alternative-control-structure-level)

                  (when (and (equal token 'T_ENDSWITCH)
                             switch-case-alternative-stack)

                    (when phps-mode-functions-verbose
                      (message "Ended alternative switch stack at %s" alternative-control-structure-level))
                    
                    (pop switch-alternative-stack)
                    (pop switch-case-alternative-stack)
                    (setq allow-custom-column-decrement t)
                    (setq alternative-control-structure-level (1- alternative-control-structure-level)))

                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; When we encounter a token except () after a control-structure
                (when (and after-special-control-structure
                           (= after-special-control-structure round-bracket-level)
                           (not (string= token ")"))
                           (not (string= token "(")))

                  ;; Handle the else if case
                  (if (equal 'T_IF token)
                      (setq after-special-control-structure-token token)

                    ;; Is token not a curly bracket - because that is a ordinary control structure syntax
                    (if (string= token "{")

                        ;; Save curly bracket level when switch starts
                        (when (equal after-special-control-structure-token 'T_SWITCH)

                          (when phps-mode-functions-verbose
                            (message "Started switch curly stack at %s" curly-bracket-level))

                          (push curly-bracket-level switch-curly-stack))

                      ;; Is it the start of an alternative control structure?
                      (if (string= token ":")

                          (progn

                            ;; Save alternative nesting level for switch
                            (when (equal after-special-control-structure-token 'T_SWITCH)

                              (when phps-mode-functions-verbose
                                (message "Started switch alternative stack at %s" alternative-control-structure-level))

                              (push alternative-control-structure-level switch-alternative-stack))

                            (setq alternative-control-structure-level (1+ alternative-control-structure-level))

                            (when phps-mode-functions-verbose
                              (message "\nIncreasing alternative-control-structure after %s %s to %s\n" after-special-control-structure-token token alternative-control-structure-level))

                            (setq line-contained-nesting-increase t)
                            (when after-special-control-structure-first-on-line
                              (setq first-token-is-nesting-increase t)))

                        ;; Don't start inline control structures after a while ($condition); expression
                        (when (not (string= token ";"))
                          (when phps-mode-functions-verbose
                            (message "\nStarted inline control-structure after %s at %s\n" after-special-control-structure-token token))

                          (setq in-inline-control-structure t)
                          (setq temp-pre-indent (1+ column-level)))))

                    (setq after-special-control-structure nil)
                    (setq after-special-control-structure-token nil)
                    (setq after-special-control-structure-first-on-line nil)))

                ;; Support extra special control structures (CASE)
                (when (and after-extra-special-control-structure
                           (string= token ":"))
                  (setq line-contained-nesting-increase t)
                  (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                  (when after-extra-special-control-structure-first-on-line
                    (setq first-token-is-nesting-decrease t))
                  (setq after-extra-special-control-structure nil))

                ;; Keep track of concatenations
                (when (> next-token-start-line-number token-end-line-number)
                  (if (or (string= token ".")
                          (string= next-token "."))
                      (progn
                        (when phps-mode-functions-verbose
                          (message "\nFound ending dot, indenting next line with one.\n"))
                        (setq concatenation-level 1))
                    (setq concatenation-level 0)))

                ;; Did we reach a semicolon inside a inline block? Close the inline block
                (when (and in-inline-control-structure
                           (string= token ";")
                           (not special-control-structure-started-this-line))
                  (setq line-contained-nesting-decrease t)
                  (setq in-inline-control-structure nil))

                ;; Did we encounter a token that supports alternative and inline control structures?
                (when (or (equal token 'T_IF)
                          (equal token 'T_WHILE)
                          (equal token 'T_FOR)
                          (equal token 'T_FOREACH)
                          (equal token 'T_SWITCH)
                          (equal token 'T_ELSE)
                          (equal token 'T_ELSEIF)
                          (equal token 'T_DEFAULT))
                  (setq after-special-control-structure-first-on-line first-token-on-line)
                  (setq after-special-control-structure round-bracket-level)
                  (setq after-special-control-structure-token token)
                  (setq special-control-structure-started-this-line t)

                  (when (and (or (equal token 'T_ELSE)
                                 (equal token 'T_ELSEIF)
                                 (equal token 'T_DEFAULT))
                             nesting-stack
                             (string= (car (cdr (cdr (car nesting-stack)))) ":"))

                    (setq alternative-control-structure-level (1- alternative-control-structure-level))

                    (when first-token-on-line
                      (setq first-token-is-nesting-decrease t))

                    (when phps-mode-functions-verbose
                      (message "\nDecreasing alternative control structure nesting at %s to %s\n" token alternative-control-structure-level))
                    ))

                ;; Keep track of assignments
                (if in-assignment
                    (if (or (string= token ";")
                            (< round-bracket-level in-assignment))
                        (progn
                          (setq in-assignment nil)
                          (setq in-assignment-level 0))
                      (when first-token-on-line
                        (setq in-assignment-level 1)
                        ;; (message "In assignment on new-line at %s" token)
                        ))
                  (when (and (not after-special-control-structure)
                             (string= token "="))
                    ;; (message "Started assignment")
                    (setq in-assignment round-bracket-level)
                    (setq in-assignment-started-this-line t)
                    (setq in-assignment-level 1)))

                ;; Did we encounter a token that supports extra special alternative control structures?
                (when (equal token 'T_CASE)
                  (setq after-extra-special-control-structure t)
                  (setq after-extra-special-control-structure-first-on-line first-token-on-line)

                  (when (and switch-case-alternative-stack
                             (= (1- alternative-control-structure-level) (car switch-case-alternative-stack)))

                    (when phps-mode-functions-verbose
                      (message "Found CASE %s vs %s" (1- alternative-control-structure-level) (car switch-case-alternative-stack)))

                    (setq alternative-control-structure-level (1- alternative-control-structure-level))
                    (when first-token-on-line
                      (setq first-token-is-nesting-decrease t))
                    (pop switch-case-alternative-stack))

                  (push alternative-control-structure-level switch-case-alternative-stack)))

              (when token
                
                ;; Calculate nesting
                (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level in-assignment-level in-class-declaration-level concatenation-level))

                ;; Keep track of whether we are inside a HEREDOC or NOWDOC
                (when (equal token 'T_START_HEREDOC)
                  (setq in-heredoc t)
                  (setq in-heredoc-started-this-line t))
                (when (equal token 'T_END_HEREDOC)
                  (setq in-heredoc nil)
                  (setq in-heredoc-ended-this-line t))

                ;; Has nesting increased?
                (when (and nesting-stack
                           (<= nesting-end (car (car nesting-stack))))

                  (when phps-mode-functions-verbose
                    (message "\nPopping %s from nesting-stack since %s is lesser or equal to %s, next value is: %s\n" (car nesting-stack) nesting-end (car (car nesting-stack)) (nth 1 nesting-stack))
                    )
                  (pop nesting-stack)

                  (if first-token-is-nesting-decrease

                      (progn
                        ;; Decrement column
                        (if allow-custom-column-decrement
                            (progn
                              (setq column-level (- column-level (- nesting-start nesting-end)))
                              (setq allow-custom-column-increment nil))
                          (setq column-level (1- column-level)))

                        ;; Prevent negative column-values
                        (when (< column-level 0)
                          (setq column-level 0)))

                    (when (not temp-post-indent)
                      (setq temp-post-indent column-level))

                    ;; Decrement column
                    (if allow-custom-column-decrement
                        (progn
                          (setq temp-post-indent (- temp-post-indent (- nesting-start nesting-end)))
                          (setq allow-custom-column-increment nil))
                      (setq temp-post-indent (1- temp-post-indent)))

                    ;; Prevent negative column-values
                    (when (< temp-post-indent 0)
                      (setq temp-post-indent 0))

                    ))

                ;; Are we on a new line or is it the last token of the buffer?
                (if (> next-token-start-line-number token-start-line-number)

                    ;; Line logic
                    (progn


                      ;; ;; Start indentation might differ from ending indentation in cases like } else {
                      (setq column-level-start column-level)

                      ;; Support temporarily pre-indent
                      (when temp-pre-indent
                        (setq column-level-start temp-pre-indent)
                        (setq temp-pre-indent nil))


                      ;; HEREDOC lines should have zero indent
                      (when (or (and in-heredoc
                                     (not in-heredoc-started-this-line))
                                in-heredoc-ended-this-line)
                        (setq column-level-start 0))


                      ;; Save line indent
                      (when phps-mode-functions-verbose
                        (message "Process line ending.	nesting: %s-%s,	line-number: %s-%s,	indent: %s.%s,	token: %s" nesting-start nesting-end token-start-line-number token-end-line-number column-level-start tuning-level token))

                      (when (> token-start-line-number 0)
                        (puthash token-start-line-number `(,column-level-start ,tuning-level) line-indents))


                      ;; Support trailing indent decrements
                      (when temp-post-indent
                        (setq column-level temp-post-indent)
                        (setq temp-post-indent nil))


                      ;; Increase indentation
                      (when (and (> nesting-end 0)
                                 (or (not nesting-stack)
                                     (> nesting-end (car (cdr (car nesting-stack))))))
                        (let ((nesting-stack-end 0))
                          (when nesting-stack
                            (setq nesting-stack-end (car (cdr (car nesting-stack)))))

                          (if allow-custom-column-increment
                              (progn
                                (setq column-level (+ column-level (- nesting-end nesting-start)))
                                (setq allow-custom-column-increment nil))
                            (setq column-level (1+ column-level)))

                          (when phps-mode-functions-verbose
                            (message "\nPushing (%s %s %s) to nesting-stack since %s is greater than %s or stack is empty\n" nesting-start nesting-end token nesting-end (car (cdr (car nesting-stack))))
                            )
                          (push `(,nesting-stack-end ,nesting-end ,token) nesting-stack)
                          (when phps-mode-functions-verbose
                            ;; (message "New stack %s, start: %s end: %s\n" nesting-stack (car (car nesting-stack)) (car (cdr (car nesting-stack))))
                            )))


                      ;; Does token span over several lines?
                      (when (> token-end-line-number token-start-line-number)
                        (let ((column-level-end column-level))

                          ;; HEREDOC lines should have zero indent
                          (when (or (and in-heredoc
                                         (not in-heredoc-started-this-line))
                                    in-heredoc-ended-this-line)
                            (setq column-level-end 0))

                          ;; (message "Token %s starts at %s and ends at %s indent %s %s" next-token token-start-line-number token-end-line-number column-level-end tuning-level)

                          ;; Indent doc-comment lines with 1 tuning
                          (when (equal token 'T_DOC_COMMENT)
                            (setq tuning-level 1))

                          (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                            (while (>= token-line-number-diff 0)
                              (puthash (- token-end-line-number token-line-number-diff) `(,column-level-end ,tuning-level) line-indents)
                              ;; (message "Saved line %s indent %s %s" (- token-end-line-number token-line-number-diff) column-level tuning-level)
                              (setq token-line-number-diff (1- token-line-number-diff))))

                          ;; Rest tuning-level used for comments
                          (setq tuning-level 0)))


                      ;; Indent token-less lines here in between last tokens if distance is more than 1 line
                      (when (and (> next-token-start-line-number (1+ token-end-line-number))
                                 (not (equal token 'T_CLOSE_TAG)))

                        (when phps-mode-functions-verbose
                          (message "\nDetected token-less lines between %s and %s, should have indent: %s\n" token-end-line-number next-token-start-line-number column-level))

                        (let ((token-line-number-diff (1- (- next-token-start-line-number token-end-line-number))))
                          (while (>= token-line-number-diff 0)
                            (puthash (- next-token-start-line-number token-line-number-diff) `(,column-level ,tuning-level) line-indents)
                            ;; (message "Saved line %s indent %s %s" (- token-end-line-number token-line-number-diff) column-level tuning-level)
                            (setq token-line-number-diff (1- token-line-number-diff)))))


                      ;; Calculate indentation level at start of line
                      (setq nesting-start (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level in-assignment-level in-class-declaration-level concatenation-level))

                      ;; Set initial values for tracking first token
                      (when (> token-start-line-number last-line-number)
                        (setq first-token-on-line t)
                        (setq first-token-is-nesting-increase nil)
                        (setq first-token-is-nesting-decrease nil)
                        (setq in-assignment-level 0)
                        (setq after-class-declaration nil)
                        (setq in-class-declaration-level 0)
                        (setq line-contained-nesting-increase nil)
                        (setq line-contained-nesting-decrease nil)
                        (setq in-assignment-started-this-line nil)
                        (setq changed-nesting-stack-in-line nil)
                        (setq class-declaration-started-this-line nil)
                        (setq in-heredoc-started-this-line nil)
                        (setq special-control-structure-started-this-line nil)))

                  ;; Current token is not first if it's not <?php or <?=
                  (when (not (or (equal token 'T_OPEN_TAG)
                                 (equal token 'T_OPEN_TAG_WITH_ECHO)))
                    (setq first-token-on-line nil))

                  (when (> token-end-line-number token-start-line-number)
                    ;; (message "Token not first on line %s starts at %s and ends at %s" token token-start-line-number token-end-line-number)
                    (when (equal token 'T_DOC_COMMENT)
                      (setq tuning-level 1))

                    (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                      (while (>= token-line-number-diff 0)
                        (puthash (- token-end-line-number token-line-number-diff) `(,column-level ,tuning-level) line-indents)
                        (setq token-line-number-diff (1- token-line-number-diff))))
                    (setq tuning-level 0))))

              ;; Update current token
              (setq token next-token)
              (setq token-start-line-number next-token-start-line-number)
              (setq token-end-line-number next-token-end-line-number)
              (setq token-number (1+ token-number))))
          line-indents))
    nil))

(defun phps-mode-functions-indent-line ()
  "Indent line."
  ;; Set lines indent if not set
  (unless (and (boundp 'phps-mode-functions-lines-indent)
               phps-mode-functions-lines-indent)
    (setq phps-mode-functions-lines-indent (phps-mode-functions-get-lines-indent)))

  (when phps-mode-functions-lines-indent
    (let ((indent (gethash (line-number-at-pos (point)) phps-mode-functions-lines-indent)))
      (when indent
        ;; (message "indent: %s %s %s" indent (car indent) (car (cdr indent)))
        (let ((indent-sum (+ (* (car indent) tab-width) (car (cdr indent))))
              (current-indentation (current-indentation))
              (line-start (line-beginning-position)))

          (when (null current-indentation)
            (setq current-indentation 0))

          ;; Only continue if current indentation is wrong
          (unless (equal indent-sum current-indentation)
            (let ((indent-diff (- indent-sum current-indentation)))
              ;; (message "Indenting to %s current column %s" indent-sum (current-indentation))
              ;; (message "inside scripting, start: %s, end: %s, indenting to column %s " start end indent-level)

              (indent-line-to indent-sum)

              ;; When indent is changed the trailing tokens and states just need to adjust their positions, this will improve speed of indent-region a lot
              (phps-mode-lexer-move-tokens line-start indent-diff)
              (phps-mode-lexer-move-states line-start indent-diff)

              )))))))

(defun phps-mode-functions-after-change (start _stop _length)
  "Track buffer change from START to STOP with length LENGTH."
  (when (string= major-mode "phps-mode")

    ;; If we haven't scheduled incremental lexer before - do it
    (when (and (not phps-mode-functions-buffer-changes-start)
               (boundp 'phps-mode-idle-interval)
               phps-mode-idle-interval)
      ;; (message "Enqueued incremental lexer")

      (run-with-idle-timer phps-mode-idle-interval nil #'phps-mode-lexer-run-incremental))

    ;; When point of change is not set or when start of new changes precedes old change - update the point
    (when (or (not phps-mode-functions-buffer-changes-start)
              (< start phps-mode-functions-buffer-changes-start))
      ;; (message "Setting start of changes from %s to %s" phps-mode-functions-buffer-changes-start start)
      (setq phps-mode-functions-buffer-changes-start start))

    ;; (message "phps-mode-functions-after-change %s %s %s" start stop length)
    ))

;; TODO Implement this
(defun phps-mode-functions-imenu-create-index-function ()
  "Create index for imenu."
  (let ((index '()))

    (when (boundp 'phps-mode-lexer-tokens)
      (let ((tokens phps-mode-lexer-tokens)
            (in-namespace-declaration nil)
            (in-class-declaration nil)
            (in-function-declaration nil))
        (dolist (token tokens)
          (let ((token-symbol (car token))
                (token-start (car (cdr token)))
                (token-end (cdr (cdr token))))
            (cond

             (in-namespace-declaration
              (cond

               ((or (string= token-symbol "{")
                    (string= token-symbol ";"))
                (setq in-namespace-declaration nil))

               ((equal token-symbol 'T_STRING)
                (let ((index-name (format "namespace %s" (buffer-substring-no-properties token-start token-end)))
                      (index-pos token-start))
                  (push `(,index-name . ,index-pos) index)))))

             (in-class-declaration
              (cond

               ((string= token-symbol "{")
                (setq in-class-declaration nil))

               ((equal token-symbol 'T_STRING)
                (let ((index-name (format "class %s" (buffer-substring-no-properties token-start token-end)))
                      (index-pos token-start))
                  (push `(,index-name . ,index-pos) index)))))

             (in-function-declaration
              (cond

               ((or (string= token-symbol "{")
                    (string= token-symbol ";"))
                (setq in-function-declaration nil))

               ((equal token-symbol 'T_STRING)
                (let ((index-name (format "function %s" (buffer-substring-no-properties token-start token-end)))
                      (index-pos token-start))
                  (push `(,index-name . ,index-pos) index)))))

             (t
              (cond

               ((equal token-symbol 'T_NAMESPACE)
                (setq in-namespace-declaration t))

               ((equal token-symbol 'T_CLASS)
                (setq in-class-declaration t))

               ((equal token-symbol 'T_FUNCTION)
                (setq in-function-declaration t)))))))))

    (nreverse index)))

(defun phps-mode-functions-init ()
  "PHP specific init-cleanup routines."

  ;; NOTE Indent-region will call this on each line of region
  (set (make-local-variable 'indent-line-function) #'phps-mode-functions-indent-line)

  ;; Support Imenu
  (set (make-local-variable 'imenu-create-index-function) #'phps-mode-functions-imenu-create-index-function)

  (when (and (boundp 'phps-mode-use-psr-2)
             phps-mode-use-psr-2)

    ;; PSR-2 : Code MUST use an indent of 4 spaces
    (set (make-local-variable 'tab-width) 4)

    ;; PSR-2 : MUST NOT use tabs for indenting
    (set (make-local-variable 'indent-tabs-mode) nil)

    )

  (set (make-local-variable 'phps-mode-functions-buffer-changes-start) nil)
  (set (make-local-variable 'phps-mode-functions-lines-indent) nil)

  (add-hook 'after-change-functions #'phps-mode-functions-after-change))


(provide 'phps-mode-functions)

;;; phps-mode-functions.el ends here
