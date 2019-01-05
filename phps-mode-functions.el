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
              (in-inline-control-structure nil)
              (after-special-control-structure nil)
              (after-special-control-structure-token nil)
              (after-special-control-structure-first-on-line nil)
              (after-extra-special-control-structure nil)
              (after-extra-special-control-structure-first-on-line nil)
              (switch-curly-stack nil)
              (curly-bracket-level 0)
              (round-bracket-level 0)
              (square-bracket-level 0)
              (alternative-control-structure-level 0)
              (inline-control-structure-level 0)
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
              (after-class-declaration nil))

          (push `(END_PARSE ,(point-max) . ,(point-max)) tokens)

          ;; Iterate through all buffer tokens from beginning to end
          (dolist (item (nreverse tokens))
            ;; (message "Items: %s %s" item phps-mode-lexer-tokens)
            (let ((next-token (car item))
                   (next-token-start (car (cdr item)))
                   (next-token-end (cdr (cdr item)))
                   (next-token-start-line-number nil)
                   (next-token-end-line-number nil))

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
                          
                          (setq column-level (1- column-level))
                          (setq nesting-start (1- nesting-start))
                          (pop nesting-stack)

                          (when first-token-on-line
                            (setq after-class-declaration t)
                            (setq first-token-is-nesting-increase nil)
                            (setq first-token-is-nesting-decrease t))

                          (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level in-assignment-level in-class-declaration-level))

                          )
                      (when first-token-on-line
                        (setq in-class-declaration-level 1)))
                  (when (equal token 'T_CLASS)
                    (setq in-class-declaration t)
                    (setq in-class-declaration-level 1)))

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

                  ;; Keep track of in scripting
                  (when (or (equal token 'T_OPEN_TAG)
                            (equal token 'T_OPEN_TAG_WITH_ECHO))
                    (setq in-scripting t))
                  (when (equal token 'T_CLOSE_TAG)
                    (setq in-scripting nil))

                  ;; Decrease switch curly stack if any
                  (when (and switch-curly-stack
                             (= curly-bracket-level (car switch-curly-stack)))
                    (setq curly-bracket-level (1- curly-bracket-level))
                    ;; (message "Found ending switch curly bracket")
                    (setq allow-custom-column-decrement t)
                    (pop switch-curly-stack))

                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Keep track of ending alternative control structure level
                (when (or (equal token 'T_ENDIF)
                          (equal token 'T_ENDWHILE)
                          (equal token 'T_ENDFOR)
                          (equal token 'T_ENDFOREACH)
                          (equal token 'T_ENDSWITCH))
                  (setq alternative-control-structure-level (1- alternative-control-structure-level))
                  ;; (message "Found ending alternative token %s %s" token alternative-control-structure-level)

                  (when (equal token 'T_ENDSWITCH)
                    (setq allow-custom-column-decrement t)
                    (setq alternative-control-structure-level (1- alternative-control-structure-level)))

                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                (when (and after-special-control-structure
                           (= after-special-control-structure round-bracket-level)
                           (not (string= token ")"))
                           (not (string= token "(")))

                  ;; Is token not a curly bracket - because that is a ordinary control structure syntax
                  (if (string= token "{")

                      (when (equal after-special-control-structure-token 'T_SWITCH)
                        ;; (message "Opening switch, increase curly brackets to %s" curly-bracket-level)
                        (push curly-bracket-level switch-curly-stack)
                        (setq allow-custom-column-increment t)
                        (setq curly-bracket-level (1+ curly-bracket-level)))

                    ;; Is it the start of an alternative control structure?
                    (if (string= token ":")
                        (progn
                          (if (or (equal after-special-control-structure-token 'T_ELSE)
                                  (equal after-special-control-structure-token 'T_ELSEIF)
                                  (equal after-special-control-structure-token 'T_DEFAULT))
                              (progn
                                (setq line-contained-nesting-increase t)
                                (when after-special-control-structure-first-on-line
                                  (setq first-token-is-nesting-decrease t)))

                            (when (equal after-special-control-structure-token 'T_SWITCH)
                              (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                              (setq allow-custom-column-increment t))

                            (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                            (setq line-contained-nesting-increase t)
                            (when after-special-control-structure-first-on-line
                              (setq first-token-is-nesting-increase t))))
                      (if (or (equal after-special-control-structure-token 'T_ELSE)
                              (equal after-special-control-structure-token 'T_ELSEIF))
                          (progn
                            (setq line-contained-nesting-increase t)
                            (when after-special-control-structure-first-on-line
                              (setq first-token-is-nesting-increase t)))
                        ;; (message "Was inline-control structure %s %s" after-special-control-structure-token token)
                        (setq inline-control-structure-level (1+ inline-control-structure-level))
                        (setq line-contained-nesting-increase t)
                        (when after-special-control-structure-first-on-line
                          (setq first-token-is-nesting-increase t))
                        (setq in-inline-control-structure t))))

                  (setq after-special-control-structure nil)
                  (setq after-special-control-structure-token nil)
                  (setq after-special-control-structure-first-on-line nil))

                ;; Support extra special control structures (CASE)
                (when (and after-extra-special-control-structure
                           (string= token ":"))
                  (setq line-contained-nesting-increase t)
                  (when after-extra-special-control-structure-first-on-line
                    (setq first-token-is-nesting-decrease t))
                  (setq after-extra-special-control-structure nil))

                ;; Did we reach a semicolon inside a inline block? Close the inline block
                (when (and in-inline-control-structure
                           (string= token ";"))
                  (setq line-contained-nesting-decrease t)
                  (setq inline-control-structure-level (1- inline-control-structure-level))
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
                  (setq after-special-control-structure-token token))

                ;; Keep track of assignments
                (if in-assignment
                    (if (or (string= token ";")
                            (< round-bracket-level in-assignment))
                        (progn
                          (setq in-assignment nil)
                          (when in-assignment-started-this-line
                            (setq in-assignment-level 0))
                          ;; (message "Assignment ended at semi-colon")
                          )
                      (when (and first-token-on-line
                                 (not in-heredoc))
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
                  (setq after-extra-special-control-structure-first-on-line first-token-on-line))

                ;; Keep track of whether we are inside a HEREDOC or NOWDOC
                (when (equal token 'T_START_HEREDOC)
                  (setq in-heredoc t))
                (when (equal token 'T_END_HEREDOC)
                  (setq in-heredoc nil))

                )

              (when token

                ;; Are we on a new line or is it the last token of the buffer?
                (if (> next-token-start-line-number token-start-line-number)

                    ;; Line logic
                    (progn

                      ;; Calculate nesting
                      (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level in-assignment-level in-class-declaration-level))

                      ;; Has nesting increased?
                      (when (and nesting-stack
                                 (<= nesting-end (car (car nesting-stack))))

                        (when phps-mode-functions-verbose
                          ;; (message "\nPopping %s from nesting-stack since %s is lesser or equal to %s, next value is: %s\n" (car nesting-stack) nesting-end (car (car nesting-stack)) (nth 1 nesting-stack))
                          )
                        (pop nesting-stack)

                        ;; Decrement column
                        (if allow-custom-column-decrement
                            (progn
                              (setq column-level (- column-level (- nesting-start nesting-end)))
                              (setq allow-custom-column-increment nil))
                          (setq column-level (1- column-level)))

                        ;; Prevent negative column-values
                        (when (< column-level 0)
                          (setq column-level 0)))


                      ;; ;; Start indentation might differ from ending indentation in cases like } else {
                      (setq column-level-start column-level)

                      ;; (when (= nesting-end nesting-start)

                      ;;   (when (and after-class-declaration
                      ;;              (> column-level-start 0))
                      ;;     (setq column-level-start (1- column-level-start)))

                      ;;   ;; ;; Handle cases like: } else {
                      ;;   ;; (when (and first-token-is-nesting-decrease
                      ;;   ;;            (not first-token-is-nesting-increase)
                      ;;   ;;            (> column-level-start 0))
                      ;;   ;;   (setq column-level-start (1- column-level-start)))

                      ;;   ;; ;; Handle cases like if (blaha)\n    echo 'blaha';
                      ;;   ;; (when (and first-token-is-nesting-increase
                      ;;   ;;            (not first-token-is-nesting-decrease))
                      ;;   ;;   (setq column-level-start (1+ column-level-start)))

                      ;;   )

                      (when phps-mode-functions-verbose
                        (message "Process line ending.	nesting: %s-%s,	line-number: %s-%s,	indent: %s.%s,	token: %s" nesting-start nesting-end token-start-line-number token-end-line-number column-level-start tuning-level token))
                      
                      ;; (message "new line %s or last token at %s, %s %s.%s (%s - %s) = %s %s %s %s %s [%s %s] %s %s %s" token-start-line-number token next-token column-level tuning-level nesting-start nesting-end round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level first-token-is-nesting-decrease first-token-is-nesting-increase in-assignment in-assignment-level in-class-declaration-level)

                      (when (> token-start-line-number 0)

                        ;; Save line indentation
                        (puthash token-start-line-number `(,column-level-start ,tuning-level) line-indents))

                      ;; TODO Handle case were current line number is more than 1 above last line number and then fill lines in-between with indentation



                      ;; Does token span over several lines?
                      (when (> token-end-line-number token-start-line-number)
                        ;; (message "Token %s starts at %s and ends at %s indent %s %s" next-token token-start-line-number token-end-line-number column-level tuning-level)

                        ;; Indent doc-comment lines with 1 tuning
                        (when (equal token 'T_DOC_COMMENT)
                          (setq tuning-level 1))

                        (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                          (while (>= token-line-number-diff 0)
                            (puthash (- token-end-line-number token-line-number-diff) `(,column-level ,tuning-level) line-indents)
                            ;; (message "Saved line %s indent %s %s" (- token-end-line-number token-line-number-diff) column-level tuning-level)
                            (setq token-line-number-diff (1- token-line-number-diff))))

                        ;; Rest tuning-level used for comments
                        (setq tuning-level 0))


                      ;; Has nesting decreased?
                      ;; If nesting-end > 0 AND (!nesting-stack OR nesting-end > nesting-stack-end), push stack, increase indent
                      (when (and (> nesting-end 0)
                                 (or (not nesting-stack)
                                     (> nesting-end (car (cdr (car nesting-stack))))))
                        (let ((nesting-stack-end 0))
                          (when nesting-stack
                            (setq nesting-stack-end (car (cdr (car nesting-stack)))))

                          ;; Increase indentation
                          (if allow-custom-column-increment
                              (progn
                                (setq column-level (+ column-level (- nesting-end nesting-start)))
                                (setq allow-custom-column-increment nil))
                            (setq column-level (1+ column-level)))

                          (when phps-mode-functions-verbose
                            ;; (message "\nPushing (%s %s) to nesting-stack since %s is greater than %s or stack is empty" nesting-start nesting-end nesting-end (car (cdr (car nesting-stack))))
                            )
                          (push `(,nesting-stack-end ,nesting-end) nesting-stack)
                          (when phps-mode-functions-verbose
                            ;; (message "New stack %s, start: %s end: %s\n" nesting-stack (car (car nesting-stack)) (car (cdr (car nesting-stack))))
                            )))

                      ;; ;; When nesting decreases but ends with a nesting increase, increase indent by one
                      ;; (when (and (< nesting-end nesting-start)
                      ;;            line-contained-nesting-increase)
                      ;;   (setq column-level (1+ column-level))
                      ;;   (when phps-mode-functions-verbose
                      ;;     (message "Pushing %s to nesting-stack since is lesser than %s" nesting-start nesting-end))
                      ;;   (push `(,nesting-start nesting-end) nesting-stack)
                      ;;   (message "New stack %s, start: %s end: %s" nesting-stack (car (car nesting-stack)) (car (cdr (car nesting-stack)))))

                      ;; Calculate indentation level at start of line
                      (setq nesting-start (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level in-assignment-level in-class-declaration-level))

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
                        (setq changed-nesting-stack-in-line nil)))

                  ;; Current token is not first
                  (setq first-token-on-line nil)

                  ;; Calculate nesting
                  (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level in-assignment-level in-class-declaration-level))

                  ;; Is current nesting-level equal or below stack-value? (#0)
                  (when (and nesting-stack
                             (<= nesting-end (car (car nesting-stack))))
                    (setq column-level (1- column-level))
                    (when phps-mode-functions-verbose
                      ;; (message "\nPopping %s from nesting-stack since %s is lesser somewhere on line, next is: %s, new column is: %s, new stack-value is: %s\n" (car nesting-stack) nesting-end (nth 1 nesting-stack) column-level (nth 1 nesting-stack))
                      )
                    (pop nesting-stack)
                    (setq changed-nesting-stack-in-line t))

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

(defun phps-mode-functions-init ()
  "PHP specific init-cleanup routines."

  ;; indent-region will call this on each line of region
  (set (make-local-variable 'indent-line-function) #'phps-mode-functions-indent-line)

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
