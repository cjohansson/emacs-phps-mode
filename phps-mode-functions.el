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

;; NOTE Also format white-space inside the line, i.e. after function declarations?

;; TODO Add support for automatic parenthesis, bracket, square-bracket, single-quote and double-quote encapsulations

;; TODO Support indentation for multi-line assignments

(defun phps-mode-functions-get-lines-indent ()
  "Get the column and tuning indentation-numbers for each line in buffer that contain tokens."
  (if (boundp 'phps-mode-lexer-tokens)
      (save-excursion
        (goto-char (point-min))
        (let ((in-scripting nil)
              (in-heredoc nil)
              (in-doc-comment nil)
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
              (tuning-level 0)
              (nesting-start 0)
              (nesting-end 0)
              (last-line-number 0)
              (first-token-on-line nil)
              (line-indents (make-hash-table :test 'equal))
              (first-token-is-nesting-decrease nil)
              (first-token-is-nesting-increase nil)
              (token-number 1)
              (last-token-number (length phps-mode-lexer-tokens))
              (last-token nil)
              (last-token-was-first-on-new-line nil)
              (allow-custom-column-increment nil)
              (allow-custom-column-decrement nil)
              (in-assignment nil)
              (in-assignment-on-new-line nil))

          ;; Iterate through all buffer tokens from beginning to end
          (dolist (item phps-mode-lexer-tokens)
            (let* ((token (car item))
                   (token-start (car (cdr item)))
                   (token-end (cdr (cdr item)))
                   (token-start-line-number (line-number-at-pos token-start t))
                   (token-end-line-number (line-number-at-pos token-end t)))

              ;; Are we on a new line or are we are last token?
              (if (or (> token-start-line-number last-line-number)
                      (= token-number last-token-number))
                  (progn

                    ;; Flag when last token was on a new line
                    (when (= token-number last-token-number)
                      (setq last-token-was-first-on-new-line t))

                    ;; Calculate indentation level at end of line
                    (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level))

                    ;; Inside assignment increment by one
                    (when in-assignment
                      (setq nesting-end (1+ nesting-end)))

                    ;; Is line ending indentation lesser than line beginning indentation?
                    (when (and (< nesting-end nesting-start)
                               (> column-level 0)
                               (not in-assignment-on-new-line))

                      ;; Decrement column
                      (if allow-custom-column-decrement
                          (progn
                            (setq column-level (- column-level (- nesting-start nesting-end)))
                            (setq allow-custom-column-increment nil))
                        (setq column-level (1- column-level))))

                    ;; Is line ending indentation equal to line beginning indentation and did we have a change of scope?
                    (when (= nesting-end nesting-start)
                      (when (and first-token-is-nesting-decrease
                                 (> column-level 0))
                        (setq column-level (1- column-level)))
                      (when first-token-is-nesting-increase
                        (setq column-level (1+ column-level))))
                    
                    ;; (message "new line at %s, %s %s.%s (%s - %s) = %s %s %s %s %s [%s %s] %s" token last-token column-level tuning-level nesting-start nesting-end round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level first-token-is-nesting-decrease first-token-is-nesting-increase in-assignment)

                    ;; Put indent-level to hash-table
                    (when (> last-line-number 0)
                      (puthash last-line-number `(,column-level ,tuning-level) line-indents))

                    (when (> token-end-line-number token-start-line-number)
                      ;; (message "Token %s starts at %s and ends at %s" token token-start-line-number token-end-line-number)
                      (when (equal token 'T_DOC_COMMENT)
                        (setq tuning-level 1))
                      (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                        (while (>= token-line-number-diff 0)
                          (puthash (- token-end-line-number token-line-number-diff) `(,column-level ,tuning-level) line-indents)
                          (setq token-line-number-diff (1- token-line-number-diff))))
                      (setq tuning-level 0))

                    ;; Is line ending indentation equal to line beginning indentation and did we have a change of scope?
                    (when (= nesting-end nesting-start)
                      (when first-token-is-nesting-decrease
                        (setq column-level (1+ column-level)))
                      (when first-token-is-nesting-increase
                        (setq column-level (1- column-level))))

                    ;; Is line ending indentation higher than line beginning indentation?
                    (when (> nesting-end nesting-start)

                      ;; Increase indentation
                      (if allow-custom-column-increment
                          (progn
                            (setq column-level (+ column-level (- nesting-end nesting-start)))
                            (setq allow-custom-column-increment nil))
                        (setq column-level (1+ column-level))))

                    ;; Calculate indentation level at start of line
                    (setq nesting-start (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level))

                    ;; Inside assignment increment by one
                    (when in-assignment
                      (setq nesting-start (1+ nesting-start)))

                    ;; Set initial values for tracking first token
                    (setq first-token-on-line t)
                    (setq first-token-is-nesting-increase nil)
                    (setq first-token-is-nesting-decrease nil))
                (setq first-token-on-line nil))

              ;; Keep track of round bracket level
              (when (string= token "(")
                (setq round-bracket-level (1+ round-bracket-level))
                (when first-token-on-line
                  (setq first-token-is-nesting-increase t)))
              (when (string= token ")")
                (setq round-bracket-level (1- round-bracket-level))
                (when first-token-on-line
                  (setq first-token-is-nesting-decrease t)))

              ;; Keep track of square bracket level
              (when (string= token "[")
                (setq square-bracket-level (1+ square-bracket-level))
                (when first-token-on-line
                  (setq first-token-is-nesting-increase t)))
              (when (string= token "]")
                (setq square-bracket-level (1- square-bracket-level))
                (when first-token-on-line
                  (setq first-token-is-nesting-decrease t)))

              ;; Keep track of curly bracket level
              (when (or (equal token 'T_CURLY_OPEN)
                        (equal token 'T_DOLLAR_OPEN_CURLY_BRACES)
                        (string= token "{"))
                (setq curly-bracket-level (1+ curly-bracket-level))
                (when first-token-on-line
                  (setq first-token-is-nesting-increase t)))
              (when (string= token "}")
                (setq curly-bracket-level (1- curly-bracket-level))

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
                              (when after-special-control-structure-first-on-line
                                (setq first-token-is-nesting-decrease t)))

                          (when (equal after-special-control-structure-token 'T_SWITCH)
                            (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                            (setq allow-custom-column-increment t))

                          (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                          (when after-special-control-structure-first-on-line
                            (setq first-token-is-nesting-increase t))))
                    (if (or (equal after-special-control-structure-token 'T_ELSE)
                            (equal after-special-control-structure-token 'T_ELSEIF))
                        (progn
                          (when after-special-control-structure-first-on-line
                            (setq first-token-is-nesting-increase t)))
                      ;; (message "Was inline-control structure %s %s" after-special-control-structure-token token)
                      (setq inline-control-structure-level (1+ inline-control-structure-level))
                      (when after-special-control-structure-first-on-line
                        (setq first-token-is-nesting-increase t))
                      (setq in-inline-control-structure t))))

                (setq after-special-control-structure nil)
                (setq after-special-control-structure-token nil)
                (setq after-special-control-structure-first-on-line nil))

              ;; Support extra special control structures (CASE)
              (when (and after-extra-special-control-structure
                         (string= token ":"))
                (when after-extra-special-control-structure-first-on-line
                  (setq first-token-is-nesting-decrease t))
                (setq after-extra-special-control-structure nil))

              ;; Did we reach a semicolon inside a inline block? Close the inline block
              (when (and in-inline-control-structure
                         (string= token ";"))
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
              (when in-assignment
                (if (string= token ";")
                    (progn
                      (setq in-assignment nil)
                      ;; (message "Assignment ended at semi-colon")
                      )
                  (when first-token-on-line
                    (if (or (equal token 'T_VARIABLE)
                            (equal token 'T_CONSTANT_ENCAPSED_STRING)
                            (equal token 'T_ENCAPSED_AND_WHITESPACE)
                            (equal token 'T_OBJECT_OPERATOR)
                            (equal token 'T_LNUMBER)
                            (equal token 'T_DNUMBER))
                        (progn
                          ;; (message "In assignment on new-line at %s" token)
                          (setq in-assignment-on-new-line t))
                      ;; (message "Not in assignment on new-line at %s" token)
                      (setq in-assignment-on-new-line nil)))))
              (when (and (not after-special-control-structure)
                         (string= token "="))
                ;; (message "Started assignment")
                (setq in-assignment t)
                (setq in-assignment-on-new-line nil))

              ;; Did we encounter a token that supports extra special alternative control structures?
              (when (equal token 'T_CASE)
                (setq after-extra-special-control-structure t)
                (setq after-extra-special-control-structure-first-on-line first-token-on-line))

              ;; Keep track of in scripting
              (when (or (equal token 'T_OPEN_TAG)
                        (equal token 'T_OPEN_TAG_WITH_ECHO))
                (setq in-scripting t))
              (when (equal token 'T_CLOSE_TAG)
                (setq in-scripting nil))

              ;; Keep track of whether we are inside a HEREDOC or NOWDOC
              (when (equal token 'T_START_HEREDOC)
                (setq in-heredoc t))
              (when (equal token 'T_END_HEREDOC)
                (setq in-heredoc nil))

              ;; Are we on a new line?
              (when (> token-start-line-number last-line-number)

                ;; Update last line number
                (setq last-line-number token-start-line-number))

              (setq token-number (1+ token-number))
              (setq last-token token)))

          ;; Process line if last token was first on new line
          (when last-token-was-first-on-new-line

            ;; Calculate indentation level at end of line
            (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level))

            ;; Inside assignment increment by one
            (when in-assignment
              (setq nesting-end (1+ nesting-end)))

            ;; Is line ending indentation lesser than line beginning indentation?
            (when (and (< nesting-end nesting-start)
                       (> column-level 0)
                       (not in-assignment-on-new-line))

              ;; Decrement column
              (if allow-custom-column-decrement
                  (progn
                    (setq column-level (- column-level (- nesting-start nesting-end)))
                    (setq allow-custom-column-increment nil))
                (setq column-level (1- column-level))))

            ;; Is line ending indentation equal to line beginning indentation and did we have a change of scope?
            (when (= nesting-end nesting-start)
              (when (and first-token-is-nesting-decrease
                         (> column-level 0))
                (setq column-level (1- column-level)))
              (when first-token-is-nesting-increase
                (setq column-level (1+ column-level))))
            
            ;; (message "last token at %s %s.%s (%s - %s) = %s %s %s %s %s [%s %s]" last-token column-level tuning-level nesting-start nesting-end round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level inline-control-structure-level first-token-is-nesting-decrease first-token-is-nesting-increase)

            ;; Put indent-level to hash-table
            (puthash last-line-number `(,column-level ,tuning-level) line-indents))

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

;; TODO This function needs to keep track of alternative syntax for the control structures: if, while, for, foreach, and switch
;; TODO This function needs to keep track of inline syntax for the control structures: if, while, for, foreach, and switch
;; TODO Support switch case as well
;; TODO Keep track of assignments as well

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

  (add-hook 'after-change-functions #'phps-mode-functions-after-change)

  ;; NOTE Implement indent-region?
  )


(provide 'phps-mode-functions)

;;; phps-mode-functions.el ends here
