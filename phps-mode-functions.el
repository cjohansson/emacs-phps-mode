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

;; NOTE Also format white-space inside the line, i.e. after function declarations?

;; TODO Add support for automatic parenthesis, bracket, square-bracket, single-quote and double-quote encapsulations

;; TODO Support inline function indentations
;; TODO Support indentation for multi-line scalar assignments

(defun phps-mode-functions-indent-line ()
  "Indent line."
  (let ((data (phps-mode-functions-get-point-data))
        (line-start (line-beginning-position)))
    (let* ((start (nth 0 data))
           (end (nth 1 data))
           (in-scripting (nth 0 start)))

      ;; Are we in scripting?
      (when in-scripting
        (let ((start-curly-bracket-level (nth 1 start))
              (start-round-bracket-level (nth 2 start))
              (start-square-bracket-level (nth 3 start))
              (start-inline-control-structure-level (nth 4 start))
              (start-alternative-control-structure-level (nth 5 start))
              (start-token-number (nth 6 start))
              (end-curly-bracket-level (nth 1 end))
              (end-round-bracket-level (nth 2 end))
              (end-square-bracket-level (nth 3 end))
              (end-inline-control-structure-level (nth 4 end))
              (end-alternative-control-structure-level (nth 5 end))
              (end-token-number (nth 6 end))
              (in-doc-comment (nth 7 start)))
          (let* ((indent-start (+ start-curly-bracket-level start-round-bracket-level start-square-bracket-level start-inline-control-structure-level start-alternative-control-structure-level))
                 (indent-end (+ end-curly-bracket-level end-round-bracket-level end-square-bracket-level end-inline-control-structure-level end-alternative-control-structure-level))
                 (indent-level indent-start)
                 (indent-adjust 0))
            ;; (message "indent-start %s, indent-end %s" indent-start indent-end)

            ;; When bracket count at start is larger than bracket count at end
            (when (and
                   (boundp 'phps-mode-lexer-tokens)
                   start-token-number
                   end-token-number)
              (let ((token-number start-token-number)
                    (valid-tokens t)
                    (last-token-is-opening-curly-bracket nil)
                    (first-token-is-closing-curly-bracket nil)
                    (tokens phps-mode-lexer-tokens)
                    (is-first-line-token t))

                ;; (message "token start %s, token end %s" start-token-number end-token-number)
                ;; (message "First token %s, last token %s" (car (nth start-token-number tokens)) (car (nth end-token-number tokens)))

                ;; Interate tokens in line and check if all are valid
                (while (and valid-tokens
                            (<= token-number end-token-number))
                  (let ((token (car (nth token-number tokens)))
                        (token-start (car (cdr (nth token-number tokens))))
                        (token-end (cdr (cdr (nth token-number tokens)))))

                    ;; Does token start on or after current line
                    ;; or does it end on or after current line?
                    (when (or (>= token-start line-start)
                              (>= token-end line-start))

                      ;; Is it the last token and is it a opening brace?
                      (when (and (= token-number end-token-number)
                                 (string= token "{"))
                        (setq last-token-is-opening-curly-bracket t))

                      ;; Is it the first line token?
                      (when is-first-line-token
                        (setq is-first-line-token nil)

                        ;; Is it a closing brace?
                        (when (or (string= token "}")
                                  (eq token 'T_ELSE)
                                  (eq token 'T_ELSEIF))
                          (setq first-token-is-closing-curly-bracket t)))

                      (when (and valid-tokens
                                 (not (or
                                       (string= token "{")
                                       (string= token "}")
                                       (string= token "(")
                                       (string= token ")")
                                       (string= token "[")
                                       (string= token "]")
                                       (string= token ";")
                                       (string= token ",")
                                       (eq token 'T_CLOSE_TAG))))
                        ;; (message "Token %s - %s in %s was invalid, line start %s" token token-number tokens line-start)
                        (setq valid-tokens nil))

                      )

                    (setq token-number (+ token-number 1))))

                (if valid-tokens
                    (progn

                      ;; If last token is a opening brace indent line one lesser column
                      (when last-token-is-opening-curly-bracket
                        ;; (message "Last token was opening brace")
                        (setq indent-level (- indent-level 1)))

                      ;; (message "Tokens was valid, decreasing indent %s - %s" (line-beginning-position) (line-end-position))
                      (setq indent-level (- indent-level (- indent-start indent-end))))

                  
                  ;; If first token is a closing brace indent line one lesser column
                  (when first-token-is-closing-curly-bracket
                    ;; (message "First token was closing brace")
                    (setq indent-level (- indent-level 1))))

                ))

            ;; If this line is part of a doc-comment increase indent with one unit
            (when in-doc-comment
              (setq indent-adjust 1))

            (let ((indent-sum (+ (* indent-level tab-width) indent-adjust))
                  (current-indentation (current-indentation)))

              (when (null current-indentation)
                (setq current-indentation 0))

              ;; Only continue if current indentation is wrong
              (when (not (equal indent-sum current-indentation))
                (let ((indent-diff (- indent-sum current-indentation)))
                  ;; (message "Indenting to %s current column %s" indent-sum (current-indentation))
                  ;; (message "inside scripting, start: %s, end: %s, indenting to column %s " start end indent-level)

                  (indent-line-to indent-sum)

                  ;; When indent is changed the trailing tokens and states just need to adjust their positions, this will improve speed of indent-region a lot
                  (phps-mode-lexer-move-tokens line-start indent-diff)
                  (phps-mode-lexer-move-states line-start indent-diff)
                  ;; (message "Moving tokens and states %s, %s to %s" indent-diff current-indentation indent-sum)
                  
                  ;; ;; Set point of change if it's not set or if it's larger than current point
                  ;; (when (or (not phps-mode-functions-buffer-changes-start)
                  ;;           (< line-start phps-mode-functions-buffer-changes-start))
                  ;;   ;; (message "Setting changes start from %s to %s" phps-mode-functions-buffer-changes-start start)
                  ;;   (setq phps-mode-functions-buffer-changes-start line-start))
                  
                  ;; (phps-mode-lexer-run-incremental)

                  )))))))))

;; TODO Fix flycheck error here
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

(defun phps-mode-functions-get-point-data ()
  "Return information about point in tokens."
  ;; (message "Point: %s in %s" (point) phps-mode-lexer-tokens)
  (when (boundp 'phps-mode-lexer-tokens)
    (save-excursion
      (beginning-of-line)
      (let ((line-beginning (point))
            (line-end (line-end-position))
            (start-in-scripting nil)
            (start-curly-bracket-level 0)
            (start-round-bracket-level 0)
            (start-square-bracket-level 0)
            (start-inline-control-structure-level 0)
            (start-alternative-control-structure-level 0)
            (start-token-number nil)
            (end-in-scripting nil)
            (end-curly-bracket-level 0)
            (end-round-bracket-level 0)
            (end-square-bracket-level 0)
            (end-inline-control-structure-level 0)
            (end-alternative-control-structure-level 0)
            (end-token-number nil)
            (line-in-doc-comment nil)
            (found-line-tokens nil)
            (after-special-control-structure nil)
            (round-brace-level 0)
            (start-expecting-semi-colon nil)
            (end-expecting-semi-colon nil)
            (first-token-on-line nil))
        (catch 'stop-iteration
          (dolist (item phps-mode-lexer-tokens)
            (let ((token (car item))
                  (token-start (car (cdr item)))
                  (token-end (cdr (cdr item))))
              ;; (message "Token: %s Start: %s End: %s Item: %s" token start end item)

              ;; Does token start after the end of line?
              (when (> token-start line-end)
                ;; (message "Stopping iteration at: %s %s" start position)
                (throw 'stop-iteration nil))

              ;; Did we find any token on current line?
              (when (and (not found-line-tokens)
                         (>= token-start line-beginning)
                         (<= token-end line-end))
                (setq found-line-tokens t))

              ;; When end of token is equal or less to beginning of current line
              (when (<= token-end line-beginning)

                ;; Increment start token number
                (if (null start-token-number)
                    (setq start-token-number 0)
                  (setq start-token-number (+ start-token-number 1)))

                (pcase token
                  ('T_OPEN_TAG (setq start-in-scripting t))
                  ('T_OPEN_TAG_WITH_ECHO (setq start-in-scripting t))
                  ('T_CLOSE_TAG (setq start-in-scripting nil))
                  ('T_CURLY_OPEN (setq start-curly-bracket-level (+ start-curly-bracket-level 1)))
                  ('T_DOLLAR_OPEN_CURLY_BRACES (setq start-curly-bracket-level (+ start-curly-bracket-level 1)))
                  ("{" (setq start-curly-bracket-level (+ start-curly-bracket-level 1)))
                  ("}" (setq start-curly-bracket-level (- start-curly-bracket-level 1)))
                  ("[" (setq start-square-bracket-level (+ start-square-bracket-level 1)))
                  ("]" (setq start-square-bracket-level (- start-square-bracket-level 1)))
                  ("(" (setq start-round-bracket-level (+ start-round-bracket-level 1)))
                  (")" (setq start-round-bracket-level (- start-round-bracket-level 1)))
                  (_))

                ;; Did we encounter end of alternative control structure?
                (when (or (equal token 'T_ENDIF)
                          (equal token 'T_ENDWHILE)
                          (equal token 'T_ENDFOR)
                          (equal token 'T_ENDFOREACH)
                          (equal token 'T_ENDSWITCH))
                  (setq start-alternative-control-structure-level (- start-alternative-control-structure-level 1)))

                ;; Reduce inline control structure level when we encounter a semi-colon after it's opening
                (when (and start-expecting-semi-colon
                           (string= token ";"))
                  (setq start-inline-control-structure-level (- start-inline-control-structure-level 1))
                  (setq start-expecting-semi-colon nil))

                )

              ;; Are we at the final line and inside a doc-comment that ends after it?
              (when (and (< token-start line-beginning)
                         (>= token-end line-end)
                         (eq token 'T_DOC_COMMENT))
                (setq line-in-doc-comment t))

              ;; When start of token is equal or less to end of curent line
              (when (<= token-start line-end)

                ;; Increment end token number
                (if (null end-token-number)
                    (setq end-token-number 0)
                  (setq end-token-number (+ end-token-number 1)))

                (pcase token
                  ('T_OPEN_TAG (setq end-in-scripting t))
                  ('T_OPEN_TAG_WITH_ECHO (setq end-in-scripting t))
                  ('T_CLOSE_TAG (setq end-in-scripting nil))
                  ('T_CURLY_OPEN (setq end-curly-bracket-level (+ end-curly-bracket-level 1)))
                  ('T_DOLLAR_OPEN_CURLY_BRACES (setq end-curly-bracket-level (+ end-curly-bracket-level 1)))
                  ("{" (setq end-curly-bracket-level (+ end-curly-bracket-level 1)))
                  ("}" (setq end-curly-bracket-level (- end-curly-bracket-level 1)))
                  ("[" (setq end-square-bracket-level (+ end-square-bracket-level 1)))
                  ("]" (setq end-square-bracket-level (- end-square-bracket-level 1)))
                  ("(" (setq end-round-bracket-level (+ end-round-bracket-level 1)))
                  (")" (setq end-round-bracket-level (- end-round-bracket-level 1)))
                  (_))

                ;; Do we encounter first token on line?
                (when (and (not first-token-on-line)
                           (>= token-start line-beginning)
                           (<= token-start line-end))
                  (setq first-token-on-line end-token-number))


                ;; Did we encounter end of alternative control structure?
                (when (or (equal token 'T_ENDIF)
                          (equal token 'T_ENDWHILE)
                          (equal token 'T_ENDFOR)
                          (equal token 'T_ENDFOREACH)
                          (equal token 'T_ENDSWITCH))
                  
                  (setq end-alternative-control-structure-level (- end-alternative-control-structure-level 1)))

                ;; Reduce inline control structure level when we encounter a semi-colon after it's opening
                (when (and end-expecting-semi-colon
                           (string= token ";"))
                  (setq end-inline-control-structure-level (- end-inline-control-structure-level 1))
                  (setq end-expecting-semi-colon nil))

                )

              ;; Keep track of general round brace level
              (when (string= token "(")
                (setq round-brace-level (+ round-brace-level 1)))
              (when (string= token ")")
                (setq round-brace-level (- round-brace-level 1)))

              ;; Are we after a special control structure
              ;; and does the round bracket level match initial round bracket level
              ;; and is token not a round bracket
              (when (and after-special-control-structure
                         (= after-special-control-structure round-brace-level)
                         (not (string= token ")"))
                         (not (string= token "(")))

                ;; Is token not a curly bracket - because that is a ordinary control structure syntax
                (when (not (string= token "{"))
                  ;; (message "After special control structure %s in buffer: %s tokens: %s token-start: %s" token (buffer-substring-no-properties (point-min) (point-max)) phps-mode-lexer-tokens token-start)
                  (if (string= token ":")
                      (progn
                        ;; (message "Was colon")

                        ;; Is token at or before line beginning?
                        (when (or (<= token-end line-beginning)
                                  (= first-token-on-line end-token-number))
                          (setq start-alternative-control-structure-level (+ start-alternative-control-structure-level 1)))

                        ;; Is token at or before line end?
                        (when (<= token-start line-end)
                          (setq end-alternative-control-structure-level (+ end-alternative-control-structure-level 1)))

                        )

                    (when (or (<= token-end line-beginning)
                              (= first-token-on-line end-token-number))
                      (setq start-inline-control-structure-level (+ start-inline-control-structure-level 1))
                      (setq start-expecting-semi-colon t))


                    (when (<= token-start line-end)
                      (setq end-inline-control-structure-level (+ end-inline-control-structure-level 1))
                      (setq end-expecting-semi-colon t))

                    ;; (message "Was not colon")
                    ))

                (setq after-special-control-structure nil))

              ;; Does current token support inline and alternative syntax?
              (when (or
                     (equal token 'T_IF)
                     (equal token 'T_WHILE)
                     (equal token 'T_FOR)
                     (equal token 'T_FOREACH)
                     (equal token 'T_SWITCH)
                     (equal token 'T_ELSE)
                     (equal token 'T_ELSEIF)
                     (equal token 'T_CASE))
                ;; (message "Found special control structure %s %s" token start-round-bracket-level)
                (setq after-special-control-structure round-brace-level)


                ))))
        (when (not found-line-tokens)
          (setq start-token-number nil)
          (setq end-token-number nil))
        (let ((data (list (list start-in-scripting start-curly-bracket-level start-round-bracket-level start-square-bracket-level start-inline-control-structure-level start-alternative-control-structure-level start-token-number line-in-doc-comment) (list end-in-scripting end-curly-bracket-level end-round-bracket-level end-square-bracket-level end-inline-control-structure-level end-alternative-control-structure-level end-token-number line-in-doc-comment))))
          ;; (message "data: %s" data)
          data)))))

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

  (add-hook 'after-change-functions #'phps-mode-functions-after-change)

  ;; NOTE Implement indent-region?
  )


(provide 'phps-mode-functions)

;;; phps-mode-functions.el ends here
