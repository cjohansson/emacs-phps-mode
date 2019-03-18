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

(defvar phps-mode-functions-allow-after-change t
  "Flag to tell us whether after change detection is enabled or not.")

(defvar phps-mode-functions-buffer-changes-start nil
  "Start point of buffer changes, nil if none.")

(defvar phps-mode-functions-lines-indent nil
  "The indentation of each line in buffer, nil if none.")

(defvar phps-mode-functions-imenu nil
  "The Imenu alist for current buffer, nil if none.")

(defvar phps-mode-functions-processed-buffer nil
  "Flag whether current buffer is processed or not.")

(defvar phps-mode-functions-verbose nil
  "Verbose messaging, default nil.")


;; NOTE Also format white-space inside the line, i.e. after function declarations?


(defun phps-mode-functions-get-buffer-changes-start ()
  "Get buffer change start."
  phps-mode-functions-buffer-changes-start)

(defun phps-mode-functions-reset-buffer-changes-start ()
  "Reset buffer change start."
  ;; (message "Reset flag for buffer changes")
  (setq phps-mode-functions-buffer-changes-start nil))

(defun phps-mode-functions-process-current-buffer ()
  "Process current buffer, generate indentations and Imenu."
  ;; (message "(phps-mode-functions-process-current-buffer)")
  (when (phps-mode-functions-get-buffer-changes-start)
    (phps-mode-lexer-run-incremental)
    (setq phps-mode-functions-processed-buffer nil))
  (unless phps-mode-functions-processed-buffer
    (phps-mode-functions--process-current-buffer)
    (setq phps-mode-functions-processed-buffer t)))

(defun phps-mode-functions-get-lines-indent ()
  "Return lines indent, process buffer if not done already."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-lines-indent)

(defun phps-mode-functions-get-imenu ()
  "Return Imenu, process buffer if not done already."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-imenu)

(defun phps-mode-functions--get-lines-in-buffer (beg end)
  "Return the number of lines in buffer between BEG and END."
  (phps-mode-functions--get-lines-in-string (buffer-substring-no-properties beg end)))

(defun phps-mode-functions--get-lines-in-string (string)
  "Return the number of lines in STRING."
  (let ((lines-in-string 0)
        (start 0))
    (while (string-match "[\n\C-m]" string start)
      (setq start (match-end 0))
      (setq lines-in-string (1+ lines-in-string)))
    lines-in-string))

(defun phps-mode-functions--process-current-buffer ()
  "Process current buffer and generation indentation and Imenu in one iteration.  Complexity: O(n)."
  (if (boundp 'phps-mode-lexer-tokens)
      (save-excursion
        ;; (message "Processing current buffer")
        (goto-char (point-min))
        (when phps-mode-functions-verbose
          (message "\nCalculation indentation for all lines in buffer:\n\n%s" (buffer-substring-no-properties (point-min) (point-max))))
        (let ((in-heredoc nil)
              (in-heredoc-started-this-line nil)
              (in-heredoc-ended-this-line nil)
              (in-inline-control-structure nil)
              (first-token-is-inline-html nil)
              (after-special-control-structure nil)
              (after-special-control-structure-token nil)
              (after-extra-special-control-structure nil)
              (after-extra-special-control-structure-first-on-line nil)
              (switch-curly-stack nil)
              (switch-alternative-stack nil)
              (switch-case-alternative-stack nil)
              (curly-bracket-level 0)
              (round-bracket-level 0)
              (square-bracket-level 0)
              (alternative-control-structure-level 0)
              (in-concatenation nil)
              (in-concatenation-round-bracket-level nil)
              (in-concatenation-square-bracket-level nil)
              (in-concatenation-level 0)
              (column-level 0)
              (column-level-start 0)
              (tuning-level 0)
              (nesting-start 0)
              (nesting-end 0)
              (last-line-number 0)
              (first-token-on-line nil)
              (line-indents (make-hash-table :test 'equal))
              (first-token-is-nesting-decrease nil)
              (token-number 1)
              (allow-custom-column-increment nil)
              (allow-custom-column-decrement nil)
              (in-assignment nil)
              (in-assignment-round-bracket-level nil)
              (in-assignment-square-bracket-level nil)
              (in-assignment-level 0)
              (in-class-declaration nil)
              (in-class-declaration-level 0)
              (in-return nil)
              (in-return-curly-bracket-level nil)
              (in-return-level 0)
              (token nil)
              (token-start nil)
              (token-end nil)
              (token-start-line-number 0)
              (token-end-line-number 0)
              (tokens (nreverse phps-mode-lexer-tokens))
              (nesting-stack nil)
              (nesting-key nil)
              (class-declaration-started-this-line nil)
              (special-control-structure-started-this-line nil)
              (temp-pre-indent nil)
              (temp-post-indent nil)
              (imenu-index '())
              (imenu-in-namespace-declaration nil)
              (imenu-in-namespace-name nil)
              (imenu-open-namespace-level nil)
              (imenu-in-class-declaration nil)
              (imenu-open-class-level nil)
              (imenu-in-class-name nil)
              (imenu-in-function-declaration nil)
              (imenu-in-function-name nil)
              (imenu-nesting-level 0)
              (incremental-line-number 1))

          (push `(END_PARSE ,(point-max) . ,(point-max)) tokens)

          ;; Iterate through all buffer tokens from beginning to end
          (dolist (item (nreverse tokens))
            ;; (message "Items: %s %s" item phps-mode-lexer-tokens)
            (let ((next-token (car item))
                  (next-token-start (car (cdr item)))
                  (next-token-end (cdr (cdr item)))
                  (next-token-start-line-number nil)
                  (next-token-end-line-number nil))

              (when token
                (setq incremental-line-number (+ incremental-line-number (phps-mode-functions--get-lines-in-buffer token-end next-token-start))))

              ;; Handle the pseudo-token for last-line
              (if (equal next-token 'END_PARSE)
                  (progn
                    (setq next-token-start-line-number (1+ token-start-line-number))
                    (setq next-token-end-line-number (1+ token-end-line-number)))
                (setq next-token-start-line-number incremental-line-number)
                (setq incremental-line-number (+ incremental-line-number (phps-mode-functions--get-lines-in-buffer next-token-start next-token-end)))
                (setq next-token-end-line-number incremental-line-number)
                (when phps-mode-functions-verbose
                  (message "Token '%s' pos: %s-%s lines: %s-%s" next-token next-token-start next-token-end next-token-start-line-number next-token-end-line-number)))

              ;; Token logic
              (when token

                ;; IMENU LOGIC

                (cond
                 ((string= token "{")
                  (setq imenu-nesting-level (1+ imenu-nesting-level)))
                 ((string= token "}")

                  (when (and imenu-open-namespace-level
                             (= imenu-open-namespace-level imenu-nesting-level)
                             imenu-in-namespace-name)
                    (setq imenu-in-namespace-name nil))

                  (when (and imenu-open-class-level
                             (= imenu-open-class-level imenu-nesting-level)
                             imenu-in-class-name)
                    (setq imenu-in-class-name nil))

                  (setq imenu-nesting-level (1- imenu-nesting-level))))
                
                (cond

                 (imenu-in-namespace-declaration
                  (cond

                   ((string= token "{")
                    (setq imenu-open-namespace-level imenu-nesting-level)
                    (setq imenu-in-namespace-declaration nil))

                   ((string= token ";")
                    (setq imenu-in-namespace-declaration nil))

                   ((and (equal token 'T_STRING)
                         (not imenu-in-namespace-name))
                    (let ((imenu-index-name (format "\\%s" (buffer-substring-no-properties token-start token-end)))
                          (imenu-index-pos token-start))
                      (setq imenu-in-namespace-name imenu-index-name)
                      (push `(,imenu-index-name . ,imenu-index-pos) imenu-index)))))

                 (imenu-in-class-declaration
                  (cond

                   ((string= token "{")
                    (setq imenu-open-class-level imenu-nesting-level)
                    (setq imenu-in-class-declaration nil))

                   ((and (equal token 'T_STRING)
                         (not imenu-in-class-name))
                    (let ((imenu-index-name (format "%s" (buffer-substring-no-properties token-start token-end)))
                          (imenu-index-pos token-start))
                      (setq imenu-in-class-name imenu-index-name)
                      (when imenu-in-namespace-name
                        (setq imenu-index-name (concat imenu-in-namespace-name "\\" imenu-index-name)))
                      (push `(,imenu-index-name . ,imenu-index-pos) imenu-index)))))

                 (imenu-in-function-declaration
                  (cond

                   ((string= token "{")
                    (setq imenu-in-function-name nil)
                    (setq imenu-in-function-declaration nil))

                   ((string= token ";")
                    (setq imenu-in-function-declaration nil))

                   ((and (equal token 'T_STRING)
                         (not imenu-in-function-name))
                    (let ((imenu-index-name (format "%s()" (buffer-substring-no-properties token-start token-end)))
                          (imenu-index-pos token-start))
                      (setq imenu-in-function-name imenu-index-name)
                      (when imenu-in-class-name
                        (setq imenu-index-name (concat imenu-in-class-name "->" imenu-index-name)))
                      (when imenu-in-namespace-name
                        (setq imenu-index-name (concat imenu-in-namespace-name "\\" imenu-index-name)))
                      (push `(,imenu-index-name . ,imenu-index-pos) imenu-index)))))

                 (t (cond

                     ((equal token 'T_NAMESPACE)
                      (setq imenu-in-namespace-name nil)
                      (setq imenu-in-namespace-declaration t))

                     ((equal token 'T_CLASS)
                      (setq imenu-in-class-name nil)
                      (setq imenu-in-class-declaration t))

                     ((equal token 'T_FUNCTION)
                      (setq imenu-in-function-name nil)
                      (setq imenu-in-function-declaration t)))))

                ;; INDENTATION LOGIC

                ;; Keep track of round bracket level
                (when (string= token "(")
                  (setq round-bracket-level (1+ round-bracket-level)))
                (when (string= token ")")
                  (setq round-bracket-level (1- round-bracket-level))
                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Keep track of square bracket level
                (when (string= token "[")
                  (setq square-bracket-level (1+ square-bracket-level)))
                (when (string= token "]")
                  (setq square-bracket-level (1- square-bracket-level))
                  (when first-token-on-line
                    (setq first-token-is-nesting-decrease t)))

                ;; Detect in inline-html
                (when (and (equal token 'T_INLINE_HTML)
                           first-token-on-line)
                  (setq first-token-is-inline-html t))

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
                  (setq curly-bracket-level (1+ curly-bracket-level)))
                (when (string= token "}")
                  (setq curly-bracket-level (1- curly-bracket-level))

                  (when (and switch-curly-stack
                             (= (1+ curly-bracket-level) (car switch-curly-stack)))

                    (when phps-mode-functions-verbose
                      (message "Ended switch curly stack at %s" curly-bracket-level))

                    (setq allow-custom-column-decrement t)
                    (pop nesting-stack)
                    (setq alternative-control-structure-level (1- alternative-control-structure-level))
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

                  (when (and (equal token 'T_ENDSWITCH)
                             switch-case-alternative-stack)

                    (when phps-mode-functions-verbose
                      (message "Ended alternative switch stack at %s" alternative-control-structure-level))
                    
                    (pop switch-alternative-stack)
                    (pop switch-case-alternative-stack)
                    (setq allow-custom-column-decrement t)
                    (pop nesting-stack)
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
                            )

                        ;; Don't start inline control structures after a while ($condition); expression
                        (when (not (string= token ";"))
                          (when phps-mode-functions-verbose
                            (message "\nStarted inline control-structure after %s at %s\n" after-special-control-structure-token token))

                          (setq in-inline-control-structure t)
                          (setq temp-pre-indent (1+ column-level)))))

                    (setq after-special-control-structure nil)
                    (setq after-special-control-structure-token nil)))

                ;; Support extra special control structures (CASE)
                (when (and after-extra-special-control-structure
                           (string= token ":"))
                  (setq alternative-control-structure-level (1+ alternative-control-structure-level))
                  (when after-extra-special-control-structure-first-on-line
                    (setq first-token-is-nesting-decrease t))
                  (setq after-extra-special-control-structure nil))

                ;; Keep track of concatenation
                (if in-concatenation
                    (when (or (string= token ";")
                              (and (string= token ")")
                                   (< round-bracket-level (car in-concatenation-round-bracket-level)))
                              (and (string= token ",")
                                   (= round-bracket-level (car in-concatenation-round-bracket-level))
                                   (= square-bracket-level (car in-concatenation-square-bracket-level)))
                              (and (string= token"]")
                                   (< square-bracket-level (car in-concatenation-square-bracket-level))))
                      (when phps-mode-functions-verbose
                        (message "Ended concatenation"))
                      (pop in-concatenation-round-bracket-level)
                      (pop in-concatenation-square-bracket-level)
                      (unless in-concatenation-round-bracket-level
                        (setq in-concatenation nil))
                      (setq in-concatenation-level (1- in-concatenation-level)))
                  (when (and (> next-token-start-line-number token-end-line-number)
                             (or (string= token ".")
                                 (string= next-token ".")))
                    (when phps-mode-functions-verbose
                      (message "Started concatenation"))
                    (setq in-concatenation t)
                    (push round-bracket-level in-concatenation-round-bracket-level)
                    (push square-bracket-level in-concatenation-square-bracket-level)
                    (setq in-concatenation-level (1+ in-concatenation-level))))

                ;; Did we reach a semicolon inside a inline block? Close the inline block
                (when (and in-inline-control-structure
                           (string= token ";")
                           (not special-control-structure-started-this-line))
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
                  (setq after-special-control-structure round-bracket-level)
                  (setq after-special-control-structure-token token)
                  (setq nesting-key token)
                  (setq special-control-structure-started-this-line t)

                  ;; ELSE and ELSEIF after a IF, ELSE, ELESIF
                  ;; and DEFAULT after a CASE
                  ;; should decrease alternative control structure level
                  (when (and nesting-stack
                             (string= (car (cdr (cdr (cdr (car nesting-stack))))) ":")
                             (or
                              (and (or (equal token 'T_ELSE)
                                       (equal token 'T_ELSEIF))
                                   (or (equal (car (cdr (cdr (car nesting-stack)))) 'T_IF)
                                       (equal (car (cdr (cdr (car nesting-stack)))) 'T_ELSEIF)
                                       (equal (car (cdr (cdr (car nesting-stack)))) 'T_ELSE)))
                              (and (equal token 'T_DEFAULT)
                                   (equal (car (cdr (cdr (car nesting-stack)))) 'T_CASE))))
                    (setq alternative-control-structure-level (1- alternative-control-structure-level))

                    (when first-token-on-line
                      (setq first-token-is-nesting-decrease t))

                    (when phps-mode-functions-verbose
                      (message "\nDecreasing alternative control structure nesting at %s to %s\n" token alternative-control-structure-level)))

                  )

                ;; Keep track of assignments
                (when in-assignment
                  (when (or (string= token ";")
                            (and (string= token ")")
                                 (or (< round-bracket-level (car in-assignment-round-bracket-level))
                                     (and
                                      (= round-bracket-level (car in-assignment-round-bracket-level))
                                      (string= next-token ")"))))
                            (and (string= token ",")
                                 (= round-bracket-level (car in-assignment-round-bracket-level))
                                 (= square-bracket-level (car in-assignment-square-bracket-level)))
                            (and (string= token"]")
                                 (< square-bracket-level (car in-assignment-square-bracket-level)))
                            (and (equal token 'T_FUNCTION)
                                 (= round-bracket-level (car in-assignment-round-bracket-level))))

                    ;; NOTE Ending an assignment because of function token is to support PSR-2 Closures
                    
                    (when phps-mode-functions-verbose
                      (message "Ended assignment at %s %s" token next-token))
                    (pop in-assignment-square-bracket-level)
                    (pop in-assignment-round-bracket-level)
                    (unless in-assignment-round-bracket-level
                      (setq in-assignment nil))
                    (setq in-assignment-level (1- in-assignment-level))))
                (when (and (not after-special-control-structure)
                           (or (string= token "=")
                               (equal token 'T_DOUBLE_ARROW)
                               (equal token 'T_CONCAT_EQUAL)
                               (equal token 'T_POW_EQUAL)
                               (equal token 'T_DIV_EQUAL)
                               (equal token 'T_PLUS_EQUAL)
                               (equal token 'T_MINUS_EQUAL)
                               (equal token 'T_MUL_EQUAL)
                               (equal token 'T_MOD_EQUAL)
                               (equal token 'T_SL_EQUAL)
                               (equal token 'T_SR_EQUAL)
                               (equal token 'T_AND_EQUAL)
                               (equal token 'T_OR_EQUAL)
                               (equal token 'T_XOR_EQUAL)
                               (equal token 'T_COALESCE_EQUAL)))
                  (when phps-mode-functions-verbose
                    (message "Started assignment"))
                  (setq in-assignment t)
                  (push round-bracket-level in-assignment-round-bracket-level)
                  (push square-bracket-level in-assignment-square-bracket-level)
                  (setq in-assignment-level (1+ in-assignment-level)))

                ;; Keep track of return expressions
                (when in-return
                  (when (and (string= token ";")
                             (= curly-bracket-level (car in-return-curly-bracket-level)))

                    (when phps-mode-functions-verbose
                      (message "Ended return at %s" token))
                    (pop in-return-curly-bracket-level)
                    (unless in-return-curly-bracket-level
                      (setq in-return nil))
                    (setq in-return-level (1- in-return-level))))
                (when (equal token 'T_RETURN)
                  (when phps-mode-functions-verbose
                    (message "Started return"))
                  (setq in-return t)
                  (push curly-bracket-level in-return-curly-bracket-level)
                  (setq in-return-level (1+ in-return-level)))

                ;; Keep track of object operators
                (when (and (equal token 'T_OBJECT_OPERATOR)
                           first-token-on-line))

                ;; Did we encounter a token that supports extra special alternative control structures?
                (when (equal token 'T_CASE)
                  (setq after-extra-special-control-structure t)
                  (setq nesting-key token)
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

                (when phps-mode-functions-verbose
                  (message "Processing token: %s" token))
                
                ;; Calculate nesting
                (setq nesting-end (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level in-assignment-level in-class-declaration-level in-concatenation-level in-return-level))

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
                  (let ((nesting-decrement 0))

                    ;; Handle case were nesting has decreased less than next as well
                    (while (and nesting-stack
                                (<= nesting-end (car (car nesting-stack))))
                      (when phps-mode-functions-verbose
                        (message "\nPopping %s from nesting-stack since %s is lesser or equal to %s, next value is: %s\n" (car nesting-stack) nesting-end (car (car nesting-stack)) (nth 1 nesting-stack)))
                      (pop nesting-stack)
                      (setq nesting-decrement (1+ nesting-decrement)))

                    (if first-token-is-nesting-decrease

                        (progn
                          ;; Decrement column
                          (if allow-custom-column-decrement
                              (progn
                                (when phps-mode-functions-verbose
                                  (message "Doing custom decrement 1 from %s to %s" column-level (- column-level (- nesting-start nesting-end))))
                                (setq column-level (- column-level (- nesting-start nesting-end)))
                                (setq allow-custom-column-decrement nil))
                            (when phps-mode-functions-verbose
                              (message "Doing regular decrement 1 from %s to %s" column-level (1- column-level)))
                            (setq column-level (- column-level nesting-decrement)))

                          ;; Prevent negative column-values
                          (when (< column-level 0)
                            (setq column-level 0)))

                      (unless temp-post-indent
                        (when phps-mode-functions-verbose
                          (message "Temporary setting post indent %s" column-level))
                        (setq temp-post-indent column-level))

                      ;; Decrement column
                      (if allow-custom-column-decrement
                          (progn
                            (when phps-mode-functions-verbose
                              (message "Doing custom decrement 2 from %s to %s" column-level (- column-level (- nesting-start nesting-end))))
                            (setq temp-post-indent (- temp-post-indent (- nesting-start nesting-end)))
                            (setq allow-custom-column-decrement nil))
                        (setq temp-post-indent (- temp-post-indent nesting-decrement)))

                      ;; Prevent negative column-values
                      (when (< temp-post-indent 0)
                        (setq temp-post-indent 0))

                      )))

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

                      ;; Inline HTML should have zero indent
                      (when first-token-is-inline-html
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
                            (message "\nPushing (%s %s %s %s) to nesting-stack since %s is greater than %s or stack is empty\n" nesting-start nesting-end nesting-key token nesting-end (car (cdr (car nesting-stack))))
                            )
                          (push `(,nesting-stack-end ,nesting-end ,nesting-key ,token) nesting-stack)
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

                          ;; Inline HTML should have no indent
                          (when (equal token 'T_INLINE_HTML)
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
                      (setq nesting-start (+ round-bracket-level square-bracket-level curly-bracket-level alternative-control-structure-level in-assignment-level in-class-declaration-level in-concatenation-level in-return-level))

                      ;; Set initial values for tracking first token
                      (when (> token-start-line-number last-line-number)
                        (setq first-token-on-line t)
                        (setq first-token-is-nesting-decrease nil)
                        (setq first-token-is-inline-html nil)
                        (setq in-class-declaration-level 0)
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
              (setq token-start next-token-start)
              (setq token-end next-token-end)
              (setq token-start-line-number next-token-start-line-number)
              (setq token-end-line-number next-token-end-line-number)
              (setq token-number (1+ token-number))))
          (setq phps-mode-functions-imenu (nreverse imenu-index))
          (setq phps-mode-functions-lines-indent line-indents)))
    (setq phps-mode-functions-imenu nil)
    (setq phps-mode-functions-lines-indent nil)))

(defun phps-mode-functions-around-newline (old-function &rest arguments)
  "Call OLD-FUNCTION with ARGUMENTS and then shift indexes if the rest of the line is just white-space."
  (if (string= major-mode "phps-mode")
      (progn
        ;; (message "Running advice")
        (let ((old-pos (point))
              (new-pos)
              (looking-at-whitespace (looking-at-p "[\ \n\t\r]*\n")))

          ;; Temporarily disable change detection to not trigger incremental lexer
          (setq phps-mode-functions-allow-after-change nil)
          (apply old-function arguments)
          (setq phps-mode-functions-allow-after-change t)

          (if looking-at-whitespace
              (progn
                ;; (message "Looking at white-space")
                (setq new-pos (point))
                (let ((diff (- new-pos old-pos)))
                  (when (> diff 0)
                    (phps-mode-lexer-move-tokens old-pos diff)
                    (phps-mode-lexer-move-states old-pos diff)
                    ;; (message "Old pos %s, new pos: %s, diff: %s" old-pos new-pos diff)
                    )))
            ;; (message "Not looking at white-space")
            )))
    (apply old-function arguments)))

(defun phps-mode-functions-indent-line ()
  "Indent line."
  (phps-mode-functions-process-current-buffer)
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

              ;; Reset change flag
              (phps-mode-functions-reset-buffer-changes-start))))))))

(defun phps-mode-functions-after-change (start _stop _length)
  "Track buffer change from START to STOP with length LENGTH."
  (when (and (string= major-mode "phps-mode")
             phps-mode-functions-allow-after-change)

    ;; If we haven't scheduled incremental lexer before - do it
    (when (and (not phps-mode-functions-buffer-changes-start)
               (boundp 'phps-mode-idle-interval)
               phps-mode-idle-interval)
      ;; (message "Enqueued incremental lexer")
      (run-with-idle-timer phps-mode-idle-interval nil #'phps-mode-lexer-run-incremental))

    ;; When point of change is not set or when start of new changes precedes old change - update the point
    (when (or (not phps-mode-functions-buffer-changes-start)
              (< start phps-mode-functions-buffer-changes-start))
      (setq phps-mode-functions-buffer-changes-start start)
      ;; (message "Setting start of changes to: %s-%s" phps-mode-functions-buffer-changes-start stop))

    ;; (message "phps-mode-functions-after-change %s %s %s" start stop length)
    )))

(defun phps-mode-functions-imenu-create-index ()
  "Get Imenu for current buffer."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-imenu)

(defun phps-mode-functions-comment-region (beg end &optional _arg)
  "Comment region from BEG to END with optional ARG."
  (save-excursion
    ;; Go to start of region
    (goto-char beg)

    (let ((end-line-number (line-number-at-pos end t))
          (current-line-number (line-number-at-pos))
          (first-line t))

      ;; Does region start at beginning of line?
      (if (not (= beg (line-beginning-position)))

          ;; Use doc comment
          (progn
            (goto-char end)
            (insert " */")
            (goto-char beg)
            (insert "/* "))

        ;; Do this for every line in region
        (while (or first-line
                   (< current-line-number end-line-number))
          (move-beginning-of-line nil)

          (when first-line
            (setq first-line nil))

          ;; Does this line contain something other than white-space?
          (unless (eq (point) (line-end-position))
            (insert "// ")
            (move-end-of-line nil)
            (insert ""))

          (when (< current-line-number end-line-number)
            (line-move 1))
          (setq current-line-number (1+ current-line-number)))))))

(defun phps-mode-functions-uncomment-region (beg end &optional _arg)
  "Comment region from BEG to END with optional ARG."
  (save-excursion

    ;; Go to start of region
    (goto-char beg)

    (let ((end-line-number (line-number-at-pos end t))
          (current-line-number (line-number-at-pos))
          (first-line t))

      ;; Does region start at beginning of line?
      (if (not (= beg (line-beginning-position)))
          (progn
            (goto-char end)
            (backward-char 3)
            (when (looking-at-p " \\*\/")
              (delete-char 3))

            (goto-char beg)
            (when (looking-at-p "\/\/ ")
              (delete-char 3))
            (when (looking-at-p "\/\\* ")
              (delete-char 3)))

        ;; Do this for every line in region
        (while (or first-line
                   (< current-line-number end-line-number))
          (move-beginning-of-line nil)

          (when first-line
            (setq first-line nil))

          ;; Does this line contain something other than white-space?
          (unless (>= (+ (point) 3) (line-end-position))
            (when (looking-at-p "\/\/ ")
              (delete-char 3))
            (when (looking-at-p "\/\\* ")
              (delete-char 3))

            (move-end-of-line nil)

            (backward-char 3)
            (when (looking-at-p " \\*\/")
              (delete-char 3)))

          (when (< current-line-number end-line-number)
            (line-move 1))
          (setq current-line-number (1+ current-line-number)))))))

(defun phps-mode-functions-init ()
  "PHP specific init-cleanup routines."

  ;; Custom indentation
  ;; NOTE Indent-region will call this on each line of region
  (set (make-local-variable 'indent-line-function) #'phps-mode-functions-indent-line)

  ;; Custom Imenu
  (set (make-local-variable 'imenu-create-index-function) #'phps-mode-functions-imenu-create-index)

  ;; Should we follow PSR-2?
  (when (and (boundp 'phps-mode-use-psr-2)
             phps-mode-use-psr-2)

    ;; Code MUST use an indent of 4 spaces
    (set (make-local-variable 'tab-width) 4)

    ;; MUST NOT use tabs for indenting
    (set (make-local-variable 'indent-tabs-mode) nil))

  ;; Add support for moving indexes quickly when making newlines
  (advice-add #'newline :around #'phps-mode-functions-around-newline)

  ;; Reset flags
  (set (make-local-variable 'phps-mode-functions-allow-after-change) t)
  (set (make-local-variable 'phps-mode-functions-buffer-changes-start) nil)
  (set (make-local-variable 'phps-mode-functions-lines-indent) nil)
  (set (make-local-variable 'phps-mode-functions-imenu) nil)
  (set (make-local-variable 'phps-mode-functions-processed-buffer) nil)

  ;; Make (comment-region) and (uncomment-region) work
  (set (make-local-variable 'comment-region-function) #'phps-mode-functions-comment-region)
  (set (make-local-variable 'uncomment-region-function) #'phps-mode-functions-uncomment-region)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  ;; Support for change detection
  (add-hook 'after-change-functions #'phps-mode-functions-after-change))


(provide 'phps-mode-functions)

;;; phps-mode-functions.el ends here
