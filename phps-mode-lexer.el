;;; phps-mode-lexer.el -- Lexer for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

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

;; The idea is gathering everything directly related to the lexer in this file,
;; any higher order meta-lexer logic goes into `phps-mode-lex-analyzer.el'.
;;
;; Features:
;; * Defines the lexer for this grammar based on the Zend PHP Lexer at
;;  `https://github.com/php/php-src/blob/master/Zend/zend_language_scanner.l'
;;  which is using re2c.

;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-wy-macros)

(require 'semantic)
(require 'semantic/lex)
(require 'subr-x)


;; INITIALIZE SETTINGS


(phps-mode-wy-macros--CG 'PARSER_MODE t)
(phps-mode-wy-macros--CG 'SHORT_TAGS t)


;; SETTINGS


;; @see https://secure.php.net/manual/en/language.types.integer.php
(defconst phps-mode-lexer--long-limit 2147483648
  "Limit for 32-bit integer.")

(defconst phps-mode-lexer--BNUM "0b[01]+"
  "Boolean number.")

(defconst phps-mode-lexer--HNUM "0x[0-9a-fA-F]+"
  "Hexadecimal number.")

(defconst phps-mode-lexer--LNUM "[0-9]+"
  "Long number.")

(defconst phps-mode-lexer--DNUM "\\([0-9]*\\.[0-9]+\\)\\|\\([0-9]+\\.[0-9]*\\)"
  "Double number.")

(defconst phps-mode-lexer--EXPONENT_DNUM
  (format "\\(\\(%s\\|%s\\)[eE][\\+-]?%s\\)"
          phps-mode-lexer--LNUM
          phps-mode-lexer--DNUM
          phps-mode-lexer--LNUM)
  "Exponent double number.")

(defconst phps-mode-lexer--LABEL
  "[A-Za-z_[:nonascii:]][0-9A-Za-z_[:nonascii:]]*"
  "Labels are used for names.")
;; NOTE original is [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*
;; NOTE Rebuilt for comparability with emacs-lisp

(defconst phps-mode-lexer--WHITESPACE "[ \n\r\t]+"
  "White-space.")

(defconst phps-mode-lexer--TABS_AND_SPACES "[ \t]*"
  "Tabs and white-spaces.")

(defconst phps-mode-lexer--TOKENS "[][;:,.()|^&+/*=%!~$<>?@-]"
  "Tokens.")
;; NOTE Original is [;:,.\[\]()|^&+-/*=%!~$<>?@]
;; NOTE The hyphen moved last since it has special meaning and to avoid it being interpreted as a range.

(defconst phps-mode-lexer--ANY_CHAR "[^z-a]"
  "Any character.  The Zend equivalent is [^] but is not possible in Emacs Lisp.")

(defconst phps-mode-lexer--NEWLINE "[\n\r]"
  "Newline characters.  The Zend equivalent is (\"\r\"|\"\n\"|\"\r\n\").")

;; VARIABLES

(defvar phps-mode-lexer--re2c-matching-body nil
  "Lambda-expression for longest matching condition.")

(defvar phps-mode-lexer--re2c-matching-length nil
  "Length of longest matching condition.")

(defvar phps-mode-lexer--re2c-matching-data nil
  "Match data for longest matching condition.")

(defvar phps-mode-lexer--EXPECTED nil
  "Flag whether something is expected or not.")


;; FUNCTIONS


(defun phps-mode-lexer--BEGIN (state)
  "Begin STATE."
  state)

;; _yy_push_state
(defun phps-mode-lexer--yy_push_state (new-state state_stack)
  "Add NEW-STATE to stack and then begin state."
  (push state state_stack)
  ;; (message "Added state %s to stack begun state %s" state new-state)
  (list new-state state_stack))

(defun phps-mode-lexer--yy_pop_state (state_stack)
  "Pop current state from stack."
  (let ((old-state (pop state_stack)))
    ;; (message "Going back to poppped state %s" old-state)
    (if old-state
        (phps-mode-lexer--BEGIN old-state)
      (signal
       'error
       (list
        (format "PHPs Lexer Error - Trying to pop last state at %d" (point))
        (point))))
    state_stack))

(defun phps-mode-lexer--MOVE_FORWARD (position)
  "Move forward to POSITION."
  (setq semantic-lex-end-point position))

(defun phps-mode-lexer--yyless (points)
  "Move lexer back POINTS."
  (setq semantic-lex-end-point (- semantic-lex-end-point points)))

(defun phps-mode-lexer--inline-char-handler ()
  "Mimic inline_char_handler."
  (let ((start (match-beginning 0)))
    (let ((string-start (search-forward "<?" nil t)))
      (if string-start
          (phps-mode-lexer--RETURN_TOKEN 'T_INLINE_HTML start (- string-start 2))
        (phps-mode-lexer--RETURN_TOKEN 'T_INLINE_HTML start (point-max))))))

(defun phps-mode-lexer--RETURN_OR_SKIP_TOKEN (token start end)
  "Return TOKEN with START and END but only in parse-mode."
  (when (phps-mode-wy-macros--CG 'PARSER_MODE)
    (phps-mode-lexer--emit-token token start end)))

(defun phps-mode-lexer--RETURN_TOKEN (token start end)
  "Push TOKEN to list with START and END."
  (phps-mode-lexer--emit-token token start end))

(defun phps-mode-lexer--emit-token (token start end state state_stack states)
  "Emit TOKEN with START and END."

  ;; (when (and
  ;;        (equal token 'T_INLINE_HTML)
  ;;        phps-mode-inline-mmm-submode
  ;;        (fboundp 'mmm-make-region))
  ;;   (mmm-make-region phps-mode-inline-mmm-submode start end))

  (semantic-lex-push-token (semantic-lex-token token start end))

  ;; Push token start, end, lexer state and state stack to variable
  (push (list start end state state_stack) states)
  states)

;; TODO Figure out what this does
(defun phps-mode-lexer--SKIP_TOKEN (_token _start _end)
  "Skip TOKEN to list with START and END.")

(defun phps-mode-lexer--re2c-rule (condition body)
  "Process rule with CONDITION and BODY."
  (when condition
    (let ((match-end (match-end 0))
          (match-beginning (match-beginning 0)))
      (let ((matching-length (- match-end match-beginning)))
        (when (> matching-length 0)
          (when (or (not phps-mode-lexer--re2c-matching-length)
                    (> matching-length phps-mode-lexer--re2c-matching-length))
            (list
             phps-mode-lexer--re2c-matching-length matching-length
             (match-data)
             body)))))))

(defun phps-mode-lexer--re2c-execute (matching_data matching_body)
  "Execute matching body (if any)."
  (if matching_body
      (progn        
        (set-match-data matching_data)
        (funcall matching_body))
    (signal
     'error
     (list "Found no matching lexer rule to execute at %d" (point)))))

;; If multiple rules match, re2c prefers the longest match.
;; If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(defun phps-mode-lexer--re2c (&optional state state_stack heredoc_label)
  "Elisp port of original Zend re2c lexer."

  (unless (boundp state)
    (setq state 'ST_INITIAL))
  (let ((old-start (point))
        (matching_body nil)
        (matching_length nil)
        (matching_data nil))

    (phps-mode-debug-message (message "Running lexer from %s" old-start))
    
    (let ((heredoc_label (car heredoc_label_stack))
          (SHEBANG (equal state 'SHEBANG))
          (ST_IN_SCRIPTING (equal state 'ST_IN_SCRIPTING))
          (ST_INITIAL (equal state 'ST_INITIAL))
          (ST_LOOKING_FOR_PROPERTY (equal state 'ST_LOOKING_FOR_PROPERTY))
          (ST_DOUBLE_QUOTES (equal state 'ST_DOUBLE_QUOTES))
          (ST_BACKQUOTE (equal state 'ST_BACKQUOTE))
          (ST_HEREDOC (equal state 'ST_HEREDOC))
          (ST_NOWDOC (equal state 'ST_NOWDOC))
          (ST_LOOKING_FOR_VARNAME (equal state 'ST_LOOKING_FOR_VARNAME))
          (ST_END_HEREDOC (equal state 'ST_END_HEREDOC))
          (ST_VAR_OFFSET (equal state 'ST_VAR_OFFSET)))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "exit"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_EXIT (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "die"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DIE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "fn"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FN (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "function"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FUNCTION (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "const"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CONST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "return"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_RETURN (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat "yield" phps-mode-lexer--WHITESPACE "from" "[^a-zA-Z0-9_\x80-\xff]")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_YIELD_FROM (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "yield"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_YIELD (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "try"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_TRY (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "catch"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CATCH (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "finally"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FINALLY (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "throw"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_THROW (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "if"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IF (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "elseif"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ELSEIF (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "endif"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDIF (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "else"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ELSE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "while"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_WHILE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "endwhile"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDWHILE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "do"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DO (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "for"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "endfor"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDFOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "foreach"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FOREACH (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "endforeach"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDFOREACH (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "declare"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DECLARE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "enddeclare"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDDECLARE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "instanceof"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INSTANCEOF (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "as"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_AS (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "switch"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SWITCH (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "endswitch"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ENDSWITCH (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "case"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CASE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "default"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DEFAULT (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "break"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_BREAK (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "continue"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CONTINUE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "goto"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_GOTO (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "echo"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ECHO (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "print"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PRINT (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "class"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CLASS (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "interface"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INTERFACE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "trait"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_TRAIT (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "extends"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_EXTENDS (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "implements"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IMPLEMENTS (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "->"))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_LOOKING_FOR_PROPERTY)
         (phps-mode-lexer--RETURN_TOKEN 'T_OBJECT_OPERATOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_IN_SCRIPTING ST_LOOKING_FOR_PROPERTY)
            (looking-at phps-mode-lexer--WHITESPACE))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties start end)))
           (if (phps-mode-wy-macros--CG 'PARSER_MODE)
               (phps-mode-lexer--MOVE_FORWARD end)
             (phps-mode-lexer--RETURN_TOKEN data start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_LOOKING_FOR_PROPERTY (looking-at "->"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_OBJECT_OPERATOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer--LABEL))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))
           (phps-mode-lexer--yy_pop_state)
           (phps-mode-lexer--RETURN_TOKEN 'T_STRING start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (let ((end (match-end 0)))
           (phps-mode-lexer--yy_pop_state)
           ;; TODO goto restart here?
           ;; (message "Restart here")
           (phps-mode-lexer--MOVE_FORWARD end))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "::"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PAAMAYIM_NEKUDOTAYIM (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\\\"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_NS_SEPARATOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\.\\.\\."))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ELLIPSIS (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\?\\?"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_COALESCE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "new"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_NEW (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "clone"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CLONE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "var"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_VAR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "\\(int\\|integer\\)"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INT_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "\\(double\\|float\\)"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DOUBLE_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "\\(real\\)"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (when (phps-mode-wy-macros--CG 'PARSER_MODE)
           (signal
            'error (list
                    (format
                     "PHPs Lexer Error - The (real) cast is deprecated, use (float) instead at %d"
                     (match-beginning 0)
                     )
                    (match-beginning 0)
                    (match-end 0))))
         (phps-mode-lexer--RETURN_TOKEN 'T_DOUBLE_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "\\(string\\|binary\\)"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_STRING_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "array"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ARRAY_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "object"
              phps-mode-lexer--TABS_AND_SPACES
              ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_OBJECT_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and
        ST_IN_SCRIPTING
        (looking-at (concat
                     "("
                     phps-mode-lexer--TABS_AND_SPACES
                     "\\(bool\\|boolean\\)"
                     phps-mode-lexer--TABS_AND_SPACES
                     ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_BOOL_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "("
              phps-mode-lexer--TABS_AND_SPACES
              "unset"
              phps-mode-lexer--TABS_AND_SPACES ")")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_UNSET_CAST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "eval"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_EVAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "include"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INCLUDE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "include_once"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INCLUDE_ONCE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "require"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_REQUIRE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "require_once"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_REQUIRE_ONCE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "namespace"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_NAMESPACE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "use"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_USE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "insteadof"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INSTEADOF (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "global"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_GLOBAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "isset"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ISSET (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "empty"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_EMPTY (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__halt_compiler"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_HALT_COMPILER (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "static"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_STATIC (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "abstract"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ABSTRACT (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "final"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FINAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "private"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PRIVATE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "protected"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PROTECTED (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "public"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PUBLIC (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "unset"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_UNSET (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "=>"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DOUBLE_ARROW (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "list"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_LIST (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "array"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_ARRAY (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "callable"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CALLABLE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\+\\+"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_INC (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "--"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DEC (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "==="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_IDENTICAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "!=="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_NOT_IDENTICAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "=="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\(!=\\|<>\\)"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_NOT_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "<=>"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SPACESHIP (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "<="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_SMALLER_OR_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at ">="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_IS_GREATER_OR_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\+="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_PLUS_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "-="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_MINUS_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\*="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_MUL_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_POW_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_POW (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "/="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DIV_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\.="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CONCAT_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "%="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_MOD_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "<<="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SL_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at ">>="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SR_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "&="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_AND_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "|="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_OR_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\^="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_XOR_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\?\\?="))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_COALESCE_EQUAL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "||"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_BOOLEAN_OR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "&&"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_BOOLEAN_AND (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "OR"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_LOGICAL_OR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "AND"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_LOGICAL_AND (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "XOR"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_LOGICAL_XOR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "<<"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SL (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at ">>"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_SR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--TOKENS))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties start end))
                (use-brace nil))
           ;; (message "Found token '%s'" data)
           (if use-brace
               (phps-mode-lexer--RETURN_TOKEN "{" start end)
             (phps-mode-lexer--RETURN_TOKEN data start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "{"))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_IN_SCRIPTING)
         (phps-mode-lexer--RETURN_TOKEN "{" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at "\\${"))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_LOOKING_FOR_VARNAME)
         (phps-mode-lexer--RETURN_TOKEN 'T_DOLLAR_OPEN_CURLY_BRACES (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "}"))
       (lambda()
         (when state_stack
           (phps-mode-lexer--yy_pop_state))
         (phps-mode-lexer--RETURN_TOKEN "}" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_LOOKING_FOR_VARNAME (looking-at (concat phps-mode-lexer--LABEL "[\\[}]")))
       (lambda()
         (let ((start (match-beginning 0))
               (end (- (match-end 0) 1)))
           ;; (message "Stopped here")
           (phps-mode-lexer--yy_pop_state)
           (phps-mode-lexer--yy_push_state 'ST_IN_SCRIPTING)
           (phps-mode-lexer--RETURN_TOKEN 'T_STRING_VARNAME start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_LOOKING_FOR_VARNAME (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (phps-mode-lexer--yy_pop_state)
         (phps-mode-lexer--yy_push_state 'ST_IN_SCRIPTING)))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--BNUM))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties (+ start 2) end))
                (long-number (string-to-number data 2)))
           ;; (message "Binary number %s from %s" long-number data)
           (if (> long-number phps-mode-lexer--long-limit)
               (phps-mode-lexer--RETURN_TOKEN 'T_DNUMBER start end)
             (phps-mode-lexer--RETURN_TOKEN 'T_LNUMBER start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--LNUM))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (string-to-number (buffer-substring-no-properties start end))))
           ;; (message "Long number: %d" data)
           (if (> data phps-mode-lexer--long-limit)
               (phps-mode-lexer--RETURN_TOKEN 'T_DNUMBER start end)
             (phps-mode-lexer--RETURN_TOKEN 'T_LNUMBER start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at phps-mode-lexer--HNUM))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties (+ start 2) end))
                (long-number (string-to-number data 16)))
           ;; (message "Hexadecimal number %s from %s" long-number data)
           (if (> long-number phps-mode-lexer--long-limit)
               (phps-mode-lexer--RETURN_TOKEN 'T_DNUMBER start end)
             (phps-mode-lexer--RETURN_TOKEN 'T_LNUMBER start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_VAR_OFFSET (looking-at "\\([0]\\|[1-9][0-9]*\\)"))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))
           (phps-mode-lexer--RETURN_TOKEN 'T_NUM_STRING start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_VAR_OFFSET (looking-at (concat "\\("
                                              phps-mode-lexer--LNUM "\\|"
                                              phps-mode-lexer--HNUM "\\|"
                                              phps-mode-lexer--BNUM "\\)")))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_NUM_STRING (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (or (looking-at phps-mode-lexer--EXPONENT_DNUM)
                                (looking-at phps-mode-lexer--DNUM)))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (_data (buffer-substring-no-properties start end)))
           ;; (message "Exponent/double at: %s" _data)
           (phps-mode-lexer--RETURN_TOKEN 'T_DNUMBER start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__CLASS__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_CLASS_C (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__TRAIT__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_TRAIT_C (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__FUNCTION__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FUNC_C (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__METHOD__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_METHOD_C (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__LINE__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_LINE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__FILE__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_FILE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__DIR__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_DIR (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "__NAMESPACE__"))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_NS_C (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and SHEBANG (looking-at (concat "#!.*" phps-mode-lexer--NEWLINE)))
       (lambda()
         (phps-mode-lexer--BEGIN 'ST_INITIAL)))

      (phps-mode-lexer--re2c-rule
       (and SHEBANG (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (phps-mode-lexer--BEGIN 'ST_INITIAL)))

      (phps-mode-lexer--re2c-rule
       (and ST_INITIAL (looking-at "<\\?="))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))
           (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
           (when (phps-mode-wy-macros--CG 'PARSER_MODE)
             (phps-mode-lexer--RETURN_TOKEN 'T_ECHO start end))
           (phps-mode-lexer--RETURN_TOKEN 'T_OPEN_TAG_WITH_ECHO start end))))

      (phps-mode-lexer--re2c-rule
       (and
        ST_INITIAL
        (looking-at (concat "<\\?php\\([ \t]\\|" phps-mode-lexer--NEWLINE "\\)")))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))
           (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
           ;; (message "Starting scripting after <?php")
           (when phps-mode-lexer--EXPECTED
             (phps-mode-lexer--SKIP_TOKEN 'T_OPEN_TAG start end))
           (phps-mode-lexer--RETURN_TOKEN 'T_OPEN_TAG start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_INITIAL (looking-at "<\\?php"))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))

           ;; Allow <?php followed by end of file.
           (cond

            ((equal end (point-max))
             (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
             (phps-mode-lexer--RETURN_OR_SKIP_TOKEN
              'T_OPEN_TAG
              start
              end))

            ((phps-mode-wy-macros--CG 'SHORT_TAGS)
             (phps-mode-lexer--yyless 3)
             (setq end (- end 3))
             (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
             (phps-mode-lexer--RETURN_OR_SKIP_TOKEN
              'T_OPEN_TAG
              start
              end))

            (t
             (phps-mode-lexer--inline-char-handler))))))

      (phps-mode-lexer--re2c-rule
       (and ST_INITIAL (looking-at "<\\?"))
       (lambda()
         (when (phps-mode-wy-macros--CG 'SHORT_TAGS)
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
             (when phps-mode-lexer--EXPECTED
               (phps-mode-lexer--SKIP_TOKEN 'T_OPEN_TAG start end))
             ;; (message "Starting scripting after <?")
             (phps-mode-lexer--RETURN_TOKEN 'T_OPEN_TAG start end)))))

      (phps-mode-lexer--re2c-rule
       (and ST_INITIAL (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (phps-mode-lexer--inline-char-handler)))

      (phps-mode-lexer--re2c-rule
       (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--LABEL
              "->"
              "[a-zA-Z_\x80-\xff]")))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_LOOKING_FOR_PROPERTY)
         (forward-char -3)
         (phps-mode-lexer--RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (- (match-end 0) 3))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--LABEL
              "\\[")))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_VAR_OFFSET)
         (phps-mode-lexer--RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_IN_SCRIPTING ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE ST_VAR_OFFSET)
            (looking-at
             (concat
              "\\$"
              phps-mode-lexer--LABEL)))
       (lambda()
         (phps-mode-lexer--RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_VAR_OFFSET (looking-at "\\]"))
       (lambda()
         (phps-mode-lexer--yy_pop_state)
         (phps-mode-lexer--RETURN_TOKEN "]" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_VAR_OFFSET (looking-at (concat "\\(" phps-mode-lexer--TOKENS
                                              "\\|[{}\"`]\\)")))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties start end)))
           (phps-mode-lexer--RETURN_TOKEN data start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_VAR_OFFSET (looking-at (concat "[ \n\r\t'#]")))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (- (match-end 0) 1)))
           (phps-mode-lexer--yy_pop_state)
           (phps-mode-lexer--RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE start end))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_IN_SCRIPTING ST_VAR_OFFSET) (looking-at phps-mode-lexer--LABEL))
       (lambda()
         ;; (message "Adding T_STRING from %s to %s" (match-beginning 0) (match-end 0))
         (phps-mode-lexer--RETURN_TOKEN 'T_STRING (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\\(#\\|//\\)"))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (_data (buffer-substring-no-properties start end))
                (line (buffer-substring-no-properties end (line-end-position))))
           (if (string-match "\\?>" line)
               (progn
                 (phps-mode-lexer--RETURN_TOKEN 'T_COMMENT start (+ end (match-beginning 0))))
             (progn
               ;; TODO Handle expecting values here
               ;; (message "Found comment 2 from %s to %s" start (line-end-position))
               (phps-mode-lexer--RETURN_TOKEN 'T_COMMENT start (line-end-position)))))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "\\(/\\*\\|/\\*\\*"
              phps-mode-lexer--WHITESPACE
              "\\)")))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (_data (buffer-substring-no-properties start end))
                (doc-com (looking-at-p (concat "/\\*\\*" phps-mode-lexer--WHITESPACE))))
           (let ((string-start (search-forward "*/" nil t)))
             (if string-start
                 (if doc-com
                     (phps-mode-lexer--RETURN_TOKEN 'T_DOC_COMMENT start (match-end 0))
                   (phps-mode-lexer--RETURN_TOKEN 'T_COMMENT start (match-end 0)))
               (progn
                 (signal
                  'error
                  (list (format
                         "PHPs Lexer Error - Unterminated comment starting at %d"
                         (point))
                        (point)
                        ))))))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at (concat "\\?>" phps-mode-lexer--NEWLINE "?")))
       (lambda()
         (let ((start (match-beginning 0))
               (end (match-end 0)))
           (when (= (- end start) 3)
             (setq end (1- end)))
           (phps-mode-lexer--BEGIN 'ST_INITIAL)
           (when (phps-mode-wy-macros--CG 'PARSER_MODE)
             (phps-mode-lexer--RETURN_TOKEN ";" start end))
           (phps-mode-lexer--RETURN_TOKEN 'T_CLOSE_TAG start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "'"))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (_data (buffer-substring-no-properties start end))
                (un-escaped-end (phps-mode-lexer--get-next-unescaped "'")))
           (if un-escaped-end
               (progn
                 (phps-mode-lexer--RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start un-escaped-end))
             (progn
               ;; Unclosed single quotes
               (phps-mode-lexer--RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE start (point-max))
               (phps-mode-lexer--MOVE_FORWARD (point-max)))))))

      ;; Double quoted string
      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "\""))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (_data (buffer-substring-no-properties start end))
                (open-quote t))

           ;; Move forward from the double-quote one character
           (forward-char)

           (while open-quote
             (let ((string-start
                    (search-forward-regexp
                     (concat
                      "\\(\""
                      "\\|\\$" phps-mode-lexer--LABEL
                      "\\|\\${" phps-mode-lexer--LABEL
                      "\\|{\\$" phps-mode-lexer--LABEL "\\)")
                     nil t)))

               ;; Do we find a ending double quote or starting variable?
               (if string-start
                   (let ((string-start (match-beginning 0))
                         (is-escaped nil)
                         (is-escaped-1 nil)
                         (is-escaped-2 nil))

                     ;; Check whether one character back is escape character
                     (goto-char (1- string-start))
                     (setq is-escaped-1 (looking-at-p "\\\\"))

                     ;; Check whether two characters back is escape character
                     (goto-char (- string-start 2))
                     (setq is-escaped-2 (looking-at-p "\\\\"))

                     (setq is-escaped
                           (and
                            is-escaped-1
                            (not is-escaped-2)))

                     ;; Do we find variable inside quote?
                     (goto-char string-start)

                     ;; Process character if it's not escaped
                     (if is-escaped
                         (forward-char 1)
                       (setq open-quote nil)
                       (if (looking-at "\"")
                           (let ((_double-quoted-string
                                  (buffer-substring-no-properties start (+ string-start 1))))
                             ;; (message "Double quoted string: %s" _double-quoted-string)
                             (phps-mode-lexer--RETURN_TOKEN
                              'T_CONSTANT_ENCAPSED_STRING
                              start
                              (+ string-start 1)))
                         ;; (message "Found variable after '%s'" (buffer-substring-no-properties start string-start))
                         (phps-mode-lexer--BEGIN 'ST_DOUBLE_QUOTES)
                         (phps-mode-lexer--RETURN_TOKEN "\"" start (1+ start))
                         (phps-mode-lexer--RETURN_TOKEN
                          'T_ENCAPSED_AND_WHITESPACE
                          (1+ start)
                          string-start))))
                 (progn
                   (setq open-quote nil)
                   (signal
                    'error
                    (list
                     (format "Found no ending of quote at %s" start)
                     start)))))))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING
            (looking-at
             (concat
              "<<<"
              phps-mode-lexer--TABS_AND_SPACES
              "\\("
              phps-mode-lexer--LABEL
              "\\|'"
              phps-mode-lexer--LABEL
              "'\\|\""
              phps-mode-lexer--LABEL
              "\"\\)"
              phps-mode-lexer--NEWLINE)))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (match-end 0))
                (data (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                (heredoc_label))

           ;; Determine if it's HEREDOC or NOWDOC and extract label here
           (if (string= (substring data 0 1) "'")
               (progn
                 (setq heredoc_label (substring data 1 (- (length data) 1)))
                 (phps-mode-lexer--BEGIN 'ST_NOWDOC))
             (progn
               (if (string= (substring data 0 1) "\"")
                   (setq heredoc_label (substring data 1 (- (length data) 1)))
                 (setq heredoc_label data))
               (phps-mode-lexer--BEGIN 'ST_HEREDOC)))

           ;; Check for ending label on the next line
           (when (string= (buffer-substring-no-properties end (+ end (length heredoc_label))) heredoc_label)
             (phps-mode-lexer--BEGIN 'ST_END_HEREDOC))

           (push heredoc_label heredoc_label_stack)
           ;; (message "Found heredoc or nowdoc at %s with label %s" data heredoc_label)

           (phps-mode-lexer--RETURN_TOKEN 'T_START_HEREDOC start end))))

      (phps-mode-lexer--re2c-rule
       (and ST_IN_SCRIPTING (looking-at "[`]"))
       (lambda()
         ;; (message "Begun backquote at %s-%s" (match-beginning 0) (match-end 0))
         (phps-mode-lexer--BEGIN 'ST_BACKQUOTE)
         (phps-mode-lexer--RETURN_TOKEN "`" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_END_HEREDOC (looking-at (concat phps-mode-lexer--ANY_CHAR)))
       (lambda()
         (let* ((start (match-beginning 0))
                (end (+ start (length heredoc_label) 1))
                (_data (buffer-substring-no-properties start end)))
           ;; (message "Found ending heredoc at %s, %s of %s" _data (thing-at-point 'line) heredoc_label)
           (pop heredoc_label_stack)
           (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
           (phps-mode-lexer--RETURN_TOKEN 'T_END_HEREDOC start end))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at (concat "{\\$")))
       (lambda()
         (phps-mode-lexer--yy_push_state 'ST_IN_SCRIPTING)
         (phps-mode-lexer--RETURN_TOKEN 'T_CURLY_OPEN (match-beginning 0) (- (match-end 0) 1))))

      (phps-mode-lexer--re2c-rule
       (and ST_DOUBLE_QUOTES (looking-at "[\"]"))
       (lambda()
         (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
         ;; (message "Ended double-quote at %s" (match-beginning 0))
         (phps-mode-lexer--RETURN_TOKEN "\"" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_BACKQUOTE (looking-at "[`]"))
       (lambda()
         (phps-mode-lexer--BEGIN 'ST_IN_SCRIPTING)
         (phps-mode-lexer--RETURN_TOKEN "`" (match-beginning 0) (match-end 0))))

      (phps-mode-lexer--re2c-rule
       (and ST_DOUBLE_QUOTES (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (let ((start (point)))
           (let ((string-start (search-forward-regexp "[^\\\\]\"" nil t)))
             (if string-start
                 (let* ((end (- (match-end 0) 1))
                        (double-quoted-string (buffer-substring-no-properties start end)))
                   ;; Do we find variable inside quote?
                   (if (or (string-match (concat "\\${" phps-mode-lexer--LABEL) double-quoted-string)
                           (string-match (concat "{\\$" phps-mode-lexer--LABEL) double-quoted-string)
                           (string-match (concat "\\$" phps-mode-lexer--LABEL) double-quoted-string))
                       (progn
                         (let ((variable-start (+ start (match-beginning 0))))

                           ;; (message "Found starting expression inside double-quoted string at: %s %s" start variable-start)
                           (phps-mode-lexer--RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start variable-start)
                           ))
                     (progn
                       (phps-mode-lexer--RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start end)
                       ;; (message "Found end of quote at %s-%s, moving ahead after '%s'" start end (buffer-substring-no-properties start end))
                       )))
               (progn
                 (signal
                  'error
                  (list
                   (format "Found no ending of double quoted region starting at %d" start)
                   start))))))))

      (phps-mode-lexer--re2c-rule
       (and ST_BACKQUOTE (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (let ((string-start (search-forward-regexp "\\([^\\\\]`\\|\\$\\|{\\)" nil t)))
           (if string-start
               (let ((start (- (match-end 0) 1)))
                 ;; (message "Skipping backquote forward over %s" (buffer-substring-no-properties old-start start))
                 (phps-mode-lexer--RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING old-start start)
                 )
             (progn
               (signal
                'error
                (list
                 (format "Found no ending of back-quoted string starting at %d" (point))
                 (point))))))))

      (phps-mode-lexer--re2c-rule
       (and ST_HEREDOC (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         ;; Check for $, ${ and {$ forward
         (let ((string-start
                (search-forward-regexp
                 (concat
                  "\\(\n"
                  heredoc_label
                  ";?\n\\|\\$"
                  phps-mode-lexer--LABEL
                  "\\|{\\$"
                  phps-mode-lexer--LABEL
                  "\\|\\${"
                  phps-mode-lexer--LABEL
                  "\\)"
                  ) nil t)))
           (if string-start
               (let* ((start (match-beginning 0))
                      (end (match-end 0))
                      (data (buffer-substring-no-properties start end)))
                 ;; (message "Found something ending at %s" data)

                 (cond

                  ((string-match (concat "\n" heredoc_label ";?\n") data)
                   ;; (message "Found heredoc end at %s-%s" start end)
                   (phps-mode-lexer--BEGIN 'ST_END_HEREDOC)
                   (phps-mode-lexer--RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start))

                  (t
                   ;; (message "Found variable at '%s'.. Skipping forward to %s" data start)
                   (phps-mode-lexer--RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start)
                   )

                  ))
             (progn
               (signal
                'error
                (list
                 (format "Found no ending of heredoc at %d" (point))
                 (point))))))))

      (phps-mode-lexer--re2c-rule
       (and ST_NOWDOC (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (let ((string-start (search-forward-regexp (concat "\n" heredoc_label ";?\\\n") nil t)))
           (if string-start
               (let* ((start (match-beginning 0))
                      (end (match-end 0))
                      (_data (buffer-substring-no-properties start end)))
                 ;; (message "Found something ending at %s" _data)
                 ;; (message "Found nowdoc end at %s-%s" start end)
                 (phps-mode-lexer--BEGIN 'ST_END_HEREDOC)
                 (phps-mode-lexer--RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start)
                 )
             (progn
               (signal
                'error
                (list
                 (format "Found no ending of newdoc starting at %d" (point))
                 (point))))))))

      (phps-mode-lexer--re2c-rule
       (and (or ST_IN_SCRIPTING ST_VAR_OFFSET) (looking-at phps-mode-lexer--ANY_CHAR))
       (lambda()
         (signal
          'error (list
                  (format "Unexpected character at %d" (point))
                  (point))))))))

(defun phps-mode-lexer--get-next-unescaped (character)
  "Find where next un-escaped CHARACTER comes, if none is found return nil."
  ;; (message "phps-mode-lexer--get-next-unescaped(%s)" character)
  (let ((escaped nil)
        (pos nil))
    (while (and (not pos)
                (< (point) (point-max)))
      (progn
        ;; (message "Setting forward one %s vs %s" (point) (point-max))
        (forward-char)
        (if (and (not escaped)
                 (looking-at-p character))
            (setq pos (1+ (point)))
          (if (looking-at-p "\\\\")
              (setq escaped (not escaped))
            (setq escaped nil)))))
    pos))


(provide 'phps-mode-lexer)

;;; phps-mode-lexer.el ends here
