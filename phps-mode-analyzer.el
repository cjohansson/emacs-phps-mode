;;; phps-mode-analyzer.el -- Lexer and helper functions for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

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

;; *Define the lexer for this grammar
;; based on the Zend PHP Lexer at
;; `https://github.com/php/php-src/blob/master/Zend/zend_language_scanner.l'
;; which is using re2c.
;;
;; * Also supply logic for indentation and imenu-handling
;; Change detection and incremental lexer
;;
;; * Syntax coloring based on lexer tokens

;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-wy-macros)

(require 'semantic)
(require 'semantic/lex)
(require 'semantic/wisent)

(require 'subr-x)

(defvar phps-mode-inline-mmm-submode nil
  "Symbol declaring what mmm-mode to use as submode in inline areas.")

(defvar phps-mode-analyzer-change-min nil
  "The minium point of change.");

(defvar phps-mode-idle-interval 1
  "Idle seconds before running the incremental lexer.")

(defvar phps-mode-functions-allow-after-change t
  "Flag to tell us whether after change detection is enabled or not.")

(defvar phps-mode-functions-idle-timer nil
  "Timer object of idle timer.")

(defvar phps-mode-functions-imenu nil
  "The Imenu alist for current buffer, nil if none.")

(defvar phps-mode-functions-lines-indent nil
  "The indentation of each line in buffer, nil if none.")

(defvar phps-mode-functions-processed-buffer nil
  "Flag whether current buffer is processed or not.")

(defvar phps-mode-analyzer-process-on-indent-and-imenu nil
  "Whether to automatically process buffer when using indent or imenu.")

(defvar phps-mode-lexer-tokens nil
  "Last lexer tokens.")

(defvar phps-mode-lexer-states nil
  "A list of lists containing start, state and state stack.")

(defvar phps-mode-lexer-buffer-length nil
  "Length of lexed buffer.")

(defvar phps-mode-lexer-buffer-contents nil
  "Contents of lexed buffer.")


;; SETTINGS


;; @see https://secure.php.net/manual/en/language.types.integer.php
(defvar phps-mode-lexer-long-limit 2147483648
  "Limit for 32-bit integer.")


(phps-mode-wy-macros-CG 'PARSER_MODE t)
(phps-mode-wy-macros-CG 'SHORT_TAGS t)


;; FLAGS/SIGNALS


(defvar phps-mode-lexer-declaring_namespace nil
  "Flag whether we are declaring namespace.")

(defvar phps-mode-lexer-prepend_trailing_brace nil
  "Flag whether we should prepend trailing brace.")

(defvar phps-mode-lexer-STATE nil
  "Current state.")

(defvar phps-mode-lexer-state_stack nil
  "Stack of states.")

(defvar phps-mode-lexer-EXPECTED nil
  "Flag whether something is expected or not.")

(defvar phps-mode-lexer-heredoc_label_stack (list)
  "The current heredoc_label.")


;; REGULAR EXPRESSIONS


(defvar phps-mode-lexer-BNUM "0b[01]+"
  "Boolean number.")

(defvar phps-mode-lexer-HNUM "0x[0-9a-fA-F]+"
  "Hexadecimal number.")

(defvar phps-mode-lexer-LNUM "[0-9]+"
  "Long number.")

(defvar phps-mode-lexer-DNUM "\\([0-9]*\\.[0-9]+\\)\\|\\([0-9]+\\.[0-9]*\\)"
  "Double number.")

(defvar phps-mode-lexer-EXPONENT_DNUM
  (format "\\(\\(%s\\|%s\\)[eE][\\+-]?%s\\)"
          phps-mode-lexer-LNUM
          phps-mode-lexer-DNUM
          phps-mode-lexer-LNUM)
  "Exponent double number.")

(defvar phps-mode-lexer-LABEL
  "[A-Za-z_[:nonascii:]][0-9A-Za-z_[:nonascii:]]*"
  "Labels are used for names.")
;; NOTE original is [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*
;; NOTE Rebuilt for comparability with emacs-lisp

(defvar phps-mode-lexer-WHITESPACE "[ \n\r\t]+"
  "White-space.")

(defvar phps-mode-lexer-TABS_AND_SPACES "[ \t]*"
  "Tabs and white-spaces.")

(defvar phps-mode-lexer-TOKENS "[][;:,.()|^&+/*=%!~$<>?@-]"
  "Tokens.")
;; NOTE Original is [;:,.\[\]()|^&+-/*=%!~$<>?@]
;; NOTE The hyphen moved last since it has special meaning and to avoid it being interpreted as a range.

(defvar phps-mode-lexer-ANY_CHAR ".\\|\n"
  "Any character.  The Zend equivalent is [^] but is not possible in Emacs Lisp.")

(defvar phps-mode-lexer-NEWLINE "\\(\r\n\\|\r\\|\n\\)"
  "Newline characters.  The Zend equivalent is (\"\r\"|\"\n\"|\"\r\n\").")


;; FUNCTIONS


(defun phps-mode-lexer-BEGIN (state)
  "Begin STATE."
  ;; (message "Begun state %s" state)
  (setq-local phps-mode-lexer-STATE state))

;; _yy_push_state
(defun phps-mode-lexer-yy_push_state (new-state)
  "Add NEW-STATE to stack and then begin state."
  (push phps-mode-lexer-STATE phps-mode-lexer-state_stack)
  ;; (message "Added state %s to stack begun state %s" phps-mode-lexer-STATE new-state)
  (phps-mode-lexer-BEGIN new-state))

(defun phps-mode-lexer-yy_pop_state ()
  "Pop current state from stack."
  (let ((old-state (pop phps-mode-lexer-state_stack)))
    ;; (message "Going back to poppped state %s" old-state)
    (if old-state
        (phps-mode-lexer-BEGIN old-state)
      (display-warning 'phps-mode "PHPs Lexer Error - Going back to nil?"))))

(defun phps-mode-lexer-MOVE_FORWARD (position)
  "Move forward to POSITION."
  (when (boundp 'semantic-lex-end-point)
    (setq semantic-lex-end-point position)))

(defun phps-mode-lexer-yyless (points)
  "Move lexer back POINTS."
  (when (boundp 'semantic-lex-end-point)
    (setq semantic-lex-end-point (- semantic-lex-end-point points))))

(defun phps-mode-lexer-set-region-syntax-color (start end properties)
  "Do syntax coloring for region START to END with PROPERTIES."
  (with-silent-modifications (set-text-properties start end properties)))

(defun phps-mode-lexer-clear-region-syntax-color (start end)
  "Clear region of syntax coloring from START to END."
  (with-silent-modifications (set-text-properties start end nil)))

(defun phps-mode-anaylzer-inline-char-handler ()
  "Mimic inline_char_handler."
  (let ((start (match-beginning 0)))
    (let ((string-start (search-forward "<?" nil t)))
      (if string-start
          (phps-mode-lexer-RETURN_TOKEN 'T_INLINE_HTML start (- string-start 2))
        (phps-mode-lexer-RETURN_TOKEN 'T_INLINE_HTML start (point-max))))))

(defun phps-mode-lexer-get-token-syntax-color (token)
  "Return syntax color for TOKEN."
  ;; Syntax coloring
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Faces-for-Font-Lock.html#Faces-for-Font-Lock
  ;; (message "Color token %s %s %s" token start end)
  (cond

   ((or
     (string= token 'T_VARIABLE)
     (string= token 'T_STRING_VARNAME))
    (list 'font-lock-face 'font-lock-variable-name-face))

   ((string= token 'T_COMMENT)
    (list 'font-lock-face 'font-lock-comment-face))

   ((string= token 'T_DOC_COMMENT)
    (list 'font-lock-face 'font-lock-doc-face))

   ((string= token 'T_INLINE_HTML)
    ;; NOTE T_INLINE_HTML is missing by purpose here to distinguish those areas from other entities
    nil)

   ((or
     (string= token 'T_STRING)
     (string= token 'T_CONSTANT_ENCAPSED_STRING)
     (string= token 'T_ENCAPSED_AND_WHITESPACE)
     (string= token 'T_NUM_STRING)
     (string= token 'T_DNUMBER)
     (string= token 'T_LNUMBER))
    (list 'font-lock-face 'font-lock-string-face))

   ((or
     (string= token 'T_DOLLAR_OPEN_CURLY_BRACES)
     (string= token 'T_CURLY_OPEN)
     (string= token 'T_OBJECT_OPERATOR)
     (string= token 'T_PAAMAYIM_NEKUDOTAYIM)
     (string= token 'T_NS_SEPARATOR)
     (string= token 'T_EXIT)
     (string= token 'T_DIE)
     (string= token 'T_RETURN)
     (string= token 'T_YIELD_FROM)
     (string= token 'T_YIELD)
     (string= token 'T_TRY)
     (string= token 'T_CATCH)
     (string= token 'T_FINALLY)
     (string= token 'T_THROW)
     (string= token 'T_IF)
     (string= token 'T_ELSEIF)
     (string= token 'T_ENDIF)
     (string= token 'T_ELSE)
     (string= token 'T_WHILE)
     (string= token 'T_ENDWHILE)
     (string= token 'T_DO)
     (string= token 'T_FUNCTION)
     (string= token 'T_FN)
     (string= token 'T_CONST)
     (string= token 'T_FOREACH)
     (string= token 'T_ENDFOREACH)
     (string= token 'T_FOR)
     (string= token 'T_ENDFOR)
     (string= token 'T_DECLARE)
     (string= token 'T_ENDDECLARE)
     (string= token 'T_INSTANCEOF)
     (string= token 'T_AS)
     (string= token 'T_SWITCH)
     (string= token 'T_ENDSWITCH)
     (string= token 'T_CASE)
     (string= token 'T_DEFAULT)
     (string= token 'T_BREAK)
     (string= token 'T_CONTINUE)
     (string= token 'T_GOTO)
     (string= token 'T_ECHO)
     (string= token 'T_PRINT)
     (string= token 'T_CLASS)
     (string= token 'T_INTERFACE)
     (string= token 'T_TRAIT)
     (string= token 'T_EXTENDS)
     (string= token 'T_IMPLEMENTS)
     (string= token 'T_NEW)
     (string= token 'T_CLONE)
     (string= token 'T_VAR)
     (string= token 'T_EVAL)
     (string= token 'T_INCLUDE_ONCE)
     (string= token 'T_INCLUDE)
     (string= token 'T_REQUIRE_ONCE)
     (string= token 'T_REQUIRE)
     (string= token 'T_NAMESPACE)
     (string= token 'T_USE)
     (string= token 'T_INSTEADOF)
     (string= token 'T_GLOBAL)
     (string= token 'T_ISSET)
     (string= token 'T_EMPTY)
     (string= token 'T_HALT_COMPILER)
     (string= token 'T_STATIC)
     (string= token 'T_ABSTRACT)
     (string= token 'T_FINAL)
     (string= token 'T_PRIVATE)
     (string= token 'T_PROTECTED)
     (string= token 'T_PUBLIC)
     (string= token 'T_UNSET)
     (string= token 'T_LIST)
     (string= token 'T_ARRAY)
     (string= token 'T_CALLABLE)
     )
    (list 'font-lock-face 'font-lock-keyword-face))

   ((or
     (string= token 'T_OPEN_TAG)
     (string= token 'T_OPEN_TAG_WITH_ECHO)
     (string= token 'T_CLOSE_TAG)
     (string= token 'T_START_HEREDOC)
     (string= token 'T_END_HEREDOC)
     (string= token 'T_ELLIPSIS)
     (string= token 'T_COALESCE)
     (string= token 'T_DOUBLE_ARROW)
     (string= token 'T_INC)
     (string= token 'T_DEC)
     (string= token 'T_IS_IDENTICAL)
     (string= token 'T_IS_NOT_IDENTICAL)
     (string= token 'T_IS_EQUAL)
     (string= token 'T_IS_NOT_EQUAL)
     (string= token 'T_SPACESHIP)
     (string= token 'T_IS_SMALLER_OR_EQUAL)
     (string= token 'T_IS_GREATER_OR_EQUAL)
     (string= token 'T_PLUS_EQUAL)
     (string= token 'T_MINUS_EQUAL)
     (string= token 'T_MUL_EQUAL)
     (string= token 'T_POW_EQUAL)
     (string= token 'T_POW)
     (string= token 'T_DIV_EQUAL)
     (string= token 'T_CONCAT_EQUAL)
     (string= token 'T_MOD_EQUAL)
     (string= token 'T_SL_EQUAL)
     (string= token 'T_SR_EQUAL)
     (string= token 'T_AND_EQUAL)
     (string= token 'T_OR_EQUAL)
     (string= token 'T_XOR_EQUAL)
     (string= token 'T_COALESCE_EQUAL)
     (string= token 'T_BOOLEAN_OR)
     (string= token 'T_BOOLEAN_AND)
     (string= token 'T_BOOLEAN_XOR)
     (string= token 'T_LOGICAL_XOR)
     (string= token 'T_LOGICAL_OR)
     (string= token 'T_LOGICAL_AND)
     (string= token 'T_SL)
     (string= token 'T_SR)
     (string= token 'T_CLASS_C)
     (string= token 'T_TRAIT_C)
     (string= token 'T_FUNC_C)
     (string= token 'T_METHOD_C)
     (string= token 'T_LINE)
     (string= token 'T_FILE)
     (string= token 'T_DIR)
     (string= token 'T_NS_C)
     (string= token 'T_INT_CAST)
     (string= token 'T_DOUBLE_CAST)
     (string= token 'T_STRING_CAST)
     (string= token 'T_ARRAY_CAST)
     (string= token 'T_OBJECT_CAST)
     (string= token 'T_BOOL_CAST)
     (string= token 'T_UNSET_CAST)
     )
    (list 'font-lock-face 'font-lock-constant-face))

   (t (list 'font-lock-face 'font-lock-constant-face))))

(defun phps-mode-lexer-RETURN_OR_SKIP_TOKEN (token start end)
  "Return TOKEN with START and END but only in parse-mode."
  (when (phps-mode-wy-macros-CG 'PARSER_MODE)
    (phps-mode-analyzer-emit-token token start end)))

(defun phps-mode-lexer-RETURN_TOKEN (token start end)
  "Push TOKEN to list with START and END."
(phps-mode-analyzer-emit-token token start end))

(defun phps-mode-analyzer-emit-token (token start end)
  "Emit TOKEN with START and END."

  ;; Colourize token
  (let ((token-syntax-color (phps-mode-lexer-get-token-syntax-color token)))
    (if token-syntax-color
        (phps-mode-lexer-set-region-syntax-color start end token-syntax-color)
      (phps-mode-lexer-clear-region-syntax-color start end)))

  ;; (when (and
  ;;        (equal token 'T_INLINE_HTML)
  ;;        phps-mode-inline-mmm-submode
  ;;        (fboundp 'mmm-make-region))
  ;;   (mmm-make-region phps-mode-inline-mmm-submode start end))

  ;; Push token start, end, lexer state and state stack to variable
  (push
   (list start end phps-mode-lexer-STATE phps-mode-lexer-state_stack) phps-mode-lexer-states)

  (semantic-lex-push-token (semantic-lex-token token start end)))

;; TODO Figure out what this does
(defun phps-mode-lexer-SKIP_TOKEN (_token _start _end)
  "Skip TOKEN to list with START and END.")

(defvar phps-mode-lexer-re2c-matching-body nil
  "Lambda-expression for longest matching condition.")

(defvar phps-mode-lexer-re2c-matching-length nil
  "Length of longest matching condition.")

(defvar phps-mode-lexer-re2c-matching-data nil
  "Match data for longest matching condition.")

(defun phps-mode-lexer-re2c-rule (condition body)
  "Process rule with CONDITION and BODY."
  (when condition
    (let ((match-end (match-end 0))
          (match-beginning (match-beginning 0)))
      (let ((matching-length (- match-end match-beginning)))
        (when (> matching-length 0)
          (when (or (not phps-mode-lexer-re2c-matching-length)
                    (> matching-length phps-mode-lexer-re2c-matching-length))
            (setq phps-mode-lexer-re2c-matching-length matching-length)
            (setq phps-mode-lexer-re2c-matching-data (match-data))
            (setq phps-mode-lexer-re2c-matching-body body)))))))

(defun phps-mode-lexer-re2c-execute ()
  "Execute matching body (if any)."
  (if phps-mode-lexer-re2c-matching-body
      (progn
        
        ;; (message "Executing body: %s" phps-mode-lexer-re2c-matching-body)
        ;; (message "Found match %s" phps-mode-lexer-re2c-matching-data)
        (set-match-data phps-mode-lexer-re2c-matching-data)
        (funcall phps-mode-lexer-re2c-matching-body))
    (error "Failed to lex input")))


;; LEXERS

;; If multiple rules match, re2c prefers the longest match. If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(define-lex-analyzer phps-mode-lexer-lex-analyzer
  "Elisp port of original Zend re2c lexer."
  t

  (let ((old-start (point)))
    (if phps-mode-lexer-tokens
        (progn
          ;; Add all updated tokens to semantic
          (phps-mode-debug-message
           (message
            "Updating semantic lexer tokens from point %s, tokens: %s, point-max: %s"
            old-start
            phps-mode-lexer-tokens
            (point-max)))
          (dolist (token phps-mode-lexer-tokens)
            (let ((start (car (cdr token)))
                  (end (cdr (cdr token)))
                  (token-name (car token)))
              (semantic-lex-push-token
               (semantic-lex-token token-name start end))))

          (phps-mode-lexer-MOVE_FORWARD (point-max)))

      (phps-mode-debug-message (message "Running lexer from %s" old-start))
      
      (let ((heredoc_label (car phps-mode-lexer-heredoc_label_stack))
            (SHEBANG (equal phps-mode-lexer-STATE 'SHEBANG))
            (ST_IN_SCRIPTING (equal phps-mode-lexer-STATE 'ST_IN_SCRIPTING))
            (ST_INITIAL (equal phps-mode-lexer-STATE 'ST_INITIAL))
            (ST_LOOKING_FOR_PROPERTY (equal phps-mode-lexer-STATE 'ST_LOOKING_FOR_PROPERTY))
            (ST_DOUBLE_QUOTES (equal phps-mode-lexer-STATE 'ST_DOUBLE_QUOTES))
            (ST_BACKQUOTE (equal phps-mode-lexer-STATE 'ST_BACKQUOTE))
            (ST_HEREDOC (equal phps-mode-lexer-STATE 'ST_HEREDOC))
            (ST_NOWDOC (equal phps-mode-lexer-STATE 'ST_NOWDOC))
            (ST_LOOKING_FOR_VARNAME (equal phps-mode-lexer-STATE 'ST_LOOKING_FOR_VARNAME))
            (ST_END_HEREDOC (equal phps-mode-lexer-STATE 'ST_END_HEREDOC))
            (ST_VAR_OFFSET (equal phps-mode-lexer-STATE 'ST_VAR_OFFSET)))

        ;; Reset re2c flags
        (setq phps-mode-lexer-re2c-matching-body nil)
        (setq phps-mode-lexer-re2c-matching-length nil)

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "exit"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_EXIT (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "die"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DIE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "fn"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FN (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "function"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FUNCTION (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "const"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CONST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "return"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_RETURN (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat "yield" phps-mode-lexer-WHITESPACE "from" "[^a-zA-Z0-9_\x80-\xff]")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_YIELD_FROM (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "yield"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_YIELD (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "try"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_TRY (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "catch"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CATCH (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "finally"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FINALLY (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "throw"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_THROW (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "if"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IF (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "elseif"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ELSEIF (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "endif"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDIF (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "else"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ELSE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "while"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_WHILE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "endwhile"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDWHILE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "do"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DO (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "for"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "endfor"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDFOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "foreach"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FOREACH (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "endforeach"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDFOREACH (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "declare"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DECLARE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "enddeclare"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDDECLARE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "instanceof"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INSTANCEOF (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "as"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_AS (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "switch"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SWITCH (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "endswitch"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ENDSWITCH (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "case"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CASE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "default"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DEFAULT (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "break"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_BREAK (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "continue"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CONTINUE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "goto"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_GOTO (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "echo"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ECHO (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "print"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PRINT (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "class"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CLASS (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "interface"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INTERFACE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "trait"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_TRAIT (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "extends"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_EXTENDS (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "implements"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IMPLEMENTS (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "->"))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_LOOKING_FOR_PROPERTY)
           (phps-mode-lexer-RETURN_TOKEN 'T_OBJECT_OPERATOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_IN_SCRIPTING ST_LOOKING_FOR_PROPERTY)
              (looking-at phps-mode-lexer-WHITESPACE))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties start end)))
             (if (phps-mode-wy-macros-CG 'PARSER_MODE)
                 (phps-mode-lexer-MOVE_FORWARD end)
               (phps-mode-lexer-RETURN_TOKEN data start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_LOOKING_FOR_PROPERTY (looking-at "->"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_OBJECT_OPERATOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer-LABEL))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (phps-mode-lexer-yy_pop_state)
             (phps-mode-lexer-RETURN_TOKEN 'T_STRING start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_LOOKING_FOR_PROPERTY (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (let ((end (match-end 0)))
             (phps-mode-lexer-yy_pop_state)
             ;; TODO goto restart here?
             ;; (message "Restart here")
             (phps-mode-lexer-MOVE_FORWARD end))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "::"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PAAMAYIM_NEKUDOTAYIM (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\\\"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_NS_SEPARATOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\.\\.\\."))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ELLIPSIS (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\?\\?"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_COALESCE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "new"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_NEW (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "clone"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CLONE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "var"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_VAR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "\\(int\\|integer\\)"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INT_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "\\(double\\|float\\)"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DOUBLE_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "\\(real\\)"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (when (phps-mode-wy-macros-CG 'PARSER_MODE)
             (display-warning 'phps-mode "PHPs Lexer Error - The (real) cast is deprecated, use (float) instead"))
           (phps-mode-lexer-RETURN_TOKEN 'T_DOUBLE_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "\\(string\\|binary\\)"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_STRING_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "array"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ARRAY_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "object"
                phps-mode-lexer-TABS_AND_SPACES
                ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_OBJECT_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at (concat "(" phps-mode-lexer-TABS_AND_SPACES "\\(bool\\|boolean\\)" phps-mode-lexer-TABS_AND_SPACES ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_BOOL_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "("
                phps-mode-lexer-TABS_AND_SPACES
                "unset"
                phps-mode-lexer-TABS_AND_SPACES ")")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_UNSET_CAST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "eval"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_EVAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "include"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INCLUDE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "include_once"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INCLUDE_ONCE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "require"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_REQUIRE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "require_once"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_REQUIRE_ONCE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "namespace"))
         (lambda()
           (setq phps-mode-lexer-declaring_namespace t)
           (phps-mode-lexer-RETURN_TOKEN 'T_NAMESPACE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "use"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_USE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "insteadof"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INSTEADOF (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "global"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_GLOBAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "isset"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ISSET (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "empty"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_EMPTY (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__halt_compiler"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_HALT_COMPILER (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "static"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_STATIC (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "abstract"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ABSTRACT (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "final"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FINAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "private"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PRIVATE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "protected"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PROTECTED (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "public"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PUBLIC (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "unset"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_UNSET (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "=>"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DOUBLE_ARROW (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "list"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_LIST (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "array"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_ARRAY (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "callable"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CALLABLE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\+\\+"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_INC (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "--"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DEC (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "==="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_IDENTICAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "!=="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_NOT_IDENTICAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "=="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\(!=\\|<>\\)"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_NOT_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "<=>"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SPACESHIP (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "<="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_SMALLER_OR_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at ">="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_IS_GREATER_OR_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\+="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_PLUS_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "-="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_MINUS_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\*="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_MUL_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_POW_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\*\\\\\\*"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_POW (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "/="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DIV_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\.="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CONCAT_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "%="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_MOD_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "<<="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SL_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at ">>="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SR_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "&="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_AND_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "|="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_OR_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\^="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_XOR_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\?\\?="))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_COALESCE_EQUAL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "||"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_BOOLEAN_OR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "&&"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_BOOLEAN_AND (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "OR"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_LOGICAL_OR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "AND"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_LOGICAL_AND (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "XOR"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_LOGICAL_XOR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "<<"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SL (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at ">>"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_SR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at phps-mode-lexer-TOKENS))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties start end))
                  (use-brace nil))
             ;; (message "Found token '%s'" data)
             (when phps-mode-lexer-declaring_namespace
               (when (string= data ";")
                 (setq phps-mode-lexer-prepend_trailing_brace t)
                 ;; (message "Set flag prepend trailing brace")
                 ;; (setq use-brace t)
                 )
               (setq phps-mode-lexer-declaring_namespace nil))
             (if use-brace
                 (phps-mode-lexer-RETURN_TOKEN "{" start end)
               (phps-mode-lexer-RETURN_TOKEN data start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "{"))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_IN_SCRIPTING)
           (when phps-mode-lexer-declaring_namespace
             (setq phps-mode-lexer-declaring_namespace nil))
           (phps-mode-lexer-RETURN_TOKEN "{" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at "\\${"))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_LOOKING_FOR_VARNAME)
           (phps-mode-lexer-RETURN_TOKEN 'T_DOLLAR_OPEN_CURLY_BRACES (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "}"))
         (lambda()
           (when phps-mode-lexer-state_stack
             (phps-mode-lexer-yy_pop_state))
           (phps-mode-lexer-RETURN_TOKEN "}" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_LOOKING_FOR_VARNAME (looking-at (concat phps-mode-lexer-LABEL "[\\[}]")))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (- (match-end 0) 1)))
             ;; (message "Stopped here")
             (phps-mode-lexer-yy_pop_state)
             (phps-mode-lexer-yy_push_state 'ST_IN_SCRIPTING)
             (phps-mode-lexer-RETURN_TOKEN 'T_STRING_VARNAME start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_LOOKING_FOR_VARNAME (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (phps-mode-lexer-yy_pop_state)
           (phps-mode-lexer-yy_push_state 'ST_IN_SCRIPTING)))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at phps-mode-lexer-BNUM))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties (+ start 2) end))
                  (long-number (string-to-number data 2)))
             ;; (message "Binary number %s from %s" long-number data)
             (if (> long-number phps-mode-lexer-long-limit)
                 (phps-mode-lexer-RETURN_TOKEN 'T_DNUMBER start end)
               (phps-mode-lexer-RETURN_TOKEN 'T_LNUMBER start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at phps-mode-lexer-LNUM))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (string-to-number (buffer-substring-no-properties start end))))
             ;; (message "Long number: %d" data)
             (if (> data phps-mode-lexer-long-limit)
                 (phps-mode-lexer-RETURN_TOKEN 'T_DNUMBER start end)
               (phps-mode-lexer-RETURN_TOKEN 'T_LNUMBER start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at phps-mode-lexer-HNUM))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties (+ start 2) end))
                  (long-number (string-to-number data 16)))
             ;; (message "Hexadecimal number %s from %s" long-number data)
             (if (> long-number phps-mode-lexer-long-limit)
                 (phps-mode-lexer-RETURN_TOKEN 'T_DNUMBER start end)
               (phps-mode-lexer-RETURN_TOKEN 'T_LNUMBER start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_VAR_OFFSET (looking-at "\\([0]\\|[1-9][0-9]*\\)"))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (phps-mode-lexer-RETURN_TOKEN 'T_NUM_STRING start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_VAR_OFFSET (looking-at (concat "\\("
                                                phps-mode-lexer-LNUM "\\|"
                                                phps-mode-lexer-HNUM "\\|"
                                                phps-mode-lexer-BNUM "\\)")))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_NUM_STRING (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (or (looking-at phps-mode-lexer-EXPONENT_DNUM)
                                  (looking-at phps-mode-lexer-DNUM)))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (_data (buffer-substring-no-properties start end)))
             ;; (message "Exponent/double at: %s" _data)
             (phps-mode-lexer-RETURN_TOKEN 'T_DNUMBER start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__CLASS__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_CLASS_C (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__TRAIT__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_TRAIT_C (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__FUNCTION__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FUNC_C (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__METHOD__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_METHOD_C (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__LINE__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_LINE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__FILE__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_FILE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__DIR__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_DIR (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "__NAMESPACE__"))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_NS_C (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and SHEBANG (looking-at (concat "#!.*" phps-mode-lexer-NEWLINE)))
         (lambda()
           (phps-mode-lexer-BEGIN 'ST_INITIAL)))

        (phps-mode-lexer-re2c-rule
         (and SHEBANG (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (phps-mode-lexer-BEGIN 'ST_INITIAL)))

        (phps-mode-lexer-re2c-rule
         (and ST_INITIAL (looking-at "<\\?="))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
             (when (phps-mode-wy-macros-CG 'PARSER_MODE)
               (phps-mode-lexer-RETURN_TOKEN 'T_ECHO start end))
             (phps-mode-lexer-RETURN_TOKEN 'T_OPEN_TAG_WITH_ECHO start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_INITIAL (looking-at (concat "<\\?php\\([ \t]\\|" phps-mode-lexer-NEWLINE "\\)")))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
             ;; (message "Starting scripting after <?php")
             (when phps-mode-lexer-EXPECTED
               (phps-mode-lexer-SKIP_TOKEN 'T_OPEN_TAG start end))
             (phps-mode-lexer-RETURN_TOKEN 'T_OPEN_TAG start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_INITIAL (looking-at "<\\?php"))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))

             ;; Allow <?php followed by end of file.
             (cond

              ((equal end (point-max))
               (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
               (phps-mode-lexer-RETURN_OR_SKIP_TOKEN
                'T_OPEN_TAG
                start
                end))

              ((phps-mode-wy-macros-CG 'SHORT_TAGS)
               (phps-mode-lexer-yyless 3)
               (setq end (- end 3))
               (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
               (phps-mode-lexer-RETURN_OR_SKIP_TOKEN
                'T_OPEN_TAG
                start
                end))

              (t
               (phps-mode-anaylzer-inline-char-handler))))))

        (phps-mode-lexer-re2c-rule
         (and ST_INITIAL (looking-at "<\\?"))
         (lambda()
           (when (phps-mode-wy-macros-CG 'SHORT_TAGS)
             (let ((start (match-beginning 0))
                   (end (match-end 0)))
               (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
               (when phps-mode-lexer-EXPECTED
                 (phps-mode-lexer-SKIP_TOKEN 'T_OPEN_TAG start end))
               ;; (message "Starting scripting after <?")
               (phps-mode-lexer-RETURN_TOKEN 'T_OPEN_TAG start end)))))

        (phps-mode-lexer-re2c-rule
         (and ST_INITIAL (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (phps-mode-anaylzer-inline-char-handler)))

        (phps-mode-lexer-re2c-rule
         (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
              (looking-at
               (concat
                "\\$"
                phps-mode-lexer-LABEL
                "->"
                "[a-zA-Z_\x80-\xff]")))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_LOOKING_FOR_PROPERTY)
           (forward-char -3)
           (phps-mode-lexer-RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (- (match-end 0) 3))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
              (looking-at
               (concat
                "\\$"
                phps-mode-lexer-LABEL
                "\\[")))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_VAR_OFFSET)
           (phps-mode-lexer-RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_IN_SCRIPTING ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE ST_VAR_OFFSET)
              (looking-at
               (concat
                "\\$"
                phps-mode-lexer-LABEL)))
         (lambda()
           (phps-mode-lexer-RETURN_TOKEN 'T_VARIABLE (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_VAR_OFFSET (looking-at "\\]"))
         (lambda()
           (phps-mode-lexer-yy_pop_state)
           (phps-mode-lexer-RETURN_TOKEN "]" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_VAR_OFFSET (looking-at (concat "\\(" phps-mode-lexer-TOKENS
                                                "\\|[{}\"`]\\)")))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties start end)))
             (phps-mode-lexer-RETURN_TOKEN data start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_VAR_OFFSET (looking-at (concat "[ \n\r\t'#]")))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (- (match-end 0) 1)))
             (phps-mode-lexer-yy_pop_state)
             (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE start end))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_IN_SCRIPTING ST_VAR_OFFSET) (looking-at phps-mode-lexer-LABEL))
         (lambda()
           ;; (message "Adding T_STRING from %s to %s" (match-beginning 0) (match-end 0))
           (phps-mode-lexer-RETURN_TOKEN 'T_STRING (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\\(#\\|//\\)"))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (_data (buffer-substring-no-properties start end))
                  (line (buffer-substring-no-properties end (line-end-position))))
             (if (string-match "\\?>" line)
                 (progn
                   (phps-mode-lexer-RETURN_TOKEN 'T_COMMENT start (+ end (match-beginning 0))))
               (progn
                 ;; TODO Handle expecting values here
                 ;; (message "Found comment 2 from %s to %s" start (line-end-position))
                 (phps-mode-lexer-RETURN_TOKEN 'T_COMMENT start (line-end-position)))))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "\\(/\\*\\|/\\*\\*"
                phps-mode-lexer-WHITESPACE
                "\\)")))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (_data (buffer-substring-no-properties start end))
                  (doc-com (looking-at-p (concat "/\\*\\*" phps-mode-lexer-WHITESPACE))))
             (let ((string-start (search-forward "*/" nil t)))
               (if string-start
                   (if doc-com
                       (phps-mode-lexer-RETURN_TOKEN 'T_DOC_COMMENT start (match-end 0))
                     (phps-mode-lexer-RETURN_TOKEN 'T_COMMENT start (match-end 0)))
                 (progn
                   (display-warning 'phps-mode
                                    (format
                                     "PHPs Lexer Error - Unterminated comment starting at %s"
                                     (point)))
                   (phps-mode-lexer-MOVE_FORWARD (point-max))))))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at (concat "\\?>" phps-mode-lexer-NEWLINE "?")))
         (lambda()
           (let ((start (match-beginning 0))
                 (end (match-end 0)))
             (when (= (- end start) 3)
               (setq end (1- end)))
             (phps-mode-lexer-BEGIN 'ST_INITIAL)
             (when (phps-mode-wy-macros-CG 'PARSER_MODE)
               (phps-mode-lexer-RETURN_TOKEN ";" start end))
             (phps-mode-lexer-RETURN_TOKEN 'T_CLOSE_TAG start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "'"))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (_data (buffer-substring-no-properties start end))
                  (un-escaped-end (phps-mode-lexer--get-next-unescaped "'")))
             (if un-escaped-end
                 (progn
                   (phps-mode-lexer-RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start un-escaped-end))
               (progn
                 ;; Unclosed single quotes
                 (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE start (point-max))
                 (phps-mode-lexer-MOVE_FORWARD (point-max)))))))

        ;; Double quoted string
        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "\""))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (_data (buffer-substring-no-properties start end))
                  (open-quote t))

             ;; Move forward from the double-quote
             (forward-char)

             (while open-quote
               (let ((string-start
                      (search-forward-regexp
                       (concat
                        "\\(\""
                        "\\|\\$" phps-mode-lexer-LABEL
                        "\\|\\${" phps-mode-lexer-LABEL
                        "\\|{\\$" phps-mode-lexer-LABEL "\\)")
                       nil t)))

                 ;; Do we find a ending double quote or starting variable?
                 (if string-start
                     (let ((string-start (match-beginning 0))
                           (is-escaped nil))

                       ;; Go to character before match start
                       (goto-char (1- string-start))

                       ;; Store whether character is escaped or not
                       (setq is-escaped (looking-at-p "\\\\"))

                       ;; Do we find variable inside quote?
                       (goto-char string-start)

                       ;; Process character if it's not escaped
                       (if is-escaped
                           (forward-char 2)
                         (setq open-quote nil)
                         (if (looking-at "\"")
                             (let ((_double-quoted-string (buffer-substring-no-properties start (+ string-start 1))))
                               ;; (message "Double quoted string: %s" _double-quoted-string)
                               (phps-mode-lexer-RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start (+ string-start 1)))
                           ;; (message "Found variable after '%s'" (buffer-substring-no-properties start string-start))
                           (phps-mode-lexer-BEGIN 'ST_DOUBLE_QUOTES)
                           (phps-mode-lexer-RETURN_TOKEN "\"" start (1+ start))
                           (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE (1+ start) string-start))))
                   (progn
                     (display-warning 'phps-mode (format "Found no ending of quote at %s" (point)))
                     (phps-mode-lexer-MOVE_FORWARD (point-max))
                     (setq open-quote nil))))))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING
              (looking-at
               (concat
                "<<<"
                phps-mode-lexer-TABS_AND_SPACES
                "\\("
                phps-mode-lexer-LABEL
                "\\|'"
                phps-mode-lexer-LABEL
                "'\\|\""
                phps-mode-lexer-LABEL
                "\"\\)"
                phps-mode-lexer-NEWLINE)))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (match-end 0))
                  (data (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                  (heredoc_label))

             ;; Determine if it's HEREDOC or NOWDOC and extract label here
             (if (string= (substring data 0 1) "'")
                 (progn
                   (setq heredoc_label (substring data 1 (- (length data) 1)))
                   (phps-mode-lexer-BEGIN 'ST_NOWDOC))
               (progn
                 (if (string= (substring data 0 1) "\"")
                     (setq heredoc_label (substring data 1 (- (length data) 1)))
                   (setq heredoc_label data))
                 (phps-mode-lexer-BEGIN 'ST_HEREDOC)))

             ;; Check for ending label on the next line
             (when (string= (buffer-substring-no-properties end (+ end (length heredoc_label))) heredoc_label)
               (phps-mode-lexer-BEGIN 'ST_END_HEREDOC))

             (push heredoc_label phps-mode-lexer-heredoc_label_stack)
             ;; (message "Found heredoc or nowdoc at %s with label %s" data heredoc_label)

             (phps-mode-lexer-RETURN_TOKEN 'T_START_HEREDOC start end))))

        (phps-mode-lexer-re2c-rule
         (and ST_IN_SCRIPTING (looking-at "[`]"))
         (lambda()
           ;; (message "Begun backquote at %s-%s" (match-beginning 0) (match-end 0))
           (phps-mode-lexer-BEGIN 'ST_BACKQUOTE)
           (phps-mode-lexer-RETURN_TOKEN "`" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_END_HEREDOC (looking-at (concat phps-mode-lexer-ANY_CHAR)))
         (lambda()
           (let* ((start (match-beginning 0))
                  (end (+ start (length heredoc_label) 1))
                  (_data (buffer-substring-no-properties start end)))
             ;; (message "Found ending heredoc at %s, %s of %s" _data (thing-at-point 'line) heredoc_label)
             (pop phps-mode-lexer-heredoc_label_stack)
             (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
             (phps-mode-lexer-RETURN_TOKEN 'T_END_HEREDOC start end))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC) (looking-at (concat "{\\$")))
         (lambda()
           (phps-mode-lexer-yy_push_state 'ST_IN_SCRIPTING)
           (phps-mode-lexer-RETURN_TOKEN 'T_CURLY_OPEN (match-beginning 0) (- (match-end 0) 1))))

        (phps-mode-lexer-re2c-rule
         (and ST_DOUBLE_QUOTES (looking-at "[\"]"))
         (lambda()
           (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
           ;; (message "Ended double-quote at %s" (match-beginning 0))
           (phps-mode-lexer-RETURN_TOKEN "\"" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_BACKQUOTE (looking-at "[`]"))
         (lambda()
           (phps-mode-lexer-BEGIN 'ST_IN_SCRIPTING)
           (phps-mode-lexer-RETURN_TOKEN "`" (match-beginning 0) (match-end 0))))

        (phps-mode-lexer-re2c-rule
         (and ST_DOUBLE_QUOTES (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (let ((start (point)))
             (let ((string-start (search-forward-regexp "[^\\\\]\"" nil t)))
               (if string-start
                   (let* ((end (- (match-end 0) 1))
                          (double-quoted-string (buffer-substring-no-properties start end)))
                     ;; Do we find variable inside quote?
                     (if (or (string-match (concat "\\${" phps-mode-lexer-LABEL) double-quoted-string)
                             (string-match (concat "{\\$" phps-mode-lexer-LABEL) double-quoted-string)
                             (string-match (concat "\\$" phps-mode-lexer-LABEL) double-quoted-string))
                         (progn
                           (let ((variable-start (+ start (match-beginning 0))))

                             ;; (message "Found starting expression inside double-quoted string at: %s %s" start variable-start)
                             (phps-mode-lexer-RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start variable-start)
                             ))
                       (progn
                         (phps-mode-lexer-RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING start end)
                         ;; (message "Found end of quote at %s-%s, moving ahead after '%s'" start end (buffer-substring-no-properties start end))
                         )))
                 (progn
                   (display-warning 'phps-mode (format "Found no ending of double quoted region starting at %s" start))
                   (phps-mode-lexer-MOVE_FORWARD (point-max))))))))

        (phps-mode-lexer-re2c-rule
         (and ST_BACKQUOTE (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (let ((string-start (search-forward-regexp "\\([^\\\\]`\\|\\$\\|{\\)" nil t)))
             (if string-start
                 (let ((start (- (match-end 0) 1)))
                   ;; (message "Skipping backquote forward over %s" (buffer-substring-no-properties old-start start))
                   (phps-mode-lexer-RETURN_TOKEN 'T_CONSTANT_ENCAPSED_STRING old-start start)
                   )
               (progn
                 (display-warning 'phps-mode (format "Found no ending of backquoted string starting at %s" (point)))
                 (phps-mode-lexer-MOVE_FORWARD (point-max)))))))

        (phps-mode-lexer-re2c-rule
         (and ST_HEREDOC (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           ;; Check for $, ${ and {$ forward
           (let ((string-start
                  (search-forward-regexp
                   (concat
                    "\\(\n"
                    heredoc_label
                    ";?\n\\|\\$"
                    phps-mode-lexer-LABEL
                    "\\|{\\$"
                    phps-mode-lexer-LABEL
                    "\\|\\${"
                    phps-mode-lexer-LABEL
                    "\\)"
                    ) nil t)))
             (if string-start
                 (let* ((start (match-beginning 0))
                        (end (match-end 0))
                        (data (buffer-substring-no-properties start end)))
                   ;; (message "Found something ending at %s" data)

                   (cond

                    ((string-match (concat "\n" heredoc_label ";?\n") data)
                                        ;, (message "Found heredoc end at %s-%s" start end)
                     (phps-mode-lexer-BEGIN 'ST_END_HEREDOC)
                     (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start))

                    (t
                     ;; (message "Found variable at '%s'.. Skipping forward to %s" data start)
                     (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start)
                     )

                    ))
               (progn
                 (display-warning 'phps-mode (format "Found no ending of heredoc at %s" (point)))
                 (phps-mode-lexer-MOVE_FORWARD (point-max)))))))

        (phps-mode-lexer-re2c-rule
         (and ST_NOWDOC (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (let ((string-start (search-forward-regexp (concat "\n" heredoc_label ";?\\\n") nil t)))
             (if string-start
                 (let* ((start (match-beginning 0))
                        (end (match-end 0))
                        (_data (buffer-substring-no-properties start end)))
                   ;; (message "Found something ending at %s" _data)
                   ;; (message "Found nowdoc end at %s-%s" start end)
                   (phps-mode-lexer-BEGIN 'ST_END_HEREDOC)
                   (phps-mode-lexer-RETURN_TOKEN 'T_ENCAPSED_AND_WHITESPACE old-start start)
                   )
               (progn
                 (display-warning 'phps-mode (format "Found no ending of newdoc starting at %s" (point)))
                 (phps-mode-lexer-MOVE_FORWARD (point-max)))))))

        (phps-mode-lexer-re2c-rule
         (and (or ST_IN_SCRIPTING ST_VAR_OFFSET) (looking-at phps-mode-lexer-ANY_CHAR))
         (lambda()
           (display-warning 'phps-mode (format "Unexpected character at %s" (point)))
           (phps-mode-lexer-MOVE_FORWARD (point-max))))

        (phps-mode-lexer-re2c-execute)))))

(define-lex phps-mode-lexer-lex
  "Call lexer analyzer action."
  phps-mode-lexer-lex-analyzer
  semantic-lex-default-action)

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

(defun phps-mode-lexer-setup (start end)
  "Just prepare other lexers for lexing region START to END."
  (phps-mode-debug-message (message "Lexer setup %s - %s" start end))
  (unless phps-mode-lexer-STATE
    (phps-mode-lexer-BEGIN 'ST_INITIAL)))

(defun phps-mode-lexer-run ()
  "Run lexer."
  (interactive)
  (phps-mode-debug-message (message "Lexer run"))
  (setq-local phps-mode-lexer-buffer-length (1- (point-max)))
  (setq-local phps-mode-lexer-buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
  (setq-local phps-mode-lexer-STATE nil)
  (setq-local phps-mode-lexer-state_stack nil)
  (setq-local phps-mode-lexer-states nil)
  (setq-local phps-mode-lexer-tokens (semantic-lex-buffer)))

(defun phps-mode-lexer-move-states (start diff)
  "Move lexer states after (or equal to) START with modification DIFF."
  (when phps-mode-lexer-states
    (setq-local phps-mode-lexer-states (phps-mode-lexer-get-moved-states phps-mode-lexer-states start diff))))

(defun phps-mode-lexer-get-moved-states (states start diff)
  "Return moved lexer STATES after (or equal to) START with modification DIFF."
  (let ((old-states states)
        (new-states '()))
    (when old-states

      ;; Iterate through states add states before start start unchanged and the others modified with diff
      (dolist (state-object (nreverse old-states))
        (let ((state-start (nth 0 state-object))
              (state-end (nth 1 state-object))
              (state-symbol (nth 2 state-object))
              (state-stack (nth 3 state-object)))
          (if (>= state-start start)
              (let ((new-state-start (+ state-start diff))
                    (new-state-end (+ state-end diff)))
                (push (list new-state-start new-state-end state-symbol state-stack) new-states))
            (if (> state-end start)
                (let ((new-state-end (+ state-end diff)))
                  (push (list state-start new-state-end state-symbol state-stack) new-states))
              (push state-object new-states))))))

    new-states))

(defun phps-mode-lexer-move-tokens (start diff)
  "Update tokens with moved lexer tokens after or equal to START with modification DIFF."
  (when phps-mode-lexer-tokens
    (setq-local phps-mode-lexer-tokens (phps-mode-lexer-get-moved-tokens phps-mode-lexer-tokens start diff))))

(defun phps-mode-lexer-get-moved-tokens (old-tokens start diff)
  "Return moved lexer OLD-TOKENS positions after (or equal to) START with DIFF points."
  (let ((new-tokens '()))
    (when old-tokens

      ;; Iterate over all tokens, add those that are to be left unchanged and add modified ones that should be changed.
      (dolist (token (nreverse old-tokens))
        (let ((token-symbol (car token))
              (token-start (car (cdr token)))
              (token-end (cdr (cdr token))))
          (if (>= token-start start)
              (let ((new-token-start (+ token-start diff))
                    (new-token-end (+ token-end diff)))
                (push `(,token-symbol ,new-token-start . ,new-token-end) new-tokens))
            (if (> token-end start)
                (let ((new-token-end (+ token-end diff)))
                  (push `(,token-symbol ,token-start . ,new-token-end) new-tokens))
              (push token new-tokens))))))
    new-tokens))

(defun phps-mode-functions--reset-changes ()
  "Reset change."
  (setq phps-mode-analyzer-change-min nil))

(defun phps-mode-analyzer-process-changes (&optional buffer)
  "Run incremental lexer on BUFFER.  Return list of performed operations."
  (unless buffer
    (setq buffer (current-buffer)))
  (phps-mode-debug-message
   (message "Run process changes on buffer '%s'" buffer))
  (with-current-buffer buffer
    (let ((run-full-lexer nil)
          (old-tokens phps-mode-lexer-tokens)
          (old-states phps-mode-lexer-states)
          (log '()))

      (if phps-mode-analyzer-change-min
          (progn
            (phps-mode-debug-message
             (message "Processing change point minimum: %s" phps-mode-analyzer-change-min))
            (let ((incremental-state nil)
                  (incremental-state-stack nil)
                  (incremental-tokens nil)
                  (head-states '())
                  (head-tokens '())
                  (change-start phps-mode-analyzer-change-min)
                  (incremental-start-new-buffer phps-mode-analyzer-change-min))

              ;; Reset processed buffer flag
              (phps-mode-functions-reset-processed-buffer)

              ;; Reset idle timer
              (phps-mode-functions--cancel-idle-timer)

              ;; Reset buffer changes minimum index
              (phps-mode-functions--reset-changes)

              ;; Reset tokens and states here
              (setq-local phps-mode-lexer-tokens nil)
              (setq-local phps-mode-lexer-states nil)
              (setq-local phps-mode-lexer-STATE nil)
              (setq-local phps-mode-lexer-state_stack nil)

              ;; NOTE Starts are inclusive while ends are exclusive buffer locations

              ;; Some tokens have dynamic length and if a change occurs at token-end
              ;; we must start the incremental process at previous token start

              ;; Build list of tokens from old buffer before start of changes (head-tokens)

              (catch 'quit
                (dolist (token old-tokens)
                  (let ((start (car (cdr token)))
                        (end (cdr (cdr token))))
                    (if (< end change-start)
                        (push token head-tokens)
                      (when (< start change-start)
                        (phps-mode-debug-message
                         (message "New incremental-start-new-buffer: %s" start))
                        (setq incremental-start-new-buffer start))
                      (throw 'quit "break")))))

              (setq head-tokens (nreverse head-tokens))
              (phps-mode-debug-message
               (message "Head tokens: %s" head-tokens)
               (message "Incremental-start-new-buffer: %s" incremental-start-new-buffer))

              ;; Did we find a start for the incremental process?
              (if head-tokens
                  (progn
                    (phps-mode-debug-message
                     (message "Found head tokens"))

                    ;; In old buffer:
                    ;; 1. Determine state (incremental-state) and state-stack (incremental-state-stack) before incremental start
                    ;; 2. Build list of states before incremental start (head-states)
                    (catch 'quit
                      (dolist (state-object (nreverse old-states))
                        (let ((end (nth 1 state-object)))
                          (if (< end change-start)
                              (progn
                                (setq incremental-state (nth 2 state-object))
                                (setq incremental-state-stack (nth 3 state-object))
                                (push state-object head-states))
                            (throw 'quit "break")))))

                    (phps-mode-debug-message
                     (message "Head states: %s" head-states)
                     (message "Incremental state: %s" incremental-state)
                     (message "State stack: %s" incremental-state-stack))

                    (if (and
                         head-states
                         incremental-state)
                        (progn
                          (phps-mode-debug-message
                           (message "Found head states"))

                          ;; Do partial lex from previous-token-end to change-stop

                          ;; Rewind lexer state here
                          (setq-local phps-mode-lexer-states head-states)
                          (setq-local phps-mode-lexer-STATE incremental-state)
                          (setq-local phps-mode-lexer-state_stack incremental-state-stack)

                          ;; Generate new tokens
                          (setq incremental-tokens (semantic-lex incremental-start-new-buffer (point-max)))

                          (setq-local phps-mode-lexer-tokens (append head-tokens incremental-tokens))

                          (push (list 'INCREMENTAL-LEX incremental-start-new-buffer) log)

                          (phps-mode-debug-message
                           (message "Incremental tokens: %s" incremental-tokens)))
                      (push (list 'FOUND-NO-HEAD-STATES incremental-start-new-buffer) log)
                      (phps-mode-debug-message
                       (message "Found no head states"))
                      (setq run-full-lexer t)))
                (push (list 'FOUND-NO-HEAD-TOKENS incremental-start-new-buffer) log)
                (phps-mode-debug-message
                 (message "Found no head tokens"))
                (setq run-full-lexer t))))
        (push (list 'FOUND-NO-CHANGE-POINT-MINIMUM) log)
        (phps-mode-debug-message
         (message "Found no change point minimum"))
        (setq run-full-lexer t))

      (when run-full-lexer
        (push (list 'RUN-FULL-LEXER) log)
        (phps-mode-debug-message
         (message "Running full lexer"))
        (phps-mode-lexer-run))
      log)))

(defun phps-mode-functions-get-processed-buffer ()
  "Get flag for whether buffer is processed or not."
  phps-mode-functions-processed-buffer)

(defun phps-mode-functions-reset-processed-buffer ()
  "Reset flag for whether buffer is processed or not."
  (setq-local phps-mode-functions-processed-buffer nil))

(defun phps-mode-functions-process-current-buffer ()
  "Process current buffer, generate indentations and Imenu, trigger incremental lexer if we have change."
  (interactive)
  (phps-mode-debug-message (message "Process current buffer"))
  (when phps-mode-functions-idle-timer
    (phps-mode-debug-message
     (message "Flag buffer as not processed since changes are detected"))
    (setq-local phps-mode-functions-processed-buffer nil)
    (when phps-mode-analyzer-process-on-indent-and-imenu
      (phps-mode-debug-message (message "Trigger incremental lexer"))
      (phps-mode-analyzer-process-changes)))
  (if (and
       (not phps-mode-functions-processed-buffer)
       (not phps-mode-functions-idle-timer))
      (progn
        (phps-mode-debug-message (message "Buffer is not processed"))
        (let ((processed
               (phps-mode-functions--process-tokens-in-string
                phps-mode-lexer-tokens
                (buffer-substring-no-properties
                 (point-min)
                 (point-max)))))
          (phps-mode-debug-message (message "Processed result: %s" processed))
          (setq-local phps-mode-functions-imenu (nth 0 processed))
          (setq-local phps-mode-functions-lines-indent (nth 1 processed)))
        (setq-local phps-mode-functions-processed-buffer t))
    (phps-mode-debug-message
     (when phps-mode-functions-processed-buffer
       (message "Buffer is already processed"))
     (when phps-mode-functions-idle-timer
       (message "Not processing buffer since there are non-lexed changes")))))

(defun phps-mode-functions-get-moved-lines-indent (old-lines-indents start-line-number diff)
  "Move OLD-LINES-INDENTS from START-LINE-NUMBER with DIFF points."
  (let ((lines-indents (make-hash-table :test 'equal))
        (line-number 1))
    (when old-lines-indents
      (let ((line-indent (gethash line-number old-lines-indents))
            (new-line-number))
        (while line-indent

          (when (< line-number start-line-number)
            ;; (message "Added new indent 3 %s from %s to %s" line-indent line-number line-number)
            (puthash line-number line-indent lines-indents))

          (when (and (> diff 0)
                     (>= line-number start-line-number)
                     (< line-number (+ start-line-number diff)))
            ;; (message "Added new indent 2 %s from %s to %s" line-indent line-number line-number)
            (puthash line-number (gethash start-line-number old-lines-indents) lines-indents))

          (when (>= line-number start-line-number)
            (setq new-line-number (+ line-number diff))
            ;; (message "Added new indent %s from %s to %s" line-indent line-number new-line-number)
            (puthash new-line-number line-indent lines-indents))

          (setq line-number (1+ line-number))
          (setq line-indent (gethash line-number old-lines-indents))))
      lines-indents)))

(defun phps-mode-functions-move-imenu-index (start diff)
  "Moved imenu from START by DIFF points."
  (when phps-mode-functions-imenu
    (setq-local phps-mode-functions-imenu
                (phps-mode-functions-get-moved-imenu phps-mode-functions-imenu start diff))
    (phps-mode-analyzer--reset-imenu)))

(defun phps-mode-functions-move-lines-indent (start-line-number diff)
  "Move lines indent from START-LINE-NUMBER with DIFF points."
  (when phps-mode-functions-lines-indent
    ;; (message "Moving line-indent index from %s with %s" start-line-number diff)
    (setq-local
     phps-mode-functions-lines-indent
     (phps-mode-functions-get-moved-lines-indent
      phps-mode-functions-lines-indent
      start-line-number
      diff))))

(defun phps-mode-functions-get-lines-indent ()
  "Return lines indent, process buffer if not done already."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-lines-indent)

(defun phps-mode-functions-get-imenu ()
  "Return Imenu, process buffer if not done already."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-imenu)

(defun phps-mode-functions-get-moved-imenu (old-index start diff)
  "Move imenu-index OLD-INDEX beginning from START with DIFF."
  (let ((new-index '()))

    (when old-index
      (if (and (listp old-index)
               (listp (car old-index)))
          (dolist (item old-index)
            (let ((sub-item (phps-mode-functions-get-moved-imenu item start diff)))
              (push (car sub-item) new-index)))
        (let ((item old-index))
          (let ((item-label (car item)))
            (if (listp (cdr item))
                (let ((sub-item (phps-mode-functions-get-moved-imenu (cdr item) start diff)))
                  (push `(,item-label . ,sub-item) new-index))
              (let ((item-start (cdr item)))
                (when (>= item-start start)
                  (setq item-start (+ item-start diff)))
                (push `(,item-label . ,item-start) new-index)))))))

    (nreverse new-index)))

(defun phps-mode-functions--get-lines-in-buffer (beg end)
  "Return the number of lines in buffer between BEG and END."
  (phps-mode-functions--get-lines-in-string (buffer-substring-no-properties beg end)))

(defun phps-mode-functions--get-lines-in-string (string)
  "Return the number of lines in STRING."
  (let ((lines-in-string 0)
        (start 0))
    (while (string-match "[\n]" string start)
      (setq start (match-end 0))
      (setq lines-in-string (1+ lines-in-string)))
    lines-in-string))

(defun phps-mode-functions--get-inline-html-indentation
    (
     inline-html
     indent
     tag-level
     curly-bracket-level
     square-bracket-level
     round-bracket-level)
  "Generate a list of indentation for each line in INLINE-HTML.
Working incrementally on INDENT, TAG-LEVEL, CURLY-BRACKET-LEVEL,
SQUARE-BRACKET-LEVEL and ROUND-BRACKET-LEVEL."
  (phps-mode-debug-message
   (message "Calculating HTML indent for: '%s'" inline-html))

  ;; Add trailing newline if missing
  (unless (string-match-p "\n$" inline-html)
    (setq inline-html (concat inline-html "\n")))

  (let ((start 0)
        (indent-start indent)
        (indent-end indent)
        (line-indents nil)
        (first-object-on-line t)
        (first-object-is-nesting-decrease nil))
    (while
        (string-match
         "\\([\n]\\)\\|\\(<[a-zA-Z]+\\)\\|\\(</[a-zA-Z]+\\)\\|\\(/>\\)\\|\\(\\[\\)\\|\\()\\)\\|\\((\\)\\|\\({\\|}\\)"
         inline-html
         start)
      (let* ((end (match-end 0))
             (string (substring inline-html (match-beginning 0) end)))

        (cond

         ((string-match-p "\n" string)

          (let ((temp-indent indent))
            (when first-object-is-nesting-decrease
              (phps-mode-debug-message
               (message "Decreasing indent with one since first object was a nesting decrease"))
              (setq temp-indent (1- indent))
              (when (< temp-indent 0)
                (setq temp-indent 0)))
            (push temp-indent line-indents))

          (setq indent-end (+ tag-level curly-bracket-level square-bracket-level round-bracket-level))
          (phps-mode-debug-message "Encountered a new-line")
          (if (> indent-end indent-start)
              (progn
                (phps-mode-debug-message
                 (message "Increasing indent since %s is above %s" indent-end indent-start))
                (setq indent (1+ indent)))
            (when (< indent-end indent-start)
              (phps-mode-debug-message
               (message "Decreasing indent since %s is below %s" indent-end indent-start))
              (setq indent (1- indent))
              (when (< indent 0)
                (setq indent 0))))

          (setq indent-start indent-end)
          (setq first-object-on-line t)
          (setq first-object-is-nesting-decrease nil))

         ((string= string "(")
          (setq round-bracket-level (1+ round-bracket-level)))
         ((string= string ")")
          (setq round-bracket-level (1- round-bracket-level)))

         ((string= string "[")
          (setq square-bracket-level (1+ square-bracket-level)))
         ((string= string "]")
          (setq square-bracket-level (1- square-bracket-level)))

         ((string= string "{")
          (setq curly-bracket-level (1+ curly-bracket-level)))
         ((string= string "}")
          (setq curly-bracket-level (1- curly-bracket-level)))

         ((string-match "<[a-zA-Z]+" string)
          (setq tag-level (1+ tag-level)))

         ((string-match "\\(</[a-zA-Z]+\\)\\|\\(/>\\)" string)
          (setq tag-level (1- tag-level)))

         )

        (when first-object-on-line
          (unless (string-match-p "\n" string)
            (setq first-object-on-line nil)
            (setq indent-end (+ tag-level curly-bracket-level square-bracket-level round-bracket-level))
            (when (< indent-end indent-start)
              (phps-mode-debug-message "First object was nesting decrease")
              (setq first-object-is-nesting-decrease t))))

        (setq start end)))
    (list (nreverse line-indents) indent tag-level curly-bracket-level square-bracket-level round-bracket-level)))

(defun phps-mode-functions--process-tokens-in-string (tokens string)
  "Generate indexes for imenu and indentation for TOKENS and STRING one pass.  Complexity: O(n)."
  (if tokens
      (progn
        (phps-mode-debug-message
         (message
          "\nCalculation indentation and imenu for all lines in buffer:\n\n%s"
          string))
        (let ((in-heredoc nil)
              (in-heredoc-started-this-line nil)
              (in-heredoc-ended-this-line nil)
              (in-inline-control-structure nil)
              (inline-html-indent 0)
              (inline-html-indent-start 0)
              (inline-html-tag-level 0)
              (inline-html-curly-bracket-level 0)
              (inline-html-square-bracket-level 0)
              (inline-html-round-bracket-level 0)
              (inline-html-is-whitespace nil)
              (inline-html-rest-is-whitespace nil)
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
              (alternative-control-structure-line 0)
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
              (first-token-on-line t)
              (line-indents (make-hash-table :test 'equal))
              (first-token-is-nesting-decrease nil)
              (token-number 1)
              (allow-custom-column-increment nil)
              (allow-custom-column-decrement nil)
              (in-assignment nil)
              (in-assignment-round-bracket-level nil)
              (in-assignment-square-bracket-level nil)
              (in-assignment-level 0)
              (in-object-operator nil)
              (in-object-operator-round-bracket-level nil)
              (in-object-operator-square-bracket-level nil)
              (after-object-operator nil)
              (in-object-operator-level 0)
              (in-class-declaration nil)
              (in-class-declaration-level 0)
              (in-return nil)
              (in-return-curly-bracket-level nil)
              (in-return-level 0)
              (previous-token nil)
              (token nil)
              (token-start nil)
              (token-end nil)
              (token-start-line-number 0)
              (token-end-line-number 0)
              (tokens (nreverse (copy-sequence tokens)))
              (nesting-stack nil)
              (nesting-key nil)
              (class-declaration-started-this-line nil)
              (special-control-structure-started-this-line nil)
              (temp-pre-indent nil)
              (temp-post-indent nil)
              (imenu-index '())
              (imenu-namespace-index '())
              (imenu-class-index '())
              (imenu-in-namespace-declaration nil)
              (imenu-in-namespace-name nil)
              (imenu-in-namespace-with-brackets nil)
              (imenu-open-namespace-level nil)
              (imenu-in-class-declaration nil)
              (imenu-open-class-level nil)
              (imenu-in-class-name nil)
              (imenu-in-function-declaration nil)
              (imenu-in-function-name nil)
              (imenu-in-function-index nil)
              (imenu-nesting-level 0)
              (incremental-line-number 1))

          (push `(END_PARSE ,(length string) . ,(length string)) tokens)

          ;; Iterate through all buffer tokens from beginning to end
          (dolist (item (nreverse tokens))
            ;; (message "Items: %s %s" item phps-mode-lexer-tokens)
            (let ((next-token (car item))
                  (next-token-start (car (cdr item)))
                  (next-token-end (cdr (cdr item)))
                  (next-token-start-line-number nil)
                  (next-token-end-line-number nil))

              (when (and token
                         (< token-end next-token-start))
                ;; NOTE We use a incremental-line-number calculation because `line-at-pos' takes a lot of time
                (setq
                 incremental-line-number
                 (+
                  incremental-line-number
                  (phps-mode-functions--get-lines-in-string
                   (substring
                    string
                    (1- token-end)
                    (1- next-token-start))))))

              ;; Handle the pseudo-token for last-line
              (if (equal next-token 'END_PARSE)
                  (progn
                    (setq next-token-start-line-number (1+ token-start-line-number))
                    (setq next-token-end-line-number (1+ token-end-line-number)))
                (setq next-token-start-line-number incremental-line-number)

                ;; NOTE We use a incremental-line-number calculation because `line-at-pos' takes a lot of time
                ;; (message "Lines for %s '%s'" next-token (substring string (1- next-token-start) (1- next-token-end)))
                (setq
                 incremental-line-number
                 (+
                  incremental-line-number
                  (phps-mode-functions--get-lines-in-string
                   (substring
                    string
                    (1- next-token-start)
                    (1- next-token-end)))))
                (setq next-token-end-line-number incremental-line-number)
                (phps-mode-debug-message
                 (message
                  "Token '%s' pos: %s-%s lines: %s-%s"
                  next-token
                  next-token-start
                  next-token-end
                  next-token-start-line-number
                  next-token-end-line-number)))

              ;; Token logic - we have one-two token look-ahead at this point
              ;; `token' is previous token
              ;; `next-token' is current token
              ;; `previous-token' is maybe two tokens back
              (when token


                ;; IMENU LOGIC

                (cond

                 ((or (string= token "{")
                      (equal token 'T_CURLY_OPEN)
                      (equal token 'T_DOLLAR_OPEN_CURLY_BRACES))
                  (setq imenu-nesting-level (1+ imenu-nesting-level)))

                 ((string= token "}")

                  (when (and imenu-open-namespace-level
                             (= imenu-open-namespace-level imenu-nesting-level)
                             imenu-in-namespace-name
                             imenu-namespace-index)
                    (let ((imenu-add-list (nreverse imenu-namespace-index)))
                      (push `(,imenu-in-namespace-name . ,imenu-add-list) imenu-index))
                    (setq imenu-in-namespace-name nil))

                  (when (and imenu-open-class-level
                             (= imenu-open-class-level imenu-nesting-level)
                             imenu-in-class-name
                             imenu-class-index)
                    (let ((imenu-add-list (nreverse imenu-class-index)))
                      (if imenu-in-namespace-name
                          (push `(,imenu-in-class-name . ,imenu-add-list) imenu-namespace-index)
                        (push `(,imenu-in-class-name . ,imenu-add-list) imenu-index)))
                    (setq imenu-in-class-name nil))

                  (setq imenu-nesting-level (1- imenu-nesting-level))))

                (cond

                 (imenu-in-namespace-declaration
                  (cond

                   ((or (string= token "{")
                        (string= token ";"))
                    (setq imenu-in-namespace-with-brackets (string= token "{"))
                    (setq imenu-open-namespace-level imenu-nesting-level)
                    (setq imenu-namespace-index '())
                    (setq imenu-in-namespace-declaration nil))

                   ((and (or (equal token 'T_STRING)
                             (equal token 'T_NS_SEPARATOR))
                         (setq
                          imenu-in-namespace-name
                          (concat
                           imenu-in-namespace-name
                           (substring
                            string
                            (1- token-start)
                            (1- token-end))))))))

                 (imenu-in-class-declaration
                  (cond

                   ((string= token "{")
                    (setq imenu-open-class-level imenu-nesting-level)
                    (setq imenu-in-class-declaration nil)
                    (setq imenu-class-index '()))

                   ((and (equal token 'T_STRING)
                         (not imenu-in-class-name))
                    (setq imenu-in-class-name (substring string (1- token-start) (1- token-end))))))

                 (imenu-in-function-declaration
                  (cond

                   ((or (string= token "{")
                        (string= token ";"))
                    (when imenu-in-function-name
                      (if imenu-in-class-name
                          (push `(,imenu-in-function-name . ,imenu-in-function-index) imenu-class-index)
                        (if imenu-in-namespace-name
                            (push `(,imenu-in-function-name . ,imenu-in-function-index) imenu-namespace-index)
                          (push `(,imenu-in-function-name . ,imenu-in-function-index) imenu-index))))
                    (setq imenu-in-function-name nil)
                    (setq imenu-in-function-declaration nil))

                   ((and (equal token 'T_STRING)
                         (not imenu-in-function-name))
                    (setq imenu-in-function-name (substring string (1- token-start) (1- token-end)))
                    (setq imenu-in-function-index token-start))))

                 (t (cond

                     ((and (not imenu-in-namespace-name)
                           (equal token 'T_NAMESPACE))
                      (setq imenu-in-namespace-name nil)
                      (setq imenu-in-namespace-declaration t))

                     ((and (not imenu-in-class-name)
                           (or (equal token 'T_CLASS)
                               (equal token 'T_INTERFACE)))
                      (setq imenu-in-class-name nil)
                      (setq imenu-in-class-declaration t))

                     ((and (not imenu-in-function-name)
                           (equal token 'T_FUNCTION))
                      (setq imenu-in-function-name nil)
                      (setq imenu-in-function-declaration t)))))

                (when (and (equal next-token 'END_PARSE)
                           imenu-in-namespace-name
                           (not imenu-in-namespace-with-brackets)
                           imenu-namespace-index)
                  (let ((imenu-add-list (nreverse imenu-namespace-index)))
                    (push `(,imenu-in-namespace-name . ,imenu-add-list) imenu-index))
                  (setq imenu-in-namespace-name nil))


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

                ;; Handle INLINE_HTML blocks
                (when (equal token 'T_INLINE_HTML)

                  ;; Flag whether inline-html is whitespace or not
                  (setq
                   inline-html-is-whitespace
                   (string=
                    (string-trim
                     (substring
                      string
                      (1- token-start)
                      (1- token-end))) ""))
                  (setq
                   inline-html-rest-is-whitespace
                   (string-match
                    "^[\ \t\r\f]+\n"
                    (substring
                     string
                     (1- token-start)
                     (1- token-end))))

                  (when first-token-on-line
                    (setq first-token-is-inline-html t))

                  (let ((inline-html-indents
                         (phps-mode-functions--get-inline-html-indentation
                          (substring
                           string
                           (1- token-start)
                           (1- token-end))
                          inline-html-indent
                          inline-html-tag-level
                          inline-html-curly-bracket-level
                          inline-html-square-bracket-level
                          inline-html-round-bracket-level)))

                    (phps-mode-debug-message
                     (message
                      "Received inline html indent: %s from inline HTML: '%s'"
                      inline-html-indents
                      (substring
                       string
                       (1- token-start)
                       (1- token-end))))

                    ;; Update indexes
                    (setq inline-html-indent (nth 1 inline-html-indents))
                    (setq inline-html-tag-level (nth 2 inline-html-indents))
                    (setq inline-html-curly-bracket-level (nth 3 inline-html-indents))
                    (setq inline-html-square-bracket-level (nth 4 inline-html-indents))
                    (setq inline-html-round-bracket-level (nth 5 inline-html-indents))

                    (phps-mode-debug-message
                     (message "First token is inline html: %s" first-token-is-inline-html))

                    ;; Does inline html span several lines or starts a new line?
                    (when (or (> token-end-line-number token-start-line-number)
                              first-token-is-inline-html)

                      ;; Token does not only contain white-space?
                      (unless inline-html-is-whitespace
                        (let ((token-line-number-diff token-start-line-number))
                          ;; Iterate lines here and add indents
                          (dolist (item (nth 0 inline-html-indents))
                            ;; Skip first line unless first token on line was inline-html
                            (when (or (not (= token-line-number-diff token-start-line-number))
                                      first-token-is-inline-html)
                              (unless (gethash token-line-number-diff line-indents)
                                (puthash token-line-number-diff (list item 0) line-indents)
                                (phps-mode-debug-message
                                 (message
                                  "Putting indent at line %s to %s from inline HTML"
                                  token-line-number-diff
                                  item))))
                            (setq token-line-number-diff (1+ token-line-number-diff))))))))

                ;; Keep track of when we are inside a class definition
                (if in-class-declaration
                    (if (string= token "{")
                        (progn
                          (setq in-class-declaration nil)
                          (setq in-class-declaration-level 0)

                          (unless class-declaration-started-this-line
                            (setq column-level (1- column-level))
                            (pop nesting-stack))

                          (when first-token-on-line
                            (setq first-token-is-nesting-decrease t))

                          )
                      (when first-token-on-line
                        (setq in-class-declaration-level 1)))

                  ;; If ::class is used as a magical class constant it should not be considered start of a class declaration
                  (when (and (equal token 'T_CLASS)
                             (or (not previous-token)
                                 (not (equal previous-token 'T_PAAMAYIM_NEKUDOTAYIM))))
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

                    (phps-mode-debug-message
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

                    (phps-mode-debug-message
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
                      (progn
                        (setq after-special-control-structure-token token)
                        (setq alternative-control-structure-line token-start-line-number))

                    ;; Is token not a curly bracket - because that is a ordinary control structure syntax
                    (if (string= token "{")

                        ;; Save curly bracket level when switch starts
                        (when (equal after-special-control-structure-token 'T_SWITCH)

                          (phps-mode-debug-message
                           (message "Started switch curly stack at %s" curly-bracket-level))

                          (push curly-bracket-level switch-curly-stack))

                      ;; Is it the start of an alternative control structure?
                      (if (string= token ":")

                          (progn

                            ;; Save alternative nesting level for switch
                            (when (equal after-special-control-structure-token 'T_SWITCH)

                              (phps-mode-debug-message
                               (message "Started switch alternative stack at %s" alternative-control-structure-level))

                              (push alternative-control-structure-level switch-alternative-stack))

                            (setq alternative-control-structure-level (1+ alternative-control-structure-level))

                            (phps-mode-debug-message
                             (message
                              "\nIncreasing alternative-control-structure after %s %s to %s\n"
                              after-special-control-structure-token
                              token
                              alternative-control-structure-level))
                            )

                        ;; Don't start inline control structures after a while ($condition); expression
                        (unless (string= token ";")
                          (phps-mode-debug-message
                           (message
                            "\nStarted inline control-structure after %s at %s\n"
                            after-special-control-structure-token
                            token))

                          (setq in-inline-control-structure t)
                          (when (< alternative-control-structure-line token-start-line-number)
                            (setq temp-pre-indent (1+ column-level))))))

                    (setq after-special-control-structure nil)
                    (setq after-special-control-structure-token nil)
                    (setq alternative-control-structure-line nil)))

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
                      (phps-mode-debug-message "Ended concatenation")
                      (pop in-concatenation-round-bracket-level)
                      (pop in-concatenation-square-bracket-level)
                      (unless in-concatenation-round-bracket-level
                        (setq in-concatenation nil))
                      (setq in-concatenation-level (1- in-concatenation-level)))
                  (when (and (> next-token-start-line-number token-end-line-number)
                             (or (string= token ".")
                                 (string= next-token ".")))
                    (phps-mode-debug-message "Started concatenation")
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
                  (setq alternative-control-structure-line token-start-line-number)
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

                    (phps-mode-debug-message
                     (message "\nDecreasing alternative control structure nesting at %s to %s\n" token alternative-control-structure-level)))

                  )

                ;; Keep track of assignments
                (when in-assignment
                  (when (or (string= token ";")
                            (and (string= token ")")
                                 (or (< round-bracket-level (car in-assignment-round-bracket-level))
                                     (and
                                      (= round-bracket-level (car in-assignment-round-bracket-level))
                                      (= square-bracket-level (car in-assignment-square-bracket-level))
                                      (or (string= next-token ")")
                                          (string= next-token "]")))))
                            (and (string= token ",")
                                 (= round-bracket-level (car in-assignment-round-bracket-level))
                                 (= square-bracket-level (car in-assignment-square-bracket-level)))
                            (and (string= token "]")
                                 (or (< square-bracket-level (car in-assignment-square-bracket-level))
                                     (and
                                      (= square-bracket-level (car in-assignment-square-bracket-level))
                                      (= round-bracket-level (car in-assignment-round-bracket-level))
                                      (or (string= next-token "]")
                                          (string= next-token ")")))))
                            (and (equal token 'T_FUNCTION)
                                 (= round-bracket-level (car in-assignment-round-bracket-level))))

                    ;; NOTE Ending an assignment because of a T_FUNCTION token is to support PSR-2 Closures
                    
                    (phps-mode-debug-message
                     (message "Ended assignment %s at %s %s" in-assignment-level token next-token))
                    (pop in-assignment-square-bracket-level)
                    (pop in-assignment-round-bracket-level)
                    (unless in-assignment-round-bracket-level
                      (setq in-assignment nil))
                    (setq in-assignment-level (1- in-assignment-level))

                    ;; Did we end two assignment at once?
                    (when (and
                           in-assignment-round-bracket-level
                           in-assignment-square-bracket-level
                           (= round-bracket-level (car in-assignment-round-bracket-level))
                           (= square-bracket-level (car in-assignment-square-bracket-level))
                           (or (string= next-token ")")
                               (string= next-token "]")))
                      (phps-mode-debug-message
                       (message "Ended another assignment %s at %s %s" in-assignment-level token next-token))
                      (pop in-assignment-square-bracket-level)
                      (pop in-assignment-round-bracket-level)
                      (unless in-assignment-round-bracket-level
                        (setq in-assignment nil))
                      (setq in-assignment-level (1- in-assignment-level)))

                    ))

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
                  (phps-mode-debug-message "Started assignment")
                  (setq in-assignment t)
                  (push round-bracket-level in-assignment-round-bracket-level)
                  (push square-bracket-level in-assignment-square-bracket-level)
                  (setq in-assignment-level (1+ in-assignment-level)))

                ;; Second token after a object-operator
                (when (and
                       in-object-operator
                       in-object-operator-round-bracket-level
                       in-object-operator-square-bracket-level
                       (<= round-bracket-level (car in-object-operator-round-bracket-level))
                       (<= square-bracket-level (car in-object-operator-square-bracket-level))
                       (not (or
                             (equal next-token 'T_OBJECT_OPERATOR)
                             (equal next-token 'T_PAAMAYIM_NEKUDOTAYIM))))
                  (phps-mode-debug-message
                   (message "Ended object-operator at %s %s at level %s" token next-token in-object-operator-level))
                  (pop in-object-operator-round-bracket-level)
                  (pop in-object-operator-square-bracket-level)
                  (setq in-object-operator-level (1- in-object-operator-level))
                  (when (= in-object-operator-level 0)
                    (setq in-object-operator nil)))

                ;; First token after a object-operator
                (when after-object-operator
                  (when (or (equal next-token 'T_STRING)
                            (string= next-token "("))
                    (progn
                      (phps-mode-debug-message
                       (message "Started object-operator at %s %s on level %s"  token next-token in-object-operator-level))
                      (push round-bracket-level in-object-operator-round-bracket-level)
                      (push square-bracket-level in-object-operator-square-bracket-level)
                      (setq in-object-operator t)
                      (setq in-object-operator-level (1+ in-object-operator-level))))
                  (setq after-object-operator nil))

                ;; Starting object-operator?
                (when (and (or (equal token 'T_OBJECT_OPERATOR)
                               (equal token 'T_PAAMAYIM_NEKUDOTAYIM))
                           (equal next-token 'T_STRING))
                  (phps-mode-debug-message
                   (message "After object-operator at %s level %s"  token in-object-operator-level))
                  (setq after-object-operator t))

                ;; Keep track of return expressions
                (when in-return
                  (when (and (string= token ";")
                             (= curly-bracket-level (car in-return-curly-bracket-level)))

                    (phps-mode-debug-message (message "Ended return at %s" token))
                    (pop in-return-curly-bracket-level)
                    (unless in-return-curly-bracket-level
                      (setq in-return nil))
                    (setq in-return-level (1- in-return-level))))
                (when (equal token 'T_RETURN)
                  (phps-mode-debug-message "Started return")
                  (setq in-return t)
                  (push curly-bracket-level in-return-curly-bracket-level)
                  (setq in-return-level (1+ in-return-level)))

                ;; Did we encounter a token that supports extra special alternative control structures?
                (when (equal token 'T_CASE)
                  (setq after-extra-special-control-structure t)
                  (setq nesting-key token)
                  (setq after-extra-special-control-structure-first-on-line first-token-on-line)

                  (when (and switch-case-alternative-stack
                             (= (1- alternative-control-structure-level) (car switch-case-alternative-stack)))

                    (phps-mode-debug-message
                     (message "Found CASE %s vs %s" (1- alternative-control-structure-level) (car switch-case-alternative-stack)))

                    (setq alternative-control-structure-level (1- alternative-control-structure-level))
                    (when first-token-on-line
                      (setq first-token-is-nesting-decrease t))
                    (pop switch-case-alternative-stack))

                  (push alternative-control-structure-level switch-case-alternative-stack)))

              ;; Do we have one token look-ahead?
              (when token

                (phps-mode-debug-message (message "Processing token: %s" token))
                
                ;; Calculate nesting
                (setq
                 nesting-end
                 (+
                  round-bracket-level
                  square-bracket-level
                  curly-bracket-level
                  alternative-control-structure-level
                  in-assignment-level
                  in-class-declaration-level
                  in-concatenation-level
                  in-return-level
                  in-object-operator-level))

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
                      (phps-mode-debug-message
                       (message
                        "\nPopping %s from nesting-stack since %s is lesser or equal to %s, next value is: %s\n"
                        (car nesting-stack)
                        nesting-end
                        (car (car nesting-stack))
                        (nth 1 nesting-stack)))
                      (pop nesting-stack)
                      (setq nesting-decrement (1+ nesting-decrement)))

                    (if first-token-is-nesting-decrease

                        (progn
                          ;; Decrement column
                          (if allow-custom-column-decrement
                              (progn
                                (phps-mode-debug-message
                                 (message
                                  "Doing custom decrement 1 from %s to %s"
                                  column-level
                                  (- column-level
                                     (- nesting-start nesting-end))))
                                (setq column-level (- column-level (- nesting-start nesting-end)))
                                (setq allow-custom-column-decrement nil))
                            (phps-mode-debug-message
                             (message
                              "Doing regular decrement 1 from %s to %s"
                              column-level
                              (1- column-level)))
                            (setq column-level (- column-level nesting-decrement)))

                          ;; Prevent negative column-values
                          (when (< column-level 0)
                            (setq column-level 0)))

                      (unless temp-post-indent
                        (phps-mode-debug-message
                         (message "Temporary setting post indent %s" column-level))
                        (setq temp-post-indent column-level))

                      ;; Decrement column
                      (if allow-custom-column-decrement
                          (progn
                            (phps-mode-debug-message
                             (message
                              "Doing custom decrement 2 from %s to %s"
                              column-level
                              (- column-level
                                 (- nesting-start nesting-end))))
                            (setq
                             temp-post-indent
                             (- temp-post-indent
                                (- nesting-start nesting-end)))
                            (setq allow-custom-column-decrement nil))
                        (setq temp-post-indent (- temp-post-indent nesting-decrement)))

                      ;; Prevent negative column-values
                      (when (< temp-post-indent 0)
                        (setq temp-post-indent 0))

                      )))

                ;; Are we on a new line or is it the last token of the buffer?
                (if (> next-token-start-line-number token-start-line-number)
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
                      (when (and first-token-is-inline-html
                                 (not inline-html-is-whitespace))
                        (phps-mode-debug-message
                         (message "Setting column-level to inline HTML indent: %s" inline-html-indent-start))
                        (setq column-level-start inline-html-indent-start))

                      ;; Save line indent
                      (phps-mode-debug-message
                       (message
                        "Process line ending.	nesting: %s-%s,	line-number: %s-%s,	indent: %s.%s,	token: %s"
                        nesting-start
                        nesting-end
                        token-start-line-number
                        token-end-line-number
                        column-level-start
                        tuning-level
                        token))

                      (when (and (> token-start-line-number 0)
                                 (or
                                  (not first-token-is-inline-html)
                                  inline-html-is-whitespace
                                  inline-html-rest-is-whitespace))
                        (phps-mode-debug-message
                         (message
                          "Putting indent on line %s to %s at #C"
                          token-start-line-number
                          column-level-start))
                        (puthash
                         token-start-line-number
                         `(,column-level-start ,tuning-level)
                         line-indents))

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

                          (phps-mode-debug-message
                           (message
                            "\nPushing (%s %s %s %s) to nesting-stack since %s is greater than %s or stack is empty\n"
                            nesting-start
                            nesting-end
                            nesting-key
                            token
                            nesting-end
                            (car (cdr (car nesting-stack))))
                           )
                          (push `(,nesting-stack-end ,nesting-end ,nesting-key ,token) nesting-stack)))


                      ;; Does token span over several lines and is it not a INLINE_HTML token?
                      (when (and (> token-end-line-number token-start-line-number)
                                 (not (equal token 'T_INLINE_HTML)))
                        (let ((column-level-end column-level))

                          ;; HEREDOC lines should have zero indent
                          (when (or (and in-heredoc
                                         (not in-heredoc-started-this-line))
                                    in-heredoc-ended-this-line)
                            (setq column-level-end 0))

                          ;; Indent doc-comment lines with 1 tuning
                          (when (equal token 'T_DOC_COMMENT)
                            (setq tuning-level 1))

                          (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                            (while (>= token-line-number-diff 0)
                              (phps-mode-debug-message
                               (message
                                "Putting indent on line %s to %s at #A"
                                (- token-end-line-number token-line-number-diff)
                                column-level-end))
                              (puthash
                               (- token-end-line-number token-line-number-diff)
                               `(,column-level-end ,tuning-level) line-indents)
                              ;; (message "Saved line %s indent %s %s" (- token-end-line-number token-line-number-diff) column-level tuning-level)
                              (setq token-line-number-diff (1- token-line-number-diff))))

                          ;; Rest tuning-level used for comments
                          (setq tuning-level 0)))

                      ;; Indent token-less lines here in between last tokens if distance is more than 1 line
                      (when (and (> next-token-start-line-number (1+ token-end-line-number))
                                 (not (equal token 'T_CLOSE_TAG)))

                        (phps-mode-debug-message
                         (message
                          "\nDetected token-less lines between %s and %s, should have indent: %s\n"
                          token-end-line-number
                          next-token-start-line-number
                          column-level))

                        (let ((token-line-number-diff (1- (- next-token-start-line-number token-end-line-number))))
                          (while (> token-line-number-diff 0)
                            (phps-mode-debug-message
                             (message
                              "Putting indent at line %s indent %s at #B"
                              (- next-token-start-line-number token-line-number-diff)
                              column-level))
                            (puthash
                             (- next-token-start-line-number token-line-number-diff)
                             `(,column-level ,tuning-level) line-indents)
                            (setq token-line-number-diff (1- token-line-number-diff)))))


                      ;; Calculate indentation level at start of line
                      (setq
                       nesting-start
                       (+
                        round-bracket-level
                        square-bracket-level
                        curly-bracket-level
                        alternative-control-structure-level
                        in-assignment-level
                        in-class-declaration-level
                        in-concatenation-level
                        in-return-level
                        in-object-operator-level))

                      ;; Set initial values for tracking first token
                      (when (> token-start-line-number last-line-number)
                        (setq inline-html-indent-start inline-html-indent)
                        (setq first-token-on-line t)
                        (setq first-token-is-nesting-decrease nil)
                        (setq first-token-is-inline-html nil)
                        (setq in-class-declaration-level 0)
                        (setq class-declaration-started-this-line nil)
                        (setq in-heredoc-started-this-line nil)
                        (setq special-control-structure-started-this-line nil)

                        ;; When line ends with multi-line inline-html flag first token as inline-html
                        (when (and
                               (equal token 'T_INLINE_HTML)
                               (not inline-html-is-whitespace)
                               (> token-end-line-number token-start-line-number))

                          (setq inline-html-is-whitespace
                                (not (null
                                      (string-match "[\r\n][ \f\t]+$" (substring string (1- token-start) (1- token-end))))))
                          (phps-mode-debug-message
                           (message "Trailing inline html line is whitespace: %s" inline-html-is-whitespace))
                          (phps-mode-debug-message
                           (message
                            "Setting first-token-is-inline-html to true since last token on line is inline-html and spans several lines"))
                          (setq first-token-is-inline-html t))))

                  ;; Current token is not first if it's not <?php or <?=
                  (unless (or (equal token 'T_OPEN_TAG)
                              (equal token 'T_OPEN_TAG_WITH_ECHO))
                    (setq first-token-on-line nil))

                  (when (> token-end-line-number token-start-line-number)
                    ;; (message "Token not first on line %s starts at %s and ends at %s" token token-start-line-number token-end-line-number)
                    (when (equal token 'T_DOC_COMMENT)
                      (setq tuning-level 1))

                    (let ((token-line-number-diff (1- (- token-end-line-number token-start-line-number))))
                      (while (>= token-line-number-diff 0)
                        (phps-mode-debug-message
                         (message
                          "Putting indent on line %s to %s at #E"
                          (-
                           token-end-line-number
                           token-line-number-diff)
                          column-level))
                        (puthash
                         (- token-end-line-number token-line-number-diff)
                         `(,column-level ,tuning-level) line-indents)
                        (setq token-line-number-diff (1- token-line-number-diff))))
                    (setq tuning-level 0))))

              ;; Update current token
              (setq previous-token token)
              (setq token next-token)
              (setq token-start next-token-start)
              (setq token-end next-token-end)
              (setq token-start-line-number next-token-start-line-number)
              (setq token-end-line-number next-token-end-line-number)
              (setq token-number (1+ token-number))))
          (list (nreverse imenu-index) line-indents)))
    (list nil nil)))

(defun phps-mode-functions-indent-line ()
  "Indent line."
  (phps-mode-debug-message (message "Indent line"))
  (phps-mode-functions-process-current-buffer)
  (if phps-mode-functions-processed-buffer
      (if phps-mode-functions-lines-indent
          (let ((line-number (line-number-at-pos (point))))
            (phps-mode-debug-message (message "Found lines indent index, indenting.."))
            (let ((indent (gethash line-number phps-mode-functions-lines-indent)))
              (if indent
                  (progn
                    (let ((indent-sum (+ (* (car indent) tab-width) (car (cdr indent))))
                          (old-indentation (current-indentation))
                          (line-start (line-beginning-position)))

                      (unless old-indentation
                        (setq old-indentation 0))

                      ;; Only continue if current indentation is wrong
                      (if (not (equal indent-sum old-indentation))
                          (progn

                            (setq-local phps-mode-functions-allow-after-change nil)
                            (indent-line-to indent-sum)
                            (setq-local phps-mode-functions-allow-after-change t)

                            (let ((indent-diff (- (current-indentation) old-indentation)))


                              ;; When indent is changed the trailing tokens and states just
                              ;; need to adjust their positions, this will improve speed of indent-region a lot
                              (phps-mode-lexer-move-tokens line-start indent-diff)
                              (phps-mode-lexer-move-states line-start indent-diff)
                              (phps-mode-functions-move-imenu-index line-start indent-diff)

                              (phps-mode-debug-message
                               (message "Lexer tokens after move: %s" phps-mode-lexer-tokens)
                               (message "Lexer states after move: %s" phps-mode-lexer-states))

                              ;; Reset change flag
                              (phps-mode-functions--reset-changes)
                              (phps-mode-functions--cancel-idle-timer)

                              ;; Update last buffer states
                              (setq-local phps-mode-lexer-buffer-length (1- (point-max)))
                              (setq-local
                               phps-mode-lexer-buffer-contents
                               (buffer-substring-no-properties (point-min) (point-max))))))))
                (phps-mode-analyzer--alternative-indentation (point))
                (phps-mode-debug-message
                 (message "Did not find indent for line, using alternative indentation..")))))
        (phps-mode-analyzer--alternative-indentation (point))
        (phps-mode-debug-message
         (message "Did not find lines indent index, using alternative indentation..")))
    (phps-mode-analyzer--alternative-indentation (point))
    (phps-mode-debug-message
     (message "Using alternative indentation since buffer is not processed yet"))))

(defun phps-mode-analyzer--alternative-indentation (&optional point)
  "Apply alternative indentation at POINT here."
  (unless point
    (setq point (point)))
  (let ((new-indentation)
        (point-at-end-of-line (equal point (line-end-position))))
    (save-excursion
      (let ((line-number (line-number-at-pos point))
            (move-length 0)
            (line-is-empty t)
            line-beginning-position
            line-end-position
            line-string
            current-line-string)
        (goto-char point)
        (setq
         current-line-string
         (buffer-substring-no-properties
          (line-beginning-position)
          (line-end-position)
          )
         )
        (when (> line-number 1)
          (while (and
                  (> line-number 0)
                  line-is-empty)
            (forward-line -1)
            (setq line-number (1- line-number))
            (beginning-of-line)
            (setq line-beginning-position (line-beginning-position))
            (setq line-end-position (line-end-position))
            (setq
             line-string
             (buffer-substring-no-properties line-beginning-position line-end-position)
             )
            (setq line-is-empty (string-match-p "^[ \t\f\r\n]*$" line-string))
            (setq move-length (1+ move-length))
            )

          (unless line-is-empty
            (let* ((old-indentation (current-indentation))
                   (current-line-starts-with-closing-bracket (phps-mode-analyzer--string-starts-with-closing-bracket-p current-line-string))
                   (line-starts-with-closing-bracket (phps-mode-analyzer--string-starts-with-closing-bracket-p line-string))
                   (bracket-level (phps-mode-analyzer--get-string-brackets-count line-string)))
              (setq new-indentation old-indentation)

              (forward-line move-length)

              (when (> bracket-level 0)
                (setq new-indentation (+ new-indentation tab-width)))

              (when (and (= bracket-level 0)
                     line-starts-with-closing-bracket)
                (setq new-indentation (+ new-indentation tab-width)))

              (when current-line-starts-with-closing-bracket
                (setq new-indentation (- new-indentation tab-width)))

              ;; Decrease indentation if current line decreases in bracket level
              (when (< new-indentation 0)
                (setq new-indentation 0))

              (indent-line-to new-indentation))))))
    ;; Only move to end of line if point is the current point and is at end of line
    (when (and (equal point (point))
               point-at-end-of-line)
      (end-of-line))
    new-indentation))

(defun phps-mode-analyzer--get-string-brackets-count (string)
  "Get bracket count for STRING."
  (let ((bracket-level 0)
        (start 0)
        (line-is-empty
         (string-match-p "^[ \t\f\r\n]*$" string)))
    (unless line-is-empty
      (while (string-match
              "\\([\]{}()[]\\|<[a-zA-Z]+\\|</[a-zA-Z]+\\|/>\\)"
              string
              start)
        (setq start (match-end 0))
        (let ((bracket (substring string (match-beginning 0) (match-end 0))))
          (cond
           ((or
             (string= bracket "{")
             (string= bracket "[")
             (string= bracket "(")
             (string= bracket "<")
             (string-match "<[a-zA-Z]+" bracket))
            (setq bracket-level (1+ bracket-level)))
           (t
            (setq bracket-level (1- bracket-level)))))))
    (* bracket-level tab-width)))

(defun phps-mode-analyzer--string-starts-with-closing-bracket-p (string)
  "Get bracket count for STRING."
  (string-match-p "^\\([\]{}()[]\\|<[a-zA-Z]+\\|</[a-zA-Z]+\\|/>\\)" string))

(defun phps-mode-functions--cancel-idle-timer ()
  "Cancel idle timer."
  (phps-mode-debug-message (message "Cancelled idle timer"))
  (when phps-mode-functions-idle-timer
    (cancel-timer phps-mode-functions-idle-timer)
    (setq-local phps-mode-functions-idle-timer nil)))

(defun phps-mode-functions--start-idle-timer ()
  "Start idle timer."
  (phps-mode-debug-message (message "Enqueued idle timer"))
  (when (boundp 'phps-mode-idle-interval)
    (let ((buffer (current-buffer)))
      (setq-local
       phps-mode-functions-idle-timer
       (run-with-idle-timer
        phps-mode-idle-interval
        nil
        #'phps-mode-analyzer-process-changes buffer)))))

(defun phps-mode-analyzer--reset-imenu ()
  "Reset imenu index."
  (when (and (boundp 'imenu--index-alist)
             imenu--index-alist)
    (setq-local imenu--index-alist nil)
    (phps-mode-debug-message (message "Cleared Imenu index"))))

(defun phps-mode-functions-after-change (start stop length)
  "Track buffer change from START to STOP with LENGTH."
  (phps-mode-debug-message
   (message "After change %s - %s, length: %s" start stop length))

  (if phps-mode-functions-allow-after-change
      (progn
        (phps-mode-debug-message (message "After change registration is enabled"))
        
        ;; If we haven't scheduled incremental lexer before - do it
        (when (and (boundp 'phps-mode-idle-interval)
                   phps-mode-idle-interval
                   (not phps-mode-functions-idle-timer))

          (phps-mode-analyzer--reset-imenu)
          (phps-mode-functions--start-idle-timer))

        (when (or
               (not phps-mode-analyzer-change-min)
               (< start phps-mode-analyzer-change-min))
          (setq phps-mode-analyzer-change-min start)))
    (phps-mode-debug-message (message "After change registration is disabled"))))

(defun phps-mode-functions-imenu-create-index ()
  "Get Imenu for current buffer."
  (phps-mode-functions-process-current-buffer)
  phps-mode-functions-imenu)

(defun phps-mode-functions-comment-region (beg end &optional _arg)
  "Comment region from BEG to END with optional _ARG."
  ;; Iterate tokens from beginning to end and comment out all PHP code
  (when-let ((tokens phps-mode-lexer-tokens))
    (let ((token-comment-start nil)
          (token-comment-end nil)
          (in-token-comment nil)
          (offset 0))
      (dolist (token tokens)
        (let ((token-label (car token))
              (token-start (car (cdr token)))
              (token-end (cdr (cdr token))))
          (when (and (>= token-start beg)
                     (<= token-end end))

            (if in-token-comment
                (cond
                 ((or
                   (equal token-label 'T_COMMENT)
                   (equal token-label 'T_DOC_COMMENT)
                   (equal token-label 'T_CLOSE_TAG))
                  (phps-mode-debug-message
                   (message
                    "Comment should end at previous token %s %s"
                    token-label
                    token-comment-end))
                  (setq in-token-comment nil))
                 (t (setq token-comment-end token-end)))

              ;; When we have a start and end of comment, comment it out
              (when (and
                     token-comment-start
                     token-comment-end)
                (let ((offset-comment-start (+ token-comment-start offset))
                      (offset-comment-end))
                  (save-excursion
                    (goto-char offset-comment-start)
                    (insert "/* "))
                  (setq offset (+ offset 3))
                  (setq offset-comment-end (+ token-comment-end offset))
                  (save-excursion
                    (goto-char offset-comment-end)
                    (insert " */"))
                  (setq offset (+ offset 3))
                  (phps-mode-debug-message
                   (message "Commented out %s-%s" offset-comment-start offset-comment-end)))
                (setq token-comment-start nil)
                (setq token-comment-end nil))

              (cond
               ((or
                 (equal token-label 'T_INLINE_HTML)
                 (equal token-label 'T_COMMENT)
                 (equal token-label 'T_DOC_COMMENT)
                 (equal token-label 'T_OPEN_TAG)
                 (equal token-label 'T_OPEN_TAG_WITH_ECHO)))
               (t
                (phps-mode-debug-message
                 (message
                  "Comment should start at %s %s-%s"
                  token-label
                  token-start
                  token-end))
                (setq token-comment-start token-start)
                (setq token-comment-end token-end)
                (setq in-token-comment t)))))))

      ;; When we have a start and end of comment, comment it out
      (when (and
             in-token-comment
             token-comment-start
             token-comment-end)
        (let ((offset-comment-start (+ token-comment-start offset))
              (offset-comment-end))
          (save-excursion
            (goto-char offset-comment-start)
            (insert "/* "))
          (setq offset (+ offset 3))
          (setq offset-comment-end (+ token-comment-end offset))
          (save-excursion
            (goto-char offset-comment-end)
            (insert " */"))
          (setq offset (+ offset 3))
          (phps-mode-debug-message
           (message "Commented out trailing %s-%s" offset-comment-start offset-comment-end)))
        (setq token-comment-start nil)
        (setq token-comment-end nil)))))

(defun phps-mode-functions-uncomment-region (beg end &optional _arg)
  "Un-comment region from BEG to END with optional ARG."
  ;; Iterate tokens from beginning to end and uncomment out all commented PHP code
  (when-let ((tokens phps-mode-lexer-tokens))
    (let ((offset 0))
      (dolist (token tokens)
        (let ((token-label (car token))
              (token-start (car (cdr token)))
              (token-end (cdr (cdr token))))
          (when (and (>= token-start beg)
                     (<= token-end end))
            (when (or
                   (equal token-label 'T_COMMENT)
                   (equal token-label 'T_DOC_COMMENT))

              (phps-mode-debug-message
               (message "Un-comment %s comment at %s %s" token-label token-start token-end))

              (let ((offset-comment-start (+ token-start offset))
                    (offset-comment-end))

                (if (equal token-label 'T_DOC_COMMENT)
                    (progn
                      (phps-mode-debug-message
                       (message "Un-comment doc comment at %s-%s" token-start token-end))
                      (save-excursion
                        (goto-char offset-comment-start)
                        (delete-char 4))
                      (setq offset (- offset 4))
                      (setq offset-comment-end (+ token-end offset))
                      (save-excursion
                        (goto-char offset-comment-end)
                        (delete-char -3))
                      (setq offset (- offset 3)))

                  (phps-mode-debug-message
                   (message "Un-comment comment starting at %s" token-start))

                  (cond

                   ((string=
                     (buffer-substring-no-properties offset-comment-start (+ offset-comment-start 1))
                     "#")
                    (save-excursion
                      (goto-char offset-comment-start)
                      (delete-char 1))
                    (setq offset (- offset 1)))
                   ((string=
                     (buffer-substring-no-properties offset-comment-start (+ offset-comment-start 2))
                     "//")
                    (save-excursion
                      (goto-char offset-comment-start)
                      (delete-char 2))
                    (setq offset (- offset 2)))
                   (t
                    (save-excursion
                      (goto-char offset-comment-start)
                      (delete-char 3))
                    (setq offset (- offset 3))))

                  
                  (setq offset-comment-end (+ token-end offset))
                  (if (string=
                       (buffer-substring-no-properties (- offset-comment-end 3) offset-comment-end)
                       " */")
                      (progn
                        (phps-mode-debug-message
                         (message "Un-comment comment ending at %s" token-end))
                        (save-excursion
                          (goto-char offset-comment-end)
                          (delete-char -3))
                        (setq offset (- offset 3)))
                    (phps-mode-debug-message
                     (message
                      "Do not un-comment comment ending at %s"
                      token-end))))))))))))

(provide 'phps-mode-analyzer)

;;; phps-mode-analyzer.el ends here
