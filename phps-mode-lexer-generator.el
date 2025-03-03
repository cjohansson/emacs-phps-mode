;;; phps-mode-lexer-generator.el -- Generate lexer rules for lexer -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:


;;; Code:


(require 'phps-mode-macros)



(defvar-local phps-mode-lexer--generated-tokens nil
  "List of current generated tokens.")

(defvar-local phps-mode-lexer--generated-new-tokens nil
  "List of current newly generated tokens.")

(defvar-local phps-mode-lexer--generated-new-tokens-index nil
  "Index started at when generated new tokens.")

(defvar-local phps-mode-lexer--state nil
  "Current state of lexer.")

(defvar-local phps-mode-lexer--state-stack nil
  "Current state-stack of lexer.")

(defvar-local phps-mode-lexer--heredoc-label nil
  "Current heredoc label.")

(defvar-local phps-mode-lexer--heredoc-label-stack nil
  "Stack of heredoc labels.")

(defvar-local phps-mode-lexer--match-length nil
  "Maximum match length.")

(defvar-local phps-mode-lexer--match-body nil
  "Lambda om maximum match.")

(defvar-local phps-mode-lexer--match-data nil
  "Match data.")

(defvar-local phps-mode-lexer--nest-location-stack nil
  "Nesting stack.")

(defvar-local phps-mode-lexer--parser-mode nil
  "Non-nil means we are in parser-mode.")

(defvar-local phps-mode-lexer--restart-flag nil
  "Non-nil means restart.")

(defvar-local phps-mode-lexer--move-flag nil
  "Non nil means move.")

;; @see https://secure.php.net/manual/en/language.types.integer.php
(defconst phps-mode-lexer--long-limit
  2147483648
  "Limit for 32-bit integer.")

(defconst phps-mode-lexer--lnum
  "[0-9]+\\(_[0-9]+\\)*"
  "Long number.")

(defconst phps-mode-lexer--dnum
  (format
   "\\(\\(%s\\)?\\.%s\\|%s\\.\\(%s\\)?\\)"
   phps-mode-lexer--lnum
   phps-mode-lexer--lnum
   phps-mode-lexer--lnum
   phps-mode-lexer--lnum)
  "Double number.")

(defconst phps-mode-lexer--exponent-dnum
  (format "\\(\\(%s\\|%s\\)[eE][\\+-]?%s\\)"
          phps-mode-lexer--lnum
          phps-mode-lexer--dnum
          phps-mode-lexer--lnum)
  "Exponent double number.")

(defconst phps-mode-lexer--hnum
  "0x[0-9a-fA-F]+\\(_[0-9a-fA-F]+\\)*"
  "Hexadecimal number.")

(defconst phps-mode-lexer--bnum
  "0b[01]+\\(_[01]+\\)*"
  "Boolean number.")

(defconst phps-mode-lexer--onum
  "0o[0-7]+\\(_[0-7]+\\)*"
  "Octal number.")

(defconst phps-mode-lexer--label
  "[A-Za-z_[:nonascii:]][0-9A-Za-z_[:nonascii:]]*"
  "Labels are used for names.")
;; NOTE original is [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]*
;; NOTE Rebuilt for comparability with emacs-lisp

(defconst phps-mode-lexer--whitespace
  "[ \n\r\t]+"
  "White-space.")

(defconst phps-mode-lexer--tabs-and-spaces
  "[ \t]*"
  "Tabs and white-spaces.")

(defconst phps-mode-lexer--tokens
  "[][;:,.()|^&+/*=%!~$<>?@-]"
  "Tokens.")
;; NOTE Original is [;:,.\[\]()|^&+-/*=%!~$<>?@]
;; NOTE The hyphen moved last since it has special meaning and to avoid it being interpreted as a range.

(defconst phps-mode-lexer--any-char
  "[^z-a]"
  "Any character.  The Zend equivalent is [^] but is not possible in Emacs Lisp.")

(defconst phps-mode-lexer--newline
  "\\(\r\\|\n\\|\r\n\\)"
  "Newline characters. ")

(defvar phps-mode-lexer-generator--table nil)

(defun phps-mode-lexer-generator--add-rule (table states conditions body)
  "Place in STATES a check for CONDITIONS to execute BODY."
  (unless (listp states)
    (setq states (list states)))
  (dolist (state states)
    (let ((old-lambdas
           (gethash
            state
            table)))
      (when old-lambdas
        (setq
         old-lambdas
         (reverse old-lambdas)))
      (push (list conditions body) old-lambdas)
      (setq old-lambdas (reverse old-lambdas))
      (puthash
       state
       old-lambdas
       table))))

(defun phps-mode-lexer-generator--lambdas ()

  ;; NOTE We use a global variable here to prevent it from being
  ;; included in the lambdas below
  (setq
   phps-mode-lexer-generator--table
   (make-hash-table :test 'equal))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "exit"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_EXIT)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "die"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_EXIT)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "fn"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FN)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "function"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FUNCTION)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "const"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CONST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "return"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_RETURN)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "#\\["))
   (lambda()
     (phps-mode-lexer--enter-nesting "[")
     (phps-mode-lexer--return-token 'T_ATTRIBUTE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "yield"
       phps-mode-lexer--whitespace
       "from"
       "[^a-zA-Z0-9_\x80-\xff]")))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_YIELD_FROM)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "yield"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_YIELD)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "try"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_TRY)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "catch"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CATCH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "finally"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FINALLY)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "throw"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_THROW)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "if"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_IF)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "elseif"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ELSEIF)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "endif"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDIF)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "else"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ELSE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "while"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_WHILE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "endwhile"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDWHILE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "do"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_DO)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "for"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "endfor"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDFOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "foreach"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FOREACH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "endforeach"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDFOREACH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "declare"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_DECLARE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "enddeclare"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDDECLARE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "instanceof"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_INSTANCEOF)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "as"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_AS)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "switch"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_SWITCH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "match"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_MATCH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "endswitch"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ENDSWITCH)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "case"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CASE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "default"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_DEFAULT)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "break"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_BREAK)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "continue"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CONTINUE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "goto"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_GOTO)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "echo"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ECHO)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "print"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_PRINT)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "class"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CLASS)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "interface"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_INTERFACE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "trait"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_TRAIT)))

  ;; The enum keyword must be followed by whitespace and another identifier.
  ;; This avoids the BC break of using enum in classes, namespaces, functions and constants.
  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at (concat "\\(enum\\)" phps-mode-lexer--whitespace "\\(extends\\|implements\\)")))
   (lambda()
     (phps-mode-lexer--yyless 4)
     (phps-mode-lexer--return-token-with-str 'T_STRING)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at (concat "\\(enum\\)" phps-mode-lexer--whitespace "[a-zA-Z_\x80-\xff]")))
   (lambda()
     (phps-mode-lexer--yyless 4)
     (phps-mode-lexer--return-token-with-indent 'T_ENUM (match-beginning 1) (match-end 1))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "extends"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_EXTENDS)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "implements"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_IMPLEMENTS)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "->"))
   (lambda()
     (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
     (phps-mode-lexer--return-token-with-indent 'T_OBJECT_OPERATOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "?->"))
   (lambda()
     (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
     (phps-mode-lexer--return-token-with-indent 'T_NULLSAFE_OBJECT_OPERATOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_IN_SCRIPTING ST_LOOKING_FOR_PROPERTY)
   (lambda() (looking-at phps-mode-lexer--whitespace))
   (lambda() (phps-mode-lexer--return-whitespace)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_PROPERTY
   (lambda() (looking-at "->"))
   (lambda() (phps-mode-lexer--return-token 'T_OBJECT_OPERATOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_PROPERTY
   (lambda() (looking-at "?->"))
   (lambda() (phps-mode-lexer--return-token 'T_NULLSAFE_OBJECT_OPERATOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_PROPERTY
   (lambda() (looking-at phps-mode-lexer--label))
   (lambda() (phps-mode-lexer--yy-pop-state)
     (phps-mode-lexer--return-token-with-str 'T_STRING 0)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_PROPERTY
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda() (phps-mode-lexer--yyless 0)
     (phps-mode-lexer--yy-pop-state)
     (phps-mode-lexer--restart)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "::"))
   (lambda() (phps-mode-lexer--return-token 'T_PAAMAYIM_NEKUDOTAYIM)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\.\\.\\."))
   (lambda() (phps-mode-lexer--return-token 'T_ELLIPSIS)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\?\\?"))
   (lambda() (phps-mode-lexer--return-token 'T_COALESCE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "new"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_NEW)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "clone"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CLONE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "var"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_VAR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "\\(int\\|integer\\)"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_INT_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "\\(double\\|float\\)"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_DOUBLE_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "\\(real\\)"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda()
     (when (phps-mode-lexer--parser-mode)
       (signal
        'phps-lexer-error
        (list
         (format
          "The (real) cast has been removed, use (float) instead at %d"
          (match-beginning 0))
         (match-beginning 0)
         (match-end 0))))
     (phps-mode-lexer--return-token 'T_DOUBLE_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "\\(string\\|binary\\)"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_STRING_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "array"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_ARRAY_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "object"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_OBJECT_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "\\(bool\\|boolean\\)"
       phps-mode-lexer--tabs-and-spaces
       ")")))
   (lambda() (phps-mode-lexer--return-token 'T_BOOL_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "("
       phps-mode-lexer--tabs-and-spaces
       "unset"
       phps-mode-lexer--tabs-and-spaces ")")))
   (lambda() (phps-mode-lexer--return-token 'T_UNSET_CAST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "eval"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_EVAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "include"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_INCLUDE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "include_once"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_INCLUDE_ONCE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "require"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_REQUIRE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "require_once"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_REQUIRE_ONCE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "namespace"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_NAMESPACE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "use"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_USE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "insteadof"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_INSTEADOF)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "global"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_GLOBAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "isset"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ISSET)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "empty"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_EMPTY)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__halt_compiler"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_HALT_COMPILER)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "static"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_STATIC)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "abstract"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ABSTRACT)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "final"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FINAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "private"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_PRIVATE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "protected"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_PROTECTED)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "public"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_PUBLIC)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "readonly"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_READONLY)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "unset"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_UNSET)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "=>"))
   (lambda() (phps-mode-lexer--return-token 'T_DOUBLE_ARROW)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "list"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_LIST)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "array"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_ARRAY)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "callable"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CALLABLE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\+\\+"))
   (lambda() (phps-mode-lexer--return-token 'T_INC)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "--"))
   (lambda() (phps-mode-lexer--return-token 'T_DEC)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "==="))
   (lambda() (phps-mode-lexer--return-token 'T_IS_IDENTICAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "!=="))
   (lambda() (phps-mode-lexer--return-token 'T_IS_NOT_IDENTICAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "=="))
   (lambda() (phps-mode-lexer--return-token 'T_IS_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\(!=\\|<>\\)"))
   (lambda() (phps-mode-lexer--return-token 'T_IS_NOT_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "<=>"))
   (lambda() (phps-mode-lexer--return-token 'T_SPACESHIP)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "<="))
   (lambda() (phps-mode-lexer--return-token 'T_IS_SMALLER_OR_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at ">="))
   (lambda() (phps-mode-lexer--return-token 'T_IS_GREATER_OR_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\+="))
   (lambda() (phps-mode-lexer--return-token 'T_PLUS_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "-="))
   (lambda() (phps-mode-lexer--return-token 'T_MINUS_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\*="))
   (lambda() (phps-mode-lexer--return-token 'T_MUL_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\*\\*"))
   (lambda() (phps-mode-lexer--return-token 'T_POW)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\*\\*="))
   (lambda() (phps-mode-lexer--return-token 'T_POW_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "/="))
   (lambda() (phps-mode-lexer--return-token 'T_DIV_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\.="))
   (lambda() (phps-mode-lexer--return-token 'T_CONCAT_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "%="))
   (lambda() (phps-mode-lexer--return-token 'T_MOD_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "<<="))
   (lambda() (phps-mode-lexer--return-token 'T_SL_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at ">>="))
   (lambda() (phps-mode-lexer--return-token 'T_SR_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "&="))
   (lambda() (phps-mode-lexer--return-token 'T_AND_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "|="))
   (lambda() (phps-mode-lexer--return-token 'T_OR_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\^="))
   (lambda() (phps-mode-lexer--return-token 'T_XOR_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\?\\?="))
   (lambda() (phps-mode-lexer--return-token 'T_COALESCE_EQUAL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "||"))
   (lambda() (phps-mode-lexer--return-token 'T_BOOLEAN_OR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "&&"))
   (lambda() (phps-mode-lexer--return-token 'T_BOOLEAN_AND)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "OR"))
   (lambda() (phps-mode-lexer--return-token 'T_LOGICAL_OR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "AND"))
   (lambda() (phps-mode-lexer--return-token 'T_LOGICAL_AND)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "XOR"))
   (lambda() (phps-mode-lexer--return-token 'T_LOGICAL_XOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "<<"))
   (lambda() (phps-mode-lexer--return-token 'T_SL)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at ">>"))
   (lambda() (phps-mode-lexer--return-token 'T_SR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at (concat "&" "[ \t\r\n]*" "\\(\\$\\|\\.\\.\\.\\)")))
   (lambda()
     (phps-mode-lexer--yyless 1)
     (phps-mode-lexer--return-token
      'T_AMPERSAND_FOLLOWED_BY_VAR_OR_VARARG
      (match-beginning 0)
      (- (match-end 0) 1))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "&"))
   (lambda() (phps-mode-lexer--return-token 'T_AMPERSAND_NOT_FOLLOWED_BY_VAR_OR_VARARG)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat "\\(" "]" "\\|" ")" "\\)")))
   (lambda() (phps-mode-lexer--return-exit-nesting-token)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat "\\(" "\\[" "\\|" "(" "\\)")))
   (lambda() (phps-mode-lexer--enter-nesting)
     (phps-mode-lexer--return-token)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at phps-mode-lexer--tokens))
   (lambda() (phps-mode-lexer--return-token)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "{"))
   (lambda()
     (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
     (phps-mode-lexer--enter-nesting "{")
     (phps-mode-lexer--return-token)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC)
   (lambda() (looking-at "\\${"))
   (lambda()
     (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_VARNAME)
     (phps-mode-lexer--enter-nesting "{")
     (phps-mode-lexer--return-token 'T_DOLLAR_OPEN_CURLY_BRACES)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "}"))
   (lambda()
     (phps-mode-lexer--reset-doc-comment)
     (when phps-mode-lexer--state-stack
       (phps-mode-lexer--yy-pop-state))
     (phps-mode-lexer--return-exit-nesting-token)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_VARNAME
   (lambda() (looking-at (concat phps-mode-lexer--label "[\\[}]")))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (1- (match-end 0)))
            (_data (buffer-substring-no-properties start end)))
       (phps-mode-lexer--yyless 1)
       (phps-mode-lexer--yy-pop-state)
       (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
       (phps-mode-lexer--return-token 'T_STRING_VARNAME start end))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_LOOKING_FOR_VARNAME
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (phps-mode-lexer--yyless 0)
     (phps-mode-lexer--yy-pop-state)
     (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
     (phps-mode-lexer--restart)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at phps-mode-lexer--bnum))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data
             (replace-regexp-in-string
              "_"
              ""
              (buffer-substring-no-properties (+ start 2) end)))
            (long-number (string-to-number data 2)))
       ;; (message "Binary number %s from %s" long-number data)
       (if (> long-number phps-mode-lexer--long-limit)
           (phps-mode-lexer--return-token 'T_DNUMBER)
         (phps-mode-lexer--return-token 'T_LNUMBER)))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at phps-mode-lexer--onum))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data (string-to-number
                   (replace-regexp-in-string
                    "_"
                    ""
                    (buffer-substring-no-properties start end))
                   8)))
       (if (> data phps-mode-lexer--long-limit)
           (phps-mode-lexer--return-token 'T_DNUMBER)
         (phps-mode-lexer--return-token 'T_LNUMBER)))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at phps-mode-lexer--lnum))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data (string-to-number
                   (replace-regexp-in-string
                    "_"
                    ""
                    (buffer-substring-no-properties start end)))))
       ;; (message "Long number: %d" data)
       (if (> data phps-mode-lexer--long-limit)
           (phps-mode-lexer--return-token 'T_DNUMBER)
         (phps-mode-lexer--return-token 'T_LNUMBER)))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at phps-mode-lexer--hnum))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data
             (replace-regexp-in-string
              "_"
              ""
              (buffer-substring-no-properties (+ start 2) end)))
            (long-number (string-to-number data 16)))
       ;; (message "Hexadecimal number %s from %s" long-number data)
       (if (> long-number phps-mode-lexer--long-limit)
           (phps-mode-lexer--return-token 'T_DNUMBER)
         (phps-mode-lexer--return-token 'T_LNUMBER)))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_VAR_OFFSET
   (lambda() (looking-at "\\([0]\\|[1-9][0-9]*\\)"))
   (lambda() (phps-mode-lexer--return-token 'T_NUM_STRING)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_VAR_OFFSET
   (lambda()
     (looking-at
      (concat "\\("
              phps-mode-lexer--lnum "\\|"
              phps-mode-lexer--hnum "\\|"
              phps-mode-lexer--bnum "\\|"
              phps-mode-lexer--onum "\\)")))
   (lambda() (phps-mode-lexer--return-token 'T_NUM_STRING)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (or (looking-at phps-mode-lexer--dnum)
         (looking-at phps-mode-lexer--exponent-dnum)))
   (lambda() (phps-mode-lexer--return-token 'T_DNUMBER)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__CLASS__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_CLASS_C)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__TRAIT__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_TRAIT_C)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__FUNCTION__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FUNC_C)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__METHOD__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_METHOD_C)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__LINE__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_LINE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__FILE__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_FILE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__DIR__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_DIR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "__NAMESPACE__"))
   (lambda() (phps-mode-lexer--return-token-with-indent 'T_NS_C)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'SHEBANG
   (lambda() (looking-at (concat "#!.*" phps-mode-lexer--newline)))
   (lambda()
     (let ((lineno
            (1+
             (phps-mode-lexer--CG 'zend_lineno))))
       (phps-mode-lexer--CG 'zend-lineno lineno))
     (phps-mode-lexer--begin 'ST_INITIAL)
     (phps-mode-lexer--restart)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'SHEBANG
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (phps-mode-lexer--yyless 0)
     (phps-mode-lexer--begin 'ST_INITIAL)
     (phps-mode-lexer--restart)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_INITIAL
   (lambda() (looking-at "<\\?="))
   (lambda()
     (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
     (when (phps-mode-lexer--parser-mode)
       (phps-mode-lexer--return-token-with-indent 'T_ECHO))
     (phps-mode-lexer--return-token 'T_OPEN_TAG_WITH_ECHO)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_INITIAL
   (lambda()
     (looking-at
      (concat
       "<\\?php\\([ \t]\\|"
       phps-mode-lexer--newline
       "\\)")))
   (lambda()
     (phps-mode-lexer--handle-newline)
     (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
     (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_INITIAL
   (lambda() (looking-at "<\\?php"))
   (lambda()
     (let ((start (match-beginning 0))
           (end (match-end 0)))

       (cond

        ;; Allow <?php followed by end of file.
        ((equal end (point-max))
         (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
         (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG))

        ;; Degenerate case: <?phpX is interpreted as <? phpX with short tags
        ((phps-mode-lexer--CG 'short-tags)
         (phps-mode-lexer--yyless 2)
         (setq end (- end 2))
         (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
         (phps-mode-lexer--return-or-skip-token
          'T_OPEN_TAG
          start
          end))

        (t
         (phps-mode-lexer--inline-char-handler))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_INITIAL
   (lambda() (looking-at "<\\?"))
   (lambda()
     (if (phps-mode-lexer--CG 'short-tags)
         (progn
           (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
           (phps-mode-lexer--return-or-skip-token 'T_OPEN_TAG))
       (phps-mode-lexer--inline-char-handler))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_INITIAL
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (if (= (point) (point-max))
         (phps-mode-lexer--return-end-token)
       (phps-mode-lexer--inline-char-handler))))

  ;; Make sure a label character follows "->" or "?->", otherwise there is no property
  ;; and "->"/"?->" will be taken literally
  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
   (lambda()
     (looking-at
      (concat
       "\\$"
       phps-mode-lexer--label
       "->"
       "[a-zA-Z_\x80-\xff]")))
   (lambda()
     (phps-mode-lexer--yyless 3)
     (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
     (phps-mode-lexer--return-token-with-str
      'T_VARIABLE
      1
      (match-beginning 0)
      (- (match-end 0) 3))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
   (lambda() (looking-at
              (concat
               "\\$"
               phps-mode-lexer--label
               "\\?->"
               "[a-zA-Z_\x80-\xff]")))
   (lambda() (phps-mode-lexer--yyless 4)
     (phps-mode-lexer--yy-push-state 'ST_LOOKING_FOR_PROPERTY)
     (phps-mode-lexer--return-token-with-str
      'T_VARIABLE
      1
      (match-beginning 0)
      (- (match-end 0) 4))))

  ;; A [ always designates a variable offset, regardless of what follows
  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_DOUBLE_QUOTES ST_HEREDOC ST_BACKQUOTE)
   (lambda()
     (looking-at
      (concat
       "\\$"
       phps-mode-lexer--label
       "\\[")))
   (lambda()
     (phps-mode-lexer--yyless 1)
     (phps-mode-lexer--yy-push-state 'ST_VAR_OFFSET)
     (phps-mode-lexer--return-token-with-str
      'T_VARIABLE
      1
      (match-beginning 0)
      (- (match-end 0) 1))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_IN_SCRIPTING
     ST_DOUBLE_QUOTES
     ST_HEREDOC
     ST_BACKQUOTE
     ST_VAR_OFFSET)
   (lambda()
     (looking-at
      (concat
       "\\$"
       phps-mode-lexer--label)))
   (lambda()
     (phps-mode-lexer--return-token-with-str 'T_VARIABLE 1)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_VAR_OFFSET
   (lambda() (looking-at "\\]"))
   (lambda()
     (phps-mode-lexer--yy-pop-state)
     (phps-mode-lexer--return-token-with-str "]" 1)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_VAR_OFFSET
   (lambda()
     (looking-at
      (concat "\\(" phps-mode-lexer--tokens
              "\\|[{}\"`]\\)")))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data (buffer-substring-no-properties start end)))
       ;; Only '[' or '-' can be valid, but returning other tokens will allow a more explicit parse error
       (phps-mode-lexer--return-token data))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_VAR_OFFSET
   (lambda() (looking-at (concat "[ \n\r\t'#]")))
   (lambda()
     ;; Invalid rule to return a more explicit parse error with proper line number
     (phps-mode-lexer--yyless 0)
     (phps-mode-lexer--yy-pop-state)
     (phps-mode-lexer--return-token-with-val 'T_ENCAPSED_AND_WHITESPACE)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "namespace"
       "\\("
       "\\\\"
       phps-mode-lexer--label
       "\\)+")))
   (lambda()
     (phps-mode-lexer--return-token-with-str
      'T_NAME_RELATIVE
      (1- (length "namespace\\")))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at (concat
                  phps-mode-lexer--label
                  "\\("
                  "\\\\"
                  phps-mode-lexer--label
                  "\\)+")))
   (lambda()
     (phps-mode-lexer--return-token-with-str
      'T_NAME_QUALIFIED
      0)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at (concat
                  "\\\\"
                  phps-mode-lexer--label
                  "\\("
                  "\\\\"
                  phps-mode-lexer--label
                  "\\)*")))
   (lambda()
     (phps-mode-lexer--return-token-with-str
      'T_NAME_FULLY_QUALIFIED
      1)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\\\"))
   (lambda() (phps-mode-lexer--return-token 'T_NS_SEPARATOR)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_IN_SCRIPTING ST_VAR_OFFSET)
   (lambda() (looking-at phps-mode-lexer--label))
   (lambda() (phps-mode-lexer--return-token-with-str 'T_STRING 0)))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\\(#\\|//\\)"))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (_data (buffer-substring-no-properties start end))
            (line (buffer-substring-no-properties end (line-end-position))))
       (if (string-match "\\?>" line)
           (phps-mode-lexer--return-or-skip-token
            'T_COMMENT
            start
            (+ end (match-beginning 0)))
         (phps-mode-lexer--return-or-skip-token
          'T_COMMENT
          start
          (line-end-position))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "\\(/\\*\\|/\\*\\*"
       phps-mode-lexer--whitespace
       "\\)")))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (_data (buffer-substring-no-properties start end))
            (doc-com (looking-at-p (concat "/\\*\\*" phps-mode-lexer--whitespace))))
       (let ((string-start (search-forward "*/" nil t)))
         (if string-start
             (if doc-com
                 (progn
                   (phps-mode-lexer--reset-doc-comment)
                   (phps-mode-lexer--return-token
                    'T_DOC_COMMENT
                    start)
                   (phps-mode-lexer--CG 'doc_comment t))
               (phps-mode-lexer--return-token
                'T_COMMENT start))
           (progn
             (signal
              'phps-lexer-error
              (list
               (format
                "Unterminated comment starting at %d"
                start)
               start))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "\\?>"
       phps-mode-lexer--newline
       "?")))
   (lambda()
     (let ((start (match-beginning 0))
           (end (match-end 0)))
       (when (= (- end start) 3)
         (setq end (1- end)))
       (phps-mode-lexer--begin 'ST_INITIAL)
       (when (phps-mode-lexer--parser-mode)
         (phps-mode-lexer--return-token
          ";"
          start
          end))
       (phps-mode-lexer--return-token
        'T_CLOSE_TAG
        start
        end))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "'"))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (_data (buffer-substring-no-properties start end))
            (un-escaped-end (phps-mode-lexer--get-next-unescaped "'")))
       (if un-escaped-end
           (phps-mode-lexer--return-token
            'T_CONSTANT_ENCAPSED_STRING
            start
            un-escaped-end)
         ;; Unclosed single quotes
         (phps-mode-lexer--return-token-with-val
          'T_ENCAPSED_AND_WHITESPACE
          start
          (point-max))
         (phps-mode-lexer--move-forward
          (point-max))))))

  ;; Double quoted string
  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "\""))
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
                  "\\|\\$" phps-mode-lexer--label
                  "\\|\\${" phps-mode-lexer--label
                  "\\|{\\$" phps-mode-lexer--label "\\)")
                 nil t)))

           ;; Do we find a ending double quote or starting variable?
           (if string-start
               (let ((string-start (match-beginning 0))
                     (last-character-is-escape-character t)
                     (is-escaped nil))

                 ;; Do we find variable inside quote?
                 (goto-char string-start)

                 ;; Backtrack until we find a character that is not a escape character
                 (while last-character-is-escape-character
                   (forward-char -1)
                   (if (looking-at-p "\\\\")
                       (setq is-escaped (not is-escaped))
                     (setq last-character-is-escape-character nil)))

                 ;; Do we find variable inside quote?
                 (goto-char string-start)

                 ;; Process character but only if it's not escaped
                 (if is-escaped
                     (forward-char 1)
                   (setq open-quote nil)
                   (if (looking-at-p "\"")
                       (let ((_double-quoted-string
                              (buffer-substring-no-properties start (+ string-start 1))))
                         ;; (message "Double quoted string: %s" _double-quoted-string)
                         (phps-mode-lexer--return-token-with-val
                          'T_CONSTANT_ENCAPSED_STRING
                          start
                          (+ string-start 1)))
                     ;; (message "Found variable after '%s' at %s-%s" (buffer-substring-no-properties start string-start) start string-start)
                     (phps-mode-lexer--begin 'ST_DOUBLE_QUOTES)
                     (phps-mode-lexer--return-token "\"" start (1+ start)))))
             (progn
               (setq open-quote nil)
               (signal
                'phps-lexer-error
                (list
                 (format "Found no ending of quote at %s" start)
                 start)))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda()
     (looking-at
      (concat
       "<<<"
       phps-mode-lexer--tabs-and-spaces
       "\\("
       phps-mode-lexer--label
       "\\|'"
       phps-mode-lexer--label
       "'\\|\""
       phps-mode-lexer--label
       "\"\\)"
       phps-mode-lexer--newline)))
   (lambda() 
     (let* ((start (match-beginning 0))
            (end (match-end 0))
            (data
             (buffer-substring-no-properties
              (match-beginning 1)
              (match-end 1))))

       ;; Determine if it's HEREDOC or NOWDOC and extract label here
       (if (string= (substring data 0 1) "'")
           (progn
             (setq
              phps-mode-lexer--heredoc-label
              (substring data 1 (- (length data) 1)))
             (phps-mode-lexer--begin 'ST_NOWDOC))
         (progn
           (if (string= (substring data 0 1) "\"")
               (setq
                phps-mode-lexer--heredoc-label
                (substring data 1 (- (length data) 1)))
             (setq
              phps-mode-lexer--heredoc-label
              data))
           (phps-mode-lexer--begin 'ST_HEREDOC)))

       ;; Check for ending label on the next line
       (when (string=
              (buffer-substring-no-properties
               end
               (+ end
                  (length
                   phps-mode-lexer--heredoc-label)))
              phps-mode-lexer--heredoc-label)
         (phps-mode-lexer--begin 'ST_END_HEREDOC))

       (push
        `(,phps-mode-lexer--heredoc-label ,start ,end)
        phps-mode-lexer--heredoc-label-stack)
       ;; (message "Found heredoc or nowdoc at %s with label %s" data phps-mode-lexer--heredoc-label)

       (phps-mode-lexer--CG
        'doc_comment
        t)
       (phps-mode-lexer--return-token
        'T_START_HEREDOC
        start
        end))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_IN_SCRIPTING
   (lambda() (looking-at "[`]"))
   (lambda()
     ;; (message "Begun backquote at %s-%s" (match-beginning 0) (match-end 0))
     (phps-mode-lexer--begin 'ST_BACKQUOTE)
     (phps-mode-lexer--return-token "`")))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_END_HEREDOC
   (lambda()
     (looking-at
      (concat phps-mode-lexer--any-char)))
   (lambda()
     (let* ((start (match-beginning 0))
            (end (+ start
                    (length
                     phps-mode-lexer--heredoc-label)))
            (_data (buffer-substring-no-properties start end)))
       ;; (message "Found ending heredoc at %s, %s of %s" _data (thing-at-point 'line) phps-mode-lexer--heredoc-label)
       (pop phps-mode-lexer--heredoc-label-stack)
       (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
       (phps-mode-lexer--return-token 'T_END_HEREDOC start end))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_DOUBLE_QUOTES ST_BACKQUOTE ST_HEREDOC)
   (lambda() (looking-at "{\\$"))
   (lambda()
     (phps-mode-lexer--yy-push-state 'ST_IN_SCRIPTING)
     (phps-mode-lexer--yyless 1)
     (phps-mode-lexer--enter-nesting "{")
     (phps-mode-lexer--return-token
      'T_CURLY_OPEN
      (match-beginning 0)
      (- (match-end 0) 1))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_DOUBLE_QUOTES
   (lambda() (looking-at "[\"]"))
   (lambda()
     (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
     (phps-mode-lexer--return-token "\"")))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_BACKQUOTE
   (lambda() (looking-at "[`]"))
   (lambda()
     (phps-mode-lexer--begin 'ST_IN_SCRIPTING)
     (phps-mode-lexer--return-token "`")))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_DOUBLE_QUOTES
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (let ((start (point))
           (start-error (car (cdr (nth 2 phps-mode-lexer--generated-tokens)))))
       (let ((string-start (search-forward-regexp "[^\\\\]\"" nil t)))
         (if string-start
             (let* ((end (- (match-end 0) 1))
                    (double-quoted-string (buffer-substring-no-properties start end)))
               ;; Do we find variable inside quote?
               (if (or (string-match (concat "\\${" phps-mode-lexer--label) double-quoted-string)
                       (string-match (concat "{\\$" phps-mode-lexer--label) double-quoted-string)
                       (string-match (concat "\\$" phps-mode-lexer--label) double-quoted-string))
                   (progn
                     (let ((variable-start (+ start (match-beginning 0))))

                       ;; (message "Found starting expression inside double-quoted string at: %s %s" start variable-start)
                       (phps-mode-lexer--return-token-with-val
                        'T_ENCAPSED_AND_WHITESPACE
                        start
                        variable-start)))
                 (progn
                   (phps-mode-lexer--return-token-with-val
                    'T_ENCAPSED_AND_WHITESPACE
                    start
                    end)
                   ;; (message "Found end of quote at %s-%s, moving ahead after '%s'" start end (buffer-substring-no-properties start end))
                   )))
           (progn
             (signal
              'phps-lexer-error
              (list
               (format "Found no ending of double quoted region starting at %d" start-error)
               start-error))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_BACKQUOTE
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (let ((start (car (cdr (car phps-mode-lexer--generated-tokens)))))
       (let ((string-start (search-forward-regexp "\\([^\\\\]`\\|\\$\\|{\\)" nil t)))
         (if string-start
             (let ((start (- (match-end 0) 1)))
               ;; (message "Skipping backquote forward over %s" (buffer-substring-no-properties phps-mode-lexer--generated-new-tokens-index start))
               (phps-mode-lexer--return-token-with-val
                'T_ENCAPSED_AND_WHITESPACE
                phps-mode-lexer--generated-new-tokens-index
                start))
           (progn
             (signal
              'phps-lexer-error
              (list
               (format "Found no ending of back-quoted string starting at %d" start)
               start))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_HEREDOC
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda() 
     ;; Check for $, ${ and {$ forward
     (let ((old-start (car (cdr (car phps-mode-lexer--heredoc-label-stack))))
           (old-end (point)))
       (let ((string-start
              (search-forward-regexp
               (concat
                "\\(\n[\t ]*\\("
                phps-mode-lexer--heredoc-label
                "\\)\\|\\$"
                phps-mode-lexer--label
                "\\|{\\$"
                phps-mode-lexer--label
                "\\|\\${"
                phps-mode-lexer--label
                "\\)"
                )
               nil
               t)))
         (if string-start
             (let* ((start (match-beginning 0))
                    (data (match-string 0)))

               (cond

                ((string-match-p
                  (concat
                   "\n[\t ]*"
                   phps-mode-lexer--heredoc-label
                   )
                  data)
                 ;; Skip possible white-spaces before label
                 (setq start (match-beginning 2))
                 ;; (message "Found heredoc end at %s-%s" start end)
                 (phps-mode-lexer--return-token-with-val
                  'T_ENCAPSED_AND_WHITESPACE
                  old-end
                  start)
                 (phps-mode-lexer--begin 'ST_END_HEREDOC))

                (t
                 ;; (message "Found variable at '%s'.. Skipping forward to %s" data start)
                 (phps-mode-lexer--return-token-with-val
                  'T_ENCAPSED_AND_WHITESPACE
                  old-end
                  start))

                ))
           (progn
             (signal
              'phps-lexer-error
              (list
               (format "Found no ending of heredoc starting at %d" old-start)
               old-start))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   'ST_NOWDOC
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda()
     (let ((start (car (cdr (car phps-mode-lexer--generated-tokens)))))
       (let ((string-start
              (search-forward-regexp
               (concat
                "\n[\t ]*\\("
                phps-mode-lexer--heredoc-label
                "\\)")
               nil
               t)))
         (if string-start
             (let* ((start (match-beginning 1)))
               (phps-mode-lexer--return-token-with-val
                'T_ENCAPSED_AND_WHITESPACE
                phps-mode-lexer--generated-new-tokens-index
                start)
               (phps-mode-lexer--begin 'ST_END_HEREDOC))
           (progn
             (signal
              'phps-lexer-error
              (list
               (format "Found no ending of nowdoc starting at %d" start)
               start))))))))

  (phps-mode-lexer-generator--add-rule
   phps-mode-lexer-generator--table
   '(ST_IN_SCRIPTING ST_VAR_OFFSET)
   (lambda() (looking-at phps-mode-lexer--any-char))
   (lambda() 
     (signal
      'phps-lexer-error
      (list
       (format "Unexpected character at %d" (match-beginning 0))
       (match-beginning 0)))))

  phps-mode-lexer-generator--table)


(defvar phps-mode-lexer--CG-data
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-lexer--CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-lexer--CG-data)
    (gethash subject phps-mode-lexer--CG-data)))

(defun phps-mode-lexer--parser-mode ()
  "Return whether we have some expected value or not."
  phps-mode-lexer--parser-mode)

(defun phps-mode-lexer--begin (state)
  "Begin STATE."
  (phps-mode-debug-message
   (message "Begin state: %s" state))
  (setq phps-mode-lexer--state state))

(defun phps-mode-lexer--yy-push-state (state)
  "Add STATE to stack and then begin state."
  (push
   phps-mode-lexer--state
   phps-mode-lexer--state-stack)
  (phps-mode-debug-message
   (message
    "Pushed state: %s"
    phps-mode-lexer--state))
  (phps-mode-lexer--begin state))

(defun phps-mode-lexer--yy-pop-state ()
  "Pop current state from stack."
  (let ((old-state (pop phps-mode-lexer--state-stack)))
    (phps-mode-debug-message
     (message
      "Popped state: %s"
      old-state))

    ;; (message "Going back to poppped state %s" old-state)
    (if old-state
        (phps-mode-lexer--begin old-state)
      (signal
       'phps-lexer-error
       (list
        (format "Trying to pop last state at %d" (point))
        (point))))))

(defun phps-mode-lexer--move-forward (position)
  "Move forward to POSITION."
  (setq-local phps-mode-lexer--move-flag position)
  (phps-mode-debug-message
   (message "Signal move forward to %S" phps-mode-lexer--move-flag)))

(defun phps-mode-lexer--yyless (_points)
  "Move lexer back POINTS."
  ;; (setq-local
  ;;  phps-mode-lexer--move-flag
  ;;  (- phps-mode-lexer--generated-new-tokens-index points))
  ;; (phps-mode-debug-message
  ;;  (message "Signal move backward to %S" phps-mode-lexer--move-flag))
  )

(defun phps-mode-lexer--inline-char-handler ()
  "Mimic inline_char_handler."
  (let ((start (match-beginning 0)))
    (let ((string-start (search-forward "<?" nil t)))
      (if string-start
          (phps-mode-lexer--return-token 'T_INLINE_HTML start (- string-start 2))
        (phps-mode-lexer--return-token 'T_INLINE_HTML start (point-max))))))

(defun phps-mode-lexer--enter-nesting (&optional opening)
  "Enter nesting of OPENING."
  (unless opening
    (setq
     opening
     (buffer-substring-no-properties
      (match-beginning 0)
      (match-end 0))))
  (phps-mode-debug-message
   (message
    "Entered nesting '%s'"
    opening))
  (push
   `(,opening ,(point))
   phps-mode-lexer--nest-location-stack))

(defun phps-mode-lexer--handle-newline ()
  "Handle newline."
  ;; TODO Implement this?
  )

(defun phps-mode-lexer--exit-nesting (closing)
  "Exit nesting of CLOSING."
  (unless phps-mode-lexer--nest-location-stack
    (signal
     'phps-lexer-error
     (list
      (format "Unmatched '%s' at point %d" closing (point))
      (point))))
  (let ((opening
         (car
          phps-mode-lexer--nest-location-stack)))
    (when (and
           opening
           (or
            (and (string= (car opening) "{")
                 (not (string= closing "}")))
            (and (string= (car opening) "[")
                 (not (string= closing "]")))
            (and (string= (car opening) "(")
                 (not (string= closing ")")))))
      (signal
       'phps-lexer-error
       (list
        (format
         "Bad nesting '%s' started at '%s' vs '%s' at %d'"
         (car opening)
         (car (cdr opening))
         closing
         (point))
        (point))))
    (phps-mode-debug-message
     (message
      "Exited nesting '%s'"
      closing))
    (pop phps-mode-lexer--nest-location-stack)
    t))

(defun phps-mode-lexer--emit-token (token start end)
  "Emit TOKEN with START and END."
  (when (= start end)
    (signal
     'phps-lexer-error
     (list
      (format "Empty token detected: %s %s %s" token start end)
      start
      end
      token)))

  (push `(,token ,start . ,end) phps-mode-lexer--generated-tokens)
  (push `(,token ,start . ,end) phps-mode-lexer--generated-new-tokens)

  (phps-mode-debug-message
   (message
    "Emitted token '%s' -> %s"
    (buffer-substring-no-properties
     start
     end)
    `(,token ,start . ,end))))

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

(defun phps-mode-lexer--skip-token (_token &optional start end)
  "Skip TOKEN to list with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (setq-local
   phps-mode-lexer--move-flag
   end)
  (phps-mode-debug-message
   (message "Signal move skip forward to %S" phps-mode-lexer--move-flag)))

(defun phps-mode-lexer--return-token (&optional token start end)
  "Return TOKEN with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (unless token
    (setq
     token
     (buffer-substring-no-properties
      start
      end)))
  (phps-mode-lexer--emit-token
   token
   start
   end))

(defun phps-mode-lexer--check-nesting-at-end ()
  "Check nesting at end."
  (when phps-mode-lexer--nest-location-stack
    (signal
     'phps-lexer-error
     (list
      (format "Bad nesting end at '%d'" (point))
      (point))))
  t)

(defun phps-mode-lexer--return-end-token ()
  "Return end token."
  (if (and
       (phps-mode-lexer--check-nesting-at-end)
       (phps-mode-lexer--parser-mode))
      (phps-mode-lexer--return-token 'T_ERROR)
    (phps-mode-lexer--return-token 'END)))

(defun phps-mode-lexer--reset-doc-comment ()
  "Reset doc comment."
  (when (phps-mode-lexer--CG 'doc_comment)
    (phps-mode-lexer--CG 'doc_comment nil)))

(defun phps-mode-lexer--return-token-with-indent (&optional token start end)
  "Return TOKEN with START and END."
  (phps-mode-lexer--return-token
   token
   start
   end))

;; TODO Do something with offset?
(defun phps-mode-lexer--return-token-with-str (token &optional _offset start end)
  "Return TOKEN at OFFSET with START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (phps-mode-lexer--return-token token start end))

(defun phps-mode-lexer--return-whitespace ()
  "Return whitespace."
  (phps-mode-lexer--move-forward (match-end 0)))

(defun phps-mode-lexer--return-exit-nesting-token (&optional token start end)
  "Return TOKEN if it does not exit a nesting with optional START and END."
  (unless start
    (setq start (match-beginning 0)))
  (unless end
    (setq end (match-end 0)))
  (unless token
    (setq
     token
     (buffer-substring-no-properties
      start
      end)))
  (if (and
       (phps-mode-lexer--exit-nesting token)
       (phps-mode-lexer--parser-mode))
      (phps-mode-lexer--return-token 'T_ERROR)
    (phps-mode-lexer--return-token
     token
     start
     end)))

(defun phps-mode-lexer--restart ()
  "Restart."
  (setq phps-mode-lexer--restart-flag t))

(defun phps-mode-lexer--return-token-with-val (&optional token start end)
  "Return TOKEN with START and END."
  (phps-mode-lexer--return-token token start end))

(defun phps-mode-lexer--return-or-skip-token (&optional token start end)
  "Return TOKEN with START and END but only in parse-mode."
  (if (phps-mode-lexer--parser-mode)
      (phps-mode-lexer--skip-token token start end)
    (phps-mode-lexer--return-token token start end)))


(provide 'phps-mode-lexer-generator)
