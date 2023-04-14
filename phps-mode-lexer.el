;;; phps-mode-lexer.el -- Lexer for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2023  Free Software Foundation, Inc.


;;; Commentary:

;; The idea is gathering everything directly related to the lexer in this file,
;; any higher order meta-lexer logic goes into `phps-mode-lex-analyzer.el'.
;;
;; Features:
;; * Defines the lexer for this grammar based on the Zend PHP 8.2 Lexer at
;; https://raw.githubusercontent.com/php/php-src/PHP-8.2/Zend/zend_language_scanner.l
;; which is using re2c.

;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-lexer-generator)

(require 'subr-x)

(define-error
  'phps-lexer-error
  "PHPs Lexer Error")


;; INITIALIZE SETTINGS


(defvar phps-mode-lexer--CG-data
  (make-hash-table :test 'equal)
  "A hash-table with all settings.")

(defun phps-mode-lexer--CG (subject &optional value)
  "Return and optionally set VALUE of SUBJECT."
  (if value
      (puthash subject value phps-mode-lexer--CG-data)
    (gethash subject phps-mode-lexer--CG-data)))

(phps-mode-lexer--CG
 'parser-mode t)
(phps-mode-lexer--CG
 'short-tags t)


;; SETTINGS


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
  "[\n\r]"
  "Newline characters.  The Zend equivalent is (\"\r\"|\"\n\"|\"\r\n\").")


;; VARIABLES


(defvar phps-mode-lexer--lambdas-by-state
  (eval-when-compile (phps-mode-lexer-generator--lambdas))
  "Hash-table of lex-analyzer rules organized by state.")

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

(defvar-local phps-mode-lexer--states nil
  "History of state and state-stack.")

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

;; HELPER FUNCTIONS


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
  (when (boundp 'phps-mode-lex-analyzer--lexer-index)
    (setq-local
     phps-mode-lex-analyzer--lexer-index
     position)))

(defun phps-mode-lexer--yyless (points)
  "Move lexer back POINTS."
  (when (boundp 'phps-mode-lex-analyzer--lexer-index)
    (setq-local
     phps-mode-lex-analyzer--lexer-index
     (- phps-mode-lex-analyzer--lexer-index points)))
  (forward-char (- points)))

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
    `(,token ,start . ,end)))
  
  ;; Push token start, end, lexer state and state stack to variable
  (push
   (list
    start
    end
    phps-mode-lexer--state
    phps-mode-lexer--state-stack
    phps-mode-lexer--heredoc-label
    phps-mode-lexer--heredoc-label-stack
    phps-mode-lexer--nest-location-stack)
   phps-mode-lexer--states))

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
  (when (boundp 'phps-mode-lex-analyzer--lexer-index)
    (setq-local
     phps-mode-lex-analyzer--lexer-index
     end)))

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
(defun phps-mode-lexer--return-token-with-str (token _offset &optional start end)
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


;; LEXER FUNCTIONS BELOW


(defun phps-mode-lexer--re2c-execute ()
  "Execute matching body (if any)."
  (if phps-mode-lexer--match-body
      (progn
        (set-match-data phps-mode-lexer--match-data)
        (funcall phps-mode-lexer--match-body))
    (signal
     'phps-lexer-error
     (list
      (format "Found no matching lexer rule to execute at %d" (point))
      (point)))))

(defun phps-mode-lexer--reset-match-data ()
  "Reset match data."
  (setq phps-mode-lexer--match-length 0)
  (setq phps-mode-lexer--match-data nil)
  (setq phps-mode-lexer--match-body nil))

;; If multiple rules match, re2c prefers the longest match.
;; If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(defun phps-mode-lexer--re2c ()
  "Elisp port of original Zend re2c lexer."

  (setq phps-mode-lexer--generated-new-tokens nil)
  (setq phps-mode-lexer--restart-flag nil)
  (let ((old-start (point)))
    (setq
     phps-mode-lexer--generated-new-tokens-index
     old-start)
    (phps-mode-debug-message
     (let ((start (point))
           (end (+ (point) 5))
           (lookahead))
       (when (> end (point-max))
         (setq end (point-max)))
       (setq
        lookahead
        (buffer-substring-no-properties
         start
         end))
       (message
        "\nRunning lexer from point %s, state: %s, lookahead: '%s'.."
        old-start
        phps-mode-lexer--state
        lookahead)))
    (phps-mode-lexer--reset-match-data)

    ;; Run rules based on state
    (when-let ((lambdas
                (gethash
                 phps-mode-lexer--state
                 phps-mode-lexer--lambdas-by-state)))
      (let ((lambda-i 0)
            (lambda-length (length lambdas)))
        (phps-mode-debug-message
         (message "Found %d lexer rules in state" lambda-length))

        (while (< lambda-i lambda-length)
          (let ((lambd (nth lambda-i lambdas)))

            (let ((lambda-result
                   (funcall (nth 0 lambd))))
              (when lambda-result
                (let ((match-end (match-end 0))
                      (match-beginning (match-beginning 0)))
                  (let ((matching-length (- match-end match-beginning)))
                    (when (> matching-length 0)
                      (when (or
                             (not phps-mode-lexer--match-length)
                             (> matching-length phps-mode-lexer--match-length))
                        (setq
                         phps-mode-lexer--match-length matching-length)
                        (setq
                         phps-mode-lexer--match-body (car (nth 1 lambd)))
                        (setq
                         phps-mode-lexer--match-data (match-data))

                        ;; Debug new matches
                        (phps-mode-debug-message
                         (message
                          "Found new best match, with length: %d, and body: %s"
                          phps-mode-lexer--match-length
                          phps-mode-lexer--match-body))))))))

            (when (fboundp 'thread-yield)
              (thread-yield)))
          (setq lambda-i (1+ lambda-i)))))

    ;; Did we find a match?
    (if phps-mode-lexer--match-length
        (progn
          (phps-mode-debug-message
           (message
            "Found final match %s"
            phps-mode-lexer--match-body))
          (phps-mode-lexer--re2c-execute)

          (when phps-mode-lexer--restart-flag
            (phps-mode-debug-message
             (message "Restarting lexer"))
            (phps-mode-lexer--re2c)))
      (phps-mode-debug-message
       (message "Found nothing at %d" (point))))))


(provide 'phps-mode-lexer)

;;; phps-mode-lexer.el ends here
