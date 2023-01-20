;;; phps-mode-lex-analyzer.el -- Lex analyzer for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Free Software Foundation, Inc.


;;; Commentary:

;; This file contains all meta-lexer logic.  That is things like:
;;
;; * Executing different kinds of lexers based on conditions
;; * Also supply logic for indentation and imenu-handling
;; * Imenu based on lexer tokens
;; * Syntax coloring based on lexer tokens


;;; Code:


(require 'phps-mode-cache)
(require 'phps-mode-lexer)
(require 'phps-mode-macros)
(require 'phps-mode-parser)
(require 'phps-mode-parser-sdt)
(require 'phps-mode-serial)
(require 'phps-mode-syntax-color)
(require 'phps-mode-ast)

(require 'subr-x)


;; FLAGS


(defvar-local phps-mode-lex-analyzer--allow-after-change-p t
  "Flag to tell us whether after change detection is enabled or not.")

(defvar-local phps-mode-lex-analyzer--change-min nil
  "The minium point of change.");

(defvar-local phps-mode-lex-analyzer--processed-buffer-p nil
  "Flag whether current buffer is processed or not.")

(defvar-local phps-mode-lex-analyzer--error-end nil
  "Point of error end or nil.")

(defvar-local phps-mode-lex-analyzer--error-message nil
  "Message of error.")

(defvar-local phps-mode-lex-analyzer--error-start nil
  "Point of error start or nil.")


;; VARIABLES


(defvar-local phps-mode-lex-analyzer--idle-timer nil
  "Timer object of idle timer.")

(defvar-local phps-mode-lex-analyzer--ast nil
  "The AST for current buffer, nil if none.")

(defvar-local phps-mode-lex-analyzer--imenu nil
  "The Imenu alist for current buffer, nil if none.")

(defvar-local phps-mode-lex-analyzer--bookkeeping nil
  "Bookkeeping of all variables in tokens.")

(defvar-local phps-mode-lex-analyzer--tokens nil
  "Latest tokens.")

(defvar-local phps-mode-lex-analyzer--state nil
  "Latest state.")

(defvar-local phps-mode-lex-analyzer--state-stack nil
  "Latest state-stack.")

(defvar-local phps-mode-lex-analyzer--states nil
  "History of state, heredoc-label, stack-stack and heredoc label stack.")

(defvar-local phps-mode-lex-analyzer--heredoc-label nil
  "Latest Heredoc label.")

(defvar-local phps-mode-lex-analyzer--heredoc-label-stack nil
  "Latest Heredoc label-stack.")

(defvar-local phps-mode-lex-analyzer--nest-location-stack nil
  "Nest location stack.")

(defvar-local phps-mode-lex-analyzer--parse-trail nil
  "Valid parse trail or nil.")

(defvar-local phps-mode-lex-analyzer--parse-error nil
  "Non-nil means an error.")

(defvar-local phps-mode-lex-analyzer--lexer-index nil
  "Index of lex-analyzer.")

(defvar-local phps-mode-lex-analyzer--lexer-max-index nil
  "Max-index of lex-analyzer.")


;; FUNCTIONS


(defun phps-mode-lex-analyzer--reset-local-variables (&optional clear-existing)
  "Reset local variables, optionally CLEAR-EXISTING."
  (setq phps-mode-lex-analyzer--allow-after-change-p t)
  (setq phps-mode-lex-analyzer--ast nil)
  (setq phps-mode-lex-analyzer--bookkeeping nil)
  (setq phps-mode-lex-analyzer--change-min nil)
  (setq phps-mode-lex-analyzer--heredoc-label nil)
  (setq phps-mode-lex-analyzer--heredoc-label-stack nil)
  (setq phps-mode-lex-analyzer--idle-timer nil)
  (setq phps-mode-lex-analyzer--imenu nil)
  (setq phps-mode-lex-analyzer--nest-location-stack nil)
  (setq phps-mode-lex-analyzer--parse-error nil)
  (setq phps-mode-lex-analyzer--parse-trail nil)
  (setq phps-mode-lex-analyzer--processed-buffer-p nil)
  (setq phps-mode-lex-analyzer--state nil)
  (setq phps-mode-lex-analyzer--state-stack nil)
  (setq phps-mode-lex-analyzer--states nil)
  (setq phps-mode-lex-analyzer--tokens nil)
  (when clear-existing
    (phps-mode-serial--kill-active
     (buffer-name))
    (when (and
           buffer-file-name
           phps-mode-cache--use-p)
      (phps-mode-cache-delete
       buffer-file-name))))

(defun phps-mode-lex-analyzer--set-region-syntax-color (start end properties)
  "Do syntax coloring for region START to END with PROPERTIES."
  (with-silent-modifications (set-text-properties start end properties)))

(defun phps-mode-lex-analyzer--clear-region-syntax-color (start end)
  "Clear region of syntax coloring from START to END."
  (with-silent-modifications (set-text-properties start end nil)))

(defun phps-mode-lex-analyzer--get-token-syntax-color (token)
  "Return syntax color for TOKEN."
  (let ((token-name (car token))
        (start)
        (end)
        (bookkeeping-index)
        (bookkeeping-value))
    (when (gethash
           token-name
           phps-mode-syntax-color--token-for-bookkeeping-p)
      (setq
       start (car (cdr token)))
      (setq
       end (cdr (cdr token)))
      (setq
       bookkeeping-index
       (list start end))
      (when phps-mode-lex-analyzer--bookkeeping
        (setq
         bookkeeping-value
         (gethash
          bookkeeping-index
          phps-mode-lex-analyzer--bookkeeping))))

    (cond

     ((when bookkeeping-value
        (if (= bookkeeping-value 0)
            'font-lock-warning-face
          'font-lock-variable-name-face)))

     ((when-let ((face
                  (gethash
                   token-name
                   phps-mode-syntax-color--token-font-face)))
        face))

     (t 'font-lock-constant-face))))


;; LEXERS


;; If multiple rules match, re2c prefers the longest match.
;; If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(defun phps-mode-lex-analyzer--re2c-lex-analyzer ()
  "Run the Elisp port of original Zend re2c lexer."
  (save-excursion
    (while
        (<
         phps-mode-lex-analyzer--lexer-index
         phps-mode-lex-analyzer--lexer-max-index)
      (goto-char phps-mode-lex-analyzer--lexer-index)
      (let ((old-index phps-mode-lex-analyzer--lexer-index))
        (phps-mode-lexer--re2c)

        (unless (or
                 phps-mode-lexer--generated-new-tokens
                 (> phps-mode-lex-analyzer--lexer-index old-index))
          (signal
           'phps-lexer-error
           '(format
             "Failed to lex buffer at position %S"
             phps-mode-lex-analyzer--lexer-index)))

        (when phps-mode-lexer--generated-new-tokens
          (setq-local
           phps-mode-lex-analyzer--lexer-index
           (cdr (cdr (car phps-mode-lexer--generated-new-tokens)))))))))

(defun phps-mode-lex-analyzer--re2c-run (&optional force-synchronous allow-cache-read allow-cache-write)
  "Run lexer, optionally FORCE-SYNCHRONOUS mode, ALLOW-CACHE-READ and ALLOW-CACHE-WRITE."
  (interactive)
  (require 'phps-mode-macros)
  (phps-mode-debug-message (message "Lexer run"))

  (let ((buffer-name (buffer-name))
        (buffer-contents (buffer-substring-no-properties (point-min) (point-max)))
        (async (and (boundp 'phps-mode-async-process)
                    phps-mode-async-process))
        (async-by-process (and (boundp 'phps-mode-async-process-using-async-el)
                               phps-mode-async-process-using-async-el)))
    (when force-synchronous
      (setq async nil))

    (phps-mode-serial-commands

     buffer-name

     (lambda()
       (phps-mode-lex-analyzer--lex-string
        buffer-contents
        nil
        nil
        nil
        nil
        nil
        nil
        nil
        nil
        nil
        buffer-file-name
        allow-cache-read
        allow-cache-write))

     (lambda(lex-result)
       (when (get-buffer buffer-name)
         (with-current-buffer buffer-name

           ;; Move variables into this buffers local variables
           (setq phps-mode-lex-analyzer--tokens (nth 0 lex-result))
           (setq phps-mode-lex-analyzer--states (nth 1 lex-result))
           (setq phps-mode-lex-analyzer--state (nth 2 lex-result))
           (setq phps-mode-lex-analyzer--state-stack (nth 3 lex-result))
           (setq phps-mode-lex-analyzer--heredoc-label (nth 4 lex-result))
           (setq phps-mode-lex-analyzer--heredoc-label-stack (nth 5 lex-result))
           (setq phps-mode-lex-analyzer--nest-location-stack (nth 6 lex-result))
           (setq phps-mode-lex-analyzer--parse-trail (nth 7 lex-result))
           (setq phps-mode-lex-analyzer--parse-error (nth 8 lex-result))
           (setq phps-mode-lex-analyzer--ast (nth 9 lex-result))
           (setq phps-mode-lex-analyzer--bookkeeping (nth 10 lex-result))
           (setq phps-mode-lex-analyzer--imenu (nth 11 lex-result))

           (setq phps-mode-lex-analyzer--processed-buffer-p t)
           (phps-mode-lex-analyzer--reset-imenu)

           ;; Apply syntax color
           (phps-mode-lex-analyzer--clear-region-syntax-color
            (point-min)
            (point-max))
           (dolist (token phps-mode-lex-analyzer--tokens)
             (let ((start (car (cdr token)))
                   (end (cdr (cdr token))))
               (let ((token-syntax-color
                      (phps-mode-lex-analyzer--get-token-syntax-color token)))
                 (when token-syntax-color
                     (phps-mode-lex-analyzer--set-region-syntax-color
                      start
                      end
                      (list 'font-lock-face token-syntax-color))))))

           ;; Reset buffer changes minimum index
           (phps-mode-lex-analyzer--reset-changes)

           ;; Signal parser error (if any)
           (if phps-mode-lex-analyzer--parse-error
               (progn

                 ;; Paint error
                 (phps-mode-lex-analyzer--set-region-syntax-color
                  (nth 4 phps-mode-lex-analyzer--parse-error)
                  (point-max)
                  (list 'font-lock-face 'font-lock-warning-face))

                 ;; Set error
                 (setq phps-mode-lex-analyzer--error-end nil)
                 (setq phps-mode-lex-analyzer--error-message (nth 1 phps-mode-lex-analyzer--parse-error))
                 (setq phps-mode-lex-analyzer--error-start (nth 4 phps-mode-lex-analyzer--parse-error))

                 ;; Signal that causes updated mode-line status
                 (signal
                  'phps-parser-error
                  (list
                   (nth 1 phps-mode-lex-analyzer--parse-error)
                   (nth 4 phps-mode-lex-analyzer--parse-error))))

             ;; Reset error
             (setq phps-mode-lex-analyzer--error-end nil)
             (setq phps-mode-lex-analyzer--error-message nil)
             (setq phps-mode-lex-analyzer--error-start nil)))))

     (lambda(result)
       (when (get-buffer buffer-name)
         (with-current-buffer buffer-name
           (let ((error-type (nth 0 result))
                 (error-message (nth 1 result))
                 (error-start (nth 2 result))
                 (error-end (nth 3 result)))
             (phps-mode-lex-analyzer--reset-local-variables)

             (when error-message
               (cond

                ((equal error-type 'phps-lexer-error)
                 (when error-start
                   (if error-end
                       (phps-mode-lex-analyzer--set-region-syntax-color
                        error-start
                        error-end
                        (list 'font-lock-face 'font-lock-warning-face))
                     (phps-mode-lex-analyzer--set-region-syntax-color
                      error-start
                      (point-max)
                      (list 'font-lock-face 'font-lock-warning-face)))

                   ;; Set error
                   (setq phps-mode-lex-analyzer--error-end (if error-end error-end nil))
                   (setq phps-mode-lex-analyzer--error-message error-message)
                   (setq phps-mode-lex-analyzer--error-start error-start)))

                (t
                 (progn

                   ;; Reset error
                   (setq phps-mode-lex-analyzer--error-end nil)
                   (setq phps-mode-lex-analyzer--error-message nil)
                   (setq phps-mode-lex-analyzer--error-start nil)

                   (display-warning
                    error-type
                    error-message
                    :warning)))))))))

     nil
     async
     async-by-process)))

(defun phps-mode-lex-analyzer--incremental-lex-string
    (buffer-name buffer-contents incremental-start-new-buffer point-max
                 head-states incremental-state incremental-state-stack incremental-heredoc-label incremental-heredoc-label-stack incremental-nest-location-stack head-tokens &optional force-synchronous filename allow-cache-write)
  "Incremental lex region."
  (let ((async (and (boundp 'phps-mode-async-process)
                    phps-mode-async-process))
        (async-by-process (and (boundp 'phps-mode-async-process-using-async-el)
                               phps-mode-async-process-using-async-el)))
    (when force-synchronous
      (setq async nil))

    (phps-mode-serial-commands

     buffer-name

     (lambda()
       (phps-mode-lex-analyzer--lex-string
        buffer-contents
        incremental-start-new-buffer
        point-max
        head-states
        incremental-state
        incremental-state-stack
        incremental-heredoc-label
        incremental-heredoc-label-stack
        incremental-nest-location-stack
        head-tokens
        filename
        nil
        allow-cache-write))

     (lambda(lex-result)
       (when (get-buffer buffer-name)
         (with-current-buffer buffer-name

           (phps-mode-debug-message
            (message "Incrementally-lexed-string: %s" result))

           (setq phps-mode-lex-analyzer--tokens (nth 0 lex-result))
           (setq phps-mode-lex-analyzer--states (nth 1 lex-result))
           (setq phps-mode-lex-analyzer--state (nth 2 lex-result))
           (setq phps-mode-lex-analyzer--state-stack (nth 3 lex-result))
           (setq phps-mode-lex-analyzer--heredoc-label (nth 4 lex-result))
           (setq phps-mode-lex-analyzer--heredoc-label-stack (nth 5 lex-result))
           (setq phps-mode-lex-analyzer--nest-location-stack (nth 6 lex-result))
           (setq phps-mode-lex-analyzer--parse-trail (nth 7 lex-result))
           (setq phps-mode-lex-analyzer--parse-error (nth 8 lex-result))
           (setq phps-mode-lex-analyzer--ast (nth 9 lex-result))
           (setq phps-mode-lex-analyzer--bookkeeping (nth 10 lex-result))
           (setq phps-mode-lex-analyzer--imenu (nth 11 lex-result))

           (phps-mode-debug-message
            (message
             "Incremental tokens: %s"
             phps-mode-lex-analyzer--tokens))

           ;; Save processed result
           (setq phps-mode-lex-analyzer--processed-buffer-p t)
           (phps-mode-lex-analyzer--reset-imenu)

           ;; Apply syntax color on tokens
           (phps-mode-lex-analyzer--clear-region-syntax-color
            incremental-start-new-buffer
            point-max)
           (dolist (token phps-mode-lex-analyzer--tokens)
             (let ((start (car (cdr token)))
                   (end (cdr (cdr token))))

               ;; Apply syntax color on token
               (let ((token-syntax-color (phps-mode-lex-analyzer--get-token-syntax-color token)))
                 (when token-syntax-color
                     (phps-mode-lex-analyzer--set-region-syntax-color start end (list 'font-lock-face token-syntax-color))))))

           ;; Reset buffer changes minimum index
           (phps-mode-lex-analyzer--reset-changes)

           ;; Signal parser error (if any)
           (if phps-mode-lex-analyzer--parse-error
               (progn

                 ;; Paint error
                 (phps-mode-lex-analyzer--set-region-syntax-color
                  (nth 4 phps-mode-lex-analyzer--parse-error)
                  (point-max)
                  (list 'font-lock-face 'font-lock-warning-face))

                 ;; Set error
                 (setq phps-mode-lex-analyzer--error-end nil)
                 (setq phps-mode-lex-analyzer--error-message (nth 1 phps-mode-lex-analyzer--parse-error))
                 (setq phps-mode-lex-analyzer--error-start (nth 4 phps-mode-lex-analyzer--parse-error))

                 ;; Signal that causes updated mode-line status
                 (signal
                  'phps-parser-error
                  (list
                   (nth 1 phps-mode-lex-analyzer--parse-error)
                   (nth 4 phps-mode-lex-analyzer--parse-error))))

             ;; Reset error
             (setq phps-mode-lex-analyzer--error-end nil)
             (setq phps-mode-lex-analyzer--error-message nil)
             (setq phps-mode-lex-analyzer--error-start nil)))))

     (lambda(result)
       (when (get-buffer buffer-name)
         (with-current-buffer buffer-name
           (let ((error-type (nth 0 result))
                 (error-message (nth 1 result))
                 (error-start (nth 2 result))
                 (error-end (nth 3 result)))

             (phps-mode-lex-analyzer--reset-local-variables)

             (when error-message
               (cond

                ((equal error-type 'phps-lexer-error)
                 (when error-start
                   (if error-end
                       (phps-mode-lex-analyzer--set-region-syntax-color
                        error-start
                        error-end
                        (list 'font-lock-face 'font-lock-warning-face))
                     (phps-mode-lex-analyzer--set-region-syntax-color
                      error-start
                      (point-max)
                      (list 'font-lock-face 'font-lock-warning-face))))

                 ;; Set error
                 (setq phps-mode-lex-analyzer--error-end (if error-end error-end nil))
                 (setq phps-mode-lex-analyzer--error-message error-message)
                 (setq phps-mode-lex-analyzer--error-start error-start))

                (t
                 (progn

                 ;; Reset error
                 (setq phps-mode-lex-analyzer--error-end nil)
                 (setq phps-mode-lex-analyzer--error-message nil)
                 (setq phps-mode-lex-analyzer--error-start nil)

                   (display-warning
                    error-type
                    error-message
                    :warning)))))))))

     nil
     async
     async-by-process)))

(defun phps-mode-lex-analyzer--move-states (start diff)
  "Move lexer states after (or equal to) START with modification DIFF."
  (when phps-mode-lex-analyzer--states
    (setq phps-mode-lex-analyzer--states
          (phps-mode-lex-analyzer--get-moved-states
           phps-mode-lex-analyzer--states
           start
           diff))))

(defun phps-mode-lex-analyzer--get-moved-states (states start diff)
  "Return moved lexer STATES after (or equal to) START with modification DIFF."
  (let ((old-states states)
        (new-states '()))
    (when old-states

      ;; Iterate through states add states before start start unchanged and the others modified with diff
      (dolist (state-object (nreverse old-states))
        (let ((state-start (nth 0 state-object))
              (state-end (nth 1 state-object))
              (state-symbol (nth 2 state-object))
              (state-stack (nth 3 state-object))
              (heredoc-label (nth 4 state-object))
              (heredoc-label-stack (nth 5 state-object))
              (nest-location-stack (nth 6 state-object)))
          (if (>= state-start start)
              (let ((new-state-start (+ state-start diff))
                    (new-state-end (+ state-end diff)))
                (push
                 (list
                  new-state-start
                  new-state-end
                  state-symbol
                  state-stack
                  heredoc-label
                  heredoc-label-stack
                  nest-location-stack)
                 new-states))
            (if (> state-end start)
                (let ((new-state-end (+ state-end diff)))
                  (push
                   (list
                    state-start
                    new-state-end
                    state-symbol
                    state-stack
                    heredoc-label
                    heredoc-label-stack
                    nest-location-stack)
                   new-states))
              (push
               state-object
               new-states))))))
    new-states))

(defun phps-mode-lex-analyzer--move-tokens (start diff)
  "Update tokens with moved lexer tokens after or equal to START with modification DIFF."
  (when phps-mode-lex-analyzer--tokens
    (setq
     phps-mode-lex-analyzer--tokens
     (phps-mode-lex-analyzer--get-moved-tokens
      phps-mode-lex-analyzer--tokens
      start
      diff))))

(defun phps-mode-lex-analyzer--get-moved-tokens (old-tokens start diff)
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

(defun phps-mode-lex-analyzer--reset-changes ()
  "Reset change."
  (setq phps-mode-lex-analyzer--change-min nil))

(defun phps-mode-lex-analyzer--process-changes (&optional buffer force-synchronous)
  "Run incremental lexer on BUFFER.  Return list of performed operations.  Optionally do it FORCE-SYNCHRONOUS."
  (unless buffer
    (setq buffer (current-buffer)))
  (phps-mode-debug-message
   (message "Run process changes on buffer '%s'" buffer))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (phps-mode-lex-analyzer--reset-imenu)
      (let ((run-full-lexer nil)
            (old-tokens phps-mode-lex-analyzer--tokens)
            (old-states phps-mode-lex-analyzer--states)
            (log '()))

        (if phps-mode-lex-analyzer--change-min
            (progn
              (phps-mode-debug-message
               (message "Processing change point minimum: %s" phps-mode-lex-analyzer--change-min))
              (let ((incremental-state nil)
                    (incremental-state-stack nil)
                    (incremental-heredoc-label nil)
                    (incremental-heredoc-label-stack nil)
                    (incremental-nest-location-stack nil)
                    (incremental-tokens nil)
                    (head-states '())
                    (head-tokens '())
                    (change-start phps-mode-lex-analyzer--change-min)
                    (incremental-start-new-buffer phps-mode-lex-analyzer--change-min))

                ;; Reset idle timer
                (phps-mode-lex-analyzer--cancel-idle-timer)

                ;; NOTE Starts are inclusive while ends are exclusive buffer locations

                ;; Some tokens have dynamic length and if a change occurs at token-end
                ;; we must start the incremental process at previous token start

                ;; Build list of tokens from old buffer before start of changes (head-tokens)

                (catch 'quit
                  (dolist (token old-tokens)
                    (let ((token-type (car token))
                          (start (car (cdr token)))
                          (end (cdr (cdr token))))
                      (if (< end change-start)
                          (push token head-tokens)
                        (when (< start change-start)
                          (when (equal token-type 'T_END_HEREDOC)
                            ;; When incremental start is on a T_END_HEREDOC token
                            ;; rewind another token to allow expansion of
                            ;; T_ENCAPSED_AND_WHITESPACE
                            (phps-mode-debug-message
                             (message
                              "Rewinding incremental start due to 'T_END_HEREDOC token"))
                            (let ((previous-token (pop head-tokens)))
                              (setq
                               start
                               (car (cdr previous-token)))))

                          (phps-mode-debug-message
                           (message
                            "New incremental-start-new-buffer: %s"
                            start))
                          (setq
                           incremental-start-new-buffer
                           start))
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
                      ;; 1. Determine state (incremental-state) and state-stack (incremental-state-stack) heredoc label (incremental-heredoc-label) heredoc-label-stack (heredoc-label-stack) before incremental start
                      ;; 2. Build list of states before incremental start (head-states)
                      (catch 'quit
                        (dolist (state-object (nreverse old-states))
                          (let ((end (nth 1 state-object)))
                            (if (<= end incremental-start-new-buffer)
                                (progn
                                  (setq incremental-state (nth 2 state-object))
                                  (setq incremental-state-stack (nth 3 state-object))
                                  (setq incremental-heredoc-label (nth 4 state-object))
                                  (setq incremental-heredoc-label-stack (nth 5 state-object))
                                  (setq incremental-nest-location-stack (nth 6 state-object))
                                  (push state-object head-states))
                              (throw 'quit "break")))))

                      (phps-mode-debug-message
                       (message "Head states: %s" head-states)
                       (message "Incremental state: %s" incremental-state)
                       (message "State stack: %s" incremental-state-stack)
                       (message "Incremental heredoc-label: %s" incremental-heredoc-label)
                       (message "Incremental heredoc-label-stack: %s" incremental-heredoc-label-stack)
                       (message "Incremental nest-location-stack: %s" incremental-nest-location-stack))

                      (if (and
                           head-states
                           incremental-state)
                          (progn
                            (phps-mode-debug-message
                             (message "Found head states"))

                            (push (list 'INCREMENTAL-LEX incremental-start-new-buffer) log)

                            ;; Do partial lex from previous-token-end to change-stop

                            (phps-mode-lex-analyzer--incremental-lex-string
                             (buffer-name)
                             (buffer-substring-no-properties (point-min) (point-max))
                             incremental-start-new-buffer
                             (point-max)
                             head-states
                             incremental-state
                             incremental-state-stack
                             incremental-heredoc-label
                             incremental-heredoc-label-stack
                             incremental-nest-location-stack
                             head-tokens
                             force-synchronous
                             (if (buffer-modified-p) nil buffer-file-name)
                             (not (buffer-modified-p)))

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
          (phps-mode-lex-analyzer--re2c-run
           force-synchronous
           nil
           (not (buffer-modified-p))))

        log))))

(defun phps-mode-lex-analyzer--get-moved-lines-indent (old-lines-indents start-line-number diff)
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

(defun phps-mode-lex-analyzer--move-imenu-index (start diff)
  "Moved imenu from START by DIFF points."
  (when phps-mode-lex-analyzer--imenu
    (setq phps-mode-lex-analyzer--imenu
          (phps-mode-lex-analyzer--get-moved-imenu phps-mode-lex-analyzer--imenu start diff))
    (phps-mode-lex-analyzer--reset-imenu)))

(defun phps-mode-lex-analyzer--get-bookkeeping ()
  "Return bookkeeping, process buffer if not done already."
  phps-mode-lex-analyzer--bookkeeping)

(defun phps-mode-lex-analyzer--get-imenu ()
  "Return Imenu, process buffer if not done already."
  phps-mode-lex-analyzer--imenu)

(defun phps-mode-lex-analyzer--get-moved-imenu (old-index start diff)
  "Move imenu-index OLD-INDEX beginning from START with DIFF."
  (let ((new-index '()))

    (when old-index
      (if (and (listp old-index)
               (listp (car old-index)))
          (dolist (item old-index)
            (let ((sub-item (phps-mode-lex-analyzer--get-moved-imenu item start diff)))
              (push (car sub-item) new-index)))
        (let ((item old-index))
          (let ((item-label (car item)))
            (if (listp (cdr item))
                (let ((sub-item (phps-mode-lex-analyzer--get-moved-imenu (cdr item) start diff)))
                  (push `(,item-label . ,sub-item) new-index))
              (let ((item-start (cdr item)))
                (when (>= item-start start)
                  (setq item-start (+ item-start diff)))
                (push `(,item-label . ,item-start) new-index)))))))

    (nreverse new-index)))

(defun phps-mode-lex-analyzer--cancel-idle-timer ()
  "Cancel idle timer."
  (phps-mode-debug-message (message "Cancelled idle timer"))
  (when phps-mode-lex-analyzer--idle-timer
    (cancel-timer phps-mode-lex-analyzer--idle-timer)
    (setq phps-mode-lex-analyzer--idle-timer nil)))

(defun phps-mode-lex-analyzer--start-idle-timer ()
  "Start idle timer."
  (phps-mode-debug-message (message "Enqueued idle timer"))
  (when (boundp 'phps-mode-idle-interval)
    (let ((buffer (current-buffer)))
      (setq
       phps-mode-lex-analyzer--idle-timer
       (run-with-idle-timer
        phps-mode-idle-interval
        nil
        #'phps-mode-lex-analyzer--process-changes buffer)))))

(defun phps-mode-lex-analyzer--reset-imenu ()
  "Reset imenu index."
  (when (and (boundp 'imenu--index-alist)
             imenu--index-alist)
    (setq imenu--index-alist nil)
    (phps-mode-debug-message (message "Cleared Imenu index"))))

(defun phps-mode-lex-analyzer--after-change (start stop length)
  "Track buffer change from START to STOP with LENGTH."
  (phps-mode-debug-message
   (message
    "After change %s - %s, length: %s, enabled: %s, idle-interval: %s" start stop length phps-mode-lex-analyzer--allow-after-change-p phps-mode-idle-interval))

  (if phps-mode-lex-analyzer--allow-after-change-p
      (progn
        (phps-mode-debug-message
         (message "After change registration is enabled"))

        ;; Kill active thread (if any)
        (phps-mode-serial--kill-active (buffer-name))
        
        ;; If we haven't scheduled incremental lexer before - do it
        (when (and (boundp 'phps-mode-idle-interval)
                   phps-mode-idle-interval
                   (not phps-mode-lex-analyzer--idle-timer))
          (phps-mode-lex-analyzer--start-idle-timer))

        ;; When change position is before previous start position - update it
        (when (or
               (not phps-mode-lex-analyzer--change-min)
               (< start phps-mode-lex-analyzer--change-min))
          (setq
           phps-mode-lex-analyzer--change-min
           start))

        (when (and
               (boundp 'phps-mode-idle-interval)
               (not phps-mode-idle-interval))
          (phps-mode-lex-analyzer--process-changes
           (current-buffer))))
    (phps-mode-debug-message
     (message "After change registration is disabled"))))

(defun phps-mode-lex-analyzer--imenu-create-index ()
  "Get Imenu for current buffer."
  phps-mode-lex-analyzer--imenu)

(defun phps-mode-lex-analyzer--comment-region (beg end &optional _arg)
  "Comment region from BEG to END with optional _ARG."

  ;; Make sure changes has been processed
  (when phps-mode-lex-analyzer--idle-timer
    (phps-mode-lex-analyzer--process-changes nil t)
    (phps-mode-lex-analyzer--cancel-idle-timer))

  ;; Iterate tokens from beginning to end and comment out all PHP code
  (when-let ((tokens phps-mode-lex-analyzer--tokens))
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

(defun phps-mode-lex-analyzer--uncomment-region (beg end &optional _arg)
  "Un-comment region from BEG to END with optional ARG."

  ;; Make sure changes has been processed
  (when phps-mode-lex-analyzer--idle-timer
    (phps-mode-lex-analyzer--process-changes nil t)
    (phps-mode-lex-analyzer--cancel-idle-timer))

  ;; Iterate tokens from beginning to end and uncomment out all commented PHP code
  (when-let ((tokens phps-mode-lex-analyzer--tokens))
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

(defun phps-mode-lex-analyzer--setup (start end)
  "Just prepare other lexers for lexing region START to END."
  (require 'phps-mode-macros)
  (phps-mode-debug-message (message "Lexer setup %s - %s" start end))
  (unless phps-mode-lex-analyzer--state
    (setq phps-mode-lex-analyzer--state 'ST_INITIAL)))

(defun phps-mode-lex-analyzer--generate-parser-tokens (lexer-tokens)
  "Generate parser-tokens from LEXER-TOKENS which are in reversed order."
  (let ((parser-tokens (make-hash-table :test 'equal))
        (previous-start))
    (dolist (token lexer-tokens)
      (let ((token-type (car token))
            (token-start (car (cdr token)))
            (token-end (cdr (cdr token))))
        (if (or
             (equal token-type 'T_OPEN_TAG)
             (equal token-type 'T_DOC_COMMENT)
             (equal token-type 'T_COMMENT))
            (when previous-start
              (puthash
               token-start
               previous-start
               parser-tokens))
          (cond
           ((equal token-type 'T_CLOSE_TAG)
            (setq
             token
             `(";" ,token-start . ,token-end)))
           ((equal token-type 'T_OPEN_TAG_WITH_ECHO)
            (setq
             token
             `(T_ECHO ,token-start . ,token-end))))
          (puthash
           token-start
           token
           parser-tokens))

        (when (and
               previous-start
               (not
                (= previous-start token-end)))
          (puthash
           token-end
           previous-start
           parser-tokens))
        (setq
         previous-start
         token-start)))
    parser-tokens))

(defun phps-mode-lex-analyzer--lex-string (contents &optional start end states state state-stack heredoc-label heredoc-label-stack nest-location-stack tokens filename allow-cache-read allow-cache-write)
  "Run lexer on CONTENTS."
  ;; Create a separate buffer, run lexer inside of it, catch errors and return them
  ;; to enable nice presentation
  (require 'phps-mode-macros)

  (let ((loaded-from-cache)
        (cache-key))

    ;; Build cache key if possible
    (when (and
           phps-mode-cache--use-p
           filename)
      (setq
       cache-key
       filename)

      ;; Load cache if possible and permitted
      (when (and
             allow-cache-read
             (phps-mode-cache-test-p
              cache-key
              filename))
        (setq
         loaded-from-cache
         (phps-mode-cache-load
          cache-key))))

    (if loaded-from-cache
        loaded-from-cache
      (let* ((buffer
              (generate-new-buffer "*PHPs Lexer*"))
             (parse-error)
             (parse-trail)
             (ast-tree)
             (bookkeeping)
             (imenu))

        ;; Create temporary buffer and run lexer in it
        (when (get-buffer buffer)
          (with-current-buffer buffer
            (insert contents)

            (if tokens
                (setq
                 phps-mode-lexer--generated-tokens
                 (nreverse tokens))
              (setq
               phps-mode-lexer--generated-tokens
               nil))
            (if state
                (setq
                 phps-mode-lexer--state state)
              (setq
               phps-mode-lexer--state
               'ST_INITIAL))

            (setq
             phps-mode-lexer--states
             states)
            (setq
             phps-mode-lexer--state-stack
             state-stack)
            (setq
             phps-mode-lexer--heredoc-label
             heredoc-label)
            (setq
             phps-mode-lexer--heredoc-label-stack
             heredoc-label-stack)
            (setq
             phps-mode-lexer--nest-location-stack
             nest-location-stack)
            (unless end
              (setq end (point-max)))
            (unless start
              (setq start (point-min)))
            (setq-local
             phps-mode-lex-analyzer--lexer-index
             start)
            (setq-local
             phps-mode-lex-analyzer--lexer-max-index
             end)

            ;; Catch errors to kill generated buffer
            (let ((got-error t))
              (unwind-protect
                  ;; Run lexer or incremental lexer
                  (progn
                    (phps-mode-lex-analyzer--re2c-lex-analyzer)
                    (setq got-error nil))
                (when got-error
                  (kill-buffer))))

            ;; Copy variables outside of buffer
            (setq state phps-mode-lexer--state)
            (setq state-stack phps-mode-lexer--state-stack)
            (setq states phps-mode-lexer--states)

            ;; NOTE Generate parser tokens here before nreverse destructs list
            (setq
             phps-mode-parser-tokens
             (phps-mode-lex-analyzer--generate-parser-tokens
              phps-mode-lexer--generated-tokens))
            (setq tokens (nreverse phps-mode-lexer--generated-tokens))
            (setq heredoc-label phps-mode-lexer--heredoc-label)
            (setq heredoc-label-stack phps-mode-lexer--heredoc-label-stack)
            (setq nest-location-stack phps-mode-lexer--nest-location-stack)

            ;; Error-free parse here
            (condition-case conditions
                (progn
                  (phps-mode-ast--generate))
              (error
               (setq
                parse-error
                conditions)))

            ;; Need to copy buffer-local values before killing buffer
            (setq parse-trail phps-mode-ast--parse-trail)
            (setq ast-tree phps-mode-ast--tree)
            (setq bookkeeping phps-mode-parser-sdt-bookkeeping)
            (setq imenu phps-mode-parser-sdt-symbol-imenu)

            (kill-buffer)))

        (let ((data
               (list
                tokens
                states
                state
                state-stack
                heredoc-label
                heredoc-label-stack
                nest-location-stack
                parse-trail
                parse-error
                ast-tree
                bookkeeping
                imenu)))

          ;; Save cache if possible and permitted
          (when (and
                 phps-mode-cache--use-p
                 allow-cache-write
                 cache-key)
            ;; (message "Saved to cache")
            (phps-mode-cache-save
             data
             cache-key))

          data)))))

(provide 'phps-mode-lex-analyzer)

;;; phps-mode-lex-analyzer.el ends here
