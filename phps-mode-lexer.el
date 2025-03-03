;;; phps-mode-lexer.el -- Lexer for PHPs -*- lexical-binding: t -*-

;; Copyright (C) 2018-2025  Free Software Foundation, Inc.


;;; Commentary:

;; The idea is gathering everything directly related to the lexer in this file,
;; any higher order meta-lexer logic goes into `phps-mode-lex-analyzer.el'.
;;
;; Features:
;; * Defines the lexer for this grammar based on the Zend PHP 8.3 Lexer at
;; https://raw.githubusercontent.com/php/php-src/PHP-8.3/Zend/zend_language_scanner.l
;; which is using re2c.

;;; Code:


(require 'phps-mode-macros)
(require 'phps-mode-lexer-generator)

(require 'subr-x)

(define-error
  'phps-lexer-error
  "PHPs Lexer Error")


;; INITIALIZE SETTINGS

(phps-mode-lexer--CG
 'parser-mode t)
(phps-mode-lexer--CG
 'short-tags t)


;; VARIABLES


(defvar phps-mode-lexer--lambdas-by-state
  (eval-when-compile (phps-mode-lexer-generator--lambdas))
  "Hash-table of lex-analyzer rules organized by state.")

(defvar-local phps-mode-lexer--cached nil
  "Hash-table of performed lexes to enable incremental lexing for the parser.")

(defvar-local phps-mode-lexer--cached-point nil
  "The point up to where the cache should be used.")


;; LEXER FUNCTIONS BELOW


;; If multiple rules match, re2c prefers the longest match.
;; If rules match the same string, the earlier rule has priority.
;; @see http://re2c.org/manual/syntax/syntax.html
(defun phps-mode-lexer--re2c (index old-state)
  "Elisp port of original Zend re2c lexer."

  (let ((cache-key index)
        (cache))
    ;; So if we have a cached lex, and we should use the cache for this point in the stream
    ;; make sure that it does not use a move operation or that the move operation is below
    ;; the valid point of the cache
    (when (and
           phps-mode-lexer--cached-point
           (<= index phps-mode-lexer--cached-point)
           phps-mode-lexer--cached)
      (setq
       cache
       (gethash cache-key phps-mode-lexer--cached)))
    (if (and
         cache
         (or
          (equal (nth 1 cache) nil)
          (<= (nth 1 cache) phps-mode-lexer--cached-point)))
        (progn
          (phps-mode-debug-message
           (message
            "\nReturning cached lex for key %S: %S"
            cache-key
            cache))
          cache)

      (let ((eof (>= index (point-max))))
        (if eof
            (progn
              (phps-mode-debug-message
               (message "Signal end of input at %S" index))
              (list nil nil old-state))

          ;; Set state here
          (let ((old-state-state (nth 0 old-state))
                (old-state-stack (nth 1 old-state))
                (old-state-heredoc-label (nth 2 old-state))
                (old-state-heredoc-label-stack (nth 3 old-state))
                (old-state-nest-location-stack (nth 4 old-state)))
            (setq
             phps-mode-lexer--state
             old-state-state)
            (setq
             phps-mode-lexer--state-stack
             old-state-stack)
            (setq
             phps-mode-lexer--heredoc-label
             old-state-heredoc-label)
            (setq
             phps-mode-lexer--heredoc-label-stack
             old-state-heredoc-label-stack)
            (setq
             phps-mode-lexer--nest-location-stack
             old-state-nest-location-stack))

          ;; Reset generated tokens
          (goto-char index)
          (setq
           phps-mode-lexer--generated-new-tokens
           nil)
          (setq
           phps-mode-lexer--generated-new-tokens-index
           index)

          (let ((tokens)
                (new-state)
                (move-to-index)
                (continue-lexer t))

            (while continue-lexer
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
                  "\nRunning lexer from point %s, state: %S, lookahead: '%s'.."
                  (point)
                  old-state
                  lookahead)))
              (setq phps-mode-lexer--move-flag nil)
              (setq phps-mode-lexer--restart-flag nil)
              (setq continue-lexer nil)

              ;; Run rules based on state
              (phps-mode-lexer--reset-match-data)
              (when-let ((lambdas
                          (gethash
                           phps-mode-lexer--state
                           phps-mode-lexer--lambdas-by-state)))
                (let ((lambda-i 0)
                      (lambda-length (length lambdas)))
                  (phps-mode-debug-message
                   (message
                    "Found %d lexer rules in state"
                    lambda-length))

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
                                   phps-mode-lexer--match-body (nth 1 lambd))
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

              (unless phps-mode-lexer--match-length
                (error "Failed to lex at %S" index))

              (phps-mode-debug-message
               (message
                "Found final match %s"
                phps-mode-lexer--match-body))
              (phps-mode-lexer--re2c-execute)

              (cond

               (phps-mode-lexer--move-flag
                (phps-mode-debug-message
                 (message
                  "Found move signal to %s"
                  phps-mode-lexer--move-flag))
                (setq
                 move-to-index
                 phps-mode-lexer--move-flag))

               (t
                (when phps-mode-lexer--restart-flag
                  (phps-mode-debug-message
                   (message "Found signal to restart lexer"))
                  (setq continue-lexer t)))

               )

              )

            (setq
             tokens
             phps-mode-lexer--generated-new-tokens)
            (setq
             new-state
             (list
              phps-mode-lexer--state
              phps-mode-lexer--state-stack
              phps-mode-lexer--heredoc-label
              phps-mode-lexer--heredoc-label-stack
              phps-mode-lexer--nest-location-stack))

            (unless phps-mode-lexer--cached
              (setq
               phps-mode-lexer--cached
               (make-hash-table :test 'equal)))

            (let ((lexer-response
                   (list tokens move-to-index new-state)))
              (phps-mode-debug-message
               (message
                "\nStored cached lex for key %S: %S"
                cache-key
                lexer-response))
              (puthash
               cache-key
               lexer-response
               phps-mode-lexer--cached)
              lexer-response)))))))

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


(provide 'phps-mode-lexer)

;;; phps-mode-lexer.el ends here
