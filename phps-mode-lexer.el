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

(phps-mode-lexer--CG
 'parser-mode t)
(phps-mode-lexer--CG
 'short-tags t)


;; VARIABLES


(defvar phps-mode-lexer--lambdas-by-state
  (eval-when-compile (phps-mode-lexer-generator--lambdas))
  "Hash-table of lex-analyzer rules organized by state.")


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
