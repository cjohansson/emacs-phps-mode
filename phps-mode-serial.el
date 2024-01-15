;;; phps-mode-serial.el --- Functions for serial commands with varied synchronicity -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024  Free Software Foundation, Inc.

;;; Commentary:


;;; Code:

;; VARIABLES


(defvar phps-mode-serial--async-processes (make-hash-table :test 'equal)
  "Table of active asynchronous processes.")

(defvar phps-mode-serial--async-threads (make-hash-table :test 'equal)
  "Table of active asynchronous threads.")

(defvar phps-mode-serial--profiling nil
  "Boolean flag whether to profile or not.")

(defface phps-mode-serial--mode-line-face-running
  '((t :inherit warning))
  "Face for PHPs mode \"running\" asynchronous process mode line indicator."
  :group 'phps-mode)

(defface phps-mode-serial--mode-line-face-success
  '((t :inherit success))
  "Face for PHPs mode \"success\" asynchronous process mode line indicator."
  :group 'phps-mode)

(defface phps-mode-serial--mode-line-face-error
  '((t :inherit error))
  "Face for PHPs mode \"error\" asynchronous process mode line indicator."
  :group 'phps-mode)

(defvar-local phps-mode-serial--status nil
  "Current status of serial-commands.")

(defconst phps-mode-serial--mode-line-status-run
  '(":"
    (:propertize (:eval (if (equal phps-mode-serial--status 'running) "Running.." ""))
                 face phps-mode-serial--mode-line-face-running)
    (:propertize (:eval (if (equal phps-mode-serial--status 'error) "Error" ""))
                 face phps-mode-serial--mode-line-face-error)
    (:propertize (:eval (if (equal phps-mode-serial--status 'success) "OK" ""))
                 face phps-mode-serial--mode-line-face-success)
    (:propertize (:eval (if (equal phps-mode-serial--status 'aborted) "Aborted" ""))
                 face phps-mode-serial--mode-line-face-error)))


;; FUNCTIONS


(defun phps-mode-serial--kill-active (key)
  "Kill active command KEY."
  (when (and
         (gethash key phps-mode-serial--async-processes)
         (process-live-p (gethash key phps-mode-serial--async-processes)))
    (let ((process-buffer (process-buffer (gethash key phps-mode-serial--async-processes))))
      (delete-process (gethash key phps-mode-serial--async-processes))
      (kill-buffer process-buffer)))
  (when (and
         (gethash key phps-mode-serial--async-threads)
         (thread-live-p (gethash key phps-mode-serial--async-threads)))
    (thread-signal (gethash key phps-mode-serial--async-threads) 'quit nil)))

(defun phps-mode-serial-commands (key start end &optional start-error end-error async async-by-process)
  "Run command with KEY, first START and if successfully then END with the
result of START as argument.  Optional arguments START-ERROR, END-ERROR that are
called on errors. ASYNC ASYNC-BY-PROCESS specifies additional options
for synchronicity."
  (let ((start-time (current-time)))
    (when phps-mode-serial--profiling
      (message "PHPs - Starting serial commands for buffer '%s'.." key))
    (with-current-buffer key
      (setq mode-line-process phps-mode-serial--mode-line-status-run)
      (setq phps-mode-serial--status 'running))
    (phps-mode-serial--kill-active key)
    (if async
        (if async-by-process
            (if (fboundp 'async-start)
                ;; Run command(s) asynchronously
                (let ((script-filename
                       (file-name-directory
                        (symbol-file 'phps-mode))))
                  (puthash

                   key

                   (async-start

                    (lambda()
                      (let ((quitted t)
                            (start-return))
                        (unwind-protect
                            (progn
                              (add-to-list 'load-path script-filename)
                              (require 'phps-mode)

                              ;; Execute start lambda
                              (condition-case conditions
                                  (let ((return (funcall start)))
                                    (setq
                                     start-return
                                     (list 'success return start-time)))
                                (error
                                 (setq
                                  start-return
                                  (list 'error conditions start-time))))
                              (setq quitted nil))
                          (when quitted
                            (setq
                             start-return
                             (list 'error 'quitted))))
                        start-return))

                    (lambda (start-return)
                      (let ((start-status (car start-return))
                            (start-value (car (cdr start-return)))
                            (start-time (car (cdr (cdr start-return))))
                            (end-return)
                            (quitted t))
                        (unwind-protect
                            (progn

                              ;; Profile execution in debug mode
                              (when phps-mode-serial--profiling
                                (let* ((end-time (current-time))
                                       (end-time-float
                                        (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                                       (start-time-float
                                        (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                                       (elapsed (- end-time-float start-time-float)))
                                  (message "Serial asynchronous process start finished, elapsed: %fs" elapsed)))

                              (if (string= start-status "success")
                                  (progn

                                    ;; Execute end lambda
                                    (condition-case conditions
                                        (progn
                                          (let ((return (funcall end start-value)))
                                            (setq
                                             end-return
                                             (list 'success return start-time))))
                                      (error
                                       (setq
                                        end-return
                                        (list 'error conditions start-time))))

                                    ;; Profile execution in debug mode
                                    (when phps-mode-serial--profiling
                                      (let* ((end-time (current-time))
                                             (end-time-float
                                              (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                                             (start-time-float
                                              (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                                             (elapsed (- end-time-float start-time-float)))
                                        (message "Serial synchronous thread finished, elapsed: %fs" elapsed)))

                                    (let ((end-status (car end-return))
                                          (end-value (cdr end-return)))

                                      (when (string= end-status "success")
                                        (with-current-buffer key
                                          (setq phps-mode-serial--status 'success)))

                                      (when (string= end-status "error")
                                        (with-current-buffer key
                                          (setq phps-mode-serial--status 'error))
                                        (when end-error
                                          (funcall end-error end-value)))))

                                (when (string= start-status "error")
                                  (with-current-buffer key
                                    (setq phps-mode-serial--status 'error))
                                  (when start-error
                                    (funcall start-error start-value))))

                              (setq quitted nil))
                          (when quitted
                            (with-current-buffer key
                              (setq phps-mode-serial--status 'aborted))))

                        end-return)))

                   phps-mode-serial--async-processes))
              (signal 'error (list "Async-start function is missing")))

          ;; Run command(s) asynchronously
          (let ((async-thread
                 (make-thread

                  (lambda()
                    (let ((quitted t)
                          (start-return))
                      (unwind-protect
                          (progn

                            ;; First execute start lambda
                            (condition-case conditions
                                (let ((return (funcall start)))
                                  (setq
                                   start-return
                                   (list 'success return start-time)))
                              (error
                               (setq
                                start-return
                                (list 'error conditions start-time))))

                            ;; Profile execution in debug mode
                            (when phps-mode-serial--profiling
                              (let* ((end-time (current-time))
                                     (end-time-float
                                      (+
                                       (car end-time)
                                       (car (cdr end-time))
                                       (* (car (cdr (cdr end-time))) 0.000001)))
                                     (start-time-float
                                      (+
                                       (car start-time)
                                       (car (cdr start-time))
                                       (* (car (cdr (cdr start-time))) 0.000001)))
                                     (elapsed (- end-time-float start-time-float)))
                                (message
                                 "Serial asynchronous thread start finished, elapsed: %fs"
                                 elapsed)))

                            (setq quitted nil))
                        (progn
                          (when quitted
                            (with-current-buffer key
                              (setq phps-mode-serial--status 'aborted)))))
                      start-return))

                  key)))
            (puthash
             key
             async-thread
             phps-mode-serial--async-threads)

            (make-thread

             (lambda()
               (let ((quitted t))
                 (unwind-protect
                     (let ((start-return (thread-join async-thread))
                           (end-return))
                       (let ((start-status (car start-return))
                             (start-value (car (cdr start-return)))
                             (start-time (car (cdr (cdr start-return)))))

                         (if (string= start-status "success")
                             (progn

                               ;; Then execute end lambda
                               (condition-case conditions
                                   (let ((return (funcall end start-value)))
                                     (setq
                                      end-return
                                      (list 'success return start-time)))
                                 (error
                                  (setq
                                   end-return
                                   (list 'error conditions start-time))))

                               ;; Profile execution
                               (when phps-mode-serial--profiling
                                 (let* ((end-time (current-time))
                                        (end-time-float
                                         (+
                                          (car end-time)
                                          (car (cdr end-time))
                                          (* (car (cdr (cdr end-time))) 0.000001)))
                                        (start-time-float
                                         (+
                                          (car start-time)
                                          (car (cdr start-time))
                                          (* (car (cdr (cdr start-time))) 0.000001)))
                                        (elapsed (- end-time-float start-time-float)))
                                   (message
                                    "Serial asynchronous thread end finished, elapsed: %fs"
                                    elapsed)))

                               (let ((end-status (car end-return))
                                     (end-value (car (cdr end-return))))

                                 (when (string= end-status "success")
                                   (with-current-buffer key
                                     (setq phps-mode-serial--status 'success)))

                                 (when (string= end-status "error")
                                   (with-current-buffer key
                                     (setq phps-mode-serial--status 'error))
                                   (when end-error
                                     (funcall end-error end-value)))))

                           (when (string= start-status "error")
                             (with-current-buffer key
                               (setq phps-mode-serial--status 'error))
                             (when start-error
                               (funcall start-error start-value)))))
                       (setq quitted nil))
                   (when quitted
                     (with-current-buffer key
                       (setq phps-mode-serial--status 'aborted)))))))))

      (let ((start-return)
            (end-return)
            (quitted t))
        (unwind-protect
            (progn

              ;; Run start and catch potential errors
              (condition-case conditions
                  (progn
                    (let ((return (funcall start)))
                      (setq start-return (list 'success return start-time))))
                (error (setq start-return (list 'error conditions start-time))))

              ;; Profile execution in debug mode
              (when phps-mode-serial--profiling
                (let* ((end-time (current-time))
                       (end-time-float
                        (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                       (start-time-float
                        (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                       (elapsed (- end-time-float start-time-float)))
                  (message "Serial synchronous thread start finished, elapsed: %fs" elapsed)))

              (let ((start-status (car start-return))
                    (start-value (car (cdr start-return)))
                    (start-time (car (cdr (cdr start-return)))))

                (if (string= start-status "success")
                    (progn

                      ;; Then execute end lambda
                      (condition-case conditions
                          (let ((return (funcall end start-value)))
                            (setq end-return (list 'success return start-time)))
                        (error (setq end-return (list 'error conditions start-time))))

                      ;; Profile execution in debug mode
                      (when phps-mode-serial--profiling
                        (let* ((end-time (current-time))
                               (end-time-float
                                (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                               (start-time-float
                                (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                               (elapsed (- end-time-float start-time-float)))
                          (message "Serial synchronous thread end finished, elapsed: %fs" elapsed)))

                      (let ((end-status (car end-return))
                            (end-value (car (cdr end-return))))

                        (when (string= end-status "success")
                          (with-current-buffer key
                            (setq phps-mode-serial--status 'success)))

                        (when (string= end-status "error")
                          (with-current-buffer key
                            (setq phps-mode-serial--status 'error))
                          (when end-error
                            (funcall end-error end-value)))))

                  (when (string= start-status "error")
                    (with-current-buffer key
                      (setq phps-mode-serial--status 'error))
                    (when start-error
                      (funcall start-error start-value)))))
              (setq quitted nil))
          (when quitted
            (with-current-buffer key
              (setq phps-mode-serial--status 'aborted))))
        end-return))))

(provide 'phps-mode-serial)
;;; phps-mode-serial.el ends here
