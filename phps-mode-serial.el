;;; phps-mode-serial.el --- Functions for synchronity -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;;; Commentary:


;;; Code:


;; VARIABLES

(defvar phps-mode-serial--async-processes (make-hash-table :test 'equal)
  "Table of active asynchronous processes.")

(defvar phps-mode-serial--async-threads (make-hash-table :test 'equal)
  "Table of active asynchronous threads.")


;; FUNCTIONS


(defun phps-mode-serial-commands--kill-active (key)
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

(defun phps-mode-serial-commands (key start end &optional async async-by-process)
  "Run command with KEY, first START and if successfully then END with the result of START as argument.  Optional arguments ASYNC ASYNC-BY-PROCESS specifies additional opions."
  (let ((start-time (current-time)))
    (message "PHPs - Starting serial commands for buffer '%s'.." key)
    (phps-mode-serial-commands--kill-active key)
    (if async
        (if async-by-process
            (progn
              (unless (fboundp 'async-start)
                (signal 'error (list "Async-start function is missing")))

              ;; Run command(s) asynchronously
              (let ((script-filename
                     (file-name-directory
                      (symbol-file 'phps-mode))))
                (puthash
                 key
                 (async-start
                  (lambda()
                    (add-to-list 'load-path script-filename)
                    (require 'phps-mode)

                    ;; Execute start lambda
                    (condition-case conditions
                        (progn
                          (let ((start-return (funcall start)))
                            (list 'success start-return start-time)))
                      (error (list 'error conditions start-time))))
                  (lambda (start-return)
                    (let ((status (car start-return))
                          (value (car (cdr start-return)))
                          (start-time (car (cdr (cdr start-return))))
                          (end-return nil))

                      ;; Profile execution in debug mode
                      (let* ((end-time (current-time))
                             (end-time-float
                              (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                             (start-time-float
                              (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                             (elapsed (- end-time-float start-time-float)))
                        (message "Serial asynchronous process start finished, elapsed: %fs" elapsed))

                      (cond
                       ((string= status "success")

                        ;; Execute end lambda
                        (condition-case conditions
                            (progn
                              (let ((return (funcall end value)))
                                (setq end-return (list 'success return start-time))))
                          (error (setq end-return (list 'error conditions start-time))))

                        ;; Profile execution in debug mode
                        (let* ((end-time (current-time))
                               (end-time-float
                                (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                               (start-time-float
                                (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                               (elapsed (- end-time-float start-time-float)))
                          (message "Serial synchronous thread finished, elapsed: %fs" elapsed))

                        (let ((status (car end-return))
                              (value (cdr end-return)))

                          (when (string= status "error")
                            (display-warning 'phps-mode (format "%s" (car value))))))
                       ((string= status "error")
                        (display-warning 'phps-mode (format "%s" (car value))))))))
                 phps-mode-serial--async-processes)))

          ;; Run command(s) asynchronously
          (puthash
           key
           (make-thread
            (lambda()
              (let ((start-return)
                    (end-return))

                ;; First execute start lambda
                (condition-case conditions
                    (let ((return (funcall start)))
                      (setq start-return (list 'success return start-time)))
                  (error (setq start-return (list 'error conditions start-time))))

                ;; Profile execution in debug mode
                (let* ((end-time (current-time))
                       (end-time-float
                        (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                       (start-time-float
                        (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                       (elapsed (- end-time-float start-time-float)))
                  (message "Serial asynchronous thread start finished, elapsed: %fs" elapsed))

                ;; (setq start-time (current-time))

                (let ((status (car start-return))
                      (value (car (cdr start-return)))
                      (start-time (car (cdr (cdr start-return)))))

                  (when (string= status "success")
                    
                    ;; Then execute end lambda
                    (condition-case conditions
                        (let ((return (funcall end value)))
                          (setq end-return (list 'success return start-time)))
                      (error (setq end-return (list 'error conditions start-time))))

                    ;; Profile execution
                    (let* ((end-time (current-time))
                           (end-time-float
                            (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                           (start-time-float
                            (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                           (elapsed (- end-time-float start-time-float)))
                      (message "Serial asynchronous thread end finished, elapsed: %fs" elapsed))

                    (let ((status (car end-return))
                          (value (car (cdr end-return))))

                      (when (string= status "error")
                        (display-warning 'phps-mode (format "%s" (car value)))))

                    (when (string= status "error")
                      (display-warning 'phps-mode (format "%s" (car value))))))))
            key)
           phps-mode-serial--async-threads))

      (let ((start-return)
            (end-return))

        ;; Run start and catch potential errors
        (condition-case conditions
            (progn
              (let ((return (funcall start)))
                (setq start-return (list 'success return start-time))))
          (error (setq start-return (list 'error conditions start-time))))

        ;; Profile execution in debug mode
        (let* ((end-time (current-time))
               (end-time-float
                (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
               (start-time-float
                (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
               (elapsed (- end-time-float start-time-float)))
          (message "Serial synchronous thread start finished, elapsed: %fs" elapsed))

        (let ((status (car start-return))
              (value (car (cdr start-return)))
              (start-time (car (cdr (cdr start-return)))))

          ;; (message "Return: %s" start-return)

          (when (string= status "success")

            ;; (setq start-time (current-time))

            ;; Then execute end lambda
            (condition-case conditions
                (let ((return (funcall end value)))
                  (setq end-return (list 'success return start-time)))
              (error (setq end-return (list 'error conditions start-time))))

            ;; Profile execution in debug mode
            (let* ((end-time (current-time))
                   (end-time-float
                    (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                   (start-time-float
                    (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                   (elapsed (- end-time-float start-time-float)))
              (message "Serial synchronous thread end finished, elapsed: %fs" elapsed))

            (let ((status (car end-return))
                  (value (car (cdr end-return))))

              ;; (message "End-status: '%s' value: '%s'" status value)

              (when (string= status "error")
                (display-warning 'phps-mode (format "%s" (car value))))))

          (when (string= status "error")
            (display-warning 'phps-mode (format "%s" (car value)))))))))


(provide 'phps-mode-serial)
;;; phps-mode-serial.el ends here
