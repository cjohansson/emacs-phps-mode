;;; phps-mode-serial.el --- Functions for synchronity -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020  Free Software Foundation, Inc.

;;; Commentary:


;;; Code:


;; TODO Need to fix error reporting in synchronous mode
;; TODO Need to add support for format buffer when using asynchronous processes
(defun phps-mode-serial-commands (key start end &optional async async-by-process profiling)
  "Run command with KEY, first START and if successfully then END with the result of START as argument.  Optional arguments ASYNC ASYNC-BY-PROCESS and PROFILING specifies additional opions."
  (let ((start-time (current-time)))
    (if (and async
             (boundp 'phps-mode-async-processes)
             (boundp 'phps-mode-async-threads))
        (if async-by-process
            (progn
              (require 'async)

              ;; Kill async process if process with associated key already exists
              (when (and
                     (gethash key phps-mode-async-processes)
                     (process-live-p (gethash key phps-mode-async-processes)))
                (let ((process-buffer (process-buffer (gethash key phps-mode-async-processes))))
                  (delete-process (gethash key phps-mode-async-processes))
                  (kill-buffer process-buffer)))

              ;; Run command(s) asynchronously
              (let ((script-filename
                     (file-name-directory
                      (symbol-file 'phps-mode-lexer-BEGIN))))
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
                      (when profiling
                        (let* ((end-time (current-time))
                               (end-time-float
                                (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                               (start-time-float
                                (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                               (elapsed (- end-time-float start-time-float)))
                          (message "Asynchronous serial start command using async.el finished, elapsed: %fs" elapsed)))

                      (setq start-time (current-time))

                      ;; (message "Running end code with status %s start-time: %s" status start-time)
                      (cond
                       ((string= status "success")
                        ;; (message "Running end code %s with argument: %s" end value)

                        ;; Execute end lambda
                        (condition-case conditions
                            (progn
                              (let ((return (funcall end value)))
                                (setq end-return (list 'success return start-time))))
                          (error (setq end-return (list 'error conditions start-time))))

                        ;; Profile execution in debug mode
                        (when profiling
                          (let* ((end-time (current-time))
                                 (end-time-float
                                  (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                                 (start-time-float
                                  (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                                 (elapsed (- end-time-float start-time-float)))
                            (message "Synchronous serial (end) command using async.el finished, elapsed: %fs" elapsed)))

                        (let ((status (car end-return))
                              (value (car (cdr end-return)))
                              (start-time (car (cdr (cdr end-return)))))

                          (when (string= status "error")
                            (display-warning 'phps-mode (format "%s" (car (cdr value)))))))
                       ((string= status "error")
                        (display-warning 'phps-mode (format "%s" (car (cdr value)))))))))
                 phps-mode-async-processes))

              ;; (message "Done running serial command asynchronously using async.el")
              )

          ;; Kill thread if thread with associated key already exists
          (when (and
                 (gethash key phps-mode-async-threads)
                 (thread-live-p (gethash key phps-mode-async-threads)))
            (thread-signal (gethash key phps-mode-async-threads) 'quit nil))

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
                (when profiling
                  (let* ((end-time (current-time))
                         (end-time-float
                          (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                         (start-time-float
                          (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                         (elapsed (- end-time-float start-time-float)))
                    (message "Asynchronous serial start command using thread finished, elapsed: %fs" elapsed)))

                (setq start-time (current-time))

                (let ((status (car start-return))
                      (value (car (cdr start-return)))
                      (start-time (car (cdr (cdr start-return)))))
                  ;; (message "Start-status: '%s' value: '%s'" status value)

                  (when (string= status "success")
                    
                    ;; Then execute end lambda
                    (condition-case conditions
                        (let ((return (funcall end value)))
                          (setq end-return (list 'success return start-time)))
                      (error (setq end-return (list 'error conditions start-time))))

                    ;; Profile execution in debug mode
                    (when profiling
                      (let* ((end-time (current-time))
                             (end-time-float
                              (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                             (start-time-float
                              (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                             (elapsed (- end-time-float start-time-float)))
                        (message "Asynchronous serial end command using thread finished, elapsed: %fs" elapsed)))

                    ;; (message "End return: %s" end-return)

                    (let ((status (car end-return))
                          (value (car (cdr end-return)))
                          (start-time (car (cdr (cdr end-return)))))

                      ;; (message "End-status: '%s' value: '%s'" status value)

                      (when (string= status "error")
                        ;; (display-warning 'phps-mode (format "Async thread (end) error: %s" (car (cdr value))) :debug)
                        (message "PHPs: Async thread (end) error: %s" (car (cdr value))))))

                  (when (string= status "error")
                    ;; (message "Start-Error: %s" (car (cdr value)))
                    ;; NOTE In the future when (display-warning) inside a (make-thread) does not crash Emacs on macOS we should use (display-warning) instead
                    (message "PHPs: Async thread (start) error: %s" (car (cdr value)))
                    ;; (display-warning 'phps-mode (format "Async thread (start) error: %s" (car (cdr value))) :debug)
                    ))))
            key)
           phps-mode-async-threads))

      (let ((start-return)
            (end-return))

        ;; Run start and catch potential errors
        (condition-case conditions
            (progn
              (let ((return (funcall start)))
                (setq start-return (list 'success return))))
          (error (setq start-return (list 'error conditions start-time))))

        ;; Profile execution in debug mode
        (when profiling
          (let* ((end-time (current-time))
                 (end-time-float
                  (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                 (start-time-float
                  (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                 (elapsed (- end-time-float start-time-float)))
            (message "Synchronous serial start finished, elapsed: %fs" elapsed)))

        (let ((status (car start-return))
              (value (car (cdr start-return)))
              (start-time (car (cdr (cdr start-return)))))

          (when (string= status "success")

            (setq start-time (current-time))

            ;; Then execute end lambda
            (condition-case conditions
                (let ((return (funcall end value)))
                  (setq end-return (list 'success return start-time)))
              (error (setq end-return (list 'error conditions start-time))))

            ;; Profile execution in debug mode
            (when profiling
              (let* ((end-time (current-time))
                     (end-time-float
                      (+ (car end-time) (car (cdr end-time)) (* (car (cdr (cdr end-time))) 0.000001)))
                     (start-time-float
                      (+ (car start-time) (car (cdr start-time)) (* (car (cdr (cdr start-time))) 0.000001)))
                     (elapsed (- end-time-float start-time-float)))
                (message "Synchronous serial end finished, elapsed: %fs" elapsed)))

            (let ((status (car end-return))
                  (value (car (cdr end-return)))
                  (start-time (car (cdr (cdr end-return)))))

              ;; (message "End-status: '%s' value: '%s'" status value)

              (when (string= status "error")
                (display-warning 'phps-mode (format "Synchronous thread (end) error: %s" (car (cdr value)))))))

          (when (string= status "error")
            (display-warning 'phps-mode (format "Synchronous thread (start) error: %s" (car (cdr value))))))))))


(provide 'phps-mode-serial)
;;; phps-mode-serial.el ends here
