#!/usr/bin/env -S guile --no-auto-compile -e main -s
!#

;; ssh-keyscan.scm example1.org example2org

(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1))

(define (main . args)
  (match (first args)
    ((command args ...)
     (for-each (lambda (host)
                 (let ((var host))
                   ((@@ (ice-9 pretty-print) pretty-print) var)
                   var))
               (apply append
                      (map (lambda (arg)
                             (let* ((port (open-pipe* OPEN_READ "ssh-keyscan" arg))
                                    (output (read-string port)))
                               (close-port port)
                               (map (lambda (host)
                                      (match (string-split host #\space)
                                        ((host public-key-type public-key)
                                         `(plain-file ,host
                                                      ,(string-join (list host public-key-type public-key))))))
                                    (string-split (string-trim-right output #\newline)
                                                  #\newline))))
                           args))))))
