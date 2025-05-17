#!/run/current-system/profile/bin/guile \
--no-auto-compile -e (lint-commit) -s
!#

(define-module (lint-commit)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (rx irregex)
  #:use-module (srfi srfi-1)
  #:export (main))

(define (match-subject subject)
  (irregex-match '(seq (seq (or "gnu" "services") ":" space
                            (or (seq (+ alphanumeric) ":" space "Update to" space (+ (or alphanumeric ".")))
                                (seq "Add" space (+ (or alphanumeric "-")))))
                       ".")
                 subject))

(define (format-match procedure string)
  (if (match-subject string)
      (format #t "[ PASS ] ~s~%" string)
      (begin (format #t "[ FAIL ] ~s~%" string)
             (exit 1))))

(define (main args)
  (define input (with-input-from-port (current-input-port) read-string))
  (match (string-split input #\newline)
    ((subject message ...)
     (format-match match-subject subject))))
