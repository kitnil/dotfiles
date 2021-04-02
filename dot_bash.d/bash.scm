(use-modules (gnu bash)
             (ice-9 format)
             (srfi srfi-41))

(define-bash-function (hello)
  (display "hello")
  (newline))

(define-bash-function (mjru-web)
  (format #t "~{~a~%~}" (stream->list (stream-range 0 10))))
