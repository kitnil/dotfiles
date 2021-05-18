(use-modules (gnu bash)
             (ice-9 format)
             (srfi srfi-41))

(define-bash-function (hello)
  (display "hello")
  (newline))

(define-bash-function (mjru-web)
  (format #t "~{~a~%~}" (stream->list (stream-range 0 10))))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(add-to-load-path (string-append %home
                                 "/.local/share/chezmoi/dotfiles/guile"))

(define-bash-function (run-vm-guix)
  ((@ (bash) run-vm-guix)))
