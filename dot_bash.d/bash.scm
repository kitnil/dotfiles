(use-modules (gnu bash)
             (ice-9 format)
             (srfi srfi-41))

(define-bash-function (hello)
  (display "hello")
  (newline))

(define-bash-function (web)
  (format #t "~{web~a.intr~%~}"
          (delete 24 (stream->list (stream-range 15 38)))))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(add-to-load-path (string-append %home
                                 "/.local/share/chezmoi/dotfiles/guile"))

(define-bash-function (run-vm-guix)
  ((@ (bash) run-vm-guix)))
