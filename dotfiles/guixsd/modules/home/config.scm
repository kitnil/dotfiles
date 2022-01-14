(define-module (home config)
  #:export (%home
            %ansible-state-directory
            %connect-program))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %ansible-state-directory
  (string-append %home "/ansible-out/files"))

(define %connect-program
  (string-append %home "/.local/bin/connect"))
