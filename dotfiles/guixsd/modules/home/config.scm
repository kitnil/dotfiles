(define-module (home config)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:export (%home
            %ansible-state-directory
            %connect-program
            %project-directory))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define %ansible-state-directory
  (string-append %home "/ansible-out/files"))

(define %connect-program
  (local-file "connect"
              (string-append %home "/.local/share/chezmoi/dot_local/bin/executable_connect")))

(define %project-directory
  (string-append %home "/.local/share/chezmoi"))
