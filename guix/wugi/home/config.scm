(define-module (wugi home config)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
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
  (local-file "connect"
              (string-append %home "/src/cgit.wugi.info/wigust/dotfiles/dot_local/bin/connect")))
