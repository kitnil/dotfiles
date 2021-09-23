(define-module (guile config))

(define %home
  (and=> (getenv "HOME")
         (lambda (home)
           home)))

(define-public %vault
  (string-append %home "/.nix-profile/bin/vault"))
