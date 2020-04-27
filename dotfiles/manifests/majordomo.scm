(use-modules (guix channels)
             (guix inferior)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.duckdns.org/git/guix/guix")
         (commit "5b7a1cb077931a020c0b7e3b12f12a7bda221d96")) ;v1.0.1
        (channel
         (name 'guix-majordomo)
         (url "https://cgit.duckdns.org/git/guix/guix-majordomo"))))

(define inferior
  (inferior-for-channels channels))

(packages->manifest (lookup-inferior-packages inferior "guile-ihs"))
