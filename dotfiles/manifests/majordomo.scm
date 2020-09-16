(use-modules (guix channels)
             (guix inferior)
             (guix profiles)
             (guix store)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (introduction (@@ (guix channels) %guix-channel-introduction))
         (commit "5b7a1cb077931a020c0b7e3b12f12a7bda221d96")) ;v1.0.1
        (channel
         (name 'guix-majordomo)
         (url "https://cgit.duckdns.org/git/guix/guix-majordomo"))))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #f
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached))

(packages->manifest (lookup-inferior-packages inferior "guile-ihs"))
