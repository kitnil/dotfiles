(use-modules (guix channels)
             (guix inferior)
             (guix profiles)
             (guix store)
             (srfi srfi-1))

(define channels
  (append (load "../channels-current.scm")
          (list (channel
                 (name 'guix-majordomo)
                 (url "https://cgit.duckdns.org/git/guix/guix-majordomo")))))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #t
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached))

(packages->manifest (list (first (lookup-inferior-packages inferior "guile-ihs"))))
