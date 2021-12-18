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
         (commit "f7e14782025bf87aaef694a21f34010b1a95f7f6")) ;v1.0.1
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (commit "7d824b5780558288881602a7a3cf8b0f56ebc08c"))))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #t
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached))

(packages->manifest (list (first (lookup-inferior-packages inferior "firefox"))
                          (first (lookup-inferior-packages inferior "openssh"))))
