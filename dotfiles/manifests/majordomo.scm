(use-modules (guix channels)
             (guix inferior)
             (guix profiles)
             (guix store)
             (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit
          "1f734a6f0a7db5b0e12091a0c869c5c4810ac80e")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'guix-majordomo)
         (url "https://github.com/6d6a/package-management-guix-majordomo.git"))))

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
