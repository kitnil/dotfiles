(define-module (wugi manifests majordomo)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (%majordomo-manifest))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
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

(define (%majordomo-manifest)
  (define inferior
    (inferior-for-channels channels))

  (packages->manifest
   (list (first (lookup-inferior-packages inferior "guile-ihs")))))
