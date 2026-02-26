(define-module (wugi etc guix channels pc0)
  #:use-module (guix channels)
  #:export (%channels-pc0))

(define %channels-pc0
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "75001089b5b43342c3e784d7ca2c42eade953211")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'nonguix)
         (url "https://cgit.wugi.info/git/guix/nonguix")
         (branch "master")
         (commit
          "da4e72efef62d48dbc2eb089c36972ff55fe6acd")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

%channels-pc0
