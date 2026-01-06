(define-module (wugi etc guix channels pc0)
  #:use-module (guix channels)
  #:export (%channels-pc0))

(define %channels-pc0
  (list (channel
         (name 'guix)
         (url "https://codeberg.org/guix/guix")
         (branch "master")
         (commit
          "6913fd701884fad3af579efdc298ae444aeaf57c")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (commit
          "bd8c5ca23e694be0b1a8cf72f05f81c70b1d7fc5")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

%channels-pc0
