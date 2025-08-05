(define-module (wugi etc guix channels workstation)
  #:use-module (guix channels)
  #:export (%channels-workstation))

(define %channels-workstation
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "4bfeff64b6e3c85a77a79c29fdb434e5a76b5b28")
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
          "e899121adbaa13cfcaeae7a5c24921bffa645771")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

%channels-workstation
