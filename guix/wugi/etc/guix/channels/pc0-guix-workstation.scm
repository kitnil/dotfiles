(define-module (wugi etc guix channels docker-image)
  #:use-module (guix channels)
  #:export (%channels-docker-image))

(define %channels-docker-image
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "looking-glass")
         (commit
          "60491edac81c73897a6a93c0b01f375e9a9b2477")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-docker-image
