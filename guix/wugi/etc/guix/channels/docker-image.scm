(define-module (wugi etc guix channels docker-image)
  #:use-module (guix channels)
  #:export (%channels-docker-image))

(define %channels-docker-image
  (list (channel
          (name 'guix)
          (url "https://cgit.wugi.info/git/guix/guix")
          (branch "master")
          (commit
           "837f3e55190f752ae41cf6af74444b33aa688071")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-docker-image
