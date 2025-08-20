(define-module (wugi etc guix channels development)
  #:use-module (guix channels)
  #:export (%channels-development))

(define %channels-development
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "a440ce45676eb96b3cfbcc01a498345e3905b769")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-development
