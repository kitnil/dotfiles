(define-module (wugi etc guix channels development)
  #:use-module (guix channels)
  #:export (%channels-development))

(define %channels-development
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "a667d499181157d5382b53a467af156196627e7b")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-development
