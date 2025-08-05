(define-module (wugi etc guix channels firejail)
  #:use-module (guix channels)
  #:export (%channels-firejail))

(define %channels-firejail
  (list (channel
          (name 'guix)
          (url "https://cgit.wugi.info/git/guix/guix")
          (branch "master")
          (commit
           "67667c4cb910e11552c701d17f2167db08884608")
          (introduction
           (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
             "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-firejail
