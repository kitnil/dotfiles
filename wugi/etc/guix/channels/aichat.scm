(define-module (wugi etc guix channels aichat)
  #:use-module (guix channels)
  #:export (%channels-aichat))

(define %channels-aichat
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "ac2d792aae241f5233ee3fdfa29cd3dbaeb9338c")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'nonguix)
         (url "https://cgit.wugi.info/git/guix/nonguix")
         (branch "aichat")
         (commit
          "3a18ed101fe2a45e75e79bbabb918f75f9a63a42")
         (introduction
          (make-channel-introduction
           "05ff83f530c8709e7374c6930f0c09b7613225a5"
           (openpgp-fingerprint
            "7238 7123 8EAC EB63 4548  5857 167F 8EA5 001A FA9C"))))))

%channels-aichat
