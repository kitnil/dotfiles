(define-module (wugi etc guix channels telegram)
  #:use-module (guix channels)
  #:export (%channels-telegram))

(define %channels-telegram
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (commit "b53fac227836bcec3a2a7a44f7720b9a23db90f2")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"     ;2020-05-26
           (openpgp-fingerprint                           ;mbakke
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'johnlepikhin)
         (url "https://cgit.wugi.info/git/guix/johnlepikhin-guix-channel.git")
         (branch "master")
         (commit "d69046922cd4a9d924e08ac31ed49a412951f8d4")
         (introduction
          (make-channel-introduction
           "487be061769c7ffc7b28e886aa3d276f82d2c2cc"
           (openpgp-fingerprint
            "7238 7123 8EAC EB63 4548  5857 167F 8EA5 001A FA9C"))))))

%channels-telegram
