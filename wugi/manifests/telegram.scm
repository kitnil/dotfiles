(define-module (wugi manifests telegram)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (%telegram-manifest))

(define channels
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
         (commit "d254b40b5edb9130629dda43f4b7ae74b26fdc41"))))

(define (%telegram-manifest)
  (define inferior
    (inferior-for-channels channels))

  (define telegram-desktop
    (first (lookup-inferior-packages inferior "telegram-desktop")))

  (packages->manifest (list telegram-desktop)))
