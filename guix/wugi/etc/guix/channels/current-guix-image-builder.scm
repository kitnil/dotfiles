(define-module (wugi etc guix channels current-guix-image-builder)
  #:use-module (guix channels)
  #:export (%channels-current-guix-image-builder))

(define %channels-current-guix-image-builder
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "9eb32716a99d7010dee9da70b0f8219ef2689a66")
         (introduction
          (make-channel-introduction
           "9eb32716a99d7010dee9da70b0f8219ef2689a66"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

%channels-current-guix-image-builder
