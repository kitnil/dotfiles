(define-module (ai)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (branch "master")
         (commit
          "ac2d792aae241f5233ee3fdfa29cd3dbaeb9338c")
         (introduction
          (make-channel-introduction
           "9eb32716a99d7010dee9da70b0f8219ef2689a66"
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

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #f
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached #:error-port (current-error-port)))

(packages->manifest (map (lambda (package-name)
                           (first
                            (lookup-inferior-packages inferior package-name)))
                         '("aichat")))
