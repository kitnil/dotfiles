(define-module (telegram)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (commit "b53fac227836bcec3a2a7a44f7720b9a23db90f2")
         (introduction (@@ (guix channels) %guix-channel-introduction)))
        (channel
         (name 'johnlepikhin)
         (url "https://cgit.wugi.info/git/guix/johnlepikhin-guix-channel.git")
         (branch "master")
         (commit "d254b40b5edb9130629dda43f4b7ae74b26fdc41"))))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #t
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached #:error-port (current-error-port)))

(define-public telegram-desktop
  (first (lookup-inferior-packages inferior "telegram-desktop")))

(packages->manifest (list telegram-desktop))
