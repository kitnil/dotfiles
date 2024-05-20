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
         (url "https://github.com/johnlepikhin/guix-channel")
         (branch "master")
         (commit "a987b6ba01cf09aacc2f0f6df9b252a141b60691"))))

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
