(define-module (deprecated)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1))

(define channels
  (list (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (introduction (@@ (guix channels) %guix-channel-introduction))
         (commit "f7e14782025bf87aaef694a21f34010b1a95f7f6")) ;v1.0.1
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (commit "7d824b5780558288881602a7a3cf8b0f56ebc08c"))
        (channel
         (name 'guix-wigust)
         (url "https://github.com/kitnil/guix-wigust")
         (branch "master")
         (commit "68340baa8cdc1af1b8a8982c2607ff1dda195ee7"))))

(define cached
  (with-store store
    (cached-channel-instance store
                             channels
                             #:authenticate? #t
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30))))

(define inferior
  (open-inferior cached))

(define-public firefox
  (first (lookup-inferior-packages inferior "firefox")))

(define-public openssh
  (first (lookup-inferior-packages inferior "openssh")))

(define-public tigervnc-client
  (last (lookup-inferior-packages inferior "tigervnc-client")))

(define-public tigervnc-server
  (last (lookup-inferior-packages inferior "tigervnc-server")))
