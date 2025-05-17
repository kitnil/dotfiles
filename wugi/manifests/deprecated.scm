(define-module (wugi manifests deprecated)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (%deprecated-manifest))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (commit "f7e14782025bf87aaef694a21f34010b1a95f7f6") ;v1.0.1
         (introduction (@@ (guix channels) %guix-channel-introduction)))
        (channel
         (name 'guix-wigust)
         (url "https://github.com/kitnil/guix-wigust")
         (branch "master")
         (commit "68340baa8cdc1af1b8a8982c2607ff1dda195ee7"))))

(define (%deprecated-manifest)
  (define inferior
    (inferior-for-channels channels))

  (define openssh
    (first (lookup-inferior-packages inferior "openssh")))

  (define tigervnc-client
    (last (lookup-inferior-packages inferior "tigervnc-client")))

  (define tigervnc-server
    (last (lookup-inferior-packages inferior "tigervnc-server")))

  (define autofs
    (last (lookup-inferior-packages inferior "autofs")))

  (packages->manifest (list openssh)))
