(define-module (wugi manifests deprecated)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:export (%deprecated-manifest
            openssh))

(define channels
  (list (channel
         (name 'guix)
         (url "https://cgit.wugi.info/git/guix/guix")
         (commit "f7e14782025bf87aaef694a21f34010b1a95f7f6") ;v1.0.1
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define (inferior)
  (inferior-for-channels channels
                         #:cache-directory "/home/oleg/.cache/guix/inferiors"))

(define (openssh)
  (first (lookup-inferior-packages (inferior) "openssh")))

(define (tigervnc-client)
  (last (lookup-inferior-packages (inferior) "tigervnc-client")))

(define (tigervnc-server)
  (last (lookup-inferior-packages (inferior) "tigervnc-server")))

(define (autofs)
  (last (lookup-inferior-packages (inferior) "autofs")))

(define (%deprecated-manifest)
  (packages->manifest (list (openssh))))
