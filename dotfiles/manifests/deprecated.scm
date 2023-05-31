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
         (commit "f7e14782025bf87aaef694a21f34010b1a95f7f6") ;v1.0.1
         (introduction (@@ (guix channels) %guix-channel-introduction)))
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
  (open-inferior cached #:error-port (current-error-port)))

(define-public openssh
  (first (lookup-inferior-packages inferior "openssh")))

(define-public tigervnc-client
  (last (lookup-inferior-packages inferior "tigervnc-client")))

(define-public tigervnc-server
  (last (lookup-inferior-packages inferior "tigervnc-server")))

(define-public autofs
  (last (lookup-inferior-packages inferior "autofs")))

;; XXX: manually installed Guix package with "guix build" and "guix install"
;; /gnu/store/sy4v75yrn1fks0b0i6y9k70rmw5xh7rp-alacritty-0.12.0.drv because of
;; broken clipboard on wayland.
(define-public alacritty
  (last (lookup-inferior-packages inferior "alacritty")))

(define-public libreoffice
  (last (lookup-inferior-packages inferior "libreoffice")))
