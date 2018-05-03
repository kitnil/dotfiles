(define-module (fiore manifests emacs)
  #:use-module (fiore manifests guix-collection)
  #:use-module (fiore manifests wigust)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(packages->manifest `(,@guix-collection-packages-emacs
                      ,@guix-wigust-packages-emacs))
