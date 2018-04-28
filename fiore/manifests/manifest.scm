(define-module (fiore manifests manifest)
  #:use-module (fiore manifests guix-collection)
  #:use-module (fiore manifests wigust)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout
                      ,@guix-wigust-packages))
