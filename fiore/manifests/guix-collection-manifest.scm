(define-module (fiore manifests guix-collection-manifest)
  #:use-module (fiore manifests guix-collection)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout))
