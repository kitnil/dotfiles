(define-module (fiore manifests all)
  #:use-module (fiore manifests guix-collection)
  #:use-module (fiore manifests wigust)
  #:use-module (gnu)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (fiore manifests fiore))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout
                      ,@guix-wigust-packages
                      ,@%fiore-packages))
