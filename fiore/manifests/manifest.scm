(use-modules (fiore manifests guix-collection)
             (fiore manifests wigust)
             (gnu)
             (guix packages)
             (guix profiles))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout
                      ,@guix-wigust-packages))
