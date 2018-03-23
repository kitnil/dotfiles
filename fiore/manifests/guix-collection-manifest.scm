(use-modules (fiore manifests guix-collection)
             (gnu)
             (guix packages)
             (guix profiles))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-collection-packages-multiout))
