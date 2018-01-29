(use-modules (guix-packages) (wigust-packages))

(packages->manifest `(,@guix-collection-packages
                      ,@guix-wigust-packages
                      ,@guix-collection-packages-multi
                      ,@guix-collection-packages-big))
