(define-module (gnu packages wigust-guile)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (guix build utils))

(define-public haunt-symlinks
  (package
    (inherit haunt)
    (name "haunt-symlinks")
    (source
     (origin
       (inherit (package-source haunt))
       (patches (search-patches "haunt-asset-follow-symlinks.patch"))))))
