(define-module (packages linux)
  #:use-module (nongnu packages linux)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public linux-5.15-with-patched-bluetooth
  (package
    (inherit linux-5.15)
    (source
     (origin
       (inherit (package-source linux-5.15))
       (patches (append (search-patches "linux-5.15-fix-bluetooth.patch")
                        (origin-patches (package-source linux-5.15))))))))
