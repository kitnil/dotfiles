(define-module (packages virtualization)
  #:use-module (gnu packages)
  #:use-module (gnu packages virtualization)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1))

(define-public qemu-evdev
  (package
    (inherit qemu)
    (source
     (origin
       (inherit (package-source qemu))
       (patches (append (search-patches "qemu-evdev.patch")
                        (origin-patches (package-source qemu))))))))
