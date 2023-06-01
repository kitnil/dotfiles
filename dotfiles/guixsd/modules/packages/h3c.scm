(define-module (packages h3c)
  #:use-module (home services h3c)
  #:use-module (utils package))

(define-public state-to-vc-sw4-mr11
  (package-from-program-file h3c-configuration->vc-sw4-mr11.intr))

(define-public state-to-vc-sw4-mr12
  (package-from-program-file h3c-configuration->vc-sw4-mr12.intr))

(define-public state-to-vc-sw4-mr13
  (package-from-program-file h3c-configuration->vc-sw4-mr13.intr))

(define-public state-to-vc-sw4-mr14
  (package-from-program-file h3c-configuration->vc-sw4-mr14.intr))
