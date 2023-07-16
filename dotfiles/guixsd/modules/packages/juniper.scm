(define-module (packages juniper)
  #:use-module (home services juniper)
  #:use-module (utils package))

(define-public state-to-vc-sr1-mr13-14
  (package-from-program-file juniper-configuration->vc-sr1-mr13-14.intr))

(define-public state-to-vc-sr1-dh507-508
  (package-from-program-file juniper-configuration->vc-sr1-dh507-508.intr))

(define-public state-to-vc-sw2-mr13
  (package-from-program-file juniper-configuration->vc-sw2-mr13.intr))
