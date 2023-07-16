(define-module (packages cisco)
  #:use-module (home services cisco)
  #:use-module (utils package))

(define-public state-to-vc-sw1-dh507
  (package-from-program-file cisco-configuration->vc-sw1-dh507.intr))

(define-public state-to-vc-sw2-dh507
  (package-from-program-file cisco-configuration->vc-sw2-dh507.intr))

(define-public state-to-vc-sw1-dh508
  (package-from-program-file cisco-configuration->vc-sw1-dh508.intr))

(define-public state-to-vc-sw2-dh508
  (package-from-program-file cisco-configuration->vc-sw2-dh508.intr))

(define-public state-to-vc-sw1-mr11
  (package-from-program-file cisco-configuration->vc-sw1-mr11.intr))

(define-public state-to-vc-sw1-mr12
  (package-from-program-file cisco-configuration->vc-sw1-mr12.intr))

(define-public state-to-vc-sw2-mr12
  (package-from-program-file cisco-configuration->vc-sw2-mr12.intr))

(define-public state-to-vc-sw3-mr13
  (package-from-program-file cisco-configuration->vc-sw3-mr13.intr))

(define-public state-to-vc-sw1-mr14
  (package-from-program-file cisco-configuration->vc-sw1-mr14.intr))

(define-public state-to-vc-sw2-mr14
  (package-from-program-file cisco-configuration->vc-sw2-mr14.intr))
