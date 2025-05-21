(define-module (wugi manifests guile)
  #:use-module (guix profiles)
  #:use-module (gnu packages guile-xyz)
  #:export (%guile-manifest))

(define (%guile-manifest)
  (define guile-packages
    (list guile-swayer))

  (packages->manifest (append guile-packages)))
