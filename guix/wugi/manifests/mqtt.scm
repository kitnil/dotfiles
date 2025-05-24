(define-module (wugi manifests mqtt)
  #:use-module (guix profiles)
  #:use-module (wugi packages networking)
  #:export (%mqtt-manifest))

(define (%mqtt-manifest)
  (packages->manifest
   (list plumber)))
