(define-module (wugi manifests kubernetes)
  #:use-module (guix profiles)
  #:use-module (wugi packages kubernetes)
  #:export (%kubernetes-manifest))

(define (%kubernetes-manifest)
  (packages->manifest (list cilium flux)))
