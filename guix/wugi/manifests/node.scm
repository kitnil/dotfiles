(define-module (wugi manifests node)
  #:use-module (guix profiles)
  #:use-module (gnu packages node)
  #:use-module (guix profiles)
  #:export (%node-manifest))

(define (%node-manifest)
  (packages->manifest (list node)))
