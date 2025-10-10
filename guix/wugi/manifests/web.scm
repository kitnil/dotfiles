(define-module (wugi manifests web)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages librewolf)
  #:export (%web-manifest))

(define (%web-manifest)
  (packages->manifest (list librewolf)))

(%web-manifest)
