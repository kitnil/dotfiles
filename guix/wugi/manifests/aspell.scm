(define-module (wugi manifests aspell)
  #:use-module (gnu)
  #:use-module (guix profiles)
  #:use-module (gnu packages aspell)
  #:export (%aspell-manifest))

(define (%aspell-manifest)
  (packages->manifest (list aspell aspell-dict-en aspell-dict-ru)))
