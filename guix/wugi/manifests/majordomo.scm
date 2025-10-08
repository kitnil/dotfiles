(define-module (wugi manifests majordomo)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels majordomo)
  #:export (%majordomo-manifest))

(define (%majordomo-manifest)
  (define inferior
    (inferior-for-channels %channels-majordomo
                           #:cache-directory "/home/oleg/.cache/guix/inferiors"))

  (packages->manifest
   (list (first (lookup-inferior-packages inferior "guile-ihs")))))
