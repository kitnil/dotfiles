(define-module (wugi manifests ai)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels aichat)
  #:export (%ai-manifest))

(define (%ai-manifest)
  (define inferior
    (inferior-for-channels %channels-aichat
                           #:cache-directory "/home/oleg/.cache/guix/inferiors"))

  (define aichat
    (first (lookup-inferior-packages inferior "aichat")))

  (packages->manifest (list aichat)))
