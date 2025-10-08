(define-module (wugi manifests telegram)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels telegram)
  #:export (%telegram-manifest))

(define (%telegram-manifest)
  (define inferior
    (inferior-for-channels %channels-telegram
                           #:cache-directory "/home/oleg/.cache/guix/inferiors"))

  (define telegram-desktop
    (first (lookup-inferior-packages inferior "telegram-desktop")))

  (packages->manifest (list telegram-desktop)))
