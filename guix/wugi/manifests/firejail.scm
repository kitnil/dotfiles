(define-module (wugi manifests firejail)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels firejail)
  #:export (%firejail-manifest))

(define (%firejail-manifest)
  (define inferior
    (inferior-for-channels %channels-firejail))

  (define firejail-disable-sandbox-check
    (first (lookup-inferior-packages inferior "firejail-disable-sandbox-check")))

  (packages->manifest (list firejail-disable-sandbox-check)))
