;; This "manifest" file can be passed to 'guix package -m' to reproduce
;; the content of your profile.  This is "symbolic": it only specifies
;; package names.  To reproduce the exact same profile, you also need to
;; capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(define-module (wugi manifests tor-browsers)
  #:use-module (guix profiles)
  #:use-module (gnu packages tor-browsers)
  #:export (%tor-browsers-manifest))

(define (%tor-browsers-manifest)
  (packages->manifest (list torbrowser)))
