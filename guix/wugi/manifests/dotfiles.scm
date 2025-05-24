(define-module (wugi manifests dotfiles)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (guix profiles)
  #:export (%dotfiles-manifest))

(define (%dotfiles-manifest)
  (packages->manifest
   (list autoconf automake git gnu-make gnupg pkg-config skopeo yq)))
