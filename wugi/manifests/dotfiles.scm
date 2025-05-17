(define-module (wugi manifests dotfiles)
  #:use-module (guix profiles)
  #:use-module (wugi packages admin)
  #:use-module (wugi packages containers)
  #:use-module (wugi packages kubernetes)
  #:use-module (wugi packages networking)
  #:use-module (nongnu packages k8s)
  #:export (%dotfiles-manifest))

(define (%dotfiles-manifest)
  (packages->manifest
   (list plumber

         k3d
         k9s
         kompose
         kubectl
         kubernetes-helm
         nerdctl
         ;; virtctl ;TODO: Failed to build.

         spacer)))
