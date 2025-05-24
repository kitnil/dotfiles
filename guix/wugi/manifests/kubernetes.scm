(define-module (wugi manifests kubernetes)
  #:use-module (guix profiles)
  #:use-module (nongnu packages k8s)
  #:use-module (wugi packages admin)
  #:use-module (wugi packages containers)
  #:use-module (wugi packages kubernetes)
  #:use-module (wugi packages networking)
  #:export (%kubernetes-manifest))

(define (%kubernetes-manifest)
  (packages->manifest
   (list cilium
         flux
         k3d
         k9s
         kompose
         kubectl
         kubernetes-helm
         nerdctl)))
