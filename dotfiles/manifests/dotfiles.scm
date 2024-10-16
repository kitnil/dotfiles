(use-modules (packages admin)
             (packages containers)
             (packages kubernetes)
             (packages networking)
             (nongnu packages k8s))

(packages->manifest
 (list plumber

       k3d
       k9s
       kompose
       kubectl
       kubernetes-helm
       nerdctl
       ;; virtctl ;TODO: Failed to build.

       spacer))
