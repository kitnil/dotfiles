(use-modules (packages containers)
             (packages kubernetes)
             (packages networking))

(packages->manifest
 (list plumber

       k3d
       k9s
       kompose
       kubectl
       kubernetes-helm
       nerdctl
       ;; virtctl ;TODO: Failed to build.
       ))
