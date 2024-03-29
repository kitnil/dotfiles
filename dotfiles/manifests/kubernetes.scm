(use-modules (guix profiles)
             (packages kubernetes))

(packages->manifest (list cilium flux))
