(use-modules (guix profiles)
             (nongnu packages mozilla))

(packages->manifest (list firefox/wayland))
