(use-modules (guix profiles)
             (nongnu packages chrome)
             (nongnu packages mozilla))

(packages->manifest (list firefox google-chrome-stable))
