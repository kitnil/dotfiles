(use-modules (guix profiles)
             (gnu packages guile-xyz))

(define guile-packages
  (list guile-swayer))

(packages->manifest (append guile-packages))
