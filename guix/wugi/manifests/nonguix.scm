(define-module (wugi manifests nonguix)
  #:use-module (guix profiles)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (nongnu packages fonts)
  #:export (%nonguix-manifest))

(define (%nonguix-manifest)
  (define fonts
    (list font-awesome-nonfree
          font-microsoft-andale-mono
          font-microsoft-arial
          font-microsoft-arial-black
          font-microsoft-comic-sans-ms
          font-microsoft-couirer-new
          font-microsoft-courier-new
          font-microsoft-georgia
          font-microsoft-impact
          font-microsoft-times-new-roman
          font-microsoft-trebuchet-ms
          font-microsoft-verdana
          font-microsoft-web-core-fonts
          font-microsoft-webdings
          font-ubuntu))
  (packages->manifest fonts))
