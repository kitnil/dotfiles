(define-module (wugi manifests nonguix)
  #:use-module (guix channels)
  #:use-module (guix inferior)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (wugi etc guix channels workstation)
  #:use-module (wugi utils)
  #:use-module (nongnu packages fonts)
  #:use-module (nongnu packages k8s)
  #:use-module (nongnu packages game-client)
  #:export (%nonguix-manifest))

(define (%nonguix-manifest)
  (packages->manifest (append (list font-awesome-nonfree
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
                                    font-ubuntu)
                              (list kubectl)
                              (list steam))))
