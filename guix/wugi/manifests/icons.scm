(define-module (wugi manifests icons)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages xfce)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:export (%icons-manifest))

(define (%icons-manifest)
  (define icons
    (list adwaita-icon-theme
          arc-icon-theme
          delft-icon-theme
          elementary-xfce-icon-theme
          faba-icon-theme
          flat-remix-icon-theme
          gnome-icon-theme
          hicolor-icon-theme
          lxde-icon-theme
          mate-icon-theme
          moka-icon-theme
          papirus-icon-theme
          qogir-icon-theme
          tango-icon-theme))

  (packages->manifest icons))
