(define-module (wugi manifests wm)
  #:use-module (guix profiles)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix profiles)
  #:export (%wm-manifest))

(define (%wm-manifest)
  (define wm
    (list sway))

  (define sway-utils
    (list bemenu grim fnott mako slurp swayidle waybar))

  (define clipboard
    (list wl-clipboard))

  (define audio
    (list wireplumber))

  (define wayland-utils
    (list wayvnc
          wl-mirror
          wtype))

  (packages->manifest (append audio
                              clipboard
                              wm
                              sway-utils
                              wayland-utils)))
