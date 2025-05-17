(define-module (wugi manifests wm)
  #:use-module (guix profiles)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages image)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages tor-browsers)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix profiles)
  #:export (manifest-wm))



(define (packages-wm)
  (define terminals
    (list alacritty))

  (define wm
    (list sway))

  (define i3
    (list i3-wm i3status))

  (define sway-utils
    (list bemenu grim fnott slurp swayidle waybar))

  (define clipboard
    (list wl-clipboard))

  (define wayland-utils
    (list wayvnc
          wl-mirror
          wtype
          xdg-desktop-portal
          xdg-desktop-portal-gtk
          xdg-desktop-portal-wlr))

  (append clipboard
          terminals
          wm
          sway-utils
          i3
          wayland-utils))

(define (manifest-wm)
  (packages->manifest (packages-wm)))
