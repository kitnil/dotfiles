(use-modules (guix profiles)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages wm)
             (gnu packages xdisorg)
             (deprecated))

(define office
  (list (@ (deprecated) libreoffice)))

(define terminals
  (list (@ (deprecated) alacritty)))

(define menus
  (list dmenu wofi))

(define wm
  (list sway))

(define sway-utils
  (list swayidle))

(define clipboard
  (list wl-clipboard))

(packages->manifest (append clipboard menus office terminals wm sway-utils))
