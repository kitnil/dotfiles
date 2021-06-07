(use-modules (guix profiles)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages wm)
             (gnu packages xdisorg))

(define terminals
  (list alacritty))

(define menus
  (list dmenu wofi))

(define wm
  (list sway))

(define sway-utils
  (list swayidle))

(define clipboard
  (list wl-clipboard))

(packages->manifest (append clipboard menus terminals wm sway-utils))
