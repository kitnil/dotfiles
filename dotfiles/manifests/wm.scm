(use-modules (guix profiles)
             (gnu packages fonts)
             (gnu packages freedesktop)
             (gnu packages image)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages tor-browsers)
             (gnu packages wm)
             (gnu packages xdisorg))

(define terminals
  (list alacritty))

(define wm
  (list sway))

(define i3
  (list i3-wm i3status))

(define sway-utils
  (list bemenu grim slurp swayidle))

(define clipboard
  (list wl-clipboard))

(define wayland-utils
  (list wtype))

(packages->manifest (append clipboard
                            terminals
                            wm
                            sway-utils
                            i3
                            wayland-utils))
