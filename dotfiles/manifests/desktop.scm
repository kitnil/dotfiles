(use-modules (guix profiles)
             (gnu packages fonts)
             (gnu packages image)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages tor-browsers)
             (gnu packages wm)
             (gnu packages xdisorg))

(define terminals
  (list alacritty))

(define menus
  (list dmenu wofi))

(define wm
  (list sway))

(define sway-utils
  (list bemenu slurp swayidle))

(define clipboard
  (list wl-clipboard))

(define fonts
  ;; useful for emoji in alacritty
  (list font-google-noto-emoji))

(define browsers
  (list torbrowser))

(packages->manifest (append clipboard
                            fonts
                            menus
                            terminals
                            browsers
                            wm
                            sway-utils))
