(use-modules (guix profiles)
             (gnu packages fonts)
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
  (list bemenu swayidle))

(define clipboard
  (list wl-clipboard))

(define fonts
  ;; useful for emoji in alacritty
  (list font-google-noto-emoji))

(packages->manifest (append clipboard
                            fonts
                            menus
                            terminals
                            wm
                            sway-utils))
