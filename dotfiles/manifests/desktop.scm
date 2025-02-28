(use-modules (guix profiles)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages freedesktop)
             (gnu packages image)
             (gnu packages suckless)
             (gnu packages terminals)
             (gnu packages xorg)
             (gnu packages wm)
             (gnu packages xdisorg))

(define terminals
  (list alacritty))

(define menus
  (list dmenu wofi))

(define fonts
  (list fontconfig
        font-awesome
        font-dejavu
        font-liberation
        font-google-noto ;emoji in chromium
        font-misc-misc
        font-wqy-zenhei))

(packages->manifest (append fonts
                            menus
                            terminals))
