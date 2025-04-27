(use-modules (guix profiles)
             (guix packages)
             (gnu packages)
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
  (append fontconfig
          (delete '()
                  (fold-packages (lambda (x xs)
                                   (cons (if (string-prefix? "font-"
                                                             (package-name x))
                                             x
                                             '())
                                         xs))
                                 '()))))

(packages->manifest (append fonts
                            menus
                            terminals))
