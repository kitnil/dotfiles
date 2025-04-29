(use-modules (gnu packages gnome)
             (gnu packages gnome-xyz)
             (gnu packages lxde)
             (gnu packages mate)
             (gnu packages xfce)
             (gnu packages)
             (guix packages)
             (guix profiles))


(define icons
  (delete '()
          (fold-packages (lambda (x xs)
                           (cons (if (string-suffix? "-icon-theme"
                                                     (package-name x))
                                     x
                                     '())
                                 xs))
                         '())))


(packages->manifest icons)

