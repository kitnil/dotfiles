(define-module (rss planet)
  #:use-module (feed rss))

(define-public planet-emacs
  (feed
   (name "planet-emacs")
   (description "Emacs planet.")
   (uri "http://planet.emacsen.org/atom.xml")))

(define-public planet-gnu
  (feed
   (name "planet-gnu")
   (description "Gnu planet.")
   (uri "http://planet.gnu.org/atom.xml")))

(define-public planet-lisp
  (feed
   (name "planet-lisp")
   (description "Lisp planet.")
   (uri "http://planet.lisp.org/rss20.xml")))

(define-public planet-scheme
  (feed
   (name "planet-scheme")
   (description "Scheme planet.")
   (uri "http://www.scheme.dk/planet/atom.xml")))
