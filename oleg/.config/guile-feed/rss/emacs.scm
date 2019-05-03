(define-module (rss emacs)
  #:use-module (feed rss))

(define-public emacs-bennee
  (feed
   (name "emacs-bennee")
   (description "Alex Bennee blog.")
   (uri "https://www.bennee.com/~alex/blog/feed/")))

(define-public emacs-cestlaz
  (feed
   (name "emacs-cestlaz")
   (description "Mike Zamansky blog.")
   (uri "https://cestlaz.github.io/rss.xml")))

(define-public emacs-libraries-io
  (feed
   (name "emacs-libraries-io")
   (description "Emacs Libraries.io")
   (uri "https://libraries.io/search.atom?order=desc&platforms=Emacs&sort=created_at")))

(define-public emacs-sachachua
  (feed
   (name "emacs-sachachua")
   (description "Sacha Chua")
   (uri "https://sachachua.com/blog/feed/")))

