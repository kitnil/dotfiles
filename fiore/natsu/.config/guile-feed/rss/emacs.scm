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
