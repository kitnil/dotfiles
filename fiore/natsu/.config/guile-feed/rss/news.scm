(define-module (rss news)
  #:use-module (feed rss))

(define-public fedoramagazine
  (feed
   (name "fedoramagazine")
   (description "Fedora online magazine.")
   (uri "https://fedoramagazine.org/feed/")))

(define-public lwn
  (feed
   (name "lwn")
   (description "GNU/Linux online magazine.")
   (uri "https://lwn.net/headlines/newrss")))
