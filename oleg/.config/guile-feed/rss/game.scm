(define-module (rss game)
  #:use-module (feed rss))

(define-public game-reddit-freegames
  (feed
   (name "game-reddit-freegames")
   (description "Reddit free as beer games.")
   (uri (reddit-rss "freegames"))))
