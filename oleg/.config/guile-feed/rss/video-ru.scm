(define-module (rss video-ru)
  #:use-module (feed rss))

(define-public video-game-one
  (feed
   (name "video-game-one")
   (description "Рекомендованный канал Nitroxenus")
   (uri (youtube-user "elementaller"))))

(define-public video-cpp-prosto
  (feed
   (name "cpp-prosto")
   (description "Язык программирования C")
   (uri (youtube-channel "UC_ehNByPcItZU3pXL-4skUA"))))

(define-public video-nitro-live
  (feed
   (name "video-nitro-live")
   (description "")
   (uri (youtube-channel "UC1RZz5_cdVQHhhYJVpCDqHA"))))

(define-public video-nitroxsenys
  (feed
   (name "video-nitroxsenys")
   (description "")
   (uri (youtube-channel "UCF3d6ZcTRBhnrNC0-cvzicw"))))

(define-public video-pashtet495
  (feed
   (name "video-pashtet495")
   (description "Рекомендованный канал Nitroxenus")
   (uri (youtube-user "Pashtet495"))))

(define-public video-stalkash
  (feed
   (name "video-stalkash")
   (description "Рекомендованный канал Nitroxenus")
   (uri (youtube-channel "UCOpm7EqPBtznEwYNNZrz1FQ"))))

(define-public video-twitch-artgameslp
  (feed
   (name "video-twitch-artgameslp")
   (description "Игровой канал ArtGamesLP")
   (uri (twitch-user "artgameslp"))))
