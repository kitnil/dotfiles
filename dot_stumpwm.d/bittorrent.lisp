(in-package :stumpwm)

(defcommand qbittorrent () ()
  "Start or focus qbittorrent."
  (run-or-raise "qbittorrent" '(:class "qBittorrent")))
