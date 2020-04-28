(in-package :stumpwm)

(defun youtube-dl-output (dir)
  (concat dir "/" "%(title)s.%(ext)s"))

(defvar *music-directory* "/srv/music")

(defvar *youtube-dl-output-music*
  (youtube-dl-output *music-directory*))

(defun youtube-dl-command (url &key (ad-hoc nil) (music nil))
  (message (format nil "Download ~s." url))
  (term-shell-command (format nil "sh -c 'TMOUT=20; ~a; notify-send \"youtube-dl finished\"; read -p \"Press Enter to close.\"'"
                              (join `("youtube-dl"
                                      "--restrict-filenames"
                                      ,@(if music
                                            (list (format nil "--output=~s" *youtube-dl-output-music*))
                                            nil)
                                      ,@(if ad-hoc
                                            (list (format nil "--exec ~s" ad-hoc))
                                            nil)
                                      ,(format nil "~s" (if (string-contains "list=" url)
                                                            (car (split-string url "&"))
                                                            url)))))
                      :title (if music "youtube-dl-music" "youtube-dl")
                      :font '("-fa" "Monospace" "-fs" "8")))

(defcommand youtube-dl () ()
  (youtube-dl-command (get-x-selection)))

(defcommand youtube-dl-music () ()
  (youtube-dl-command (get-x-selection) :music t))

(defcommand youtube-dl-play () ()
  (youtube-dl-command (get-x-selection)
                      :ad-hoc "mpv --no-stop-screensaver --title=youtube-dl-music --no-resume-playback {}"))

(defcommand youtube-dl-music-play () ()
  (youtube-dl-command (get-x-selection)
                      :music t
                      :ad-hoc "mpv --no-stop-screensaver --title=youtube-dl-music --no-resume-playback {}"))

(defcommand youtube-dl-music-play-url (url) ((:string "URL: "))
  (youtube-dl-command url
                      :music t
                      :ad-hoc "mpv --no-stop-screensaver --title=youtube-dl-music --no-resume-playback {}"))
