(in-package :stumpwm)

(defvar *browser* "firefox")

(defvar *jenkins-url*
  "https://jenkins.wugi.info")

(defvar youtube-playlist-cool-music
  "PLmjgicsUWIkvEKkLN01vm85neXAik3yU2")

(defun browse-url-firefox (url &optional new-window)
  (run-shell-command (join `("firefox" ,@(if new-window "--new-window" '())
                                       ,url))))

(defun browse-url-firefox* (url &key (new-window t) title (theme 'dark) ssb)
  (let ((command (join `(,@(if (equal theme 'dark) '("GTK_THEME=Adwaita:dark") '())
                           "nixGLIntel" "firefox"
                           ,@(if new-window '("--new-window") '())
                           ,@(if ssb '("--ssb") '())
                           ,url))))
    (if title
        (run-or-raise command `(:title ,title))
        (run-shell-command command))))

(defcommand music-youtube () ()
  (let ((window (current-window)))
    (if (and window (string= (window-res window)
                             "www.youtube.com__playlist"))
        (other-in-frame-or-fother)
        (run-or-raise (concat "nixGLIntel chromium --app=https://www.youtube.com/playlist?list="
                              youtube-playlist-cool-music)
                      '(:instance "www.youtube.com")))))

(defcommand jenkins () ()
  (browse-url-firefox* *jenkins-url*
                       :title "Jenkins"))

(defcommand youtube () ()
  (browse-url-firefox* "https://www.youtube.com/feed/subscriptions" t))

(defcommand twitch-tome4 () ()
  (browse-url-firefox* "https://www.twitch.tv/directory/game/Tales%20of%20Maj'Eyal" t))

(defcommand jenkins-index () ()
  (browse-url-firefox* *jenkins-url* "Jenkins"))

(defcommand tometips () ()
  (run-shell-command "chromium --app=https://tometips.github.io"))

(defcommand discord () ()
  (run-shell-command "chromium --app=https://discordapp.com/"))

(defcommand jenkins-chromium () ()
  (run-or-raise (concat "chromium --app=" *jenkins-url*)
                '(:instance "jenkins")))

(defcommand cuirass () ()
  (browse-url-firefox* "https://grafana.wugi.info/d/Ob67YJYiz/fiore?refresh=30s&orgId=1&var-host=cuirass"))

(defcommand yoo (url) ((:string "YouTube URL: "))
  (gnew "youtube")
  (restore-group (current-group) (read-dump-from-file "/home/oleg/youtube.lisp"))
  (term-shell-command (format nil "mpv --no-resume-playback --mute=yes ~s" url))
  (browse-url-firefox* (format nil "https://www.youtube.com/live_chat?v=~a&is_popout=1"
                              (cadr (split-string url "=")))
           t))

(defcommand guix-ci () ()
  (browse-url-browse-url-firefox* "http://ci.guix.info/jobset/guix-master"))

(defcommand guix-ci-package (package) ((:string "package: "))
  (unless (string= package "")
    (browse-url-browse-url-firefox*
     (concat "http://ci.guix.info/search?query=spec%3Aguix-master+system%3Ax86_64-linux+"
             package)
     t)))

(defcommand jenkins-ci-wigust () ()
  (browse-url-browse-url-firefox* (format nil "~a/job/wigust/" *jenkins-url*)))

(defcommand jenkins-ci-guix () ()
  (browse-url-browse-url-firefox* (format nil "~a/job/guix/" *jenkins-url*)))

(defcommand repology-guix-outdated () ()
  (browse-url-browse-url-firefox* "https://repology.org/projects/?inrepo=gnuguix&outdated=1"))

(defcommand twitch-channel-chat (channel) ((:string "channel: "))
  (run-shell-command
   (concat "chromium --app=https://www.twitch.tv/popout/"
           channel "/chat?popout=")))

(defcommand youtube () ()
  "Open YouTube in Firefox."
  (run-shell-command "firefox --new-window https://www.youtube.com"))

(defcommand w3m () ()
  "Open a W3M browser in user's home directory."
  (run-shell-command (join (list *xterm-command*
                                 *xterm-theme-light*
                                 *xterm-no-scrollbar*
                                 *term-execute-flag*
                                 (w3m "~")))))

(defcommand irc-guix-log () ()
  "Open an IRC Guix log in `w3m'."
  (run-shell-command (join (list *xterm-command*
                                 *xterm-theme-light*
                                 *xterm-no-scrollbar*
                                 *term-execute-flag*
                                 (w3m "https://gnunet.org/bot/log/guix/")))))


(defcommand browse-transmission () ()
  "Open transmissin WEB client."
  (run-shell-command (join (list *browser*
                                 (concat "http://torrent."
                                         *transmission-hostname* ".local")))))

;; Origin <https://github.com/alezost/stumpwm-config/blob/master/utils.lisp#L332>
(defcommand browse-url (url) (:shell "Browse URL: ")
  "Browse URL with ‘*browser*’."
  (run-prog *browser* :args (list url) :wait nil :search t))

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "IceCat")))

(defcommand firefox-test () ()
  "Start of focus firefox."
  (run-shell-command (join (list (firefox-command) "-P" "test"))
                     '(:class "Firefox")))

(defun firefox-command ()
  (join `(,@(if dark-theme '("GTK_THEME=Adwaita:dark") nil)

            ;; https://discourse.nixos.org/t/fonts-in-nix-installed-packages-on-a-non-nixos-system/5871/9
            "FONTCONFIG_FILE=/run/current-system/profile/etc/fonts/fonts.conf"

            "nixGLIntel" "firefox")))

(defcommand firefox () ()
  "Start of focus firefox."
  (run-or-raise (firefox-command) '(:class "Firefox")))

(defcommand firefox-new-window () ()
  "Start Firefox."
  (run-shell-command "firefox --new-window"))

(defcommand firefox-esr-52 () ()
  "Start of focus Firefox ESR 52."
  (run-shell-command "firefox-esr-52 --new-instance -P esr52"))

(defcommand chromium () ()
  "Start or focus Chromium."
  (run-or-raise "chromium" '(:class "Chromium-browser")))

(defcommand chromium-new-window () ()
  "Start Chromium."
  (run-shell-command "chromium"))

(defcommand chromium-app (url) ((:string "URL: "))
  "Start Chromium with app mode."
  (run-shell-command (format nil "chromium --app=~s" url)))

(defcommand chromium-proxy () ()
  "Start Chromium via proxy"
  (run-shell-command (concat "chromium"
                             " --proxy-server='socks5://localhost:9050'"
                             " --host-resolver-rules='MAP * ~NOTFOUND"
                             " , EXCLUDE localhost'")))

(defcommand twitchy () ()
  (term-shell-command "twitchy"))

(defcommand zabbix () ()
  (run-shell-command "chromium --new-window https://zabbix.wugi.info/"))


;;;
;;; Reference cards
;;;

(defun browse-refcard (url)
  (run-shell-command (format nil "curl --silent ~s | zathura -" url)))

(defcommand refcard-gnus () ()
  (browse-refcard "https://www.gnu.org/software/emacs/refcards/pdf/gnus-refcard.pdf"))
