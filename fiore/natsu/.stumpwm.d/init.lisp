(in-package :stumpwm)

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(set-module-dir "/home/natsu/.stumpwm.d/modules/")

(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr")

(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "DejaVu Sans Mono"
                         :subfamily "Book"
                         :size 12))

(setf *window-border-style* :none)

(setf *ignore-wm-inc-hints* t)
(set-msg-border-width 2)

(setf *normal-border-width* 0
      *transient-border-width* 0
      *maxsize-border-width* 0)

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(clear-window-placement-rules)

(restore-window-placement-rules "~/.desktop.lisp")

;; Last rule to match takes precedence!
;; If the argument to :title or :role begins with an ellipsis, a
;; substring match is performed.
;; If the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; If the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c ." '(:class "Emacs")))

(defcommand conkeror () ()
  "Start or focus conkeror."
  (run-or-raise "conkeror" '(:class "Conkeror")))

;; Origin <https://github.com/alezost/stumpwm-config/blob/master/utils.lisp#L332>
(defcommand wi-conkeror-browse-url (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "Icecat")))

(defcommand chromium () ()
  "Start or focus chromium."
  (run-or-raise "exec /home/natsu/.guix-profile.d/chromium/bin/chromium"
                '(:class "Chromium-browser")))

(defcommand chromium-proxy () ()
  "Start Chromium via proxy"
  (run-shell-command (concat "chromium"
                             " --proxy-server='socks5://localhost:9050'"
                             " --host-resolver-rules='MAP * ~NOTFOUND"
                             " , EXCLUDE localhost'")))

(define-key *root-map* (kbd "w") "conkeror")
(define-key *root-map* (kbd "C-w") "conkeror")
(define-key *root-map* (kbd "M-w") "chromium")

(defcommand youtube () ()
  "Start Chromium YouTube"
  (run-shell-command (concat "chromium"
                             " --profile-directory=Default"
                             " --app-id=adnlfjpnmidfimlkaohpidplnoimahfh")))

(defcommand mpv () ()
  "Start or focus mpv."
  (run-or-raise "mpv" '(:class "mpv")))

(defcommand wi-xclip-emacs () ()
  "Open file from clipboard."
  (run-shell-command (join (list "exec emacsclient -c" (get-x-selection)) #\ )))

(defcommand xclip-mpv () ()
  "Play video from clipboard."
  (run-shell-command (join (list "exec mpv" (get-x-selection)) #\ )))

(defcommand kodi-cli-youtube () ()
  "Send video from clipboard to Kodi."
  (run-shell-command (join (list "exec kodi-cli -y" (get-x-selection)) #\ )))

(defcommand mpv-music () ()
  "Play music."
  (run-shell-command (concat "exec mpv"
                             " --keep-open=no"
                             " --msg-level=all=no"
                             " --no-resume-playback"
                             " /srv/music/*")))

(defcommand youtube-dl () ()
  "Download video."
  (run-shell-command (concat "exec xterm -name youtube-dl"
                             " -e youtube-dl"
                             " $(xclip -o -selection clipboard)")))

(defcommand youtube-dl-play () ()
  "Download video and play it."
  (run-shell-command (concat "exec xterm -name youtube-dl"
                             " -e youtube-dl"
                             " --exec 'mpv {}'"
                             " $(xclip -o -selection clipboard)")))

(define-key *root-map* (kbd "m") "mpv")
(define-key *root-map* (kbd "C-m") "xclip-mpv")
(define-key *root-map* (kbd "C-e") "wi-xclip-emacs")
(define-key *root-map* (kbd "C-M-c") "wi-xterm-big-screen")

(defcommand turn-screen-off () ()
            "Turn screen off."
            (run-shell-command "exec xset dpms force off"))

(defun join-to-stream (stream list &optional (delimiter #\&))
  (destructuring-bind (&optional first &rest rest) list
    (when first
      (write-string first stream)
      (when rest
        (write-char delimiter stream)
        (join-to-stream stream rest delimiter)))))

(defun join (list &optional (delimiter #\&))
  (with-output-to-string (stream)
    (join-to-stream stream list delimiter)))

;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock -mode blank")
(define-key *root-map* (kbd "M-l") "turn-screen-off")

(defcommand suspend () ()
  (run-shell-command "exec loginctl suspend"))

(defcommand run-xterm-command (cmd &optional collect-output-p) ((:shell "/bin/sh -c "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd "-e" cmd) #\ ))
            :wait nil))
(define-key *root-map* (kbd "M-!") "run-xterm-command")

(defcommand pulsemixer () ()
  "Download video."
  (run-shell-command "exec xterm -name pulsemixer -e pulsemixer"))
(define-key *root-map* (kbd "v") "pulsemixer")

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))
(define-key *root-map* (kbd "C-v") "alsamixer")

(defcommand xterm () ()
  "Start or focus XTerm."
  (run-or-raise "xterm" '(:class "XTerm")))
(define-key *root-map* (kbd "c") "xterm")

(defcommand xterm-name (cmd &optional collect-output-p) ((:string "window name: "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd) #\ ))
            :wait nil))

(defcommand wi-xterm-dark
    (session &optional collect-output-p) ((:string "session name: "))
  "Run `xterm' with dark theme."
  (run-prog *shell-program*
            :args (list "-c" (join (list "exec" "xterm"
                                         "-bg" "black" "-fg" "white"
                                         "-title" session
                                         "-e" "screen" "-S" session)
                                   #\ ))
            :wait nil))

(defcommand wi-screen
    (session &optional collect-output-p)
    ((:string "session name: "))
  "Run `screen' session."
  (run-prog *shell-program*
            :args
            (list "-c"
                  (join (list "env" "STY=" ; Do not complain `$STY' in `screen'.
                              "xterm" "-title" session
                              "-e" "screen" "-S" session)
                        #\ ))
            :wait nil))

(setq *wi-xterm-big-command*
    "exec xterm -fa 'Monospace' -fs 24")

(defcommand wi-xterm-big () ()
  "Start XTerm with big fonts."
  (run-shell-command *wi-xterm-big-command*))

(defcommand wi-xterm-big-screen () ()
  "Start XTerm with big fonts."
  (run-shell-command (concat *wi-xterm-big-command* " -e screen")))

(defcommand wi-sensors () ()
  "Start XTerm with `sensors'."
  (run-shell-command  (concat *wi-xterm-big-command* " -e watch sensors")))

(defcommand github-star () ()
  "Move mouse to star a project."
  (ratwarp 1308 176)
  (ratclick))

(defcommand qemu-debian () ()
  "Run GNOME Debian in QEMU."
  (run-shell-command (concat "exec " (getenv "HOME") "/bin/debian.sh")))

(defparameter wi-dark-theme t)
(set-focus-color "#daa520")
(set-border-color "#daa520")
(set-float-focus-color "#daa520")
(defcommand wi-toggle-theme () ()
  (if wi-dark-theme
      (progn (setq *mode-line-border-color*     "#ffffff"
                   *mode-line-foreground-color* "#000000"
                   *mode-line-background-color* "#ffffff")
             (set-win-bg-color "#dcdad5")
             (set-unfocus-color "#FFFFFF")
             (set-fg-color "#000000")
             (set-bg-color "#ffffff")
             (run-shell-command "xsetroot -solid grey")
             (setq wi-dark-theme nil))
      (progn (setq *mode-line-border-color*     "#000000"
                   *mode-line-foreground-color* "#ffffff"
                   *mode-line-background-color* "#000000")
             (set-win-bg-color "#000000")
             (set-unfocus-color "#000000")
             (set-fg-color "#ffffff")
             (set-bg-color "#000000")
             (run-shell-command "xsetroot -solid black")
             (setq wi-dark-theme t))))

(load-module "cpu")
(load-module "mem")
(load-module "disk")
(setf *mode-line-timeout* 2)
(setf *TIME-MODELINE-STRING* "%a %b %e %k:%M")
(setf *screen-mode-line-format*
      (list "[%n]:" '(:eval (write-to-string (group-number (current-group))))
            "    "
            '(:eval (write-to-string (window-number (current-window))))
            ":"
            "["
            '(:eval (window-class (current-window)))
            " "
            '(:eval (window-name (current-window)))
            "]"
            "^>    %D    %M    %t    %c    %d"))
(setf *mode-line-pad-x* 0)
(setf *mode-line-pad-y* 0)
(mode-line)

(defcommand warp-mouse-active-frame () ()
  "Move mouse cursor to the top right of current frame."
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame)
                          (frame-width current-frame))
                       100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us" "ru")

(load-module "screenshot")

(defcommand scroll-other-window () ()
  (stumpwm:run-commands "fother" "window-send-string  " "fother"))

(define-key *root-map* (kbd "C-M-v") "scroll-other-window")

(defcommand set-background-dark () ()
  (run-shell-command "xsetroot -solid black"))
(defun time-date-and-time-restrict ()
  (time-format "%Y-%m-%d-%H-%M-%S"))

;; Tuesday January 3 2005 23:05:25
(setq *time-format-string-default* "%A %B %e %Y %k:%M:%S")

(defun screenshot-filename ()
  (concat (time-date-and-time-restrict)
          ".png"))

(defcommand screenshot-default () ()
  "Screenshot with filename like 2017-10-30-03-29-16.png"
  (eval-command (concat "screenshot-window " (screenshot-filename))))

(define-key *root-map* (kbd "Print") "screenshot-default")

(defcommand wi-trans () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I en:ru'"))

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

(load-module "globalwindows")

(ql:quickload "cffi")
(ql:quickload "usocket-server")

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
