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
                         :size 16))

(setf *window-border-style* :tight)

(setf *ignore-wm-inc-hints* t)

(set-win-bg-color "#dcdad5")
(set-unfocus-color "#FFFFFF")
(set-focus-color "#daa520")
(set-fg-color "#ffffff")
(set-bg-color "#daa520")
(set-border-color "#daa520")
(set-msg-border-width 0)

(set-float-focus-color "#daa520")

(setf *normal-border-width* 0
      *transient-border-width* 0
      *maxsize-border-width* 0)

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(clear-window-placement-rules)

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

(defcommand turn-screen-off () ()
            "Turn screen off."
            (run-shell-command "exec xset dpms force off"))

(defcommand kill-mpv () ()
  "Kill all mpv instanses."
  (run-shell-command "killall mpv"))
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

(setq *wi-xterm-big-command*
    "exec xterm -fa 'Monospace' -fs 24")

(defcommand wi-xterm-big () ()
  "Start XTerm with big fonts."
  (run-shell-command *wi-xterm-big-command*))

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

(setq *mode-line-border-color*     "#ffffff"
      *mode-line-foreground-color* "#000000"
      *mode-line-background-color* "#ffffff")

(setf *screen-mode-line-format* "[^B%n^b] %W")

(defcommand warp-mouse-active-frame () ()
  "Move mouse cursor to the top right of current frame."
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame)
                          (frame-width current-frame))
                       100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 5)

(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us" "ru")

(load-module "screenshot")

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

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

(load-module "globalwindows")
(define-key *root-map* (kbd "M-quoteright") "global-windowlist")
(define-key *root-map* (kbd "M-quotedbl") "global-pull-windowlist")

(ql:quickload "cffi")
(ql:quickload "usocket-server")

(load-module "pinentry")

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
