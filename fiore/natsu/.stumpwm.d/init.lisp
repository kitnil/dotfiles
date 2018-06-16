;; Copyright © 2018 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(load "~/quicklisp/setup.lisp")

(ql:quickload "cffi")
(ql:quickload "usocket-server")

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)

(set-module-dir "~/.stumpwm.d/modules/")

(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command "xrdb -merge ~/.Xresources")

;; Wallpaper
;; (run-shell-command "feh --bg-scale ~/Pictures/Wallpapers/current.png")
(run-shell-command "xsetroot -solid black")

;; Disable PC speaker
(run-shell-command "xset -b")

;; Disable accessiblity features
;; (run-shell-command "xkbset -a")

;;;
;;; Keyboard
;;;

(ql:quickload "clx-truetype")
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "DejaVu Sans Mono"
                         :subfamily "Book"
                         :size 14))

;; Use keyboard as mouse with <Shift+Num Lock>
;; https://en.wikipedia.org/wiki/Mouse_keys
(run-shell-command "setxkbmap -option keypad:pointerkeys")

;; Keyboard layout
;; (run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle")

;; Keyboard speed
;; (run-shell-command "xset s 0")
;; (run-shell-command "xset dpms 0 0 1800")

;; (run-shell-command "xmodmap ~/.Xmodmap")

(setf *message-window-y-padding* 3)

(setf *window-border-style* :none)

(setf *ignore-wm-inc-hints* t)
(set-msg-border-width 2)

(setf *normal-border-width* 0)
(setf *transient-border-width* 0)
(setf *maxsize-border-width* 0)


;;;
;;; General functions for use
;;;

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

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(clear-window-placement-rules)

(restore-window-placement-rules "~/.desktop.lisp")
(restore-from-file "~/.stumpwm-dump-desktop.lisp")
;; (dump-desktop-to-file "~/.stumpwm-dump-desktop.lisp")

;; Last rule to match takes precedence!
;; If the argument to :title or :role begins with an ellipsis, a
;; substring match is performed.
;; If the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; If the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.


;;;
;;; XTerm
;;;

(defvar *default-group-name*
  "default")

(setf *window-format* "%m%n%s %c %50t")

(defvar *wi-xterm-command*
  "exec xterm")

(defvar *wi-xterm-big-command*
  "exec xterm -fa 'Monospace' -fs 24")

(defun w3m (url)
  "Return a `w3m' command to open a URL."
  (concat "w3m " url))

(defvar *wi-xterm-theme-light*
  "-bg white -fg black")

(defvar *wi-xterm-theme-dark*
  "-bg black -fg white")

(defvar *wi-xterm-no-scrollbar*
  "+sb")

(defvar *wi-term-execute-flag*
  "-e")

(defvar *wi-conkeror-command*
  "conkeror")

(defvar *wi-browser*
  *wi-conkeror-command*)

(defvar *wi-transmission-hostname*
  "magnolia")


;;;
;;; Emacs
;;;

(defcommand emacs () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c ." '(:class "Emacs")))

(defcommand emacs-org-capture () ()
  "Capture URL with Emacs Org from GUI clipboard"
  (run-shell-command
   (join (list "exec"
               (concat (getenv "HOME") "/bin/emacs-org-capture")
               (get-x-selection))
         #\ )))


;;;
;;; Conkeror
;;;

(defcommand conkeror () ()
  "Start or focus conkeror."
  (run-or-raise *wi-conkeror-command* '(:class "Conkeror")))

(defcommand wi-browse-transmission () ()
  "Open transmissin WEB client."
  (run-shell-command (join (list *wi-browser*
                                 (concat "http://torrent."
                                         *wi-transmission-hostname* ".local"))
                           #\ )))

;; Origin <https://github.com/alezost/stumpwm-config/blob/master/utils.lisp#L332>
(defcommand wi-conkeror-browse-url (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "Icecat")))

(defcommand firefox () ()
  "Start of focus firefox."
  (run-or-raise "firefox-latest" '(:class "Firefox")))

(defcommand wi-chromium () ()
  "Start or focus Chromium."
  (run-or-raise "chromium" '(:class "Chromium-browser")))

(defcommand chromium-proxy () ()
  "Start Chromium via proxy"
  (run-shell-command (concat "chromium"
                             " --proxy-server='socks5://localhost:9050'"
                             " --host-resolver-rules='MAP * ~NOTFOUND"
                             " , EXCLUDE localhost'")))

(defcommand wi-w3m () ()
  "Open a W3M browser in user's home directory."
  (run-shell-command (join (list *wi-xterm-command*
                                 *wi-xterm-theme-light*
                                 *wi-xterm-no-scrollbar*
                                 *wi-term-execute-flag*
                                 (w3m "~"))
                           #\ )))

(defcommand wi-irc-guix-log () ()
  "Open an IRC Guix log in `w3m'."
  (run-shell-command (join (list *wi-xterm-command*
                                 *wi-xterm-theme-light*
                                 *wi-xterm-no-scrollbar*
                                 *wi-term-execute-flag*
                                 (w3m "https://gnunet.org/bot/log/guix/"))
                           #\ )))

(defcommand youtube () ()
  "Start Chromium YouTube"
  (run-shell-command (concat "chromium"
                             " --profile-directory=Default"
                             " --app-id=adnlfjpnmidfimlkaohpidplnoimahfh")))


;;;
;;; MPV
;;;

;; Look for audio devices ‘mpv --audio-device=help’
(defvar *wi-headphones*
  "pulse/alsa_output.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "Pulseaudio device.  Heaphones.")

(defvar *wi-mpv-program* "mpv"
 "The name by which to invoke MPV.")

(defparameter *wi-mpv-headphones*
  nil
  "If non-nil use heaphones in MPV.")

(defvar *wi-mpv-default-arguments*
  '("--keep-open=no"))

(defparameter *wi-mpv-arguments*
  *wi-mpv-default-arguments*)

(defcommand wi-toggle-mpv-arguments () ()
  (if *wi-mpv-headphones*
      (progn
        (setf *wi-mpv-arguments* *wi-mpv-default-arguments*)
        (setf *wi-mpv-headphones* nil))
      (progn
        (setf *wi-mpv-arguments*
              `(,@*wi-mpv-default-arguments*
                ,(concat "--audio-device=" *wi-headphones*)))
        (setf *wi-mpv-headphones* t))))

(defcommand wi-mpv () ()
  "Start or focus mpv."
  (run-or-raise (join `(,*wi-mpv-program* ,@*wi-mpv-arguments*) #\ )
                '(:class "mpv")))

(defcommand wi-xclip-mpv () ()
  "Play video from clipboard with mpv."
  (run-shell-command
   (join `(,*wi-mpv-program* ,@*wi-mpv-arguments* ,(get-x-selection))
         #\ )))


;;;
;;; Streamlink
;;;

(defvar *wi-streamlink-program* "streamlink")
(defvar *wi-streamlink-arguments* '("-p" "mpv"))
(defvar *wi-streamlink-player-arguments* nil)
(defvar *wi-streamlink-quality* "best")

(defun wi-single-quote-string (str)
  (let ((string-quote "'"))
    (concat string-quote str string-quote)))

(defun wi-quote-string (str)
  (let ((string-quote "\""))
    (concat string-quote str string-quote)))

(defcommand wi-xclip-streamlink () ()
  "Play video from clipboard with streamlink."
  (run-shell-command
   (join `(,*wi-streamlink-program*
           ,@*wi-streamlink-arguments*
           ,@(if *wi-streamlink-player-arguments*
                 `("--player-args" ,(wi-single-quote-string
                                     (join *wi-streamlink-player-arguments*
                                           #\ ))))
           ,(get-x-selection)
           ,*wi-streamlink-quality*)
         #\ )))

(defcommand wi-xclip-emacs () ()
  "Open file from clipboard."
  (run-shell-command
   (join (list "exec emacsclient -c" (get-x-selection)) #\ )))

(defcommand emacs-anywhere () ()
  "Run `emacs-anywhere'."
  (run-shell-command "emacs-anywhere"))

(defcommand mpv-music () ()
  "Play music."
  (run-shell-command (concat "exec mpv"
                             " --keep-open=no"
                             " --msg-level=all=no"
                             " --no-resume-playback"
                             " /srv/music/*")))



(defcommand epson () ()
  "Open XTerm with Epson."
  (run-shell-command
   (join (list *wi-xterm-command* *wi-xterm-theme-dark*
               *wi-xterm-no-scrollbar* *wi-term-execute-flag*
               "sudo wi-qemu-epson.sh")
         #\ )))

(defcommand epson-no-graphic () ()
  "Open XTerm with Epson."
  (run-shell-command
   (join (list *wi-xterm-command* *wi-xterm-theme-dark*
               *wi-xterm-no-scrollbar* *wi-term-execute-flag*
               "sudo wi-qemu-epson.sh -display none")
         #\ )))

(defcommand htop () ()
  "Open XTerm with htop."
  (run-shell-command
   (join (list *wi-xterm-command* *wi-xterm-theme-dark*
               *wi-xterm-no-scrollbar* *wi-term-execute-flag*
               "htop")
         #\ )))

(defcommand rofi-twitchy () ()
  "Open Rofi with Twitchy plugin."
  (run-shell-command "rofi -modi twitchy:rofi-twitchy -show twitchy"))

(defcommand twitchy () ()
  "Open XTerm with twitchy."
  (run-shell-command
   (join (list *wi-xterm-command* *wi-xterm-theme-dark*
               *wi-xterm-no-scrollbar* *wi-term-execute-flag*
               "twitchy")
         #\ )))

(defcommand kodi-cli-youtube () ()
  "Send video from clipboard to Kodi."
  (run-shell-command
   (join (list "exec kodi-cli -y" (get-x-selection)) #\ )))

(defcommand youtube-dl () ()
  "Download video."
  (run-shell-command
   (join '("exec" "xterm" "-name youtube-dl" "-e" "youtube-dl"
           "$(xclip -o -selection clipboard)")
         #\ )))

(defcommand youtube-dl-play () ()
  "Download video and play it."
  (run-shell-command
   (join '("exec" "xterm" "-name" "youtube-dl" "-e" "youtube-dl"
           "--exec" "'mpv {}'"
           "$(xclip -o -selection clipboard)")
         #\ )))

(defcommand turn-screen-off () ()
  "Turn screen off."
  (run-shell-command "exec xset dpms force off"))

(defcommand suspend () ()
  (run-shell-command "exec loginctl suspend"))

(defcommand run-xterm-command (cmd &optional collect-output-p)
    ((:shell "/bin/sh -c "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd "-e" cmd)
                                   #\ ))
            :wait nil))

(defvar *wi-pulsemixer-command*
  "pulsemixer")

(defcommand pulsemixer () ()
  "Download video."
  (run-shell-command (join (list *wi-xterm-command*
                                 *wi-xterm-theme-dark*
                                 *wi-xterm-no-scrollbar*
                                 *wi-term-execute-flag*
                                 *wi-pulsemixer-command*)
                           #\ )))

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))

(defcommand wi-run-or-raise-xterm () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *wi-xterm-command* *wi-xterm-theme-light*)
         #\ )
   '(:class "XTerm")))

(defcommand wi-run-xterm () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list *wi-xterm-command*
                                         *wi-xterm-theme-dark*
                                         *wi-xterm-no-scrollbar*)
                                   #\ ))
            :wait nil))

(defcommand wi-xterm-dark-no-scrollbar () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *wi-xterm-command* *wi-xterm-theme-dark* "+sb") #\ )
   '(:class "XTerm")))

(defcommand xterm-name (cmd &optional collect-output-p)
    ((:string "window name: "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd) #\ ))
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

(defcommand wi-xterm-big () ()
  "Start XTerm with big fonts."
  (run-shell-command *wi-xterm-big-command*))

(defcommand wi-xterm-big-screen () ()
  "Start XTerm with big fonts."
  (run-shell-command
   (concat *wi-xterm-big-command* " -e screen")))

(defcommand wi-sensors () ()
  "Start XTerm with `sensors'."
  (run-shell-command
   (join (list *wi-xterm-big-command* "-e" "watch" "sensors")
         #\ )))

(defcommand wi-github-star () ()
  "Move mouse to star a project."
  (ratwarp 1308 176)
  (ratclick))

(defcommand wi-qemu-debian () ()
  "Run GNOME Debian in QEMU."
  (run-shell-command (concat "exec " (getenv "HOME") "/bin/debian.sh")))

(defparameter wi-dark-theme nil)
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
(wi-toggle-theme)

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
            "^>    "
            '(:eval (join (split-string (run-shell-command "sensors | grep 'Core.*°C' | cut -d ' ' -f 10 | tr -d [:cntrl:]" t) "°C") #\ ))
            "    %d"))
(setf *mode-line-pad-x* 0)
(setf *mode-line-pad-y* 0)
;; (mode-line)

;; TODO: Deny the all windows in the mpv class from taking focus.
;; (push '(:class "mpv") *deny-raise-request*)
;; (push '(:class "mpv") *deny-map-request*)

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

(defcommand wi-trans () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I en:ru'"))

(define-key *root-map* (kbd "m") "wi-mpv")
(define-key *root-map* (kbd "C-m") "wi-xclip-mpv")
(define-key *root-map* (kbd "M-m") "wi-xclip-streamlink")

(define-key *root-map* (kbd "u") "emacs-org-capture")
(define-key *root-map* (kbd "C-e") "wi-xclip-emacs")
(define-key *root-map* (kbd "C-M-c") "wi-xterm-big-screen")
(define-key *root-map* (kbd "M-e") "emacs-anywhere")

;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock -mode blank")
(define-key *root-map* (kbd "M-l") "turn-screen-off")

(define-key *root-map* (kbd "M-!") "run-xterm-command")
(define-key *root-map* (kbd "v") "pulsemixer")
(define-key *root-map* (kbd "C-v") "alsamixer")
(define-key *root-map* (kbd "c") "wi-run-or-raise-xterm")
(define-key *root-map* (kbd "C-c") "wi-run-xterm")

(define-key *root-map* (kbd "C-M-v") "scroll-other-window")
(define-key *root-map* (kbd "Print") "screenshot-default")

(define-key *root-map* (kbd "w") "conkeror")
(define-key *root-map* (kbd "C-w") "conkeror")
(define-key *root-map* (kbd "M-w") "firefox")

;; (define-key *top-map* (kbd "s-1") "gselect 1")
;; (define-key *top-map* (kbd "s-2") "gselect 2")
;; (define-key *top-map* (kbd "s-3") "gselect 3")
;; (define-key *top-map* (kbd "s-4") "gselect 4")
;; (define-key *top-map* (kbd "s-t") "trans")
(define-key *top-map* (kbd "s-o") "other-in-frame")
(define-key *top-map* (kbd "s-t") "pull-hidden-other")
;; (define-key *top-map* (kbd "s-c") "")
;; (define-key *top-map* (kbd "s-TAB") "fother")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "M-s-n") "gnext")
(define-key *top-map* (kbd "M-s-p") "gprev")

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

(load-module "globalwindows")

(load-module "command-history")

(load-module "winner-mode")

(define-key *root-map* (kbd "S-Left") "winner-undo")
(define-key *root-map* (kbd "S-Right") "winner-redo")

(add-hook *post-command-hook*
          (lambda (command)
            (when (member command winner-mode:*default-commands*)
              (winner-mode:dump-group-to-file))))

(defcommand dump-group-to-file (file) ((:rest "Dump To File: "))
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))
