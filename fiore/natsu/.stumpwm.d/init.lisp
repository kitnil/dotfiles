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

;; Use keyboard as mouse with <Shift+Num Lock>
;; https://en.wikipedia.org/wiki/Mouse_keys
(run-shell-command "setxkbmap -option keypad:pointerkeys")

;; Keyboard layout
;; (run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle")

;; Keyboard speed
;; (run-shell-command "xset s 0")
;; (run-shell-command "xset dpms 0 0 1800")

(run-shell-command "xmodmap ~/.Xmodmap")

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

(defun join (list &optional (delimiter #\ ))
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

(defvar *xterm-command*
  "exec xterm")

(defvar *xterm-big-command*
  "exec xterm -fa 'Monospace' -fs 24")

(defun w3m (url)
  "Return a `w3m' command to open a URL."
  (concat "w3m " url))

(defvar *xterm-theme-light*
  "-bg white -fg black")

(defvar *xterm-theme-dark*
  "-bg black -fg white")

(defvar *xterm-no-scrollbar*
  "+sb")

(defvar *st-command*
  "exec st")

(defvar *term-execute-flag*
  "-e")

(defvar *st-exec-flag* "-e")

(defvar *st-font-flag* "-f")

(defvar *st-font*
  "Monospace:size=12")

(defvar *conkeror-command*
  "conkeror")

(defvar *browser* *conkeror-command*)

(defvar *transmission-hostname*
  "magnolia")


;;;
;;; Emacs
;;;

(defcommand emacsclient () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c ." '(:class "Emacs")))

(defcommand emacs-org-capture () ()
  "Capture URL with Emacs Org from GUI clipboard"
  (run-shell-command
   (join (list "exec"
               (concat (getenv "HOME") "/bin/emacs-org-capture")
               (get-x-selection)))))


;;;
;;; Conkeror
;;;

(defcommand conkeror () ()
  "Start or focus conkeror."
  (run-or-raise *conkeror-command* '(:class "Conkeror")))

(defcommand browse-transmission () ()
  "Open transmissin WEB client."
  (run-shell-command (join (list *browser*
                                 (concat "http://torrent."
                                         *transmission-hostname* ".local")))))

;; Origin <https://github.com/alezost/stumpwm-config/blob/master/utils.lisp#L332>
(defcommand conkeror-browse-url (url) ((:shell "Browse URL: "))
  "Browse URL with conkeror."
  (run-prog "conkeror" :args (list url) :wait nil :search t))

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "Icecat")))

(defcommand firefox () ()
  "Start of focus firefox."
  (run-or-raise "firefox-latest" '(:class "Firefox")))

(defcommand chromium () ()
  "Start or focus Chromium."
  (run-or-raise "chromium" '(:class "Chromium-browser")))

(defcommand chromium-proxy () ()
  "Start Chromium via proxy"
  (run-shell-command (concat "chromium"
                             " --proxy-server='socks5://localhost:9050'"
                             " --host-resolver-rules='MAP * ~NOTFOUND"
                             " , EXCLUDE localhost'")))

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

(defcommand youtube () ()
  "Start Chromium YouTube"
  (run-shell-command (concat "chromium"
                             " --profile-directory=Default"
                             " --app-id=adnlfjpnmidfimlkaohpidplnoimahfh")))


;;;
;;; MPV
;;;

;; Look for audio devices ‘mpv --audio-device=help’
(defvar *headphones*
  "pulse/alsa_output.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "Pulseaudio device.  Heaphones.")

(defvar *mpv-program* "mpv"
 "The name by which to invoke MPV.")

(defparameter *mpv-headphones*
  nil
  "If non-nil use heaphones in MPV.")

(defvar *mpv-default-arguments*
  '("--keep-open=no"))

(defparameter *mpv-arguments*
  *mpv-default-arguments*)

(defcommand toggle-mpv-arguments () ()
  (if *mpv-headphones*
      (progn
        (setf *mpv-arguments* *mpv-default-arguments*)
        (setf *mpv-headphones* nil))
      (progn
        (setf *mpv-arguments*
              `(,@*mpv-default-arguments*
                ,(concat "--audio-device=" *headphones*)))
        (setf *mpv-headphones* t))))

(defcommand mpv () ()
  "Start or focus mpv."
  (run-or-raise (join `(,*mpv-program* ,@*mpv-arguments*))
                '(:class "mpv")))

(defcommand xclip-mpv () ()
  "Play video from clipboard with mpv."
  (run-shell-command
   (join `(,*mpv-program* ,@*mpv-arguments* ,(get-x-selection)))))

(defcommand mpv-watch () ()
  "Play video from file with mpv."
  (run-shell-command
   (join `(,*mpv-program* ,@*mpv-arguments* "$(cat /home/natsu/watch)"))))


;;;
;;; Streamlink
;;;

(defvar *streamlink-program* "streamlink")
(defvar *streamlink-arguments* '("-p" "mpv"))
(defvar *streamlink-player-arguments* nil)
(defvar *streamlink-quality* "best")

(defun single-quote-string (str)
  (let ((string-quote "'"))
    (concat string-quote str string-quote)))

(defun quote-string (str)
  (let ((string-quote "\""))
    (concat string-quote str string-quote)))

(defcommand xclip-streamlink () ()
  "Play video from clipboard with streamlink."
  (run-shell-command
   (join `(,*streamlink-program*
           ,@*streamlink-arguments*
           ,@(if *streamlink-player-arguments*
                 `("--player-args" ,(single-quote-string
                                     (join *streamlink-player-arguments*
                                           #\ ))))
           ,(get-x-selection)
           ,*streamlink-quality*))))

(defcommand xclip-emacs () ()
  "Open file from clipboard."
  (run-shell-command
   (join (list "exec emacsclient -c" (get-x-selection)))))

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


;;;
;;; youtube-dl
;;;

(defun youtube-dl-output (dir)
  (concat dir "/" "%(title)s.%(ext)s"))

(defvar *music-directory* "/srv/music")

(defvar *youtube-dl-output-music*
  (youtube-dl-output *music-directory*))

(defcommand youtube-dl () ()
  (term-shell-command (join (list "youtube-dl" (get-x-selection)))))

(defcommand youtube-dl-music () ()
  (let ((command (join (list "youtube-dl"
                             "--output"
                             (single-quote-string *youtube-dl-output-music*)
                             (get-x-selection)))))
    (message (format nil "Run: ~a" command))
    (term-shell-command command)))

(defcommand youtube-dl-play () ()
  "Download video and play it."
  (run-shell-command
   (join '("exec" "xterm" "-name" "youtube-dl" "-e" "youtube-dl"
           "--exec" "'mpv {}'"
           "$(xclip -o -selection clipboard)"))))


;;;
;;; Misc
;;;

(defun term-shell-command (command &optional (terminal 'xterm))
  (run-shell-command
   (let ((terminal-name (string-downcase (symbol-name terminal))))
     (case terminal
       ((xterm)
        (join (list terminal-name *xterm-theme-dark*
                    *xterm-no-scrollbar*
                    *term-execute-flag* command)))
       ((st)
        (join (list terminal-name
                    *st-font-flag* *st-font*
                    *st-exec-flag* command)))))))

(defcommand epson () ()
  (term-shell-command "sudo qemu-epson.sh"))

(defcommand epson-no-graphic () ()
  (term-shell-command "sudo qemu-epson.sh -display none"))

(defcommand glances () ()
  (term-shell-command "glances"))

(defcommand htop () ()
  (term-shell-command "htop"))

(defcommand rofi-drun () ()
  "Open Rofi to launch `.desktop' file."
  (run-shell-command "rofi -modi run,drun -show drun"))

(defcommand rofi-twitchy () ()
  "Open Rofi with Twitchy plugin."
  (run-shell-command "rofi -modi twitchy:rofi-twitchy -show twitchy"))

(defcommand twitchy () ()
  (term-shell-command "twitchy"))

(defcommand kodi-cli-youtube () ()
  "Send video from clipboard to Kodi."
  (run-shell-command
   (join (list "exec kodi-cli -y" (get-x-selection)))))

(defcommand turn-screen-off () ()
  "Turn screen off."
  (run-shell-command "exec xset dpms force off"))

(defcommand suspend () ()
  (run-shell-command "exec loginctl suspend"))

(defcommand run-xterm-command (cmd &optional collect-output-p)
    ((:shell "/bin/sh -c "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd "-e" cmd)))
            :wait nil))

(defcommand pulsemixer () ()
  (term-shell-command "pulsemixer" 'st))

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))

(defcommand run-or-raise-xterm () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-theme-light*))
   '(:class "XTerm")))

(defcommand run-xterm-light () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list *xterm-command*
                                         *xterm-theme-light*
                                         *xterm-no-scrollbar*)))
            :wait nil))

(defcommand run-xterm () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list *xterm-command*
                                         *xterm-theme-dark*
                                         *xterm-no-scrollbar*)))
            :wait nil))

(defcommand xterm-dark-no-scrollbar () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-theme-dark* "+sb"))
   '(:class "XTerm")))

(defcommand xterm-name (cmd &optional collect-output-p)
    ((:string "window name: "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd)))
            :wait nil))

(defcommand screen
    (session &optional collect-output-p)
    ((:string "session name: "))
  "Run `screen' session."
  (run-prog *shell-program*
            :args
            (list "-c"
                  (join (list "env" "STY=" ; Do not complain `$STY' in `screen'.
                              "xterm" "-title" session
                              "-e" "screen" "-S" session)))
            :wait nil))

(defcommand xterm-big () ()
  "Start XTerm with big fonts."
  (run-shell-command *xterm-big-command*))

(defcommand xterm-big-screen () ()
  "Start XTerm with big fonts."
  (run-shell-command
   (concat *xterm-big-command* " -e screen")))

(defcommand sensors () ()
  "Start XTerm with `sensors'."
  (run-shell-command
   (join (list *xterm-big-command* "-e" "watch" "sensors"))))

(defcommand github-star () ()
  "Move mouse to star a project."
  (ratwarp 1308 176)
  (ratclick))

(defcommand qemu-debian () ()
  "Run GNOME Debian in QEMU."
  (run-shell-command (concat "exec " (getenv "HOME") "/bin/debian.sh")))

(defparameter dark-theme nil)
(set-focus-color "#daa520")
(set-border-color "#daa520")
(set-float-focus-color "#daa520")
(defcommand toggle-theme () ()
  (if dark-theme
      (progn (setq *mode-line-border-color*     "#ffffff"
                   *mode-line-foreground-color* "#000000"
                   *mode-line-background-color* "#ffffff")
             (set-win-bg-color "#dcdad5")
             (set-unfocus-color "#FFFFFF")
             (set-fg-color "#000000")
             (set-bg-color "#ffffff")
             (run-shell-command "xsetroot -solid grey")
             (setq dark-theme nil))
      (progn (setq *mode-line-border-color*     "#000000"
                   *mode-line-foreground-color* "#ffffff"
                   *mode-line-background-color* "#000000")
             (set-win-bg-color "#000000")
             (set-unfocus-color "#000000")
             (set-fg-color "#ffffff")
             (set-bg-color "#000000")
             (run-shell-command "xsetroot -solid black")
             (setq dark-theme t))))
(toggle-theme)

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
            '(:eval (join (split-string (run-shell-command "sensors | grep 'Core.*°C' | cut -d ' ' -f 10 | tr -d [:cntrl:]" t) "°C")))
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

(defcommand trans () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I en:ru'"))

(define-key *root-map* (kbd "e") "emacsclient")

(define-key *root-map* (kbd "m") "mpv")
(define-key *root-map* (kbd "C-m") "xclip-mpv")
(define-key *root-map* (kbd "M-m") "xclip-streamlink")

(define-key *root-map* (kbd "u") "emacs-org-capture")
(define-key *root-map* (kbd "C-e") "xclip-emacs")
(define-key *root-map* (kbd "C-M-c") "xterm-big-screen")
(define-key *root-map* (kbd "M-e") "emacs-anywhere")

;; Lock screen
(define-key *root-map* (kbd "C-l") "exec xlock -mode blank")
(define-key *root-map* (kbd "M-l") "turn-screen-off")

(define-key *root-map* (kbd "M-!") "run-xterm-command")
(define-key *root-map* (kbd "v") "pulsemixer")
(define-key *root-map* (kbd "C-v") "alsamixer")
(define-key *root-map* (kbd "c") "run-or-raise-xterm")
(define-key *root-map* (kbd "C-c") "run-xterm")
(define-key *root-map* (kbd "C-M-c") "run-xterm-light")

(define-key *root-map* (kbd "C-M-v") "scroll-other-window")
(define-key *root-map* (kbd "Print") "screenshot-default")

(define-key *root-map* (kbd "w") "conkeror")
(define-key *root-map* (kbd "C-w") "conkeror")
(define-key *root-map* (kbd "M-w") "firefox")

(define-key *top-map* (kbd "s-m") "mpv")
(define-key *top-map* (kbd "s-v") "xclip-mpv")
(define-key *top-map* (kbd "s-e") "emacsclient")
(define-key *top-map* (kbd "s-w") "firefox")
(define-key *top-map* (kbd "s-c") "run-or-raise-xterm")

(define-key *top-map* (kbd "s-o") "other-in-frame")
(define-key *top-map* (kbd "s-t") "pull-hidden-other")
(define-key *top-map* (kbd "s-\"") "frame-windowlist")
(define-key *top-map* (kbd "s-s") "sibling")
;; (define-key *top-map* (kbd "s-c") "")
;; (define-key *top-map* (kbd "s-TAB") "fother")
(define-key *top-map* (kbd "M-s-n") "gnext")
(define-key *top-map* (kbd "M-s-p") "gprev")
(define-key *top-map* (kbd "s-n") "pull-hidden-next")
(define-key *top-map* (kbd "s-p") "pull-hidden-previous")

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us" "ru")

(ql:quickload "zpng")
(load-module "screenshot")

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

(ql:quickload "clx-truetype")
(setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "DejaVu Sans Mono"
                         :subfamily "Book"
                         :size 14))

(defcommand majordomo-web-health () ()
  "Run `xterm' with `jord-health' script."
  (run-shell-command "exec xterm -name web-health -e '~/bin/jord-health && echo \"\" && read -n 1 -s -r -p \"Press any key to close.\"'"))

(define-key *top-map* (kbd "s-h") "majordomo-web-health")

(defcommand ponymix-decrease () ()
  (run-shell-command "ponymix decrease 5"))

(defcommand ponymix-increase () ()
  (run-shell-command "ponymix increase 5"))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "ponymix-increase")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "ponymix-decrease")

(define-key *root-map* (kbd "C-b") "warp-mouse-active-frame")
