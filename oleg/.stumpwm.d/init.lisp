;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

(set-module-dir "/home/oleg/.stumpwm.d/modules/")

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(defcommand cursor-theme () ()
  (run-shell-command "xsetroot -cursor_name left_ptr"))

(cursor-theme)

(defcommand xrdb () ()
  (run-shell-command "xrdb /home/oleg/.Xresources"))

(xrdb)

(defcommand speaker-disable () ()
  (run-shell-command "xset -b"))

(speaker-disable)

(defcommand current-window->clipboard () ()
  (putsel (window-title (current-window))))

;;;
;;; Keyboard
;;;

;; Use keyboard as mouse with <Shift+Num Lock>
;; https://en.wikipedia.org/wiki/Mouse_keys
(defcommand pointer-keys () ()
  (run-shell-command "setxkbmap -option keypad:pointerkeys"))

(pointer-keys)

;; Keyboard layout
(defcommand keyboard-layout () ()
  (run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle"))

(keyboard-layout)

(defcommand xmodmap () ()
  (run-shell-command "xmodmap " "/home/oleg/.Xmodmap"))

(xmodmap)

(progn
  (set-msg-border-width 4)
  (setf *ignore-wm-inc-hints* t)
  (setf *maxsize-border-width* 3)
  (setf *message-window-y-padding* 3)
  (setf *normal-border-width* 3)
  (setf *transient-border-width* 3)
  (setf *window-border-style* :thin)
  )


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

(defcommand mouse-click () ()
  (setf *mouse-focus-policy* :click))

(defcommand mouse-sloppy () ()
  (setf *mouse-focus-policy* :sloppy))

(setf *suppress-window-placement-indicator* t)
(setf *new-frame-action* :empty)

(defcommand desktop-restore (desktop rules) ((:string "Restore desktop: ")
                                             (:string "Restore rules: "))
  (let ((desktop (format nil "~a/.stumpwm.d/desktop/~a.lisp" "/home/oleg" desktop))
        (rules (format nil "~a/.stumpwm.d/rules/~a.lisp" "/home/oleg" rules)))
    (message (format nil "Restore desktop from ~s file." desktop))
    (message (format nil "Restore rules from ~s file." rules))
    (restore-from-file desktop)
    (clear-window-placement-rules)
    (restore-window-placement-rules rules)
    (place-existing-windows)))


;;;
;;; XTerm
;;;

(defvar *default-group-name*
  "default")

(setf *window-format* "%m%n%s %c %50t")

(defvar *xterm-command* "/home/oleg/.guix-profile/bin/xterm")
(defvar *xterm-big-command*
  (join '("exec" "/home/oleg/.guix-profile/bin/xterm"
          "-fa" "Monospace" "-fs" "24")))
(defvar *xterm-no-scrollbar* "+sb")
(defvar *xterm-theme-dark* "-bg black -fg white")
(defvar *xterm-theme-light* "-bg white -fg black")
(defvar *browser* "icecat")
(defvar *st-command* "exec st")
(defvar *st-exec-flag* "-e")
(defvar *st-font* "Monospace:size=12")
(defvar *st-font-flag* "-f")
(defvar *term-execute-flag* "-e")
(defvar *transmission-hostname* "magnolia")


;;;
;;; Emacs
;;;

(defcommand emacsclient () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

(defcommand emacsclient-new () ()
  (run-shell-command "emacsclient -c"))

(defcommand emacs-org-capture () ()
  "Capture URL with Emacs Org from GUI clipboard"
  (run-shell-command
   (join (list "exec"
               (concat (getenv "HOME") "/bin/emacs-org-capture")
               (get-x-selection)))))


;;;
;;; WEB
;;;

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

(defcommand firefox () ()
  "Start of focus firefox."
  (run-or-raise "firefox" '(:class "Firefox")))

(defcommand firefox-new-window () ()
  "Start Firefox."
  (run-shell-command "firefox --new-window"))

(defcommand chromium () ()
  "Start or focus Chromium."
  (run-or-raise "chromium" '(:class "Chromium-browser")))

(define-key *top-map* (kbd "C-s-w") "chromium")

(defcommand chromium-new-window () ()
  "Start Chromium."
  (run-shell-command "chromium"))

(define-key *top-map* (kbd "C-s-W") "chromium-new-window")

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
  (run-shell-command "chromium --app=https://youtube.com/"))


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
  (let ((clipboard (get-x-selection)))
    (run-shell-command
     (join `(,*mpv-program* ,@*mpv-arguments* ,clipboard)))
    (message (concat "Play " clipboard))))

(defcommand xclip-kdeconnect-handler () ()
  "Open URL on Android device via KDE Connect."
  (let ((clipboard (get-x-selection)))
    (run-shell-command
     (join `("kdeconnect-handler" ,clipboard)))
    (message (concat "Open URL on Android " clipboard))))

(defcommand mpv-watch () ()
  "Play video from file with mpv."
  (run-shell-command
   (join `(,*mpv-program* ,@*mpv-arguments* ,(concat "$(cat " "/home/oleg" "/watch)")))))

(defcommand majordomo-vnc () ()
  "Connect to Majordomo VNC"
  (run-shell-command "majordomo-vnc"))

(define-key *root-map* (kbd "V") "majordomo-vnc")


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

(defun term-shell-command (command &key (terminal 'xterm) (color (if dark-theme "dark" "light")) (font nil) (title nil))
  (run-shell-command
   (let ((terminal-name (string-downcase (symbol-name terminal))))
     (case terminal
       ((xterm)
        (xterm-command :color color :command command :font font :title title))
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

(defcommand sampler () ()
  (term-shell-command "sampler -c ~/.config/sampler/config.yaml"
                      :color "dark"
                      :font '("-fa" "Monospace" "-fs" "10")))

(define-key *top-map* (kbd "C-s-h") "sampler")

(defcommand top () ()
  (term-shell-command "top"))

(defcommand htop () ()
  (term-shell-command "htop"))

(defcommand guile () ()
  (term-shell-command "guile"))

(defcommand guix-repl () ()
  (term-shell-command "guix repl"))

(defcommand guix-pull () ()
  (term-shell-command "sh -c 'guix pull; read'"))

(defcommand notmuch () ()
  (term-shell-command "sh -c 'notmuch new; read'"))

(defcommand python () ()
  (term-shell-command "python3"))

(defcommand neofetch () ()
  (term-shell-command "sh -c 'neofetch; read'"))

(define-key *top-map* (kbd "C-s-s") "neofetch")

(defcommand rofi-drun () ()
  "Open Rofi to launch `.desktop' file."
  (run-shell-command "rofi -modi run,drun -show drun"))

(defcommand rofi-ssh () ()
  "Open Rofi ssh list."
  (run-shell-command
   (format nil "rofi -ssh-command '{terminal} -fg ~s -bg ~s -title \"ssh@{host}\" -e {ssh-client} {host}' -width 20 -terminal '~a' -modi ssh -show ssh"
           (if dark-theme "#ffffff" "#000000")
           (if dark-theme "#000000" "#f0fff0")
           (xterm-command))))

(defcommand rofi-mycli () ()
  "Open Rofi mycli."
  (run-shell-command "rofi -modi mycli:/home/oleg/bin/rofi-mycli -show mycli"))

(define-key *top-map* (kbd "C-S-s-RET") "rofi-mycli")

(defcommand rofi-window () ()
  "Open Rofi window list."
  (run-shell-command "rofi -modi window -show window"))

(defcommand rofi-twitchy () ()
  "Open Rofi with Twitchy plugin."
  (run-shell-command "rofi -modi twitchy:rofi-twitchy -show twitchy"))

;; (define-key *root-map* (kbd "@") "rofi-drun")

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
            :args (list "-c" (join (list "/home/oleg/.guix-profile/bin/xterm -name" cmd "-e" cmd)))
            :wait nil))

(defcommand pulsemixer () ()
  (term-shell-command "pulsemixer" :terminal 'st))

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))

(defcommand run-or-raise-xterm () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-theme-dark* *xterm-no-scrollbar*))
   '(:class "XTerm")))

(defcommand run-or-raise-xterm-named (title) ((:string "title: "))
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-no-scrollbar* "-sl" "100000" "-title" title))
   `(:class "XTerm" :title ,title)))

(defun small-framep ()
  (cond ((string= (class-name (class-of (current-group))) "FLOAT-GROUP") nil)

        ((string= (getenv "DISPLAY") ":1")
         (let ((frame (frame-number (tile-group-current-frame (current-group)))))
           (if (= frame 0) nil t)))

        ((string= (getenv "DISPLAY") ":0")
         (let ((frame (frame-number (tile-group-current-frame (current-group)))))
           (if (string-equal (group-name (current-group)) "Default")
               (not (or (= frame 2) (= frame 1)))
               (not (or (= frame 0) (= frame 1))))))

        (t nil)))

(defcommand current-frame-smallp () ()
  (if (small-framep)
      (progn (message "small") 1)
      (progn (message "big") 0)))

(defun xterm-command (&key (color (if dark-theme "dark" "light")) (command nil) (title nil) (font nil))
  (join `(,*xterm-command*
          ;; Make sure XTerm terminal size is appropriate for current StumpWM frame.
          ,@(if font
                font
                (if (small-framep)
                    '("-fa" "Monospace" "-fs" "8")
                    '()))
          "-sl" "1000000" ;number of lines
          ,*xterm-no-scrollbar*
          ,(if (string= color "light") *xterm-theme-light* *xterm-theme-dark*)
          ,@(if title `("-title" ,title) '())
          ,@(if command `("-e" ,command) '()))))

(defcommand run-xterm () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (xterm-command))
            :wait nil))

(defcommand xterm-dark-no-scrollbar () ()
  "Start or focus XTerm."
  (run-shell-command (xterm-command :color (if dark-theme "light" "dark"))))

(define-key *top-map* (kbd "C-M-S-s-RET") "xterm-dark-no-scrollbar")

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

(defcommand st () ()
  "Start st."
  (run-shell-command "st -f 'Monospace:size=12'"))

(defcommand st-tmux () ()
  "Start st with tmux."
  (run-shell-command "st -f 'Monospace:size=12' -e tmux"))

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

(setf *float-window-border* 0)
(setf *float-window-title-height* 0)

(setf *mode-line-position* :bottom)

(defparameter dark-theme t)
(progn
  (set-focus-color "#90EE90")
  (set-border-color "#90EE90")
  (set-float-focus-color "#90EE90"))
(defcommand toggle-theme () ()
  (if dark-theme
      (progn (setq *mode-line-border-color*     "#ffffff"
                   *mode-line-foreground-color* "#000000"
                   *mode-line-background-color* "#ffffff")
             (set-win-bg-color "#ffffff")
             (set-unfocus-color "#ffffff")
             (set-fg-color "#000000")
             (set-bg-color "#ffffff")
             (run-shell-command "xsetroot -solid '#ffffff'")
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
(setq *suppress-frame-indicator* t)
;; (sync-all-frame-windows (current-group))

(setf *mode-line-timeout* 2)
(setf *TIME-MODELINE-STRING* "%a, %e %b %Y %k:%M")
(setf *screen-mode-line-format*
      (list "[%n]:" '(:eval (write-to-string (group-number (current-group))))
            (make-string 4 :initial-element #\space)
            '(:eval (write-to-string (window-number (current-window))))
            ":"
            "["
            '(:eval (window-class (current-window)))
            " "
            '(:eval (window-name (current-window)))
            "]"
            "^>    "
            "%N"
            "    %d"))
(setf *mode-line-pad-x* 10)
(setf *mode-line-pad-y* 5)
(setf *mode-line-border-width* 0)
(mode-line)

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

(defcommand xfce-screenshooter () ()
    (run-shell-command "xfce4-screenshooter"))

(define-key *top-map* (kbd "SunPrint_Screen") "xfce-screenshooter")

(defcommand trans-en-ru () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I en:ru'"))

(defcommand trans-ru-en () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I ru:en'"))

(define-key *root-map* (kbd "e") "emacsclient")

(defcommand osd-sound () ()
  (run-shell-command "if pgrep -f osd-sound > /dev/null; then pkill osd-sound; osd-sound; else osd-sound; fi"))

(defcommand volume-decrease () ()
  (run-shell-command "ponymix decrease 5"))

(defcommand volume-increase () ()
  (run-shell-command "ponymix increase 5"))

(define-interactive-keymap volume nil
  ((kbd "-") "volume-decrease")
  ((kbd "=") "volume-increase"))

(defcommand pavucontrol () ()
  (run-shell-command "pavucontrol"))

(define-key *root-map* (kbd "c") "run-xterm")
(define-key *root-map* (kbd "C-c") "run-xterm")
(define-key *root-map* (kbd "C-M-c") "run-xterm")
(define-key *top-map* (kbd "C-s-RET") "run-or-raise-xterm-named")

(defcommand xfce-terminal () ()
  (run-shell-command "xfce4-terminal"))

(define-key *top-map* (kbd "M-s-RET") "xfce-terminal")

(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
     collect n))

(define-key *top-map* (kbd "M-s-n") "gnext")
(define-key *top-map* (kbd "M-s-p") "gprev")

(defcommand emacs-shell () ()
  ""
  (run-shell-command "stumpish emacsclient")
  (run-shell-command "emacsclient -e '(progn (shell) (delete-other-windows))'"))

(define-key *root-map* (kbd "quoteleft") "emacs-shell")

(defcommand dump-group-to-file (file) (:rest "Dump To File: ")
  "Dumps the frames of the current group of the current screen to the named file."
  (dump-to-file (dump-group (current-group)) file))

(defcommand majordomo-web-health () ()
  "Run `xterm' with `jord-health' script."
  (run-shell-command "exec xterm -name web-health -e '~/bin/jord-health && echo \"\" && read -n 1 -s -r -p \"Press any key to close.\"'"))

(defcommand jord-loadavg () ()
  "Run `xterm' with `jord-loadavg' script."
  (run-shell-command "xterm -name web-health -e '~/bin/jord-loadavg && echo \"\" && read -n 1 -s -r -p \"Press any key to close.\"'"))

(defcommand jord-php () ()
  "Run `xterm' with `jord-php' script."
  (run-shell-command "exec xterm -name web-health -e '~/bin/jord-php | grep -v 200 && echo \"\" && read -n 1 -s -r -p \"Press any key to close.\"'"))

(defcommand ponymix-decrease () ()
  (run-shell-command "ponymix decrease 5"))

(defcommand ponymix-increase () ()
  (run-shell-command "ponymix increase 5"))

(define-key *top-map* (kbd "XF86AudioRaiseVolume") "ponymix-increase")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "ponymix-decrease")

(defcommand xkill () ()
  "Run `xkill'."
  (run-shell-command "xkill"))

(defcommand vnc-magnolia () ()
  (run-shell-command "exec vncviewer localhost:59555"))

(defcommand pass-route () ()
  (window-send-string "***REMOVED***
"))

(define-key *top-map* (kbd "C-s-a") "pass-route")

;; XXX: Security
(defcommand pass-eng () ()
  (window-send-string "***REMOVED***
"))

;; XXX: Security
(defcommand pass-sup () ()
  (window-send-string "***REMOVED***
"))

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(flet ((firefox (url &optional dark)
         (run-shell-command (format nil (if dark
                                            "GTK_THEME=Adwaita:dark firefox -P dark --new-window ~S"
                                            "firefox -P light --new-window ~S")
                                    url))))

  (defcommand firefox-light (url) ((:string "URL: "))
    (firefox url))

  (defcommand firefox-dark (url) ((:string "URL: "))
    (firefox url t))

  (defcommand alerta () ()
    (firefox "https://alerta.wugi.info/"))

  (defcommand grafana () ()
    (firefox "https://grafana.wugi.info/" t))

  (defcommand majordomo-la-24-hours () ()
    (firefox "http://grafana.intr/d/000000021/telegraf-system-dashboard-shared?panelId=54694&fullscreen&orgId=1&var-datasource=influx-telegraf&var-inter=$__auto_interval_inter&var-server=web.*&var-mountpoint=All&var-cpu=All&var-disk=All&var-netif=All&var-server_role=shared-hosting&var-dc=Miran&from=now-12h&to=now" t))

  (defcommand majordomo-upstream () ()
    (firefox "http://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?refresh=5s&orgId=1"))

  (defcommand alerta () ()
    (firefox "http://alerta.intr/#/alerts?sort-by=lastReceiveTime&status=open&status=unknown&environment=Production"))

  (defcommand cerb () ()
    (firefox "http://cerberus.intr/index.php/"))

  (defcommand majordomo-zabbix () ()
    (firefox "https://zabbix.intr/dashboard.php?fullscreen=1"))

  (defcommand zabbix () ()
    (firefox "https://zabbix.wugi.info/"))

  (defcommand kibana () ()
    (firefox "http://localhost:5601/app/kibana"))

  (defcommand gitlab () ()
    (firefox "https://gitlab.wugi.info/" t))

  (defcommand music-youtube () ()
    (run-or-raise "chromium --app=https://music.youtube.com/"
                  '(:instance "music.youtube.com")))

  (defcommand youtube () ()
    (firefox "https://www.youtube.com/feed/subscriptions" t))

  (defcommand twitch () ()
    (firefox "https://www.twitch.tv/directory/game/Tales%20of%20Maj'Eyal" t))

  (defcommand jenkins-index () ()
    (firefox "https://jenkins.wugi.info/"))

  (defcommand jenkins-last-build-guixsd () ()
    (firefox "https://jenkins.wugi.info/job/fiore/lastBuild/console"))

  (defcommand tometips () ()
    (run-shell-command "chromium --app=https://tometips.github.io"))

  (defcommand discord () ()
    (run-shell-command "chromium --app=https://discordapp.com/"))

  (defcommand jenkins () ()
    (run-or-raise "chromium --app=http://localhost:8090/"
                  '(:instance "jenkins")))

  (defcommand ci (package) ((:string "package: "))
    (unless (string= package "")
      (run-shell-command (concat "chromium --app=http://ci.guix.info/search?query=spec%3Aguix-master+system%3Ax86_64-linux+" package))))

  (defcommand cuirass () ()
    (firefox "https://grafana.wugi.info/d/Ob67YJYiz/fiore?refresh=30s&orgId=1&var-host=cuirass"))

  (defcommand yoo (url) ((:string "YouTube URL: "))
    (gnew "youtube")
    (restore-group (current-group) (read-dump-from-file "/home/oleg/youtube.lisp"))
    (term-shell-command (format nil "mpv --no-resume-playback --mute=yes ~s" url))
    (firefox (format nil "https://www.youtube.com/live_chat?v=~a&is_popout=1"
                     (cadr (split-string url "=")))
             t)))

(defcommand xpanes-top () ()
  (term-shell-command "xpanes -C 1 -c 'autossh -M0 -t {} -- top -d 10' localhost spb workstation oracle"
                      :color 'dark))

(defcommand xpanes-guix () ()
  (term-shell-command "xpanes -C 1 -c 'ssh -t {}' workstation spb"
                      :color 'dark))

(defun emacsclient-command (&rest args)
  (run-shell-command (format nil "emacsclient ~a" (join args))))

(defun emacsclient-eval (command)
  (emacsclient-command (format nil "-e ~s" command)))

(defun emacs-buffer (buffer)
  (emacsclient-command "-s" "chat"
                       (format nil "-e ~s"
                               (format nil "(switch-to-buffer ~s)" buffer))
                       "-c"))

(defun grid-split-3x3 ()
  (flet ((split ()
           (progn
             (hsplit)
             (vsplit)
             (fnext)
             (fnext)
             (vsplit)
             (fnext)
             (fnext))))
    (progn
      (hsplit)
      (vsplit)
      (split)
      (split)
      (vsplit)
      (split)
      (split))))

(defun emacs-erc ()
  (mapcar #'(lambda (buffer)
              (emacs-buffer buffer))
          '("#bash" "#bootstrappable" "##C" "#chicken" "#emacs" "#erc" "#fsf"
            "#gdb" "#gnus" "#guile" "#guix" "##linux" "#lisp" "#nixos"
            "#scheme" "#stumpwm")))

(defcommand gnus () ()
  (progn (run-shell-command "emacsclient --eval '(gnus)'")
         (run-shell-command "stumpish emacsclient")))

(defcommand majordomo-find-project () ()
  (progn (run-shell-command "emacsclient --eval '(majordomo-ivy-find-project)'")
         (run-shell-command "stumpish emacsclient")))

(defcommand org () ()
  (progn (run-shell-command "emacsclient --eval '(plain-org-wiki)'")
         (run-shell-command "stumpish emacsclient")))

(defcommand guix-wigust () ()
  (progn (run-shell-command "emacsclient --eval '(let ((default-directory (expand-file-name \"~/src/guix-wigust/guix/wigust/packages/\"))) (counsel-find-file))'")
         (run-shell-command "stumpish emacsclient")))

(defcommand helm-tramp () ()
  (progn (run-shell-command "emacsclient --eval '(helm-tramp)'")
         (run-shell-command "stumpish emacsclient")))

(define-key *top-map* (kbd "C-s-e") "helm-tramp")

(defun auto-pull-frames ()
  (mapcar #'(lambda (frame)
              (pull-window-by-number frame)
              (fnext))
          (range 15 :min 0 :step 1)))

(defmacro emacs-bind (key command)
  `(progn
     (defcommand ,(intern (concat "emacs-" command)) () ()
       (emacsclient-eval (format nil "(~a)" ,command)))
     (define-key *top-map* (kbd ,key) ,(concat "emacs-" command))))

(defcommand emacs-mms-next () ()
  (emacsclient-eval (format nil "(~a)" "emms-next")))

(defcommand emacs-emms-previous () ()
  (emacsclient-eval (format nil "(~a)" "emms-previous")))

(defcommand emacs-emms-pause () ()
  (emacsclient-eval (format nil "(~a)" "emms-pause")))

(defcommand emacs-emms-random () ()
  (emacsclient-eval (format nil "(~a)" "emms-random")))

(defcommand racket () ()
  (run-shell-command "drracket"))

(defcommand resolution () ()
  (run-shell-command "xrandr --output HDMI1 --mode 1920x1080 ; xgamma -gamma 1.0"))

(mapcar #'(lambda (x)
            (gnew (write-to-string x)))
	(range 10 :min 2 :step 1))

(gnew-float "0")

(stumpwm:run-commands "gselect 1")

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(defcommand window-resize-by-half-horizontal () ()
  ""
  (resize (- (round (/ (parse-integer (format-expand *window-formatters* "%w" (current-window))) 2.0))) 0))

(define-key *top-map* (kbd "C-s-Right") "window-resize-by-half-horizontal")

(defcommand window-resize-by-half-vertical () ()
  ""
  (resize 0 (- (round (/ (parse-integer (format-expand *window-formatters* "%h" (current-window))) 2)))))

(define-key *top-map* (kbd "C-s-Left") "window-resize-by-half-vertical")

(defcommand emms () ()
  (unless (uiop/utility:string-prefix-p "emacs"
                                        (window-name (current-window)))
    (run-shell-command "stumpish emacsclient"))
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (emms) (delete-other-windows)))))))

(defcommand emacs-gnus () ()
  (unless (uiop/utility:string-prefix-p "emacs"
                                        (window-name (current-window)))
    (run-shell-command "stumpish emacsclient"))
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (gnus) (delete-other-windows)))))))

(defcommand guix () ()
  (unless (uiop/utility:string-prefix-p "emacs"
                                        (window-name (current-window)))
    (run-shell-command "stumpish emacsclient"))
  (run-shell-command "magit ~/src/guix"))

(defcommand elfeed () ()
  (unless (uiop/utility:string-prefix-p "emacs"
                                        (window-name (current-window)))
    (run-shell-command "stumpish emacsclient"))
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (elfeed) (delete-other-windows)))))))

(define-key *top-map* (kbd "C-s-Up") "next-in-frame")
(define-key *top-map* (kbd "C-s-Down") "prev-in-frame")

(defcommand vnc (display) ((:string "display: "))
  (run-shell-command (concat "vncviewer 127.0.0.1:" display)))

(defcommand clipmenu () ()
  (run-shell-command "CM_HISTLENGTH=25 CM_LAUNCHER=rofi clipmenu"))

(define-key *root-map* (kbd "y") "clipmenu")
(define-key *root-map* (kbd "C-y") "clipmenu")
(define-key *root-map* (kbd "Y") "xclip-kdeconnect-handler")

(defcommand mpv-music () ()
  (run-shell-command "mpv https://www.youtube.com/playlist?list=PLmjgicsUWIkvEKkLN01vm85neXAik3yU2"))

(defcommand alerta-close-youtube () ()
  (run-shell-command "alerta close --filter resource=YouTube"))

(define-remapped-keys
    '(("(Firefox|Chrome)"
       ("C-n"   . "Down")
       ("C-p"   . "Up")
       ;; ("C-f"   . "Right")
       ;; ("C-b"   . "Left")
       ;; ("C-v"   . "Next")
       ;; ("M-v"   . "Prior")
       ;; ("M-w"   . "C-c")
       ;; ("C-w"   . "C-x")
       ;; ("C-y"   . "C-v")
       ;; ("M-<"   . "Home")
       ;; ("M->"   . "End")
       ;; ("C-M-b" . "M-Left")
       ;; ("C-M-f" . "M-Right")
       ;; ("C-k"   . ("C-S-End" "C-x"))
       )))

;; https://lists.gnu.org/archive/html/help-guix/2017-01/msg00033.html
(require "asdf")
(load "/home/oleg/.guix-profile/share/emacs/site-lisp/guix.d/slime-2.24/swank.asd")
(require :swank)
(defcommand swank (port) ((:string "Port number: "))
  (sb-thread:make-thread
   (lambda ()
     (swank:create-server :port (parse-integer port) :dont-close t))))

(defcommand screen-off () ()
  (run-shell-command "xrandr --output HDMI3 --off"))

(defcommand screen-on () ()
  (run-shell-command "xrandr --output HDMI3 --auto && xrandr --output HDMI3 --right-of HDMI1"))

(defcommand sxhkd-restart () ()
  (run-shell-command "pkill sxhkd")
  (run-shell-command "sxhkd"))

(defcommand passmenu () ()
  (run-shell-command "passmenu"))

(defcommand dunst-disable () ()
  (run-shell-command "pkill -SIGUSR1 dunst"))

(defcommand dunst-enable () ()
  (run-shell-command "pkill -SIGUSR2 dunst"))

(load-module "notifications")

(load-module "command-history")

(load-module "swm-gaps")

;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 5)

;; Inner gaps run along all the 4 borders of a window
(setf swm-gaps:*inner-gaps-size* 5)

(setf swm-gaps:*outer-gaps-size* 0)

(defcommand alerta () ()
  (run-or-raise "" '(:title "Alerta - Mozilla Firefox")))

(defun bind-obs ()
  ;; TODO: Unhardcode window id.
  ;;       Don't use xdotool.

  (defcommand obs-firefox () ()
    "Start of focus firefox."
    (run-or-raise "firefox" '(:class "Firefox"))
    ;; (window-send-string (kbd "s-3"))
    ;; (run-shell-command "for window in $(xdotool search --class obs); do xdotool key --window $window key super+3; done")
    (run-shell-command "xdotool key --window 65011717 key super+3"))

  (define-key *top-map* (kbd "s-w") "obs-firefox")
  (define-key *top-map* (kbd "s-W") "firefox")

  (defcommand obs-emacsclient () ()
    "Start emacs unless it is already running, in which case focus it."
    (run-or-raise "emacsclient -c" '(:class "Emacs"))
    (run-shell-command "xdotool key --window 65011717 key super+4"))

  (define-key *top-map* (kbd "s-e") "obs-emacsclient")
  (define-key *top-map* (kbd "s-E") "emacsclient")
  (define-key *top-map* (kbd "C-s-E") "emacsclient-new"))

(defun bind-super ()
  (define-key *top-map* (kbd "s-+") "pavucontrol")
  (define-key *top-map* (kbd "s-_") "pulsemixer")
  (define-key *top-map* (kbd "s-r") "guile")
  (define-key *top-map* (kbd "s-R") "guix-repl")
  (define-key *top-map* (kbd "C-s-r") "python")
  (define-key *top-map* (kbd "s-f") "fullscreen")
  (define-key *top-map* (kbd "s-H") "glances")
  (define-key *top-map* (kbd "s-t") "top")
  (define-key *top-map* (kbd "s-T") "tometips")
  (define-key *top-map* (kbd "s-h") "htop")
  (define-key *top-map* (kbd "s-x") "rofi-drun")
  (define-key *top-map* (kbd "S-s-RET") "rofi-ssh")
  (define-key *top-map* (kbd "s-quoteright") "rofi-window")
  (define-key *top-map* (kbd "s-KP_Add") "volume-increase")
  (define-key *top-map* (kbd "s-KP_Subtract") "volume-decrease")
  (define-key *top-map* (kbd "s-RET") "run-xterm")
  (define-key *top-map* (kbd "s-e") "emacsclient")
  (define-key *top-map* (kbd "s-E") "emacsclient-new")
  (define-key *top-map* (kbd "s-m") "mpv")
  (define-key *top-map* (kbd "s-n") "next-in-frame")
  (define-key *top-map* (kbd "s-p") "prev-in-frame")
  (define-key *top-map* (kbd "s-w") "firefox")
  (define-key *top-map* (kbd "s-W") "firefox-new-window")
  (define-key *top-map* (kbd "s-quoteleft") "emacs-shell")
  (define-key *top-map* (kbd "s-=") "ponymix-increase")
  (define-key *top-map* (kbd "s--") "ponymix-decrease")
  (define-key *top-map* (kbd "s-a") "pass-eng")
  (define-key *top-map* (kbd "s-A") "pass-sup")
  (define-key *top-map* (kbd "s-j") "music-youtube")
  (define-key *top-map* (kbd "s-J") "jenkins")
  (define-key *top-map* (kbd "s-g") "gnus")
  (define-key *top-map* (kbd "s-C") "org")
  (define-key *top-map* (kbd "C-s-C") "majordomo-find-project")
  (define-key *top-map* (kbd "s-i") "guix-wigust")
  (define-key *top-map* (kbd "s-!") "gmove-and-follow 1")
  (define-key *top-map* (kbd "s-@") "gmove-and-follow 2")
  (define-key *top-map* (kbd "s-#") "gmove-and-follow 3")
  (define-key *top-map* (kbd "s-$") "gmove-and-follow 4")
  (define-key *top-map* (kbd "s-%") "gmove-and-follow 5")
  (define-key *top-map* (kbd "s-^") "gmove-and-follow 6")
  (define-key *top-map* (kbd "s-&") "gmove-and-follow 7")
  (define-key *top-map* (kbd "s-*") "gmove-and-follow 8")
  (define-key *top-map* (kbd "s-(") "gmove-and-follow 9")
  (define-key *top-map* (kbd "s-)") "gmove-and-follow 0")
  (define-key *top-map* (kbd "s-KP_Enter") "run-xterm")
  (define-key *top-map* (kbd "s-k") "delete")
  (define-key *top-map* (kbd "s-Right") "move-focus right")
  (define-key *top-map* (kbd "s-Left") "move-focus left")
  (define-key *top-map* (kbd "s-Up") "move-focus up")
  (define-key *top-map* (kbd "s-Down") "move-focus down")
  (define-key *top-map* (kbd "s-Tab") "other-in-frame")
  (define-key *top-map* (kbd "C-s-Tab") "fother")
  (define-key *top-map* (kbd "s-ISO_Left_Tab") "fother")
  (define-key *top-map* (kbd "S-s-Right") "move-window right")
  (define-key *top-map* (kbd "S-s-Left") "move-window left")
  (define-key *top-map* (kbd "S-s-Up") "move-window up")
  (define-key *top-map* (kbd "S-s-Down") "move-window down")
  (define-key *top-map* (kbd "s-v") "pulsemixer")
  (define-key *top-map* (kbd "s-c") "run-or-raise-xterm")
  (define-key *top-map* (kbd "s-F") "move-focus right")
  (define-key *top-map* (kbd "s-B") "move-focus left")
  (define-key *top-map* (kbd "s-P") "move-focus up")
  (define-key *top-map* (kbd "s-N") "move-focus down")
  (define-key *top-map* (kbd "s-s") "passmenu")
  (define-key *top-map* (kbd "s-j") "music-youtube")

  ;; Rebind groups to PREFIX-NUMBER.
  (mapcar #'(lambda (x)
              (define-key *top-map* (kbd (concat "s-" (write-to-string x)))
                (format nil "gselect ~D" x))
              (define-key *top-map* (kbd (concat "M-s-" (write-to-string x)))
                (format nil "gmove ~D" x))
              (define-key *top-map* (kbd (concat "C-s-" (write-to-string x)))
                (format nil "~A ~D" "select-window-by-number" x)))
          (range 10 :min 0 :step 1)))

(cond ((string= (getenv "DISPLAY") ":0")
       (restore-from-file "/home/oleg/src/dotfiles/oleg/.stumpwm.d/desktop/9.lisp")
       (define-frame-preference "Default" (0 NIL T :CLASS "quassel" :TITLE "Chat Monitor"))
       (define-frame-preference "Default" (3 NIL T :CLASS "XTerm" :TITLE "alerta"))
       (define-frame-preference "Default" (4 NIL T :CLASS "XTerm" :TITLE "notmuch"))
       (define-frame-preference "Default" (2 NIL T :CLASS "t-engine"))
       (define-frame-preference "Default" (4 NIL T :CLASS "mpv" :TITLE "emacs-emms"))
       (define-frame-preference "Default" (4 NIL T :CLASS "mpv" :TITLE "firefox"))
       (define-frame-preference "Default" (1 NIL T :CLASS "obs"))
       (bind-super))

      ((string= (getenv "DISPLAY") ":1")
       (run-shell-command "xsetroot -solid grey")
       (restore-from-file "/home/oleg/src/dotfiles/oleg/.stumpwm.d/desktop/10.lisp")
       (define-frame-preference "Default" (1 NIL T :CLASS "quassel" :TITLE "Chat Monitor"))
       (define-frame-preference "Default" (2 NIL T :CLASS "XTerm" :TITLE "alerta"))
       (define-frame-preference "Default" (0 NIL T :CLASS "Qemu-system-x86_64"))
       (swm-gaps:toggle-gaps) ;XXX: Make declarative.
       (bind-super)
       (define-key *top-map* (kbd "s-m") "alerta"))

      (t (set-prefix-key (kbd "C-i"))))
