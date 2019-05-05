;; Copyright © 2018, 2019 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

;; (set-module-dir "~/.guix-profile/share/common-lisp/sbcl-bundle-systems")

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(setf *home* "/home/oleg")
(set-module-dir (concat *home* "/.stumpwm.d/modules/"))

(set-prefix-key (kbd "C-i"))

(run-shell-command "xsetroot -cursor_name left_ptr")
(run-shell-command (concat "xrdb -merge " *home* "/.Xresources"))

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
(run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle")

;; Keyboard speed
;; (run-shell-command "xset s 0")
;; (run-shell-command "xset dpms 0 0 1800")

(run-shell-command "xmodmap " (concat *home* "/.Xmodmap"))

(run-shell-command "keynav")

(run-shell-command "dunst")

(setf *message-window-y-padding* 3)

(setf *window-border-style* :thin)

(setf *ignore-wm-inc-hints* t)
(set-msg-border-width 4)

(setf *normal-border-width* 5)
(setf *transient-border-width* 5)
(setf *maxsize-border-width* 5)


;;;
;;; General functions for use
;;;

(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
     collect n))

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

(setf *suppress-window-placement-indicator* t)

(setf *new-frame-action* :empty)

(defcommand desktop-restore (desktop rules) ((:string "Restore desktop: ")
                                             (:string "Restore rules: "))
  (let ((desktop (format nil "~a/.stumpwm.d/desktop/~a.lisp" *home* desktop))
        (rules (format nil "~a/.stumpwm.d/rules/~a.lisp" *home* rules)))
    (message (format nil "Restore desktop from ~s file." desktop))
    (message (format nil "Restore rules from ~s file." rules))
    (restore-from-file desktop)
    (clear-window-placement-rules)
    (restore-window-placement-rules rules)
    (place-existing-windows)))

(define-frame-preference "Default" (1 NIL T :CLASS "mpv" :TITLE "emacs-emms"))

;; (restore-window-placement-rules "/home/oleg/.stumpwm.d/rules/4.lisp")

;; (restore-from-file "/home/user/.stumpwm-dump-desktop.lisp")
;; (restore-from-file "~/.stumpwm-dump-desktop.lisp")
;; (dump-desktop-to-file "~/.stumpwm-dump-desktop.lisp")

;; (restore-from-file "/home/user/.stumpwm-dump-desktop.lisp")
;; (restore-window-placement-rules "/home/user/1.lisp")

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
  "exec /run/current-system/profile/bin/xterm")

(defvar *xterm-big-command*
  "exec /run/current-system/profile/bin/xterm -fa 'Monospace' -fs 24")

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

(defvar *browser* "icecat")

(defvar *transmission-hostname*
  "magnolia")


;;;
;;; Emacs
;;;

(defcommand emacsclient () ()
  "Start emacs unless it is already running, in which case focus it."
  (run-or-raise "emacsclient -c" '(:class "Emacs")))

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
  "Start of focus firefox."
  (run-shell-command "firefox --new-window"))

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
  (let ((clipboard (get-x-selection)))
    (run-shell-command
     (join `(,*mpv-program* ,@*mpv-arguments* ,clipboard)))
    (message (concat "Play " clipboard))))

(defcommand mpv-watch () ()
  "Play video from file with mpv."
  (run-shell-command
   (join `(,*mpv-program* ,@*mpv-arguments* ,(concat "$(cat " *home* "/watch)")))))


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

(defcommand neofetch () ()
  (term-shell-command "sh -c 'neofetch; read'"))

(defcommand rofi-drun () ()
  "Open Rofi to launch `.desktop' file."
  (run-shell-command "rofi -modi run,drun -show drun"))

(defcommand rofi-ssh () ()
  "Open Rofi ssh list."
  (run-shell-command "rofi -width 20 -terminal 'xterm +sb' -modi ssh -show ssh"))

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
(set-focus-color "#90EE90")
(set-border-color "#90EE90")
(set-float-focus-color "#90EE90")
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
(setf *TIME-MODELINE-STRING* "%a, %e %b %Y %k:%M")
;; (mapcar (lambda (g)
;;           (group-number g)
;;           (group-name g))
;;         (screen-groups (current-screen)))
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
            ;; "b:" '(:eval (stumpwm:run-shell-command "/home/oleg/bin/jenkins-active-jobs" t))
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

(defcommand trans-en-ru () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I en:ru'"))

(defcommand trans-ru-en () ()
  "Run `xterm' with `trans' in interactive mode."
  (run-shell-command "exec xterm -name trans -e 'trans -I ru:en'"))

(define-key *root-map* (kbd "e") "emacsclient")

;; (define-key *root-map* (kbd "m") "mpv")
;; (define-key *root-map* (kbd "C-m") "xclip-mpv")
;; (define-key *root-map* (kbd "M-m") "xclip-streamlink")

(defcommand rofi-stumpwm () ()
  (run-shell-command (concat "rofi -show stumpwm -modi stumpwm:"
                             *home* "/bin/rofi-stumpwm")))

(define-key *top-map* (kbd "s-:") "rofi-stumpwm")

;; (define-key *root-map* (kbd "C-e") "xclip-emacs")
;; (define-key *root-map* (kbd "C-M-c") "xterm-big-screen")
;; (define-key *root-map* (kbd "M-e") "emacs-anywhere")

;; Lock screen
;; (define-key *root-map* (kbd "C-l") "exec xlock -mode blank")
;; (define-key *root-map* (kbd "M-l") "turn-screen-off")

;; (define-key *root-map* (kbd "M-!") "run-xterm-command")
;; (define-key *root-map* (kbd "v") "pulsemixer")
;; (define-key *root-map* (kbd "C-v") "alsamixer")

(defcommand osd-sound () ()
  (run-shell-command "if pgrep -f osd-sound > /dev/null; then pkill osd-sound; osd-sound; else osd-sound; fi"))

(defcommand volume-decrease () ()
  (run-shell-command "osd-sound sset Master 5%-"))

(defcommand volume-increase () ()
  (run-shell-command "osd-sound sset Master 5%+"))

(define-interactive-keymap volume nil
  ((kbd "-") "volume-decrease")
  ((kbd "=") "volume-increase"))

(define-key *root-map* (kbd "c") "run-xterm")
(define-key *root-map* (kbd "C-c") "run-xterm")
;; (define-key *root-map* (kbd "C-M-c") "run-xterm-light")

;; (define-key *root-map* (kbd "C-M-v") "scroll-other-window")
(define-key *top-map* (kbd "Print") "screenshot-default")

(define-key *top-map* (kbd "s-w") "firefox")
(define-key *top-map* (kbd "s-M-w") "firefox-new-window")

;; Rebind groups to PREFIX-NUMBER.
(mapcar #'(lambda (x) (define-key *top-map* (kbd (concat "s-" (write-to-string x)))
			(format nil "~A ~D" "gselect" x)))
	(range 10 :min 1 :step 1))

(define-key *top-map* (kbd "M-s-n") "gnext")
(define-key *top-map* (kbd "M-s-p") "gprev")
;; (define-key *top-map* (kbd "s-M-h") "jord-php")
;; (define-key *top-map* (kbd "s-TAB") "fother")
;; (define-key *top-map* (kbd "s-\"") "frame-windowlist")
;; (define-key *top-map* (kbd "s-c") "run-or-raise-xterm")
;; (define-key *top-map* (kbd "s-c") "run-or-raise-xterm")
;; (define-key *top-map* (kbd "s-e") "emacs")
;; (define-key *top-map* (kbd "s-e") "emacsclient")
;; (define-key *top-map* (kbd "s-h") "jord-loadavg")
;; (define-key *top-map* (kbd "s-h") nil)
(define-key *top-map* (kbd "s-m") "mpv")
(define-key *top-map* (kbd "s-n") "next-in-frame")
(define-key *top-map* (kbd "s-p") "prev-in-frame")
;; (define-key *top-map* (kbd "s-s") "sibling")
;; (define-key *top-map* (kbd "s-t") "pull-hidden-other")
;; (define-key *top-map* (kbd "s-v") "xclip-mpv")
;; (define-key *top-map* (kbd "s-w") "firefox")
;; (define-key *top-map* (kbd "s-w") "firefox")

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

;; (define-key *root-map* (kbd "C-b") "warp-mouse-active-frame")
(defcommand xkill () ()
  "Run `xkill'."
  (run-shell-command "xkill"))

;; (define-key *root-map* (kbd "X") "xkill")

(defcommand vnc-magnolia () ()
  (run-shell-command "exec vncviewer localhost:59555"))

(defcommand pass-input () ()
  (window-send-string "***REMOVED***"))

(defcommand pass () ()
  (run-shell-command "echo -n ***REMOVED*** | xclip -selection primary"))

;; (define-remapped-keys
;;     '(("(Firefox|Chrome|Chromium)"
;;        ("C-n"   . "Down")
;;        ("C-p"   . "Up")
;;        ("C-f"   . "Right")
;;        ("C-b"   . "Left")
;;        ("C-v"   . "Next")
;;        ("M-v"   . "Prior")
;;        ("M-w"   . "C-c")
;;        ("C-w"   . "C-x")
;;        ("C-y"   . "C-v")
;;        ("M-<"   . "Home")
;;        ("M->"   . "End")
;;        ("C-M-b" . "M-Left")
;;        ("C-M-f" . "M-Right")
;;        ("C-k"   . ("C-S-End" "C-x")))))

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

  (defcommand zabbix () ()
    (firefox "https://zabbix.wugi.info/"))

  (defcommand kibana () ()
    (firefox "http://localhost:5601/app/kibana"))

  (defcommand gitlab () ()
    (firefox "https://gitlab.wugi.info/" t))

  (defcommand youtube () ()
    (firefox "https://www.youtube.com/feed/subscriptions" t))

  (defcommand twitch () ()
    (firefox "https://www.twitch.tv/directory/game/Tales%20of%20Maj'Eyal" t))

  (defcommand jenkins-index () ()
    (firefox "https://jenkins.wugi.info/"))

  (defcommand jenkins-last-build-guixsd () ()
    (firefox "https://jenkins.wugi.info/job/fiore/lastBuild/console"))

  (defcommand cuirass () ()
    (firefox "https://grafana.wugi.info/d/Ob67YJYiz/fiore?refresh=30s&orgId=1&var-host=cuirass"))

  (defcommand yoo (url) ((:string "YouTube URL: "))
    (gnew "youtube")
    (restore-group (current-group) (read-dump-from-file (concat *home* "/youtube.lisp")))
    (term-shell-command (format nil "mpv --no-resume-playback --mute=yes ~s" url))
    (firefox (format nil "https://www.youtube.com/live_chat?v=~a&is_popout=1"
                     (cadr (split-string url "=")))
             t)))

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
            "#gdb" "#gnus" "#guile" "#guix" "##linux" "#lisp" "#nixos" "#scheme" "#stumpwm")))

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

;; Rebind groups to PREFIX-NUMBER.
;; (mapcar #'(lambda (x)
;;             (when (> x 1)
;;               (gnew (write-to-string x)))
;;             (define-key *top-map* (kbd (concat "s-" (write-to-string x)))
;; 	      (format nil "~A ~D" "gselect" x)))
;; 	(range 10 :min 1 :step 1))

(define-key *top-map* (kbd "s-!") "gmove-and-follow 1")
(define-key *top-map* (kbd "s-@") "gmove-and-follow 2")
(define-key *top-map* (kbd "s-#") "gmove-and-follow 3")
(define-key *top-map* (kbd "s-$") "gmove-and-follow 4")

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(defcommand window-resize-by-half-horizontal () ()
  ""
  (resize (- (/ (parse-integer (format-expand *window-formatters* "%w" (current-window))) 2)) 0))

(defcommand window-resize-by-half-vertical () ()
  ""
  (resize 0 (- (/ (parse-integer (format-expand *window-formatters* "%h" (current-window))) 2))))

(defcommand emacs-shell () ()
  ""
  (unless (uiop/utility:string-prefix-p "emacs"
                                        (window-name (current-window)))
    (run-shell-command "stumpish emacsclient"))
  (run-shell-command "emacsclient -e '(progn (shell) (delete-other-windows))'"))

;; (define-key *root-map* (kbd "quoteleft") "emacs-shell")

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

(define-key *top-map* (kbd "s-Tab") "fother")

(define-key *top-map* (kbd "s-k") "delete")

(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "s-Left") "move-focus left")
(define-key *top-map* (kbd "s-Up") "move-focus up")
(define-key *top-map* (kbd "s-Down") "move-focus down")

(define-key *top-map* (kbd "S-s-Right") "move-window right")
(define-key *top-map* (kbd "S-s-Left") "move-window left")
(define-key *top-map* (kbd "S-s-Up") "move-window up")
(define-key *top-map* (kbd "S-s-Down") "move-window down")

(defcommand vnc (display) ((:string "display: "))
  (run-shell-command (concat "vncviewer 127.0.0.1:" display)))

(define-key *top-map* (kbd "s-v") "pulsemixer")
(define-key *top-map* (kbd "s-r") "vnc")
(define-key *top-map* (kbd "s-e") "emacsclient")
(define-key *top-map* (kbd "s-c") "run-or-raise-xterm")

(define-key *top-map* (kbd "s-f") "move-focus right")
(define-key *top-map* (kbd "s-b") "move-focus left")
(define-key *top-map* (kbd "s-p") "move-focus up")
(define-key *top-map* (kbd "s-n") "move-focus down")
(define-key *top-map* (kbd "s-!") "exec")

(defcommand mpv-music () ()
  (run-shell-command "mpv https://www.youtube.com/playlist?list=PLmjgicsUWIkvEKkLN01vm85neXAik3yU2"))

(defcommand alerta-close-youtube () ()
  (run-shell-command "alerta close --filter resource=YouTube"))

;; (require :swank)
;; (swank-loader:init)
;; (sb-thread:make-thread
;;  (lambda ()
;;    (swank:create-server :port 9007 :dont-close t)))

;; (setq swank:*use-dedicated-output-stream* nil)

;; (run-shell-command "emacsclient -c -s test /home/oleg/src/dotfiles/oleg/.stumpwm.d/init.lisp")

;; (swank:create-server :port 9006 :dont-close t)

