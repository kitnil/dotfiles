;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;; Released under the GNU GPLv3 or any later version.

(in-package :stumpwm)

;; Load modules from https://github.com/stumpwm/stumpwm-contrib
(set-module-dir (concat (getenv "HOME") "/.stumpwm.d/modules/"))

(setf *startup-message* nil)
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)

(defcommand cursor-theme () ()
  (run-shell-command "xsetroot -cursor_name left_ptr"))

(cursor-theme)

(defcommand keynav () ()
  (run-shell-command "keynav"))

(defcommand xrdb () ()
  (run-shell-command (format nil "xrdb ~a/.Xresources" (getenv "HOME"))))

(xrdb)

(defcommand speaker-disable () ()
  (run-shell-command "xset -b"))

(defcommand xset-900 () ()
  (run-shell-command "xset s 900 900")
  (run-shell-command "xset dpms 900 900 900"))

(defcommand xset-3600 () ()
  (run-shell-command "xset s 3600 3600")
  (run-shell-command "xset dpms 3600 3600 3600"))

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

(define-key *root-map* (kbd "C-i") "set-prefix-key C-i")
(define-key *root-map* (kbd "C-t") "set-prefix-key C-t")

;; Keyboard layout
(defcommand keyboard-layout () ()
  (run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle"))

(keyboard-layout)

(defcommand xmodmap () ()
  (run-shell-command "xmodmap " (concat "/.Xmodmap" (getenv "HOME"))))

(xmodmap)

(progn
  (set-msg-border-width 4)
  (setf *ignore-wm-inc-hints* t)
  (setf *window-border-style* :thin))

(defcommand toggle-window-borders () ()
  (if (or (= *maxsize-border-width* 3)
          (= *message-window-y-padding* 3)
          (= *normal-border-width* 3)
          (= *transient-border-width* 3))
      (progn (setf *maxsize-border-width* 0)
             (setf *message-window-y-padding* 0)
             (setf *normal-border-width* 0)
             (setf *transient-border-width* 0)
             (setq *suppress-frame-indicator* nil))
      (progn (setf *maxsize-border-width* 3)
             (setf *message-window-y-padding* 3)
             (setf *normal-border-width* 3)
             (setf *transient-border-width* 3)
             (setq *suppress-frame-indicator* t))))


;;;
;;; General functions for use
;;;

(defun switch-to-emacs ()
  (unless (uiop/utility:string-prefix-p "Emacs"
                                        (window-class (current-window)))
    (run-shell-command "stumpish emacsclient")))

(defun password-store-show (password)
    (run-shell-command (format nil "gpg -d ~a/.password-store/~a.gpg 2>/dev/null"
                               (getenv "HOME") password)
     t))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

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

;; https://stackoverflow.com/a/34628127
(defun string-contains (string1 string2)
  (cond
   ((zerop (length string1)) nil) ; string1 is empty (no need to test it every time)
   ((> (length string1) (length string2)) nil) ; string1 is longer than string2
   ((string= string1 (subseq string2 0 (length string1))) string1) ; string2 starts with string1
   (t (string-contains string1 (subseq string2 1))))) ; otherwise shorten string2 by 1 and start over

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
  (let ((desktop (format nil "~a/.stumpwm.d/desktop/~a.lisp" (getenv "HOME") desktop))
        (rules (format nil "~a/.stumpwm.d/rules/~a.lisp" (getenv "HOME") rules)))
    (message (format nil "Restore desktop from ~s file." desktop))
    (message (format nil "Restore rules from ~s file." rules))
    (restore-from-file desktop)
    (clear-window-placement-rules)
    (restore-window-placement-rules rules)
    (place-existing-windows)))

(defvar workstation?
  (string-equal (file-get-contents "/etc/hostname") "workstation-guixsd"))


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

(defun firefox-command ()
  (join `(,@(if dark-theme '("GTK_THEME=Adwaita:dark") nil) "nixGLIntel" "firefox")))

;; (defcommand firefox-test () ()
;;   "Start of focus firefox."
;;   (run-shell-command (join (list (firefox-command) "-P" "test"))
;;                      '(:class "Firefox")))

(defcommand firefox () ()
  "Start of focus firefox."
  (run-or-raise (firefox-command) '(:class "Firefox")))

(defcommand firefox-new-window () ()
  "Start Firefox."
  (run-shell-command "firefox --new-window"))

(defcommand firefox-beta () ()
  "Start Firefox Beta."
  (run-shell-command
   (join
    (list (concat (getenv "HOME") "/.nix-profile.d/firefox-beta-bin/firefox-beta-bin/bin/firefox")
          "-P" "beta" "--new-instance"))))

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
   (join `(,*mpv-program* ,@*mpv-arguments* ,(concat "$(cat " (getenv "HOME") "/watch)")))))

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

(defun youtube-dl-command (url &key (ad-hoc nil) (music nil))
  (message (format nil "Download ~s." url))
  (term-shell-command (format nil "sh -c 'TMOUT=20; ~a; read -p \"Press Enter to close.\"'"
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


;;;
;;; Misc
;;;

(defun term-shell-command (command &key
                                     (terminal 'xterm)
                                     (color (if dark-theme "dark" "light"))
                                     (font nil)
                                     (title nil)
                                     (scrollbar nil))
  (run-shell-command
   (let ((terminal-name (string-downcase (symbol-name terminal))))
     (case terminal
       ((xterm)
        (xterm-command :color color :command command :font font :title title :scrollbar scrollbar))
       ((st)
        (join (list terminal-name
                    *st-font-flag* (if font font *st-font*)
                    *st-exec-flag* command)))))))

(defcommand zoom () ()
  (run-shell-command "nixGLIntel boomer"))

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

(defcommand guix-pull () ()
  (term-shell-command "sh -c 'guix pull; read'"))

(defun fetch-mail-command (command)
  (format nil "sh -c 'TMOUT=20; echo \"Fetch mail.\"; ~a; notify-send \"Mail fetched.\"; read -p \"Press Enter to close.\"'"
          command))

(defcommand mbsync-majordomo () ()
  (term-shell-command (fetch-mail-command "mbsync -a majordomo")
                      :title "mbsync-majordomo"
                      :font '("-fa" "Monospace" "-fs" "8")
                      :color "dark"))

(defcommand notmuch () ()
  (term-shell-command (fetch-mail-command "notmuch new")
                      :title "notmuch"
                      :font '("-fa" "Monospace" "-fs" "8")
                      :color "dark"))

(defvar majordomo-webs
  ;; without web19
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(web15 web16 web17 web18
            web20 web21 web22 web23 web25 web26 web27 web28 web29
            web30 web31 web32 web33 web34 web35 web36 web37)))

(defvar majordomo-dh
  (mapcar (lambda (x)
            (sb-unicode:lowercase (string x)))
          '(dh1-mr dh2-mr dh3-mr)))

(defvar majordomo-vpn
  (mapcar (lambda (x)
            (concat "vpn-" (sb-unicode:lowercase (string x)) ".majordomo.ru"))
          '(miran dh office)))

(defcommand xpanes-vpn-ssh () ()
  (term-shell-command (join `("xpanes -C 1 -c 'ssh {}'" ,@majordomo-vpn))
                      :title "xpanes-vpn-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand xpanes-dh-ssh () ()
  (term-shell-command (join `("xpanes -c 'ssh {}.intr'" ,@majordomo-dh))
                      :title "xpanes-dh-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand xpanes-web-ssh () ()
  (term-shell-command (join `("xpanes -c 'ssh {}.intr'" ,@majordomo-webs))
                      :title "xpanes-web-ssh"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand xpanes-web-top () ()
  (term-shell-command (join `("xpanes -c 'ssh -t {}.intr top'" ,@majordomo-webs))
                      :title "xpanes-web-top"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defun xpanes-command (command)
  (format nil "xpanes -c ~s" command))

(defcommand xpanes-web-mycli () ()
  (term-shell-command (join `(,(xpanes-command
                                (format nil "mycli --password ~a -d {}"
                                        (password-store-show "majordomo/web/mysql/root")))
                               ,@majordomo-webs))
                      :title "xpanes-web-mycli"
                      :font '("-fa" "Monospace" "-fs" "6")))

(defcommand xpanes-restic-snapshots () ()
  (term-shell-command (join `(,(xpanes-command (join `("sudo" "-i" ,(format nil "RESTIC_PASSWORD=~a"
                                                                            (password-store-show "wugi.info/restic/all"))
                                                              ,(format nil "~a/.guix-profile/bin/restic" (getenv "HOME"))
                                                              "-r" "/srv/backup/{}" "snapshots")))
                               ,@'("guixsd" "majordomo" "oracle" "spb")))
                      :title "xpanes-restic-snapshots"))

(defcommand-alias restic-snapshots xpanes-restic-snapshots)

(defcommand xpanes-routers () ()
  (term-shell-command (join `(,(xpanes-command "ssh -t {}")
                               ,@'("router4.intr" "vpn-miran.majordomo.ru" "vpn-dh.majordomo.ru")))
                      :title "xpanes-routers"))

(defun zathura (file)
  (run-shell-command (format nil "zathura ~s" file)))

(defcommand arcconf () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/arcconf/arcconf.pdf")))

(defcommand doc-bash () ()
  (zathura (concat (getenv "HOME") "/.guix-profile/share/doc/slides-concise-gnu-bash/slides-concise-gnu-bash.pdf")))


;;;
;;; REPL
;;;

(defcommand repl-groovy () ()
  (term-shell-command "groovysh" :scrollbar t))

(defcommand repl-guile () ()
  (term-shell-command "guile" :scrollbar t))

(defcommand guix-repl () ()
  (term-shell-command "guix repl" :scrollbar t))

(defcommand repl-gdb () ()
  (term-shell-command "gdb" :scrollbar t :title "repl-gdb"))

(defcommand repl-php () ()
  (term-shell-command "php -a" :scrollbar t :title "repl-php"))

(defcommand repl-python () ()
  (term-shell-command "python3" :scrollbar t :title "repl-python"))

(defcommand repl-r () ()
  (term-shell-command "R" :scrollbar t :title "repl-r"))

(defcommand repl-racket () ()
  (term-shell-command "racket" :scrollbar t :title "repl-racket"))

(defcommand repl-nix () ()
  (term-shell-command "nix repl '<nixpkgs>'" :scrollbar t :title "repl-nix"))

(defcommand repl-emacs () ()
  (term-shell-command "emacs -nw -q -e 'ielm'" :scrollbar t :title "repl-emacs"))

(defcommand repl-kawa () ()
  (term-shell-command "guix environment --ad-hoc coreutils openjdk kawa rlwrap -- rlwrap kawa" :scrollbar t :title "repl-kawa"))

(defcommand repl-ocaml () ()
  (term-shell-command "guix environment --pure --ad-hoc ocaml -- ocaml" :scrollbar t :title "repl-ocaml"))

(defcommand repl-octave () ()
  (term-shell-command "octave" :scrollbar t :title "repl-octave"))

(defcommand repl-node () ()
  (term-shell-command "node" :scrollbar t :title "repl-node"))

(defcommand repl-sbcl () ()
  (term-shell-command "rlwrap sbcl" :scrollbar t :title "repl-sbcl"))

(defcommand repl-chez-scheme () ()
  (term-shell-command "guix environment --ad-hoc chez-scheme -- chez-scheme" :scrollbar t :title "repl-chez-scheme"))

(defcommand repl-chicken () ()
  (term-shell-command "rlwrap csi" :scrollbar t :title "repl-chicken"))

(defcommand repl-c () ()
  (term-shell-command "docker run --rm -it bic:latest" :scrollbar t :title "repl-c"))

(defcommand repl-hy () ()
  (term-shell-command "docker run --rm -it hylang:python3.5-buster" :scrollbar t :title "repl-hy"))

(defcommand repl-java () ()
  (term-shell-command "docker run --rm -it openjdk:9" :scrollbar t :title "repl-java"))

(defcommand repl-elm () ()
  (term-shell-command "guix environment --pure --ad-hoc elm-compiler node -- elm repl"
                      :scrollbar t :title "repl-elm"))

(defcommand repl-clisp () ()
  (term-shell-command "guix environment --pure --ad-hoc clisp -- clisp"
                      :scrollbar t :title "repl-clisp"))

(defcommand repl-erlang () ()
  (term-shell-command "guix environment --pure --ad-hoc erlang -- erl"
                      :scrollbar t :title "repl-erlang"))

(defcommand repl-lua () ()
  (term-shell-command "guix environment --pure --ad-hoc lua -- lua"
                      :scrollbar t :title "repl-lua"))

(defcommand repl-ghci () ()
  (term-shell-command "ghci" :scrollbar t :title "repl-ghci"))

(defcommand repl-go () ()
  (term-shell-command "gore" :scrollbar t :title "repl-go"))

(defcommand repl-resty () ()
  (term-shell-command (format nil (join '("docker" "run" "--entrypoint"
                                          "''" "--rm" "-it"
                                          "openresty/openresty:bionic" "bash" "-c" "~s"))
                              (join '("apt update" "apt install -y git"
                                      "luarocks install lua-resty-repl"
                                      "resty-repl")
                                    #\;))
                      :scrollbar t :title "repl-java"))

(defcommand repl-ruby () ()
  (term-shell-command "guix environment --pure --ad-hoc ruby -- irb"
                      :scrollbar t :title "repl-ruby"))

(defcommand repl-bash-pure () ()
  (term-shell-command "env -i \"$(command -v bash)\" --login --noprofile --norc"
                      :scrollbar t :title "repl-bash-pure"))

(defcommand repl-ansible (group) ((:string "Ansible inventory group: "))
  (term-shell-command (format nil "ansible-console ~a" group)
                      :scrollbar t :title "repl-ansible" :color 'dark))


;;;
;;; Docker
;;;

(defcommand docker-debian () ()
  (term-shell-command "docker run --rm -it debian:10" :scrollbar t :title "docker-debian"))

(defcommand neofetch () ()
  (term-shell-command "sh -c 'neofetch; read'"))

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

;; TODO:
;; (defcommand rofi-mytop () ()
;;   "Open Rofi mytop."
;;   (run-shell-command "rofi -modi mycli:/home/oleg/bin/rofi-mycli -show mycli"))

(defcommand rofi-mycli () ()
  "Open Rofi mycli."
  (run-shell-command "rofi -modi mycli:/home/oleg/bin/rofi-mycli -show mycli"))

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
  (term-shell-command "pulsemixer" :terminal 'st :font "Monospace:size=8"))

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))

(defcommand quassel-monitor () ()
  (run-or-raise "quassel" '(:class "quassel" :title "Chat Monitor")))

(defcommand run-or-raise-xterm () ()
  "Start or focus XTerm."
  (run-or-raise
   (join (list *xterm-command* *xterm-theme-dark* *xterm-no-scrollbar*))
   '(:class "XTerm")))

(defcommand run-xterm-named (title) ((:string "title: "))
  "Start or focus XTerm."
  (run-shell-command (xterm-command :color (if dark-theme "dark" "light")
                                    :title title
                                    :scrollbar t)))

(defun small-framep ()
  (cond ((string= (class-name (class-of (current-group))) "FLOAT-GROUP") nil)

        ((string= (getenv "DISPLAY") ":1")
         (let ((frame (frame-number (tile-group-current-frame (current-group)))))
           (if (= frame 0) nil t)))

        ((and (not workstation?) (string= (getenv "DISPLAY") ":0"))
         (let ((frame (frame-number (tile-group-current-frame (current-group)))))
           (if (string-equal (group-name (current-group)) "Default")
	       (or (= frame 0) (= frame 1))
	       (or (= frame 2) (= frame 1)))))

        (t nil)))

(defcommand current-frame-smallp () ()
  (if (small-framep)
      (progn (message "small") 1)
      (progn (message "big") 0)))

(defun xterm-command (&key
                        (color (if dark-theme "dark" "light"))
                        (command nil)
                        (title nil)
                        (font nil)
                        (scrollbar nil))
  (join `(,*xterm-command*
          ;; Make sure XTerm terminal size is appropriate for current StumpWM frame.
          ,@(if font
                font
                (if (small-framep)
                    '("-fa" "Monospace" "-fs" "8")
                    '()))
          "-sl" "1000000" ;number of lines
          ,(if scrollbar "-sb" *xterm-no-scrollbar*)
          ,(if (string= color "light") *xterm-theme-light* *xterm-theme-dark*)
          ,@(if title `("-title" ,title) '())
          ,@(if command `("-e" ,command) '()))))

(defcommand run-xterm () ()
  "Start or focus XTerm."
  (run-prog *shell-program*
            :args (list "-c" (xterm-command :scrollbar t))
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

(defcommand debian-unstable () ()
  (run-shell-command
   (join (list "qemu-system-x86_64" "-daemonize" "-smp" "cores=4,threads=1"
               "-m" "4096" "-enable-kvm" "-cpu" "host" "-daemonize"
               "-vnc" ":5" "-hda" "~/vm/debian-unstable.qcow2"
               "-cdrom" "~/Downloads/debian-testing-amd64-netinst.iso"))))

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
      (progn (setq *mode-line-border-color*     "#000000"
                   *mode-line-foreground-color* "#ffffff"
                   *mode-line-background-color* "#000000")
             (set-win-bg-color "#000000")
             (set-unfocus-color "#000000")
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
            (join (mapcar (lambda (window)
                            (let ((wn (window-name window)))
                              (format nil "~s:[~s]" (window-number window) (if (> (length wn) 5)
                                                                               (concat (subseq wn 0 5) "...")
                                                                               wn))))
                          (take 5 (sort-windows-by-number (group-windows (current-group))))))
            (make-string 4 :initial-element #\space)
            '(:eval (write-to-string (window-number (current-window))))
            ":"
            "["
            '(:eval (window-class (current-window)))
            " "
            '(:eval (window-name (current-window)))
            "]"
            "^>"
            (make-string 4 :initial-element #\space)
            "%N"
            (make-string 4 :initial-element #\space)
            "%d"))

(setf *mode-line-pad-x* 10)
(setf *mode-line-pad-y* 5)
(setf *mode-line-border-width* 0)

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
  (term-shell-command "trans -I en:ru" :title "trans-en-ru"))

(defcommand trans-ru-en () ()
  "Run `xterm' with `trans' in interactive mode."
  (term-shell-command "trans -I ru:en" :title "trans-ru-en"))

(define-key *root-map* (kbd "e") "emacsclient")

(defcommand osd-sound () ()
  (run-shell-command "if pgrep -f osd-sound > /dev/null; then pkill osd-sound; osd-sound; else osd-sound; fi"))

(defcommand volume-decrease () ()
  (run-shell-command "ponymix decrease 5"))

(defcommand volume-increase () ()
  (run-shell-command "ponymix increase 5"))

(defcommand volume-toggle () ()
  (run-shell-command "ponymix toggle"))

(define-interactive-keymap volume nil
  ((kbd "-") "volume-decrease")
  ((kbd "=") "volume-increase"))

(defcommand pavucontrol () ()
  (run-shell-command "pavucontrol"))

(define-key *root-map* (kbd "c") "run-xterm")
(define-key *root-map* (kbd "C-c") "run-xterm")
(define-key *root-map* (kbd "C-M-c") "run-xterm")
(define-key *top-map* (kbd "C-s-RET") "run-xterm-named")

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
  (switch-to-emacs)
  (run-shell-command "emacsclient -e '(progn (shell) (delete-other-windows))'"))

(define-key *root-map* (kbd "quoteleft") "emacs-shell")

(defcommand emacs-guix-edit () ()
  (let ((clipboard (get-x-selection)))
    (message (format nil "Guix edit ~a" clipboard))
    (switch-to-emacs)
    (run-shell-command (format nil "emacsclient -e '(guix-edit ~s)'" clipboard))))

(define-key *top-map* (kbd "M-s-e") "emacs-guix-edit")

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

(defcommand ponymix-increase-device-0 () ()
  (run-shell-command "ponymix increase --device 0 5"))

(defcommand ponymix-decrease-device-0 () ()
  (run-shell-command "ponymix decrease --device 0 5"))

(define-key *top-map* (kbd "C-s-=") "ponymix-increase-device-0")
(define-key *top-map* (kbd "C-s--") "ponymix-decrease-device-0")

(defcommand xkill () ()
  "Run `xkill'."
  (run-shell-command "xkill"))

(defcommand vnc-magnolia () ()
  (run-shell-command "exec vncviewer localhost:59555"))

(defcommand pass-route () ()
  (window-send-string (format nil "~a~%" (password-store-show "majordomo/ssh/router"))))

(defcommand pass-eng () ()
  (window-send-string (format nil "~a~%" (password-store-show "majordomo/ssh/eng"))))

(defcommand pass-ipmi () ()
  (window-send-string (format nil "~a" (password-store-show "majordomo/ipmi/ADMIN"))))

(defcommand insert-ssh-key () ()
  (window-send-string
   (format nil "mkdir ~~/.ssh; cat >> ~~/.ssh/authorized_keys <<'EOF'~%~aEOF"
           (file-get-contents (concat (getenv "HOME") "/.ssh/id_rsa.pub")))))

(defcommand docker-pull () ()
  (window-send-string (format nil "~a~%" "docker ps --format '{{ .Image }}' | grep master | sort -u | xargs -I{} docker pull {}")))

(define-key *top-map* (kbd "s-D") "docker-pull")

(defcommand pass-sup () ()
  (window-send-string (format nil "~a~%" (password-store-show "majordomo/ssh/sup"))))

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(defcommand twitch-channel-chat (channel) ((:string "channel: "))
  (run-shell-command
   (concat "chromium --app=https://www.twitch.tv/popout/"
           channel "/chat?popout=")))

(defcommand ipmi () ()
  (run-shell-command (concat (getenv "HOME")
                             "/.nix-profile.d/ipmiview/ipmiview/bin/IPMIView")))

(defcommand ipkvm () ()
  (run-shell-command
   (join (list (concat (getenv "HOME")
                       "/.nix-profile.d/firefox-esr-52/firefox-esr-52/bin/firefox")
               "-P" "esr52" "--new-instance"))))

(defcommand mongo-prod () ()
  (term-shell-command (concat "mongo mongodb://admin:"
                              (password-store-show "majordomo/mongo/ci.intr/admin")
                              "@hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/admin?replicaSet=hms-rs0")
                      :scrollbar t
                      :title "mongo-prod"))

(defcommand mongo-dev () ()
  (term-shell-command (concat "mongo mongodb://admin:"
                              (password-store-show "majordomo/mongo/ci.intr/admin")
                              "@ci.intr:27017/admin")
                      :scrollbar t
                      :title "mongo-dev"))


(defvar *jenkins-url*
  "https://jenkins.wugi.info")

(flet ((firefox (url title &optional dark)
         (run-or-raise (format nil (if dark
                                       "GTK_THEME=Adwaita:dark nixGLIntel firefox -P dark --new-window ~S"
                                       "firefox --new-window ~S")
                               url)
                       `(:title ,title))))

  (defcommand nexus () ()
    (firefox "http://nexus.intr" "Nexus"))

  (defcommand alerta () ()
    (firefox "https://alerta.intr" "Alerta"))

  (defcommand grafana-netflow () ()
    (firefox "https://grafana.intr/d/000000042/netflow?orgId=1&refresh=1m" "Netflow"))

  (defcommand grafana-upstream-interfaces () ()
    (firefox "https://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?orgId=1" "Upstream interfaces"))

  (defcommand slack () ()
    (firefox "https://mjru.slack.com/" "Slack"))

  (defcommand grafana () ()
    (firefox "https://grafana.intr/" "Grafana"))

  ;; TODO:
  ;; (defcommand majordomo-la-24-hours () ()
  ;;   (firefox "http://grafana.intr/d/000000021/telegraf-system-dashboard-shared?panelId=54694&fullscreen&orgId=1&var-datasource=influx-telegraf&var-inter=$__auto_interval_inter&var-server=web.*&var-mountpoint=All&var-cpu=All&var-disk=All&var-netif=All&var-server_role=shared-hosting&var-dc=Miran&from=now-12h&to=now" t))

  ;; TODO:
  ;; (defcommand majordomo-upstream () ()
  ;;   (firefox "http://grafana.intr/d/6QgXJjmik/upstream-interfaces-traffic?refresh=5s&orgId=1"))

  (defcommand cerb () ()
    (firefox "http://cerberus.intr/" "Cerberus"))

  (defcommand majordomo-zabbix () ()
    (firefox "https://zabbix.intr/dashboard.php?fullscreen=1" "Dashboard"))

  (defcommand kibana () ()
    (firefox "https://kibana.intr/" "Kibana"))

  (defcommand gitlab () ()
    (firefox "https://gitlab.intr/" "GitLab"))

  (defcommand jenkins () ()
    (firefox *jenkins-url* "Jenkins"))

  (defcommand jenkins-mj () ()
    (firefox "https://jenkins.intr/" "Jenkins"))

  (defcommand music-youtube () ()
    (run-or-raise "chromium --app=https://music.youtube.com/"
                  '(:instance "music.youtube.com")))

  (defcommand youtube () ()
    (firefox "https://www.youtube.com/feed/subscriptions" t))

  (defcommand twitch-tome4 () ()
    (firefox "https://www.twitch.tv/directory/game/Tales%20of%20Maj'Eyal" t))

  (defcommand jenkins-index () ()
    (firefox *jenkins-url* "Jenkins"))

  (defcommand tometips () ()
    (run-shell-command "chromium --app=https://tometips.github.io"))

  (defcommand discord () ()
    (run-shell-command "chromium --app=https://discordapp.com/"))

  (defcommand jenkins-chromium () ()
    (run-or-raise (concat "chromium --app=" *jenkins-url*)
                  '(:instance "jenkins")))

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
  (term-shell-command "xpanes -C 1 -c 'autossh -M0 -t {} -- top -d 10' localhost spb workstation.intr ci.intr kvm15.intr oracle"
                      :color 'dark :title "xpanes-top" :font '("-fa" "Monospace" "-fs" "6")))

(defcommand xpanes-guix () ()
  (term-shell-command "xpanes -C 1 -c 'ssh -t {}' guixsd workstation.intr spb"
                      :color 'dark))

(defun browse-url-firefox (url &optional new-window)
  (run-shell-command
   (join `("firefox"
           ,@(if new-window '("--new-window") '())
           ,url))))

(defcommand guix-ci () ()
  (browse-url-firefox "http://ci.guix.info/jobset/guix-master" t))

(defcommand guix-ci-package (package) ((:string "package: "))
  (unless (string= package "")
    (browse-url-firefox
     (concat "http://ci.guix.info/search?query=spec%3Aguix-master+system%3Ax86_64-linux+"
             package)
     t)))

(defcommand ci-wigust () ()
  (browse-url-firefox (format nil "~a/job/wigust/" *jenkins-url*) t))

(define-key *top-map* (kbd "M-s-w") "ci-wigust")

(defcommand ci-guix () ()
  (browse-url-firefox (format nil "~a/job/guix/" *jenkins-url*) t))

(define-key *top-map* (kbd "M-s-g") "ci-guix")

(defcommand repology-guix-outdated () ()
  (browse-url-firefox "https://repology.org/projects/?inrepo=gnuguix&outdated=1" t))

(define-key *top-map* (kbd "M-s-d") "repology-guix-outdated")

(defvar *majordomo-jenkins-url*
  "https://jenkins.intr")

(defcommand majordomo-jenkins-webservices () ()
  (browse-url-firefox (format nil "~a/job/webservices" *majordomo-jenkins-url*) t))

(define-key *top-map* (kbd "M-s-W") "majordomo-jenkins-webservices")

(defcommand majordomo-jenkins-group (group) ((:string "Group: "))
  (browse-url-firefox (format nil "~a/job/~a" *majordomo-jenkins-url* group) t))

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
         (switch-to-emacs)))

(defcommand majordomo-find-project () ()
  (progn (run-shell-command "emacsclient --eval '(mj-project-ivy)'")
         (switch-to-emacs)))

(defcommand org () ()
  (progn (run-shell-command "emacsclient --eval '(plain-org-wiki)'")
         (switch-to-emacs)))

(defcommand emacs-todo () ()
  (progn (run-shell-command (format nil "emacsclient --eval '(find-file ~s)'" "~/src/org/todo.org"))
         (switch-to-emacs)))

(defcommand org-agenda () ()
  (progn (run-shell-command "emacsclient --eval '(org-agenda)'")
         (switch-to-emacs)))

(defcommand guix-wigust () ()
  (progn (run-shell-command "emacsclient --eval '(let ((default-directory (expand-file-name \"~/src/guix-wigust/guix/wigust/packages/\"))) (counsel-find-file))'")
         (switch-to-emacs)))

(defcommand helm-tramp () ()
  (progn (run-shell-command "emacsclient --eval '(helm-tramp)'")
         (switch-to-emacs)))

(defcommand mj-installed-servers () ()
  (progn (run-shell-command "emacsclient --eval '(mj-installed-servers)'")
         (switch-to-emacs)))

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

(defcommand window-resize-by-half-vertical () ()
  ""
  (resize 0 (- (round (/ (parse-integer (format-expand *window-formatters* "%h" (current-window))) 2)))))

(defcommand emms () ()
  (switch-to-emacs)
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (emms) (delete-other-windows)))))))

(defcommand emacs-gnus () ()
  (switch-to-emacs)
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (gnus) (delete-other-windows)))))))

(defcommand guix () ()
  (switch-to-emacs)
  (run-shell-command "magit ~/src/guix"))

(defcommand elfeed () ()
  (switch-to-emacs)
  (run-shell-command
   (format nil "emacsclient -e ~s"
           (sb-unicode:lowercase
            (write-to-string
             '(progn (elfeed) (delete-other-windows)))))))

(define-key *top-map* (kbd "C-s-Up") "next-in-frame")
(define-key *top-map* (kbd "C-s-Down") "prev-in-frame")

(defcommand vnc (display) ((:string "display: "))
  (run-shell-command (concat "vncviewer -passwd .vnc/passwd 127.0.0.1:" display)))

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
(load "/home/oleg/.guix-profile/share/emacs/site-lisp/swank.asd")
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

(defcommand random-password (length) ((:string "Password length: "))
  (window-send-string (run-shell-command (format nil "bash -i -c 'random-password ~a'" length) t)))

(load-module "notifications")

(load-module "command-history")

(load-module "swm-gaps")

;; Head gaps run along the 4 borders of the monitor(s)
(setf swm-gaps:*head-gaps-size* 5)

;; Inner gaps run along all the 4 borders of a window
(setf swm-gaps:*inner-gaps-size* 5)

(setf swm-gaps:*outer-gaps-size* 0)

(defun cisco-connect-command (host)
  (join (list "env" (format nil "TELNET_PASSWORD=~s" (password-store-show "majordomo/general"))
              "cisco-interact" host)))

(defun cisco-connect (host)
  (term-shell-command (cisco-connect-command host)
                      :title (format nil "telnet-~a" host)
                      :scrollbar t))

(defmacro define-mj-cisco (command)
  `(progn
     (defcommand ,command () ()
       (cisco-connect ,(sb-unicode:lowercase (string command))))))

(define-mj-cisco sw1-mr11.intr)
(define-mj-cisco sw1-mr12.intr)
(define-mj-cisco sw2-mr12.intr)
(define-mj-cisco sw1-mr14.intr)
(define-mj-cisco sw1-mr116.intr)
(define-mj-cisco sw1-mr143.intr)
(define-mj-cisco sw1-dh507.intr)

(defcommand hms-current-stack () ()
  (message
   (run-shell-command
    (join
     (list "curl"
           "--user" (format nil "jenkins:~a"
                            (password-store-show "majordomo/jenkins/jenkins"))
           "--request" "GET" "http://nginx1.intr:8080/hms"))
    t)))

(defun bind-super ()
  (define-key *top-map* (kbd "C-S-s-RET") "rofi-mycli")
  (define-key *top-map* (kbd "C-s-s") "neofetch")
  (define-key *top-map* (kbd "s-+") "pavucontrol")
  (define-key *top-map* (kbd "s-_") "volume-toggle")
  (define-key *top-map* (kbd "s-r") "repl-guile")
  (define-key *top-map* (kbd "M-s-r") "repl-ghci")
  (define-key *top-map* (kbd "s-R") "guix-repl")
  (define-key *top-map* (kbd "C-s-r") "repl-python")
  (define-key *top-map* (kbd "C-M-s-RET") "repl-r")
  (define-key *top-map* (kbd "C-s-R") "repl-nix")
  (define-key *top-map* (kbd "C-M-s-R") "repl-node")
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
  (define-key *top-map* (kbd "s-l") "emacs-todo")
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
  (define-key *top-map* (kbd "C-s-a") "pass-route")
  (define-key *top-map* (kbd "s-j") "music-youtube")
  (define-key *top-map* (kbd "s-J") "jenkins")
  (define-key *top-map* (kbd "s-g") "gnus")
  (define-key *top-map* (kbd "s-G") "notmuch")
  (define-key *top-map* (kbd "s-C") "org")
  (define-key *top-map* (kbd "C-s-c") "org-agenda")
  (define-key *top-map* (kbd "C-s-C") "majordomo-find-project")
  (define-key *top-map* (kbd "s-i") "quassel-monitor")
  (mapcar #'(lambda (pair)
              (let ((command (concat "gmove-and-follow " (write-to-string (car pair))))
                    (key (concat "s-" (string (cdr pair)))))
                (define-key *top-map* (kbd key) command)))
          '((1 . #\!) (2 . #\@) (3 . #\#)
            (4 . #\$) (5 . #\%) (6 . #\^)
            (7 . #\&) (8 . #\*) (9 . #\()
            (0 . #\))))
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
  (define-key *top-map* (kbd "s-S") "passmenu")
  (define-key *top-map* (kbd "s-j") "music-youtube")
  (define-key *top-map* (kbd "s-u") "alerta")
  (define-key *top-map* (kbd "s-U") "grafana-upstream-interfaces")
  (define-key *top-map* (kbd "C-s-u") "grafana-netflow")
  (define-key *top-map* (kbd "s-s") "slack")
  (define-key *top-map* (kbd "s-y") "mj-installed-servers")
  (define-key *top-map* (kbd "s-;") "colon")
  (define-key *top-map* (kbd "s-[") "place-existing-windows")
  (define-key *top-map* (kbd "s-SunPrint_Screen") "zoom")
  (define-key *top-map* (kbd "s-b") "zoom")
  (define-key *top-map* (kbd "C-s-Right") "window-resize-by-half-horizontal")
  (define-key *top-map* (kbd "C-s-Left") "window-resize-by-half-vertical")

  ;; Rebind groups to PREFIX-NUMBER.
  (mapcar #'(lambda (x)
              (define-key *top-map* (kbd (concat "s-" (write-to-string x)))
                (format nil "gselect ~D" x))
              (define-key *top-map* (kbd (concat "M-s-" (write-to-string x)))
                (format nil "group-~D-start-programs" x))
              (define-key *top-map* (kbd (concat "C-s-" (write-to-string x)))
                (format nil "~A ~D" "select-window-by-number" x)))
          (range 10 :min 0 :step 1)))

(defcommand group-2-start-programs () ()
  (run-commands "gselect 2")
  (unless (current-window)
    (run-shell-command "emacsclient -c")
    (run-shell-command (firefox-command))))

(defcommand group-5-start-programs () ()
  (run-commands "gselect 5")
  (unless (current-window)
    (run-shell-command "emacsclient -c")
    (run-shell-command (firefox-command))))

(defun frame-parameters-display-0 ()
  (define-frame-preference "Default" (1 NIL T :CLASS "quassel" :TITLE "Chat Monitor"))
  (define-frame-preference "Default" (3 NIL T :CLASS "XTerm" :TITLE "alerta"))
  (define-frame-preference "Default" (5 NIL T :CLASS "Firefox" :TITLE "Alerta"))
  (define-frame-preference "Default" (0 NIL T :TITLE "pulsemixer"))
  (define-frame-preference "Default" (0 NIL T :CLASS "XTerm" :TITLE "notmuch"))
  (define-frame-preference "Default" (0 NIL T :CLASS "XTerm" :TITLE "mbsync-majordomo"))
  (define-frame-preference "Default" (0 NIL T :CLASS "XTerm" :TITLE "youtube-dl"))
  (define-frame-preference "Default" (0 NIL T :CLASS "XTerm" :TITLE "youtube-dl-music"))
  ;; (define-frame-preference "Default" (2 NIL T :CLASS "t-engine"))
  (define-frame-preference "Default" (4 NIL T :CLASS "mpv" :TITLE "emacs-emms"))
  (define-frame-preference "Default" (4 NIL T :CLASS "mpv" :TITLE "firefox"))
  (define-frame-preference "Default" (1 NIL T :CLASS "mpv" :TITLE "youtube-dl-music"))
  (define-frame-preference "3" (0 NIL T :TITLE "https://jenkins.wugi.info - Dashboard [Jenkins] - Mozilla Firefox"))
  (define-frame-preference "Default" (1 NIL NIL :CLASS "obs"))
  (define-frame-preference "2" (0 NIL NIL :CLASS "Emacs"))
  (define-frame-preference "2" (1 NIL NIL :CLASS "Firefox"))
  (define-frame-preference "5" (0 NIL NIL :CLASS "Emacs"))
  (define-frame-preference "5" (1 NIL NIL :CLASS "Firefox"))
  (define-frame-preference "3" (1 NIL T :CLASS "Firefox" :TITLE "Slack"))
  (define-frame-preference "2" (0 NIL T :CLASS "Firefox" :TITLE "jenkins")))

(cond ((string= (getenv "DISPLAY") ":0")
       (setf *maxsize-border-width* 0)
       (setf *message-window-y-padding* 0)
       (setf *normal-border-width* 0)
       (setf *transient-border-width* 0)
       (unless workstation?
	 (restore-from-file "/home/oleg/src/dotfiles/oleg/.stumpwm.d/desktop/9.lisp")
	 (bind-super)
	 (setf *maxsize-border-width* 3)
	 (setf *message-window-y-padding* 3)
	 (setf *normal-border-width* 3)
	 (setf *transient-border-width* 3))
       (frame-parameters-display-0)
       (set-background-dark))

      ((string= (getenv "DISPLAY") ":1")
       (run-shell-command "xsetroot -solid grey")
       (restore-from-file "/home/oleg/src/dotfiles/oleg/.stumpwm.d/desktop/10.lisp")
       (define-frame-preference "Default" (1 NIL T :CLASS "quassel" :TITLE "Chat Monitor"))
       (define-frame-preference "Default" (2 NIL T :CLASS "XTerm" :TITLE "alerta"))
       (define-frame-preference "Default" (0 NIL NIL :CLASS "Qemu-system-x86_64"))
       (swm-gaps:toggle-gaps) ;XXX: Make declarative.
       (bind-super)
       (define-key *top-map* (kbd "s-m") "alerta")
       (setf *maxsize-border-width* 3)
       (setf *message-window-y-padding* 3)
       (setf *normal-border-width* 3)
       (setf *transient-border-width* 3))

      (t (set-prefix-key (kbd "C-i"))))
