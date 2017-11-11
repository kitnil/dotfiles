;; -*-lisp-*-

(in-package :stumpwm)

(setf *startup-message* nil)

(set-module-dir "/home/natsu/.stumpwm.d/modules/")

(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr")


;;;
;;; Fonts
;;;

(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font
                         :family "DejaVu Sans Mono"
                         :subfamily "Book"
                         :size 14))


;;;
;;; Windows colors
;;;

(setf *window-border-style* :tight)

(setf *ignore-wm-inc-hints* t)

(set-win-bg-color "#DCDAD5")
(set-unfocus-color "#000000")
(set-focus-color "#A52A2A")
(set-fg-color "#000000")
(set-bg-color "#FFFFFF")
(set-border-color "#A52A2A")
(set-msg-border-width 3)

(setf *normal-border-width* 5
      *transient-border-width* 5
      *maxsize-border-width* 5)


;;;
;;; Window placement policy
;;;

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(clear-window-placement-rules)

(define-frame-preference "Default"
    (0 nil nil :class "mpv")
  (1 nil nil :class "Emacs")
  (2 nil nil :class "XTerm"))

;; Last rule to match takes precedence!
;; If the argument to :title or :role begins with an ellipsis, a
;; substring match is performed.
;; If the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; If the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.


;;;
;;; Browsers
;;;

(defcommand conkeror () ()
  "Start or focus conkeror."
  (run-or-raise "conkeror" '(:class "Conkeror")))

(define-key *root-map* (kbd "C-w") "conkeror")

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "Icecat")))

(define-key *root-map* (kbd "w") "icecat")

(defcommand chromium () ()
  "Start or focus chromium."
  (run-or-raise "chromium" '(:class "Chromium-browser")))

(define-key *root-map* (kbd "M-w") "chromium")


;;;
;;; Video
;;;

(defcommand mpv () ()
  "Start or focus mpv."
  (run-or-raise "mpv" '(:class "mpv")))

(defcommand xclip-mpv () ()
  "Play video from clipboard."
  (run-shell-command (join (list "exec mpv" (get-x-selection)) #\ )))

(defcommand kodi-cli-youtube () ()
  "Send video from clipboard to Kodi."
  (run-shell-command (join (list "exec kodi-cli -y" (get-x-selection)) #\ )))

(defcommand mpv-music () ()
  "Play music."
  (run-shell-command "exec mpv --keep-open=no --msg-level=all=no --no-resume-playback /srv/music/*"))

(defcommand youtube-dl () ()
  "Download video."
  (run-shell-command "exec urxvtc -name youtube-dl -e youtube-dl $(xclip -o -selection clipboard)"))

(defcommand youtube-dl-play () ()
  "Download video and play it."
  (run-shell-command "exec urxvtc -name youtube-dl -e youtube-dl --exec 'mpv {}' $(xclip -o -selection clipboard)"))

(define-key *root-map* (kbd "v") "mpv")
(define-key *root-map* (kbd "C-v") "xclip-mpv")

(defcommand turn-screen-off () ()
            "Turn screen off."
            (run-shell-command "exec xset dpms force off"))

(push '(:class "mpv") stumpwm:*deny-raise-request*)
(push '(:class "mpv") *deny-map-request*)


;;;
;;; Utils
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

(defcommand run-xterm-command (cmd &optional collect-output-p) ((:shell "/bin/sh -c "))
  "Run the specified shell command in XTerm."
  (run-prog *shell-program*
            :args (list "-c" (join (list "xterm -name" cmd "-e" cmd) #\ ))
            :wait nil))

(define-key *root-map* (kbd "M-!") "run-xterm-command")


;;;
;;; Pulseaudio
;;;

(defcommand vol-up () ()
  "Increase the volume from the shell"
  (run-shell-command
   "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') + 0xf00)) ) &> /dev/null"))

(defcommand vol-down () ()
  "Decrease the volume  from the shell"
  (run-shell-command
   "pacmd set-sink-volume 0 $(printf '0x%x' $(( $(pacmd dump|grep set-sink-volume|cut -f3 -d' ') - 0xf00)) ) &> /dev/null"))

(define-interactive-keymap volume ()
  ((kbd "Up") "vol-up")
  ((kbd "Down") "vol-down"))

(define-key *root-map* (kbd "M-v") "volume")


;;;
;;; Terminal
;;;

(defcommand xterm () ()
  "Start or focus XTerm."
  (run-or-raise "xterm" '(:class "XTerm")))

(define-key *root-map* (kbd "c") "xterm")


;;;
;;; Mode-line
;;;

(setq *mode-line-border-color*     "#000000"
      *mode-line-foreground-color* "#ffffff"
      *mode-line-background-color* "#000000")

(defcommand toggle-modeline () ()
  "Toggle mode line."
  (stumpwm:toggle-mode-line (stumpwm:current-screen)
                            (stumpwm:current-head)))

(setf *screen-mode-line-format* "%n^>%c%l%d")


;;;
;;; Frames
;;;

(defcommand warp-mouse-active-frame () ()
  "Move mouse cursor to the top right of current frame."
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame)
                          (frame-width current-frame))
                       100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))


;;;
;;; Gaps
;;;

;; (load-module "swm-gaps")
;; (setf swm-gaps:*inner-gaps-size* 5
;;       swm-gaps:*outer-gaps-size* 25)


;;;
;;; Keyboard layouts
;;;

(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us" "ru")


;;;
;;; Screenshots
;;;

(load-module "screenshot")

(defun time-date-and-time-restrict ()
  (time-format "%Y-%m-%d-%H-%M-%S"))

(defun screenshot-filename ()
  (concat (time-date-and-time-restrict)
          ".png"))

(defcommand screenshot-default () ()
  "Screenshot with filename like 2017-10-30-03-29-16.png"
  (eval-command (concat "screenshot-window " (screenshot-filename))))

(define-key *root-map* (kbd "Print") "screenshot-default")


;;;
;;; Clipboard
;;;

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)


;;;
;;; Global windows selection
;;;

(load-module "globalwindows")
(define-key *root-map* (kbd "M-quoteright") "global-windowlist")
(define-key *root-map* (kbd "M-quotedbl") "global-pull-windowlist")


;;;
;;; Pinentry
;;;

;; Dependencies
(ql:quickload "cffi")
(ql:quickload "usocket-server")

(load-module "pinentry")


;;;
;;; SLIME
;;;

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
