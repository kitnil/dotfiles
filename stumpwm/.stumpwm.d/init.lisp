;; -*-lisp-*-

(in-package :stumpwm)

(setf *startup-message* nil) ; Disable welcome message.
(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr") ; Fix cursor icon

(defcommand emms-previous () ()
            "Emacs Emms previous in playlist."
            (run-shell-command "emacsclient --eval '(emms-previous)'"))

(defcommand emms-next () ()
            "Emacs Emms next in playlist."
            (run-shell-command "emacsclient --eval '(emms-next)'"))

(defcommand emms-stop () ()
            "Emacs Emms stop."
            (run-shell-command "emacsclient --eval '(emms-stop)'"))

(define-interactive-keymap (emms) ()
  ((kbd "SPC") "emms-stop")

  ((kbd "Up") "emms-previous")
  ((kbd "C-p") "emms-previous")
  ((kbd "p") "emms-previous")
  ((kbd "k") "emms-previous")

  ((kbd "Down") "emms-next")
  ((kbd "C-n") "emms-next")
  ((kbd "n") "emms-next")
  ((kbd "j") "emms-next"))

(set-module-dir "/home/natsu/.stumpwm.d/modules/")


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
;;; Gravity
;;;

(load-module "cpu")
(load-module "disk")
(load-module "mem")
(load-module "net")

(setf *timeout-wait* 3
      *mode-line-timeout* 5)

(setf *time-format-string-default* (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B"))

(setf *window-border-style* :tight)

;; Don't set it to “sloppy”,
;; because it could switch window after switch desktop
(setf *mouse-focus-policy* :click)

(setf *window-info-format* (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t"))

(setf *window-format* "%s%m %n [%i] %c")
(setf *mode-line-timeout* 1)

(setq *ignore-wm-inc-hints* t)


;;;
;;; Windows colors
;;;

(set-win-bg-color "#DCDAD5")
(set-unfocus-color "#000000")
(set-focus-color "#A52A2A")
(set-fg-color "#FFFFFF")
(set-bg-color "#000000")
(set-border-color "#FF0000")
(set-msg-border-width 3)

(setf *normal-border-width* 5
      *transient-border-width* 5
      *maxsize-border-width* 5)


;;;
;;; Window placement policy
;;;

(clear-window-placement-rules)

(define-frame-preference "Default"
    (0 nil nil :class "mpv")
  (1 nil nil :class "Emacs")
  (2 nil nil :class "XTerm"))

;; Last rule to match takes precedence!
;; TIP: if the argument to :title or :role begins with an ellipsis, a substring
;; match is performed.
;; TIP: if the :create flag is set then a missing group will be created and
;; restored from *data-dir*/create file.
;; TIP: if the :restore flag is set then group dump is restored even for an
;; existing group using *data-dir*/restore file.


;;;
;;; Browsers
;;;

(defcommand icecat () ()
  "Start or focus icecat."
  (run-or-raise "icecat" '(:class "Icecat")))

(define-key *root-map* (kbd "w") "icecat")

(defcommand conkeror () ()
  "Start or focus conkeror."
  (run-or-raise "conkeror" '(:class "Conkeror")))

(defcommand chromium () ()
  "Start or focus chromium."
  (run-or-raise "chromium-browser" '(:class "Chromium-browser")))

(define-key *root-map* (kbd "C-w") "chromium")


;;;
;;; Video
;;;

(defcommand pavucontrol () ()
  "Start or focus pavucontrol."
  (run-or-raise "pavucontrol" '(:class "Pavucontrol")))

(defcommand pulsemixer () ()
  "Start pulsemixer."
  (run-shell-command "xterm -e pulsemixer"))

(defcommand mpv () ()
  "Start or focus mpv."
  (run-or-raise "mpv" '(:class "mpv")))

(defcommand xclip-mpv () ()
  "Play video from clipboard."
  (run-shell-command "exec mpv $(xclip -o -selection clipboard)"))

(defcommand mpv-music () ()
  "Play music."
  (run-shell-command "exec mpv --msg-level=all=no --no-resume-playback /srv/music/*"))

(defcommand youtube-dl () ()
  (run-shell-command "exec urxvtc -name youtube-dl -e youtube-dl $(xclip -o -selection clipboard)"))

(defcommand youtube-dl-play () ()
  "Download video and play it."
  (run-shell-command "exec urxvtc -name youtube-dl -e youtube-dl --exec 'mpv {}' $(xclip -o -selection clipboard)"))

(define-key *root-map* (kbd "v") "mpv")
(define-key *root-map* (kbd "C-v") "xclip-mpv")


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
            (let* ((current-frame (tile-group-current-frame (current-group)))
                   (pointer-x (- (+ (frame-x current-frame)
                                    (frame-width current-frame))
                                 100))
                   (pointer-y (+ 100 (frame-y current-frame))))
              (warp-pointer (current-screen) pointer-x pointer-y)))


;;;
;;; Gaps
;;;

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 25)

(load-module "kbd-layouts")
(kbd-layouts:keyboard-layout-list "us" "ru")

(load-module "screenshot")

(load-module "clipboard-history")
(define-key *root-map* (kbd "C-y") "show-clipboard-history")
(clipboard-history:start-clipboard-manager)

(load-module "globalwindows")
(define-key *root-map* (kbd "M-1") "global-windowlist")
(define-key *root-map* (kbd "M-2") "global-pull-windowlist")


;;;
;;; Pinentry
;;;

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
