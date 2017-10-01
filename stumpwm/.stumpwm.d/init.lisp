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
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 14))


;;;
;;; Gravity
;;;

(load-module "cpu")
(load-module "disk")
(load-module "mem")
(load-module "net")

(setf
 *message-window-gravity* :center
 *input-window-gravity*   :center
 *timeout-wait*           3
 *window-info-format*
 (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t")

 *normal-border-width* 3
 *transient-border-width* 3
 *maxsize-border-width* 3
 *window-border-style* :tight

 *time-format-string-default*
 (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B")

 *mode-line-timeout* 5

 *screen-mode-line-format*
 '("[%n]" ; Group name
   "  %w" ; Windows list
   "^>"   ; Align right
   "  %c" ; CPU
   ;; (:eval (time-format "%k:%M")) ; Net
   "  %l")

 ;; Don't set it to “sloppy”,
 ;; because it could switch window after switch desktop
 *mouse-focus-policy* :click)

(setf *window-format* "%m%n%s%c")
(setf *mode-line-timeout* 1)

(setq *ignore-wm-inc-hints* t)


;;;
;;; Windows colors
;;;

(set-win-bg-color "#DCDAD5")
(set-unfocus-color "#000000")
(set-focus-color "#DCDAD5")
(set-fg-color "#000000")
(set-bg-color "#FFFFFF")
(set-border-color "#66CD00")
(set-msg-border-width 3)


;;;
;;; Window placement policy
;;;

(clear-window-placement-rules)

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
            "Start icecat unless it is already running, in which case focus it."
            (run-or-raise "icecat" '(:class "Icecat")))

(defcommand conkeror () ()
            "Start conkeror unless it is already running, in which case focus it."
            (run-or-raise "conkeror" '(:class "Conkeror")))

(defcommand chromium () ()
  "Start chromium unless it is already running, in which case focus it."
  (run-or-raise "chromium-browser" '(:class "Chromium-browser")))


;;;
;;; Video
;;;

(defcommand pavucontrol () ()
            "Start pavucontrol unless it is already running, in which
case focus it."
            (run-or-raise "pavucontrol" '(:class "Pavucontrol")))

(defcommand mpv () ()
            "Start mpv unless it is already running, in which case focus it."
            (run-or-raise "mpv" '(:class "mpv")))

(defcommand xclip-mpv () ()
            "Play video from clipboard."
            (stumpwm:run-shell-command
             "exec mpv $(xclip -o -selection clipboard)"))

(push '(:class "mpv") stumpwm:*deny-raise-request*)
(push '(:class "mpv") *deny-map-request*)

(define-key *root-map* (kbd "v") "mpv")
(define-key *root-map* (kbd "C-v") "xclip-mpv")


;;;
;;; Terminal
;;;

(defcommand xterm () ()
            "Start XTerm unless it is already running, in which case focus it."
            (run-or-raise "xterm" '(:class "XTerm")))



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

(toggle-modeline) ; Turn on start


;;;
;;; Frames
;;;

(defcommand warp-mouse-active-frame () ()
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame) (frame-width current-frame)) 100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))


;;;
;;; Gaps
;;;

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 10)
(setf swm-gaps:*outer-gaps-size* 10)

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
;;; SLIME
;;;

(ql:quickload "swank")
(swank-loader:init)
(swank:create-server :port 4004
                     :style swank:*communication-style*
                     :dont-close t)
