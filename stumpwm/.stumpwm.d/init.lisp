;; -*-lisp-*-

(in-package :stumpwm)

(setf *startup-message* nil) ; Disable welcome message.
(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr") ; Fix cursor icon

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
(set-unfocus-color "#A9A9A9")
(set-focus-color "#66CD00")
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

(defcommand mpv () ()
            "Start mpv unless it is already running, in which case focus it."
            (run-or-raise "mpv" '(:class "mpv")))


;;;
;;; Mode-line
;;;

(setq *mode-line-border-color*     "#DCDAD5"
      *mode-line-foreground-color* "#000000"
      *mode-line-background-color* "#DCDAD5")

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
