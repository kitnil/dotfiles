(in-package :stumpwm)

(defcommand xkill () ()
  "Run `xkill'."
  (run-shell-command "xkill"))


;;;
;;; Configuration
;;;

(defcommand cursor-theme () ()
  (run-shell-command "xsetroot -cursor_name left_ptr"))

(defcommand xrdb () ()
  (run-shell-command (format nil "xrdb ~a/.Xresources" (getenv "HOME"))))


;;;
;;; Audio
;;;

(defcommand speaker-disable () ()
  (run-shell-command "xset -b"))


;;;
;;; Keyboard
;;;

;; Use keyboard as mouse with <Shift+Num Lock>
;; https://en.wikipedia.org/wiki/Mouse_keys
(defcommand pointer-keys () ()
  (run-shell-command "setxkbmap -option keypad:pointerkeys"))

;; Keyboard layout
(defcommand keyboard-layout () ()
  (run-shell-command "setxkbmap -layout us,ru -option grp:win_space_toggle"))

(defcommand xmodmap () ()
  (run-shell-command "xmodmap " (concat "/.Xmodmap" (getenv "HOME"))))

(defcommand keynav () ()
  (run-shell-command "keynav"))


;;;
;;; Screen
;;;

(defcommand turn-screen-off () ()
  "Turn screen off."
  (run-shell-command "exec xset dpms force off"))

(defcommand screen-off () ()
  (run-shell-command "xrandr --output HDMI3 --off"))

(defcommand screen-on () ()
  (run-shell-command "xrandr --output HDMI3 --auto && xrandr --output HDMI3 --right-of HDMI1"))

(defcommand reset-resolution-and-gamma () ()
  (run-shell-command "xrandr --output HDMI1 --mode 1920x1080 ; xgamma -gamma 1.0"))


;;;
;;; Drag and drop
;;;

(defcommand drag-and-drop () ()
  (run-shell-command "dragon"))

(defcommand drag-and-drop-and-exit () ()
  (run-shell-command "dragon --and-exit"))


;;;
;;; Hooks
;;;

(add-hook *start-hook*
          (lambda ()
            (xrdb)
            (speaker-disable)
            (pointer-keys)
            (keyboard-layout)
            (xmodmap)))
