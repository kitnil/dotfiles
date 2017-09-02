;; -*-lisp-*-

(in-package :stumpwm)

(setf *startup-message* nil) ; Disable welcome message.
(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr") ; Fix cursor icon

(set-module-dir "/home/natsu/.stumpwm.d/modules/")

;; Prompt the user for an interactive command.  The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))


;;;
;;; Keybinds
;;;

(stumpwm:set-prefix-key (stumpwm:kbd "F20"))

(define-key *root-map* (kbd "RET") "exec urxvt")
(define-key *root-map* (kbd "C-l") "exec xlock")

;; Pulseaudio
(define-key *root-map* (kbd "m") "exec ponymix toggle")
(define-key *root-map* (kbd ",") "exec ponymix decrease 5")
(define-key *root-map* (kbd ".") "exec ponymix increase 5")

(define-key *root-map* (kbd "/") ; Switch between loudspeakers and headphones
  "exec /home/natsu/bin/pulseaudio-switch-sink.sh")

(define-key *root-map* (kbd "\\") "exec /home/natsu/bin/toggle-input-method.sh")

;; Mpv
(define-key *root-map* (kbd "v") "exec /home/natsu/bin/xclip-mpv.sh")


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
;; (load-module "mpd")
(load-module "mem")
(load-module "net")
;; (load-module "battery-portable")
;; (load-module "notifications")

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

;; Use this command to see window properties; needed by the
;; (define-frame-preference ...) functions, below.

(define-key *root-map* (kbd "I") "show-window-properties")

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
;;; Emacs
;;;

(define-frame-preference "Emacs"
  (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  (1 t t :create "emacs-dump" :class "Emacs")
  (0 t t :class "Conkeror")
  (2 t t :class "mpv"))



;;;
;;; Browsers
;;;

(define-frame-preference "Chromium"
    (0 t t :create "chromium-dump" :class "Chromium-browser"))

(defcommand conkeror () ()
  "Start conkeror unless it is already running, in which case focus it."
  (run-or-raise "conkeror" '(:class "Conkeror")))

(define-key *root-map* (kbd "w") "conkeror")

(defcommand chromium () ()
  "Start chromium unless it is already running, in which case focus it."
  (run-or-raise "chromium-browser" '(:class "Chromium-browser")))


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

(define-key *root-map* (kbd "b") "toggle-modeline")


;;;
;;; Groups
;;;

(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
     collect n))

;; Rebind groups to PREFIX-NUMBER.
(mapcar #'(lambda (x) (define-key *root-map* (kbd (write-to-string x))
			(format nil "~A ~D" "gselect" x)))
	(range 10 :min 1 :step 1))



;;;
;;; Frames
;;;

(defcommand warp-mouse-active-frame () ()
  (let* ((current-frame (tile-group-current-frame (current-group)))
         (pointer-x (- (+ (frame-x current-frame) (frame-width current-frame)) 100))
         (pointer-y (+ 100 (frame-y current-frame))))
    (warp-pointer (current-screen) pointer-x pointer-y)))

(define-key *root-map* (kbd "t") "warp-mouse-active-frame")

(define-key *root-map* (kbd "C-2") "vsplit")
(define-key *root-map* (kbd "C-3") "hsplit")



;;;
;;; Screenshots
;;;

(define-key *root-map* (kbd "s") "exec scrot '/home/natsu/Pictures/Screenshots/%Y-%m-%d_$wx$h.png'")


;;;
;;; Gaps
;;;

(load-module "swm-gaps")
(setf swm-gaps:*inner-gaps-size* 10)
(setf swm-gaps:*outer-gaps-size* 10)

(defcommand my-fullscreen () ()
	    (swm-gaps:toggle-gaps)
	    (fullscreen))

(define-key *root-map* (kbd "F11") "my-fullscreen")
