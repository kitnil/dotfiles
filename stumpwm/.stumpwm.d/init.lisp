;; -*-lisp-*-

;; (push #p"/home/natsu/src/stumpwm/" asdf:*central-registry*)
;; (ql:quickload "stumpwm")

(in-package :stumpwm)

;; (require 'swank)
;; (in-package :swank)
;; (swank:create-server)

(set-module-dir "/home/natsu/.stumpwm.d/modules/")

;; Prompt the user for an interactive command.  The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
  (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
    (when cmd
      (eval-command cmd t))))

(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr") ; Fix cursor icon


;;;
;;; Keybinds
;;;

(set-prefix-key (kbd "F20")) ; Super (Win) key in redefined .Xmodmap

(define-key *root-map* (kbd "RET") "exec urxvt")

(define-key *root-map* (kbd "b") "colon1 exec icecat http://www.")
(define-key *root-map* (kbd "C-s") "colon1 exec xterm -e ssh localhost")
(define-key *root-map* (kbd "C-l") "exec xlock")

;; (define-key *root-map* (kbd "\\") "exec setxkbmap us")

;; Pulseaudio management
(define-key *root-map* (kbd "m") "exec ponymix toggle")
(define-key *root-map* (kbd ",") "exec ponymix decrease 5")
(define-key *root-map* (kbd ".") "exec ponymix increase 5")
(define-key *root-map* (kbd "/") ; Switch between loudspeakers and headphones
  "exec /home/natsu/bin/pulseaudio-switch-sink.sh")
(define-key *root-map* (kbd "\\") "exec /home/natsu/bin/toggle-input-method.sh")
;; Open video from clipboard with mpv
(define-key *root-map* (kbd "v") "exec /home/natsu/bin/xclip-mpv.sh")


;;;
;;; Frames
;;;

;; (defun send-key-last-frame (key)
;;   (focus-last-frame (current-group))
;;   (let ((scroll-keycode
;;          (xlib:keysym->keycodes *display*
;;                                 (keysym-name->keysym key))))
;;     (xtest:fake-key-event *display* scroll-keycode t)
;;     (xtest:fake-key-event *display* scroll-keycode nil)
;;     (focus-last-frame (current-group))))

;; (defcommand scroll-last-down () ()
;;   "Scroll down the last frame that had focus"
;;   (send-key-last-frame "Page_Down"))

;; (defcommand scroll-last-up () ()
;;   "Scroll up the last frame that had focus"
;;   (send-key-last-frame "Page_Up"))

;; (define-key *root-map* (kbd "C-M-v") "scroll-last-down")
;; (global "s-b" "scroll-last-up")

;; Web jump (works for Google and Imdb)
(defmacro make-web-jump (name prefix)
  `(defcommand ,(intern name) (search) ((:rest ,(concatenate 'string name " search: ")))
    (substitute #\+ #\Space search)
    (run-shell-command (concatenate 'string ,prefix search))))

(make-web-jump "google" "firefox http://www.google.fr/search?q=")
(make-web-jump "imdb" "firefox http://www.imdb.com/find?q=")

;; C-t M-s is a terrble binding, but you get the idea.
(define-key *root-map* (kbd "M-s") "google")
(define-key *root-map* (kbd "i") "imdb")


;;;
;;; Fonts
;;;

(set-font "-misc-fixed-medium-r-semicondensed--13-120-75-75-c-60-iso8859-1")
;; (in-package :clx-truetype)
(load-module "ttf-fonts")
(xft:cache-fonts)
(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 14))


;;;
;;; Gravity
;;;

(setf
 *message-window-gravity* :center
 *input-window-gravity*   :center

 *window-info-format*
 (format nil "^>^B^5*%c ^b^6*%w^7*x^6*%h^7*~%%t")

 *time-format-string-default*
 (format nil "^5*%H:%M:%S~%^2*%A~%^7*%d %B")

 *mode-line-timeout* 5
 *screen-mode-line-format*
 '("^5*" (:eval (time-format "%k:%M"))
   " ^2*%n"                     ; group name
   " ^7*%c"                     ; cpu
   " ^6*%l"                     ; net
   al/battery-mode-string)

 *mouse-focus-policy* :sloppy)


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
(define-frame-preference "Default"
  ;; frame raise lock (lock AND raise == jumpto)
  (0 t nil :class "Konqueror" :role "...konqueror-mainwindow")
  (1 t nil :class "XTerm"))

(define-frame-preference "Ardour"
  (0 t   t   :instance "ardour_editor" :type :normal)
  (0 t   t   :title "Ardour - Session Control")
  (0 nil nil :class "XTerm")
  (1 t   nil :type :normal)
  (1 t   t   :instance "ardour_mixer")
  (2 t   t   :instance "jvmetro")
  (1 t   t   :instance "qjackctl")
  (3 t   t   :instance "qjackctl" :role "qjackctlMainForm"))

(define-frame-preference "Shareland"
  (0 t   nil :class "XTerm")
  (1 nil t   :class "aMule"))

(define-frame-preference "Emacs"
  (1 t t :restore "emacs-editing-dump" :title "...xdvi")
  (0 t t :create "emacs-dump" :class "Emacs"))



