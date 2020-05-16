(in-package :stumpwm)


;;;
;;; Background
;;;

(defcommand xplanet () ()
  (run-shell-command "xplanet -center '+960+540' -fov 1"))

(defcommand set-background-dark () ()
  (run-shell-command "xsetroot -solid black"))

(set-background-dark)
(xplanet)


;;;
;;; Wrappers
;;;

(set-msg-border-width 4)
(setf *ignore-wm-inc-hints* t)
(setf *window-border-style* :thin)
(setf *window-format* "%m%n%s %c %50t")
(setf *suppress-window-placement-indicator* t)

(setf *float-window-border* 0)
(setf *float-window-title-height* 0)

(defparameter dark-theme nil)
(progn
  (set-focus-color "#0a420a")
  (set-border-color "#0a420a")
  (set-float-focus-color "#0a420a"))
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

(defcommand current-theme () ()
  (if dark-theme "dark" "light"))

(add-hook *focus-frame-hook*
          (lambda (current-frame last-frame)
            (redisplay)))


;;;
;;; Windows
;;;

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
;;; Fonts
;;;

;; (require :ttf-fonts)
;; (setf xft:*font-dirs* '("/run/current-system/profile/share/fonts/"))
;; (setf clx-truetype:+font-cache-filename+ (concat (getenv "HOME") "/.fonts/font-cache.sexp"))
;; (xft:cache-fonts)
;; (set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 11))
;; (set-font "9x15bold")


;;;
;;; GTK
;;;

(sb-posix:setenv "GTK_THEME" "Adwaita:dark" 1)

(defcommand xcompmgr () ()
  (run-shell-command "xcompmgr -c -C -t-5 -l-5 -r10 -o.55"))

(defcommand picom () ()
  (run-shell-command "picom -cCGfF -o 0.38 -O 200 -I 200 -t 0 -l 0 -r 3 -D2 -m 0.88"))
