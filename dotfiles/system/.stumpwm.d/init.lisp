(in-package :stumpwm)

(run-shell-command "xrdb ~/.Xresources")


;;;
;;; Swank
;;;

(require :swank)

(swank-loader:init)

(sb-thread:make-thread
 (lambda () (swank:create-server :port 4005 :dont-close t))
 :name "swank")


;;;
;;; Window
;;;

(setf *normal-border-width* 10)

