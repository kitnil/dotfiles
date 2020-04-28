(in-package :stumpwm)

(defun frame-parameters-display-1 ()
  (define-frame-preference "Default" (1 NIL T :CLASS "XTerm" :TITLE "mbsync-majordomo"))
  (define-frame-preference "Default" (1 NIL T :CLASS "quassel" :TITLE "Chat Monitor"))
  (define-frame-preference "Default" (2 NIL T :CLASS "XTerm" :TITLE "alerta"))
  (define-frame-preference "Default" (2 NIL T :CLASS "XTerm" :TITLE "notmuch"))
  (define-frame-preference "Default" (2 NIL T :TITLE "pulsemixer")))
