(in-package :stumpwm)

(defcommand obs () ()
  (run-shell-command (join (list "nixGLIntel" "obs"))))
