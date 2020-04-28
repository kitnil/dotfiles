(in-package :stumpwm)

(defcommand racket () ()
  (run-shell-command "drracket"))

(defcommand editor () ()
  (run-shell-command "leafpad"))
