(in-package :stumpwm)

(defcommand racket () ()
  (run-shell-command "drracket"))

(defcommand editor () ()
  (run-or-raise "leafpad" '(:class "Leafpad") ))
