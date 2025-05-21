(in-package :stumpwm)

(defcommand quassel-monitor () ()
  (run-or-raise "quassel" '(:class "quassel" :title "Chat Monitor")))
