(in-package :stumpwm)

(defcommand brightness (percentage) ((:number "Percentage: "))
  (run-shell-command (join (list "brightness" (write-to-string percentage)))))
