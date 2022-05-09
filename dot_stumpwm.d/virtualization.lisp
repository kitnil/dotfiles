(in-package :stumpwm)

(defcommand virt-manager () ()
  "Start or focus virt-manager."
  (run-or-raise "virt-manager" '(:class ".virt-manager-real")))

(defcommand looking-glass-client () ()
  "Start of focus looking-glass-client-wrapper."
  (run-or-raise "looking-glass-client-wrapper -F"
                '(:class "looking-glass-client")))
