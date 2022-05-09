(in-package :stumpwm)

(defcommand virt-manager () ()
  "Start or focus virt-manager."
  (run-or-raise "virt-manager" '(:class ".virt-manager-real")))
