(in-package :stumpwm)

(defcommand adb-key-up () ()
  (run-shell-command "adb shell input keyevent 19"))

(defcommand adb-key-down () ()
  (run-shell-command "adb shell input keyevent 20"))

(defcommand adb-key-left () ()
  (run-shell-command "adb shell input keyevent 21"))

(defcommand adb-key-right () ()
  (run-shell-command "adb shell input keyevent 22"))

(defcommand adb-key-back () ()
  (run-shell-command "adb shell input keyevent 4"))

(defcommand adb-key-enter () ()
  (run-shell-command "adb shell input keyevent 66"))

(defcommand adb-key-volume-up () ()
  (run-shell-command "adb shell input keyevent 24"))

(defcommand adb-key-volume-down () ()
  (run-shell-command "adb shell input keyevent 25"))

(defcommand adb-key-power () ()
  (run-shell-command "adb shell input keyevent 26"))
