(in-package :stumpwm)


(defcommand xclip-kdeconnect-handler () ()
  "Open URL on Android device via KDE Connect."
  (let ((clipboard (get-x-selection)))
    (run-shell-command
     (join `("kdeconnect-handler" ,clipboard)))
    (message (concat "Open URL on Android " clipboard))))

(defcommand scrcpy () ()
  (run-shell-command "scrcpy --max-fps 30 --turn-screen-off"))

(defcommand kdeconnect-indicator () ()
  (run-shell-command
   (concat (getenv "HOME") "/.guix-profile/bin/kdeconnect-indicator")))
