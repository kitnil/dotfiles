(in-package :stumpwm)

(defcommand dunst-disable () ()
  (run-shell-command "pkill -SIGUSR1 dunst"))

(defcommand dunst-enable () ()
  (run-shell-command "pkill -SIGUSR2 dunst"))

(defcommand dunst () ()
  (run-shell-command
   (concat (getenv "HOME") "/.guix-profile/bin/dunst")))

