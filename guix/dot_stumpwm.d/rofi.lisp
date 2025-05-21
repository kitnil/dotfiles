(in-package :stumpwm)

(defun run-rofi (command &optional dark)
  (run-shell-command
   (join `("rofi" ,command))))

(defun run-rofi* (command &optional dark)
  (run-shell-command
   (join `("rofi" ,@command))))

(defcommand rofi-drun () ()
  "Open Rofi to launch `.desktop' file."
  (run-rofi "-modi run,drun -show drun"))

(defcommand rofi-ssh () ()
  "Open Rofi ssh list."
  (run-rofi
   (format nil "-ssh-command '{terminal} --title \"ssh@{host}\" --command {ssh-client} {host}' -width 20 -terminal '~a' -modi ssh -show ssh"
           "alacritty")))

(defcommand rofi-mycli () ()
  "Open Rofi mycli."
  (run-rofi (concat "-modi mycli:" (getenv "HOME") "/.local/bin/rofi-mycli -show mycli")))

(defcommand rofi-window () ()
  "Open Rofi window list."
  (run-rofi "-modi window -show window"))

(defcommand rofi-twitchy () ()
  "Open Rofi with Twitchy plugin."
  (run-rofi "-modi twitchy:rofi-twitchy -show twitchy"))

(defcommand rofi-mytop () ()
  "Open Rofi mytop."
  (run-rofi "-modi mycli:/home/oleg/.local/bin/rofi-mycli -show mycli"))
