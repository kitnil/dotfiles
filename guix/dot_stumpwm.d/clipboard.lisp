(in-package :stumpwm)

(defcommand current-window->clipboard () ()
  (putsel (window-title (current-window))))

(sb-posix:setenv "CM_HISTLENGTH" "25" 1)
(sb-posix:setenv "CM_LAUNCHER" "rofi" 1)

(defcommand greenclip () ()
  (run-shell-command
   (join `("rofi"
           ,(format nil "-modi ~s -show clipboard -run-command '{cmd}'" "clipboard:greenclip print")))))

(defcommand espanso-daemon () ()
  (run-shell-command
   (join (list (concat (getenv "HOME") "/.nix-profile/bin/espanso")
               "start"))))
