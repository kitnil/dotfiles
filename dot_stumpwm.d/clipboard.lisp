(in-package :stumpwm)

(defcommand current-window->clipboard () ()
  (putsel (window-title (current-window))))

(sb-posix:setenv "CM_HISTLENGTH" "25" 1)
(sb-posix:setenv "CM_LAUNCHER" "rofi" 1)

(defcommand greenclip () ()
  (run-shell-command
   (join `("rofi"
           ,@(if (or dark-theme dark) '("-theme DarkBlue") '())
           ,(format nil "-modi ~s -show" "clipboard:greenclip print")))))

(defcommand greenclip-daemon () ()
  (run-shell-command
   (join (list (concat (getenv "HOME") "/.nix-profile/bin/greenclip")
               "daemon"))))

(defcommand espanso-daemon () ()
  (run-shell-command
   (join (list (concat (getenv "HOME") "/.nix-profile/bin/espanso")
               "start"))))
