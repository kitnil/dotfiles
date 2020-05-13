(in-package :stumpwm)

(defcommand current-window->clipboard () ()
  (putsel (window-title (current-window))))

(sb-posix:setenv "CM_HISTLENGTH" "25" 1)
(sb-posix:setenv "CM_LAUNCHER" "rofi" 1)

(defcommand clipmenu () ()
  (run-shell-command
   (join `("clipmenu" ,@(if (or dark-theme dark) '("-theme DarkBlue") '())))))

(defcommand clipmenud () ()
  (run-shell-command
   (concat (getenv "HOME") "/.nix-profile/bin/clipmenud")))

