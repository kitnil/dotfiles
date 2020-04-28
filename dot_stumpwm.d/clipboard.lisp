(in-package :stumpwm)

(defcommand current-window->clipboard () ()
  (putsel (window-title (current-window))))

(defcommand clipmenu () ()
  (run-shell-command "CM_HISTLENGTH=25 CM_LAUNCHER=rofi clipmenu"))

(defcommand clipmenud () ()
  (run-shell-command
   (concat (getenv "HOME") "/.nix-profile/bin/clipmenud")))

