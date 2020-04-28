(in-package :stumpwm)

(defun screenshot-filename ()
  (concat (time-date-and-time-restrict)
          ".png"))

(defcommand screenshot-default () ()
  "Screenshot with filename like 2017-10-30-03-29-16.png"
  (eval-command (concat "screenshot-window " (screenshot-filename))))

(defcommand xfce-screenshooter () ()
    (run-shell-command "xfce4-screenshooter"))

(defcommand zoom () ()
  (run-shell-command "nixGLIntel boomer"))
