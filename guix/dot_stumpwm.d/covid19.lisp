(in-package :stumpwm)

(defvar *covid-19-count* "")

(defcommand covid-19-update-count () ()
  (setq *covid-19-count* (run-shell-command "covid19" t))
  (mode-line-update))
