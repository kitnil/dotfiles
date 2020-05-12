(in-package :stumpwm)

(defvar *spb-disk-free-root-counter* 0)

(defcommand spb-disk-free-root-update-counter () ()
  (setq *spb-disk-free-root-counter* (disk-free "/" :remote "spb")))

