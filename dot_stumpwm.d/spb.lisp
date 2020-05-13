(in-package :stumpwm)

(defvar *spb-disk-free-root-counter* 0)

(defcommand spb-disk-free-root-update-counter () ()
  (sb-thread:make-thread
   (lambda ()
     (setq *spb-disk-free-root-counter* (disk-free "/" :remote "spb")))
   :name "spb-disk-free-root-update-counter"))

