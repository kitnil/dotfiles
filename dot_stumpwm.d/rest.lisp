(in-package :stumpwm)

(defvar *notify-to-rest* t)
(defvar *notify-to-rest-period* (* 60 20))

(defcommand toggle-notify-to-rest () ()
  (if *notify-to-rest*
      (setf *notify-to-rest* nil)
      (setf *notify-to-rest* t)))

(defcommand notify-to-rest-set-period (period) ((:string "Period of notifications in seconds: "))
  (if *notify-to-rest-period*
      (setf *notify-to-rest-period* period)
      (setf *notify-to-rest-period* 3600)))

(defcommand notify-to-rest () ()
  (sb-thread:make-thread
   (lambda ()
     (loop while (or *notify-to-rest* *work-time*)
        do (progn (run-shell-command
                   (format nil "notify-send --urgency=low ~s"
                           (format nil "Take a break.  Next notification in ~a seconds."
                                   *notify-to-rest-period*)))
                  (sleep *notify-to-rest-period*))))
   :name "notify-to-rest"))
