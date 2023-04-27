(in-package :stumpwm)

(defvar *windows-volume-current*
  100)

(defvar *windows-volume-increment*
  20)

(defcommand windows-volume-current-update (value) ()
  (sb-thread:make-thread
   (lambda ()
     (setq *windows-volume-current* value))
   :name "windows-volume-current-update"))

(defcommand windows-volume-decrease () ()
  (when (> *windows-volume-current* 0)
    (let ((value (- *windows-volume-current*
                    *windows-volume-increment*)))
      (run-shell-command (format nil "windows volume ~a" value))
      (windows-volume-current-update value))))

(defcommand windows-volume-increase () ()
  (when (< *windows-volume-current* 100)
    (let ((value (+ *windows-volume-current*
                    *windows-volume-increment*)))
      (run-shell-command (format nil "windows volume ~a" value))
      (windows-volume-current-update value))))
