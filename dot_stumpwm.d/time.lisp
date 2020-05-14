(in-package :stumpwm)

(defvar *force-free-time* nil)
(defcommand toggle-free-time () ()
  (if *force-free-time*
      (setf *force-free-time* nil)
      (setf *force-free-time* t)))

(defvar *force-work-time* nil)
(defcommand toggle-work-time () ()
  (if *force-work-time*
      (setf *force-work-time* nil)
      (setf *force-work-time* t)))

(defun free-time? ()
  (if *force-work-time*
      nil
      (let ((day-of-week "%u")
            (hour (parse-integer (time-format "%k"))))
        (or *force-free-time*
            (> (parse-integer (time-format day-of-week)) 5)
            (or (>= hour 18)
                (< hour 10))))))
