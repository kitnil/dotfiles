(in-package :stumpwm)

(defun disk-free (target &key remote)
  (let ((size (fourth (split-string (second (split-string (run-shell-command (if remote
                                                                                 (format nil "ssh ~a -- df -h ~a" remote target)
                                                                                 (format nil "df -h ~a" target))
                                                                             t)
                                                          '(#\newline)))
                                    '(#\space)))))
    (cond ((uiop/utility:string-suffix-p size "G")
           (format nil "~a GB" (delete #\G size)))
          ((uiop/utility:string-suffix-p size "M")
           (format nil "~a MB" (delete #\M size)))
          (t size))))

(defvar *disk-free-root-counter* 0)

(defcommand disk-free-root-update-counter () ()
  (setq *disk-free-root-counter* (disk-free "/")))

(defvar *disk-free-srv-counter* 0)

(defcommand disk-free-srv-update-counter () ()
  (setq *disk-free-srv-counter* (disk-free "/srv")))
