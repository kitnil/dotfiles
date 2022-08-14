(in-package :stumpwm)

(defun disk-free (target &key remote (standby ""))
  (let ((size (fourth (split-string (second (split-string (run-shell-command (if remote
                                                                                 (format nil "ssh ~a -- df -h ~a" remote target)
                                                                                 (format nil "df -h ~a" target))
                                                                             t)
                                                          '(#\newline)))
                                    '(#\space))))
        (standby
          (if (string= standby "")
              nil
              (string= (car
                        (last (split-string
                               (string-trim
                                '(#\Newline)
                                (run-shell-command (format nil "sudo hdparm -C ~a" standby) t))
                               '(#\space))))
                       "standby"))))
    (if standby
        "standby"
        (cond ((uiop/utility:string-suffix-p size "G")
               (format nil "~a GB" (delete #\G size)))
              ((uiop/utility:string-suffix-p size "M")
               (format nil "~a MB" (delete #\M size)))
              ((uiop/utility:string-suffix-p size "T")
               (format nil "~a TB" (delete #\T size)))
              (t size)))))

(defvar *disk-free-root-counter* 0)

(defcommand disk-free-root-update-counter () ()
  (setq *disk-free-root-counter* (disk-free "/")))

(defvar *disk-free-srv-counter* 0)

(defcommand disk-free-srv-update-counter () ()
  (setq *disk-free-srv-counter*
        (disk-free "/srv" :standby "/dev/disk/by-label/data18")))
