(in-package :stumpwm)

(defun temp-current ()
  "Returns CPU temperature."
  (with-open-file (file #P"/sys/devices/virtual/thermal/thermal_zone0/temp" :if-does-not-exist nil)
    (let ((temp (read-line file)))
      (parse-integer
       (cond ((equal (car (coerce temp 'list)) #\1)
              (subseq temp 0 (length "000")))
             ((uiop/utility:string-suffix-p temp "000")
              (subseq temp 0 (- (length "000") 1)))
             (t temp))))))
