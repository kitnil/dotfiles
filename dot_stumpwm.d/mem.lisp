(in-package :stumpwm)

(defun get-proc-fd-field (s field)
  (if s
      (do ((line (read-line s nil nil) (read-line s nil nil)))
	  ((null line) nil)
	(let ((split (cl-ppcre:split "\\s*:\\s*" line)))
	  (when (string= (car split) field) (return (cadr split)))))
      ""))

(defun mem-usage ()
  "Returns amount of available memory."
  (with-open-file (file #P"/proc/meminfo" :if-does-not-exist nil)
    (read-from-string (get-proc-fd-field file "MemAvailable"))))

(defun fmt-mem-available (mem &optional color)
  "Returns a string representing the current available memory."
  (let* ((available (truncate (/ mem 1000))))
    (if color
        (cond ((< available (* 1024 8)) (format nil "^[^B^2*MEM: ~4D MB^]" available))
              ((< available (* 1024 4)) (format nil "^[^B^1*MEM: ~4D MB^]" available))
              (t (format nil "MEM: ~4D MB" available)))
        (format nil "MEM: ~4D MB" available))))
