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

(defun fmt-mem-available (mem)
  "Returns a string representing the current available memory."
  (let* ((available (truncate (/ mem 1000))))
    (format nil "~4D MB" available)))
