(in-package :stumpwm)

;; https://stackoverflow.com/a/48122810
(defun filter (predicate x)
   (if (consp x)  ; if x is a cons, that is a tree:
       (let ((ca (car x))
             (cd (filter predicate (cdr x)))) ; filter always the cdr
         (if (listp ca)                       ; if the car of x is a list (nil or cons)
             (cons (filter predicate ca) cd)  ; then filter also the car
             (if (funcall predicate ca) (cons ca cd) cd))) ; car is a non-nil atom!
       x))        ; if x is a atom (nil or the last cdr of an improper list), return x

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun join-to-stream (stream list &optional (delimiter #\&))
  (destructuring-bind (&optional first &rest rest) list
    (when first
      (write-string first stream)
      (when rest
        (write-char delimiter stream)
        (join-to-stream stream rest delimiter)))))

(defun join (list &optional (delimiter #\ ))
  (with-output-to-string (stream)
    (join-to-stream stream list delimiter)))

;; https://stackoverflow.com/a/34628127
(defun string-contains (string1 string2)
  (cond
   ((zerop (length string1)) nil) ; string1 is empty (no need to test it every time)
   ((> (length string1) (length string2)) nil) ; string1 is longer than string2
   ((string= string1 (subseq string2 0 (length string1))) string1) ; string2 starts with string1
   (t (string-contains string1 (subseq string2 1))))) ; otherwise shorten string2 by 1 and start over

(defun range (max &key (min 0) (step 1))
  "Get a list of integers."
  (loop for n from min below max by step
     collect n))

(defun rename-group (old-name new-name)
  (or (find-group (current-screen) new-name)
      (%grename new-name (find-group (current-screen) old-name))))

(defun current-window-width ()
  (format-expand *window-formatters* "%w" (current-window)))

(defun current-window-height ()
  (format-expand *window-formatters* "%h" (current-window)))

(defun auto-pull-frames ()
  (mapcar #'(lambda (frame)
              (pull-window-by-number frame)
              (fnext))
          (range 15 :min 0 :step 1)))

(defun single-quote-string (str)
  (let ((string-quote "'"))
    (concat string-quote str string-quote)))

(defun quote-string (str)
  (let ((string-quote "\""))
    (concat string-quote str string-quote)))

(defun time-date-and-time-restrict ()
  (time-format "%Y-%m-%d-%H-%M-%S"))
