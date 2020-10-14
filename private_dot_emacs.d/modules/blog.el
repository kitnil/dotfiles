(defun blog-insert-entry ()
  (interactive)
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (day (nth 3 now))
         (hour (nth 2 now))
         (minute (nth 1 now))
         (date (format "%d-%d-%d" year month day)))
    (insert "CLOSED: ")
    (newline)
    (insert ":LOGBOOK:")
    (newline)
    (insert (concat "- State \"DONE\"       from \"\"           " (concat "[" date "]")))
    (newline)
    (insert ":END:")
    (newline)
    (insert ":PROPERTIES:")
    (newline)
    (insert (concat ":CREATED:  " (concat "[" date "]")))
    (newline)
    (insert ":ID: " date "-" (read-string "Article ID: "))
    (newline)
    (insert ":END:")
    (newline)))
