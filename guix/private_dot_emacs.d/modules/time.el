(setq display-time-24hr-format t) ; No AM/PM
(setq calendar-date-style 'european) ; day/month/year style calendar
(setq calendar-week-start-day 1) ; Monday is the first day of the week

(defun wi-calendar-current-date-time ()
  "Return the formated string of current year month day hour minute."
  (interactive)
  (let* ((now (decode-time))
         (year (nth 5 now))
         (month (nth 4 now))
         (day (nth 3 now))
         (hour (nth 2 now))
         (minute (nth 1 now)))
    (kill-new (format "%d-%d-%d %d:%d" year month day hour minute))))

;; Interested in those timezones
(with-eval-after-load 'time
  (setq display-time-world-time-format "%Z\t%a\t%d %B %H:%M")
  (setq display-time-world-list
        '(("Europe/Moscow" "Europe/Moscow")
          ("Europe/Berlin" "Europe/Berlin")
          ("Europe/London" "Europe/London")
          ("Europe/Istanbul" "Europe/Istanbul")
          ("America/Winnipeg" "America/Winnipeg")
          ("America/New_York" "America/New_York")
          ("Asia/Tokyo" "Asia/Tokyo")
          ("Asia/Bangkok" "Thailand"))))
