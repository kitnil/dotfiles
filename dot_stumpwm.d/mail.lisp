(in-package :stumpwm)

(defun fetch-mail-command (command)
  (format nil "sh -c 'TMOUT=20; echo \"Fetch mail.\"; ~a; notify-send \"Mail fetched.\"; read -p \"Press Enter to close.\"'"
          command))

(defcommand mbsync-majordomo () ()
  (term-shell-command (fetch-mail-command "mbsync -a majordomo")
                      :title "mbsync-majordomo"
                      :font '("-fa" "Monospace" "-fs" "8")
                      :color "dark"))

(defcommand notmuch () ()
  (term-shell-command (fetch-mail-command "notmuch new")
                      :title "notmuch"
                      :terminal 'st
                      :font "Monospace:size=8"
                      :color "dark"))

(defvar *notmuch-job* nil)

(defcommand toggle-notmuch-job () ()
  (if *notmuch-job*
      (setf *notmuch-job* nil)
      (setf *notmuch-job* t)))

(defcommand notmuch-job () ()
  (sb-thread:make-thread
   (lambda ()
     (loop while *notmuch-job* do (progn (notmuch) (sleep 3600))))))


