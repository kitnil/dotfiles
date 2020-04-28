(in-package :stumpwm)

(defun password-store-show (password)
    (run-shell-command (format nil "gpg -d ~a/.password-store/~a.gpg 2>/dev/null"
                               (getenv "HOME") password)
     t))

(defcommand passmenu () ()
  (run-shell-command "passmenu -c -l 30"))

(defcommand random-password (length) ((:string "Password length: "))
  (window-send-string (run-shell-command (format nil "bash -i -c 'random-password ~a'" length) t)))

