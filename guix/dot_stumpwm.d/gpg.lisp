(in-package :stumpwm)

(defun gpg-key-opened? (key)
  (not (= (length (run-shell-command
                   (format nil "gpg --pinentry-mode cancel -d ~s" key) t))
          0)))
