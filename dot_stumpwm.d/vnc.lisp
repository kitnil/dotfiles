(in-package :stumpwm)

(defun vnc-command (port &key (host "127.0.0.1") (view-only nil))
  (join `("vncviewer"
          "-AutoSelect=0"
          "-PreferredEncoding=Raw"
          "-FullColor=1"
          "-NoJPEG=1"
          "-CompressLevel=0"
          "-passwd" ,(concat (getenv "HOME") "/.vnc/passwd")
          ,@(if view-only '("-ViewOnly") '())
          ,(concat host ":" (write-to-string port)))))

(defcommand vnc (display) ((:string "display: "))
  (run-shell-command (vnc-command (parse-integer display))))

(defcommand vnc-magnolia () ()
  (run-shell-command "exec vncviewer localhost:59555"))
