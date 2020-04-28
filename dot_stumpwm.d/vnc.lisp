(in-package :stumpwm)

(defcommand vnc (display &optional view-only) ((:string "display: "))
  (run-shell-command
   (join `("vncviewer"
           "-AutoSelect=0"
           "-PreferredEncoding=Raw"
           "-FullColor=1"
           "-NoJPEG=1"
           "-CompressLevel=0"
           "-passwd" ,(concat (getenv "HOME") "/.vnc/passwd")
           ,@(if view-only '("-ViewOnly") '())
           ,(concat "127.0.0.1:" display)))))

(defcommand vnc-magnolia () ()
  (run-shell-command "exec vncviewer localhost:59555"))
