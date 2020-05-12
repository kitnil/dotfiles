(in-package :stumpwm)

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda () (clipmenud))
              (lambda () (kdeconnect-indicator))
              (lambda () (dunst))
              (lambda () (cursor-theme))
              (lambda () (keynav))
              (lambda () (sb-thread:make-thread
                     (lambda ()
                       (run-shell-command
                        (join (list (xdotool-behave-screen-edge
                                     (vnc-command 5901)
                                     :position "top"))))
                       (run-shell-command
                        (join (list (xdotool-behave-screen-edge
                                     (format nil "sh -c ~s"
                                             "echo '(music-youtube)' | stumpish -e eval")
                                     :position "bottom"))))
                       (run-shell-command
                        (join (list (xdotool-behave-screen-edge
                                     (format nil "sh -c ~s"
                                             "echo '(editor)' | stumpish -e eval")
                                     :position "left")))))
                     :name "xdotool"))))
