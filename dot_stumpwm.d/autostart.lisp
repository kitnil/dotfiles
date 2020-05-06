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
                        (join (list "xdotool behave_screen_edge --delay 500 top exec"
                                    (vnc-command 5901))))
                       (run-shell-command
                        (join (list (format nil "xdotool behave_screen_edge --delay 500 bottom exec sh -c ~s"
                                            "echo '(music-youtube)' | stumpish -e eval"))))
                       (run-shell-command
                        (join (list (format nil "xdotool behave_screen_edge --delay 500 left exec sh -c ~s"
                                            "echo '(editor)' | stumpish -e eval")))))))))
