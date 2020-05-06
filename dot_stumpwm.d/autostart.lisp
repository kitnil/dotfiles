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
                                    (vnc-command 5901)))))))))
