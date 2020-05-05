(in-package :stumpwm)

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda () (quassel-monitor))
              (lambda () (music-youtube))
              (lambda () (clipmenud))
              (lambda () (kdeconnect-indicator))
              (lambda () (dunst))
              (lambda () (majordomo-alerta-top))
              (lambda () (pulsemixer))
              (lambda () (cursor-theme))
              (lambda () (keynav))
              (lambda () (sb-thread:make-thread
                     (lambda ()
                       (run-shell-command
                        (join (list "xdotool behave_screen_edge --delay 500 top exec"
                                    (vnc-command 5901)))))))
              (lambda () (sb-thread:make-thread
                     (lambda ()
                       (sleep 5)
                       (place-existing-windows)
                       (run-commands "fselect 2")
                       (run-shell-command (concat (getenv "HOME") "/bin/run-emacs")))))))
