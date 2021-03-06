(in-package :stumpwm)

(mapcar (lambda (func)
          (add-hook *start-hook* func))
        (list (lambda () (kdeconnect-indicator))
              (lambda () (dunst))
              (lambda ()
                (greenclip-daemon)
                (espanso-daemon)
                (picom)
                (idesk)
                (run-shell-command "/home/oleg/bin/wallpaper"))
              (lambda () (cursor-theme))
              (lambda () (keynav))
              (lambda () (keyboard-layout))
              (lambda () (xmodmap))
              (lambda () (run-commands "volume-decrease")) ;start pulseaudio
              (lambda () (run-shell-command "/home/oleg/bin/xmodmap.sh"))
              ;; (lambda () (sb-thread:make-thread
              ;;        (lambda ()
              ;;          (run-shell-command
              ;;           (join (list (xdotool-behave-screen-edge
              ;;                        (vnc-command 5901)
              ;;                        :position "top"))))
              ;;          (run-shell-command
              ;;           (join (list (xdotool-behave-screen-edge
              ;;                        (format nil "sh -c ~s"
              ;;                                "echo '(music-youtube)' | stumpish -e eval")
              ;;                        :position "bottom"))))
              ;;          (run-shell-command
              ;;           (join (list (xdotool-behave-screen-edge
              ;;                        (format nil "sh -c ~s"
              ;;                                "echo '(editor)' | stumpish -e eval")
              ;;                        :position "left")))))
              ;;        :name "xdotool"))
              ))
