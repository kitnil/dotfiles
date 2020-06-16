(in-package :stumpwm)

;; Look for audio devices ‘mpv --audio-device=help’
(defvar *headphones*
  "pulse/alsa_output.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "Pulseaudio device.  Heaphones.")

(defcommand osd-sound () ()
  (run-shell-command "if pgrep -f osd-sound > /dev/null; then pkill osd-sound; osd-sound; else osd-sound; fi"))

(defun volume-current ()
  (if (= 1 (parse-integer (run-shell-command "ponymix is-muted && printf 0 || printf 1" t)))
      (bar (parse-integer (string-trim '(#\Newline) (run-shell-command "ponymix get-volume" t))) 10
           #\#
           #\ )
      "MUTED"))

(defcommand volume-current-message () ()
  (message (format nil "~a ~a"
                   "Volume:"
                   (volume-current))))

(defcommand volume-current-update () ()
  (sb-thread:make-thread
   (lambda ()
     (setq *volume-current* (volume-current)))
   :name "volume-current-update"))

(defcommand volume-decrease () ()
  (run-shell-command "ponymix decrease 5")
  (volume-current-update)
  (unless (head-mode-line (current-head))
    (volume-current-message)))

(defcommand volume-increase () ()
  (run-shell-command "ponymix increase 5")
  (volume-current-update)
  (unless (head-mode-line (current-head))
    (volume-current-message)))

(defcommand volume-toggle () ()
  (run-shell-command "ponymix toggle")
  (volume-current-update)
  (unless (head-mode-line (current-head))
    (volume-current-message)))

(define-interactive-keymap volume nil
  ((kbd "-") "volume-decrease")
  ((kbd "=") "volume-increase"))

(defcommand volume-increase-device-0 () ()
  (run-shell-command "ponymix increase --device 0 5"))

(defcommand volume-decrease-device-0 () ()
  (run-shell-command "ponymix decrease --device 0 5"))

(defcommand pavucontrol () ()
  (run-shell-command "pavucontrol"))

(defcommand pulsemixer () ()
  (term-shell-command "pulsemixer" :terminal 'st :font "Monospace:size=8"))

(defcommand alsamixer () ()
  "Download video."
  (run-shell-command "exec xterm -name alsamixer -e alsamixer"))
