;; Look for audio devices ‘mpv --audio-device=help’
(defvar ‎wi-headphones "pulse/alsa_output\
.usb-Logitech_Logitech_USB_Headset-00.analog-stereo"
  "My USB headphones.")

(setq pulseaudio-control-pactl-path
      "/run/current-system/profile/bin/pactl")


;;;
;;; EMMS
;;;

(with-eval-after-load 'emms-playlist-mode
  (require 'emms-setup)

  (with-eval-after-load 'emms-setup
    (emms-all)
    (emms-default-players)
    (setq emms-player-mpv-parameters
	  (append '("--no-terminal" "--force-window=no" "--audio-display=no"
                    "--no-resume-playback" "--keep-open=no"
                    "--audio-device=pulse/alsa_output.usb\
-Logitech_Logitech_USB_Headset-00.analog-stereo"
                    "--title=\"emacs-emms\"")
		  emms-player-mpv-parameters))

    (setq emms-volume-change-function #'emms-volume-pulse-change)
    (setq emms-player-next-function 'emms-next-noerror)
    (emms-mode-line -1)
    (setq emms-playing-time-display-p nil)
    (setq emms-playlist-mode-center-when-go t))

  (defmacro define-emms-advice-after (procedure after-procedure)
    `(progn
       (defun ,(intern (concat (symbol-name procedure)
                               "-"
                               (mapconcat 'identity
                                          (cdr (split-string (symbol-name after-procedure)
                                                             "-"))
                                          "-")))
           nil
         ,(format "Evaluate `%S' and call `%S'." procedure after-procedure)
         (interactive)
         (,procedure)
         (,after-procedure))))

  (define-emms-advice-after emms-next emms-playlist-mode-center-current)
  (define-emms-advice-after emms-previous emms-playlist-mode-center-current)
  (define-emms-advice-after emms-random emms-playlist-mode-center-current)
  (define-emms-advice-after emms-next emms-show)
  (define-emms-advice-after emms-previous emms-show)
  (define-emms-advice-after emms-random emms-show)

  (dolist (map (list emms-playlist-mode-map))
    (define-key map (kbd "n") 'emms-next-playlist-mode-center-current)
    (define-key map (kbd "p") 'emms-previous-playlist-mode-center-current)
    (define-key map (kbd "r") 'emms-random-playlist-mode-center-current)
    (define-key map (kbd "<backspace>") 'emms-player-simple-mpv-speed-normal)
    (define-key map (kbd "T") 'emms-player-simple-mpv-ontop)
    (define-key map (kbd "F") 'emms-player-simple-mpv-fullscreen)
    (define-key map (kbd "9") 'emms-volume-lower)
    (define-key map (kbd "0") 'emms-volume-raise))

  (with-eval-after-load 'emms-setup
    (setq emms-track-description-function (lambda (v) (assoc-default 'name v)))
    (setq emms-source-file-default-directory "/srv/music"))

  (with-eval-after-load 'helm-emms
    (require 'emms-setup)
    (setq helm-emms-use-track-description-function t)
    (add-to-list 'helm-emms-music-extensions "mkv")
    (add-to-list 'helm-emms-music-extensions "webm")))
