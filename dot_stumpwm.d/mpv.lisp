(in-package :stumpwm)

(defvar *mpv-program* "mpv"
 "The name by which to invoke MPV.")

(defparameter *mpv-headphones*
  nil
  "If non-nil use heaphones in MPV.")

(defvar *mpv-default-arguments*
  '("--keep-open=no"))

(defparameter *mpv-arguments*
  *mpv-default-arguments*)

(defcommand toggle-mpv-arguments () ()
  (if *mpv-headphones*
      (progn
        (setf *mpv-arguments* *mpv-default-arguments*)
        (setf *mpv-headphones* nil))
      (progn
        (setf *mpv-arguments*
              `(,@*mpv-default-arguments*
                ,(concat "--audio-device=" *headphones*)))
        (setf *mpv-headphones* t))))

(defcommand mpv () ()
  "Start or focus mpv."
  (run-or-raise (join `(,*mpv-program* ,@*mpv-arguments*))
                '(:class "mpv")))

(defcommand xclip-mpv () ()
  "Play video from clipboard with mpv."
  (let ((clipboard (get-x-selection)))
    (run-shell-command
     (join `(,*mpv-program* ,@*mpv-arguments* ,clipboard)))
    (message (concat "Play " clipboard))))

(defcommand mpv-watch () ()
  "Play video from file with mpv."
  (run-shell-command
   (join `(,*mpv-program* ,@*mpv-arguments* ,(concat "$(cat " (getenv "HOME") "/watch)")))))

(defcommand music-mpv () ()
  "Play music in MPV."
  (let ((window (current-window)))
    (if (and window (string= (window-title window) "mpv-music"))
        (other-in-frame-or-fother)
        (run-or-raise
         (join (list "mpv" "--keep-open=no" "--msg-level=all=no"
                     "--no-resume-playback" "--shuffle" "--title=mpv-music"
                     (concat "--input-unix-socket=" (getenv "HOME") "/.mpv/socket")
                     "/srv/music/*"))
         '(:title "mpv-music")))))

(defcommand mpv-next () ()
  (run-shell-command "mpvctl next"))

(defcommand mpv-previous () ()
  (run-shell-command "mpvctl previous"))

(defcommand mpv-music () ()
  (run-shell-command "mpv https://www.youtube.com/playlist?list=PLmjgicsUWIkvEKkLN01vm85neXAik3yU2"))
