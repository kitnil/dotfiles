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

(defvar *mpv-youtube-playlist*
  "https://www.youtube.com/playlist?list=PLmjgicsUWIkvEKkLN01vm85neXAik3yU2")

(defvar *mpv-music-directory*
  "/srv/music/*")

(defun music-mpv-command (&key (remote nil))
  "Play music in MPV."
  (let ((window (current-window)))
    (if (and window (string= (window-title window) "mpv-music"))
        (other-in-frame-or-fother)
        (run-or-raise
         (join `(,@(if remote
                        '("notify-send 'Run MPV YouTube Music'")
                        '())
                  ,(join (list "mpv" "--keep-open=no" "--msg-level=all=no"
                               "--no-resume-playback" "--shuffle"
                               "--title=mpv-music" (if remote
                                                       *mpv-youtube-playlist*
                                                       *mpv-music-directory*))))
               #\;)
         '(:title "mpv-music")))))

(defcommand music-mpv () ()
  (music-mpv-command))

(defcommand music-mpv-youtube () ()
  (music-mpv-command :remote t))
