(setq youtube-stream-open-chat-function
      #'browse-url-chromium-no-toolbar)

(setq youtube-stream-open-video-function #'browse-url-streamlink)

(defun ytel-watch ()
  "Stream video at point in mpv."
  (interactive)
  (let* ((video (ytel-get-current-video))
         (id    (ytel-video-id video)))
    (start-process "ytel mpv" nil
                   "mpv"
                   (concat "https://www.youtube.com/watch?v=" id)))
  (message "Starting streaming..."))

(with-eval-after-load 'ytel
  (let ((map ytel-mode-map))
    (define-key map (kbd "<return>") 'ytel-watch)))
