(defcustom wi-helm-wigust-stream-urls
  '("https://www.twitch.tv/nekrovim"
    "https://www.twitch.tv/entr_ru"
    "https://www.youtube.com/user/streamguild"
    "https://www.twitch.tv/artgameslp"
    "https://www.youtube.com/user/ArtGamesLP")
  "List of URLs passed to `wi-helm-wigust-stream'.")

(defun wi-helm-wigust-stream (func)
  "Open a streaming video URL in Chromium or Streamlink with Helm."
  (interactive (list
                (let ((engine (completing-read "Engine (chromium by default): "
                                               '(chromium streamlink))))
                  (if (string-empty-p engine)
                      "chromium"
                    engine))))
  (helm :sources (helm-build-sync-source "urls"
                   :action (lambda (candidate)
                             (funcall (cond ((string-equal func "chromium")
                                             'browse-url-chromium)
                                            ((string-equal func "streamlink")
                                             'browse-url-streamlink))
                                      candidate))
                   :candidates wi-helm-wigust-stream-urls
                   :fuzzy-match t)
        :buffer "*helm urls*"))

(defun erc-twitch ()
  "Open a current buffer with `browse-url-streamlink'"
  (interactive)
  (let ((url (concat "https://twitch.tv/"
                     (car (last (split-string (buffer-name (current-buffer))
                                              "#"))))))
    (message (concat "Open stream: " url))
    (browse-url-streamlink url)))
