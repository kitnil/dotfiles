(serialize-shell-commands
 (shell-commands
  (commands
   (list
    (shell-command (command-name 'trans)
                   (documentation "Run translate-shell.")
                   (command '("trans" "-I" "en:ru"))
                   (terminal-name "translate-shell")
                   (key "a t"))
    (shell-command (command-name 'htop)
                   (documentation "Run htop.")
                   (command '("htop"))
                   (terminal-name "htop"))
    (shell-command (command-name 'youtube-dl-music)
                   (documentation "Download a URI with youtube-dl to music directory")
                   (command (list "youtube-dl" "--output"
                                  (single-quote-string "/srv/music/%(title)s.%(ext)s")))
                   (font-size 8)
                   (clipboard? #t)
                   (color "dark"))))))

;; (serialize-select-window-by-number (stream->list (stream-range 0 9)))

