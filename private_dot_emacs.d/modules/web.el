(setq w3m-fill-column 80)


;;;
;;; EWW
;;;

(setq shr-width 80)
(setq-default shr-use-fonts nil)
(setq shr-external-browser 'browse-url-firefox)

;; Not white background in dark themes.
;; Origin <https://emacs.stackexchange.com/a/3523>
(setq shr-color-visible-luminance-min 100)
(advice-add #'shr-colorize-region
            :around (defun shr-no-colourise-region (&rest ignore)))


;;;
;;; Search engines
;;;

(with-eval-after-load 'engine-mode
  (setq engine/keybinding-prefix "C-c k")
  (engine/set-keymap-prefix (kbd engine/keybinding-prefix))
  (engine-mode))

(defengine arch-packages
  "https://www.archlinux.org/packages/?sort=&q=%s")

(defengine cpan
  "http://search.cpan.org/search?query=%s&mode=all")

(defengine cve
  "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=%s")

(defengine debfiles
  "https://packages.debian.org/search?searchon=contents&keywords=%s")

(defengine debcodesearch
  "https://codesearch.debian.net/search?q=%s")

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine explainshell
  "https://www.explainshell.com/explain?cmd=%s")

(defengine debfiles
  "https://packages.debian.org/search?searchon=contents&keywords=%s")

(defengine fdroid
  "https://f-droid.org/packages/#q=%s")

(defengine fedora-cgit
  "https://fedorapeople.org/cgit/?q=%s")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "h")

(defengine github-gpl
  (concat "https://github.com/search?ref=simplesearch&q=%s"
          "+license%%3Agpl"))

(defengine github-hippie
  (mapconcat 'identity
             '("https://github.com/search?ref=simplesearch&q=%s"
               "objectivec" "java" "javascript" "csharp" "kotlin"
               "swift" "php" "vue" "autohotkey")
             "+-language:"))

(defengine github-hippie-gpl
  (concat (mapconcat 'identity
                     '("https://github.com/search?ref=simplesearch&q=%s"
                       "objectivec" "java" "javascript" "csharp"
                       "kotlin" "swift" "php" "vue" "autohotkey")
                     "+-language:")
          "+license%%3Agpl"))

(defengine google
  "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")

(defengine google-instant
  "https://www.google.com/webhp?#q=%s&btnI=I")

(defengine google-door-music
  ;; https://github.com/gotbletu/dotfiles/blob/66b2ce9744564a48717c97163a5c34ad1b56d50e/surfraw/.config/surfraw/elvi/opendir_music
  (concat "https://www.google.com/search?q=%s"
          "%%20%%2B(.ogg|.mp3|.wav|.ac3|.flac|.wma|.m4a)"
          "%%20%%2Bintitle:%%22index%%20of%%22%%20"
          "-inurl:(jsp|pl|php|html|aspx|htm|cf|shtml)%%20"
          "-inurl:(listen77|mp3raid|mp3toss|mp3drug|index_of|wallywashis)"))

(defengine google-video
  "https://www.google.com/search?q=%s&tbm=vid")

(defengine guix-hydra
  "https://hydra.gnu.org/search?query=%s"
  :keybinding "c")

(defengine guix-hydra-job
  ;; e.g. gource-0.47
  "https://hydra.gnu.org/job/gnu/master/%s")

(defengine nixos-hydra
  "https://hydra.nixos.org/search?query=%s")

(defengine nixos-hydra-job
  "https://hydra.nixos.org/job/gnu/master/%s.x86_64-linux")

(defmacro wi-defengine-ml-gnu (idxname &optional message-id)
  `(defengine ,(if message-id
                   (intern (concat (symbol-name idxname) "-message-id"))
                 idxname)
     (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
             (if ,message-id "" "%s")
             "&submit=Search%%21"
             (if ,message-id "%%2Bmessage-id%%3A%s" "")
             "&idxname=" ,(symbol-name idxname)
             "&max=20"
             "&result=normal"
             "&sort=score")))

(defengine listinfo-gnu "https://lists.gnu.org/mailman/listinfo/%s")

(wi-defengine-ml-gnu info-gnus-english)
(wi-defengine-ml-gnu emacs-devel t)
(wi-defengine-ml-gnu emacs-devel)
(wi-defengine-ml-gnu emacs-orgmode t)
(wi-defengine-ml-gnu emacs-orgmode)
(wi-defengine-ml-gnu guix-devel t)
(wi-defengine-ml-gnu guix-devel)
(wi-defengine-ml-gnu guix-help t)
(wi-defengine-ml-gnu guix-help)
(wi-defengine-ml-gnu help-gnu-emacs t)
(wi-defengine-ml-gnu help-gnu-emacs)
(wi-defengine-ml-gnu info-gnus-english-message-id)

(defengine guix-help+devel
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-all
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=bug-guix"
          "&idxname=guix-patches"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&max=20"
          "&result=normal"
          "&sort=score"))

(defengine guix-all-date
  (concat "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
          "?query=%s"
          "&submit=Search%%21"
          "&idxname=guix-devel"
          "&idxname=help-guix"
          "&idxname=bug-guix"
          "&idxname=guix-patches"
          "&max=20"
          "&result=normal"
          "&sort=date%%3Alate")
    :keybinding "g")

(defengine mankier
  "https://www.mankier.com/?q=%s")

(defengine melpa
  "https://melpa.org/#/?q=%s"
  :keybinding "m")

(defengine openhub
  "https://www.openhub.net/p?ref=homepage&query=%s")

(defengine reddit-unixporn
  "https://www.reddit.com/r/unixporn/search?q=%s&restrict_sr=on")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(defengine searx
  "http://searx.tk/?q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")

(defengine startpage
  "https://www.startpage.com/do/search?query=%s"
  :keybinding "s")

(defengine startpage-hippie
  (concat "https://www.startpage.com/do/dsearch?query=%s"
          "+c"
          "+-c%%2B%%2B"
          "+-c%%23&cat=web"
          "&pl=opensearch"
          "&language=english"))

(defengine tldr
  "https://tldr.ostera.io/%s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")

(defengine wiktionary
  (concat "https://www.wikipedia.org/search-redirect.php?family=wiktionary"
          "&language=en" "&go=Go" "&search=%s"))

(defengine metal-archives
  "https://www.metal-archives.com/search?searchString=%s&type=band_name")

(defengine libgen
  (concat "http://libgen.io/search.php?req=%s&"
          "lg_topic=libgen&"
          "open=0&"
          "view=simple&"
          "res=25&"
          "phrase=1&"
          "column=def"))

(defengine youtube
  "https://www.youtube.com/results?aq=f&oq=&search_query=%s")

(defengine youtube-latest
  "https://www.youtube.com/results?sp=CAJQFA%%253D%%253D&search_query=%s")

(defengine youtube-live
  "https://www.youtube.com/results?sp=EgJAAQ%%253D%%253D&search_query=%s")

(defengine youtube-rss-channel
  "https://www.youtube.com/feeds/videos.xml?channel_id=%s")

(defengine youtube-rss-user
  "https://www.youtube.com/feeds/videos.xml?user=%s")

(defengine webarchive
  "https://web.archive.org/web/*/%s")


;;;
;;; browse-url
;;;

(defvar browse-url-streamlink-program "streamlink")

(defvar browse-url-streamlink-arguments '("-p" "mpv"))

(defvar browse-url-streamlink-quality "best")

(defun browse-url-streamlink (url &optional new-window)
  "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-streamlink-arguments' to mpv."
  (interactive (browse-url-interactive-arg "URL: "))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "streamlink " url)
           nil
           browse-url-streamlink-program
           `(,@browse-url-streamlink-arguments
             ,url
             ,browse-url-streamlink-quality))))

(defcustom browse-url-mpv-program "mpv"
  "The name by which to invoke MPV."
  :type 'string
  :group 'browse-url)

(defcustom browse-url-mpv-arguments '("--volume=50")
  "Arguments passed to mpv with `browse-url-mpv'."
  :type 'list
  :group 'browse-url)

(defcustom browse-url-mpv-headphones t
  "Non-nil if browse-url-mpv in headphones."
  :type 'boolean
  :group 'browse-url)

(defun toggle-browse-url-mpv-arguments ()
  "If browse-url-mpv-headphones non-nil set it to t and set
`browse-url-mpv-arguments' headphones."
  (interactive)
  (if browse-url-mpv-headphones
      (progn (setq browse-url-mpv-arguments '("--volume=50"))
             (setq browse-url-mpv-headphones nil))
    (setq browse-url-mpv-arguments
                (list "--volume=50" "--no-resume-playback"
                      "--keep-open=no"
                      (concat "--audio-device=" ‎wi-headphones)))
    (setq browse-url-mpv-headphones t))
  (message "MPV for headphones is %s"
	   (if browse-url-mpv-headphones "enabled" "disabled")))

(setq browse-url-mpv-remote-program "~/bin/mpv-remote")

(defun browse-url-mpv (url &optional new-window)
  "Ask the mpv video player to load URL.
Defaults to the URL around or before point.  Passes the strings
in the variable `browse-url-mpv-arguments' to mpv."
  (interactive (flet ((browse-url-url-at-point ; do not add `http://' prefix
                          () (or (thing-at-point 'url t)
                                 (let ((f (thing-at-point 'filename t)))
                                   f))))
                 (browse-url-interactive-arg "URL: ")))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "mpv " url) nil
           browse-url-mpv-program
           (append
            browse-url-mpv-arguments
            (list url)))))

(defun browse-url-chromium-no-toolbar (url &optional _new-window)
  "Ask the Chromium WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `browse-url-chromium-arguments' are also passed to
Chromium.
The optional argument NEW-WINDOW is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply 'start-process
	   (concat "chromium " url) nil
	   browse-url-chromium-program
	   (append
	    browse-url-chromium-arguments
	    (list (concat "--app=" url))))))

(defvar wi-debian-paste-regexp
  (rx-to-string
   `(and "http" (* "s") "://paste.debian.net/" (+ alnum) (* "/")) t)
  "Regexp matching Debian paste URL.")

(defvar wi-url-gnu-lists-regexp
  (rx-to-string
   `(and "http" (* "s") "://lists.gnu.org" (* alnum)) t)
  "Regexp matching GNU mailing lists URL.")

(defun wi-debian-paste-raw (str)
  "Return a raw URL from original STR."
  (funcall (-lambda ((protocol s domain nth s))
             (mapconcat 'identity
                        (list protocol s domain "plain" nth s)
                        "/"))
           (split-string str "/")))

(defun wi-browse-url-paste-debian (url &optional new-window)
  "Download a snippet from paste.debian.net URL and open it in a buffer.
If NEW-WINDOW is non-nil, then whenever a document would
otherwise be loaded in a new window"
  (wi-wget-switch (wi-debian-paste-raw url)))

(defvar wi-lwn-regexp
  (rx-to-string
   `(and "http" (* "s") "://lwn.net/Articles/"
         (+ alnum) (* "/") (* "rss")) t)
  "Regexp matching LWN GNU/Linux news site.")

(defvar wi-url-hydra-regexp
  (rx-to-string
   `(and "http" (* "s") "://hydra.gnu.org" (* "/")) t)
  "Regexp matching GNU Hydra CI.")

(defvar wi-url-gnunet-bot-log-regexp
  (rx "http" (zero-or-one "s") "://gnunet.org/bot/log/"
      (one-or-more alphabetic) (zero-or-one "/") line-end)
  "Regexp matching GNU Hydra CI.")

(defvar wi-twitch-url-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "twitch.tv")
  "Regexp matching Twitch.")

(defvar wi-twitch-video-url-regexp
  (concat wi-twitch-url-regexp
          (rx "/videos/" (one-or-more digit) line-end))
  "Regexp matching Twitch videos web-page.")

(defvar youtube-url-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "youtube.com")
  "Regexp matching YouTube.")

(defvar youtube-url-video-regexp
  (concat youtube-url-regexp
          (rx "/watch?v="
              (one-or-more (or alphanumeric "-" "_"))
              line-end))
  "Regexp matching YouTube videos web-page.")

(defvar youtube-short-url-video-regexp
  (rx "http" (zero-or-more "s") "://" (zero-or-more "www.")
      "youtu.be/" (one-or-more (or alphanumeric "-" "_")) line-end)
  "Regexp matching YouTube short URL.")

(defvar wi-url-github-regexp
  (rx "http" (zero-or-one "s") "://github.com")
  "Regexp matching GitHub.")

(defvar wi-url-melpa-regexp
  (rx "http" (zero-or-one "s") "://melpa.org")
  "Regexp matching Melpa.")

(defun youtube-free-url (url)
  "Convert youtube.com to hooktube.com URL and put into `kill-ring'.

WARNING:  hooktube.com requries non-free JavaScript."
  (interactive
   (let ((clipboard (x-get-clipboard)))
     (list
      (if (string-match-p youtube-url-video-regexp
                          clipboard)
          clipboard
        (read-string "YouTube video URL: ")))))
  (kill-new (concat "https://hooktube.com/watch?v="
                    (car (last (split-string (car (last (split-string url
                                                                      "/")))
                                             "="))))))

(cl-defmacro wi-url-savannah-git-commit-regexp (repository &optional (news))
  `(rx "http" (zero-or-one "s") "://git.savannah.gnu.org/"
       (zero-or-one "c") ,(format "git/%s.git/commit/" repository)
       ,(if news "etc/NEWS" "") "?id="
       (zero-or-more alphanumeric)
       line-end))

(defvar wi-url-emacs-git-commit-regexp
  (wi-url-savannah-git-commit-regexp "emacs" (list :news t)))

(defvar wi-url-guix-git-commit-regexp
  (wi-url-savannah-git-commit-regexp "guix"))

(setq browse-url-firefox-program "firefox")

(setq browse-url-browser-function
      `(("^ftp://.*" . browse-ftp-tramp)
        (,(format "^%s\\(%s\\)?\\([[:digit:]]+\\)$"
	  "https?://\\(debbugs\\|bugs\\)\\.gnu\\.org/"
	  (regexp-quote "cgi/bugreport.cgi?bug="))
         . debbugs-browse-url)
        (,youtube-url-video-regexp . browse-url-mpv)
        (,youtube-short-url-video-regexp . browse-url-mpv)
        (,wi-twitch-video-url-regexp . browse-url-mpv)
        (,wi-twitch-url-regexp . browse-url-streamlink)
        (,wi-url-hydra-regexp . browse-url-firefox)
        (,wi-lwn-regexp . eww-browse-url)
        (,wi-url-gnu-lists-regexp . eww-browse-url)
        (,wi-url-gnunet-bot-log-regexp . eww-browse-url)
        (,wi-debian-paste-regexp . wi-browse-url-paste-debian)
        (,wi-url-github-regexp . browse-url-firefox)
        (,wi-url-melpa-regexp . browse-url-firefox)
        (,wi-url-emacs-git-commit-regexp . browse-url-emacs-git-commit)
        (,wi-url-guix-git-commit-regexp . browse-url-guix-git-commit)
        ("." . browse-url-firefox)))

(defun wi-info-remote-copy-current-node ()
  "Copy URL to current Info node."
  (interactive)
  (kill-new
   (concat "https://www.gnu.org/software/"
           (file-name-sans-extension
            (file-name-nondirectory Info-current-file))
           "/manual/html_node/"
           (let ((split-str (split-string Info-current-node " ")))
             (if (> (length split-str) 1)
                 (mapconcat 'identity split-str "-")
               Info-current-node))
           ".html")))


;;;
;;; GitHub
;;;

(defvar wi-github-url-regexp
  (rx "http" (zero-or-one "s") "://github.com"))

(defvar wi-github-user-url-regexp
  (concat wi-github-url-regexp
          (rx "/" letter (one-or-more alphanumeric))))

(defvar wi-github-user-repo-url-regexp
  (concat wi-github-user-url-regexp
          (rx "/" (one-or-more (or alphanumeric "-" ".")))))

(defvar wi-github-user-repo-commit-url-regexp
  (concat wi-github-user-repo-url-regexp
          (rx "/commit" "/" (one-or-more alphanumeric))))

(defun wi-clipboard-github-url-to-commit (url)
  "Return in kill ring a commit hash from GitHub user's repository
commit URL.

https://github.com/USER/REPO/commit/SHA1-HASH => SHA1-HASH"
  (interactive
   (let ((clipboard (x-get-clipboard)))
     (list
      (if (string-match-p wi-github-user-repo-commit-url-regexp
                          clipboard)
          clipboard
        (read-string "Github user's repository commit URL: ")))))
  (kill-new (car (last (split-string url "/")))))


;;;
;;; Wget
;;;

(defun wi-wget-switch (url)
  "Download a file with wget and open it in buffer"
  (interactive "sDownload URL: ")
  (let ((buffer (generate-new-buffer "*wget*")))
    (with-current-buffer buffer
      (insert (shell-command-to-string
               (mapconcat 'identity (list "wget" "-q" "-O-" url)
                          " ")))
      (special-mode))
    (switch-to-buffer buffer)))

(defun wi-wget-switch-gunzip (url)
  "Download a file with wget and open it in buffer"
  (interactive "sDownload URL: ")
  (let ((buffer (generate-new-buffer "*wget*"))
        (command (mapconcat 'identity
                            (list "wget" "-q" "-O-" url " | " "gunzip")
                            " ")))
    (message command)
    (with-current-buffer buffer
      (insert (shell-command-to-string command))
      (special-mode))
    (switch-to-buffer buffer)))


;;;
;;; Chromium
;;;

(with-eval-after-load 'atomic-chrome
  (let ((map atomic-chrome-edit-mode-map))
    ;; (define-key map (kbd "C-c '") 'anywhere-exit)
    ;; (define-key map (kbd "C-c i") 'ispell-buffer)
    (define-key map (kbd "C-c v") 'ivy-yasnippet)))

