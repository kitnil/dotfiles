(define-public arch-packages
  (search-engine
   (name "arch-packages")
   (uri "https://www.archlinux.org/packages/?sort=&q=%s")))

(define-public cpan
  (search-engine
   (name "cpan")
   (uri "http://search.cpan.org/search?query=%s&mode=all")))

(define-public cve
  (search-engine
   (name "cve")
   (uri "https://cve.mitre.org/cgi-bin/cvekey.cgi?keyword=%s")))

(define-public debian-files
  (search-engine
   (name "debian-files")
   (uri "https://packages.debian.org/search?searchon=contents&keywords=%s")))

(define-public debcodesearch
  (search-engine
   (name "debcodesearch")
   (uri "https://codesearch.debian.net/search?q=%s")))

(define-public duckduckgo
  (search-engine
   (name "duckduckgo")
   (uri "https://duckduckgo.com/?q=%s")))

(define-public explainshell
  (search-engine
   (name "explainshell")
   (uri "https://www.explainshell.com/explain?cmd=%s")))

(define-public fdroid
  (search-engine
   (name "fdroid")
   (uri "https://f-droid.org/packages/#q=%s")))

(define-public fedora-cgit
  (search-engine
   (name "fedora-cgit")
   (uri "https://fedorapeople.org/cgit/?q=%s")))

(define-public github
  (search-engine
   (name "github")
   (uri "https://github.com/search?ref=simplesearch&q=%s")))

(define-public github-gpl
  (search-engine
   (name "github-gpl")
   (uri (string-append "https://github.com/search?ref=simplesearch&q=%s"
                       "+license%3Agpl"))))

(define-public github-hippie
  (search-engine
   (name "github-hippie")
   (uri (string-append "https://github.com/search?ref=simplesearch&q=%s"
                       "+-language:objectivec"
                       "+-language:java"
                       "+-language:javascript"
                       "+-language:csharp"
                       "+-language:kotlin"
                       "+-language:swift"
                       "+-language:php"
                       "+-language:vue"
                       "+-language:autohotkey"))))

(define-public github-hippie-gpl
  (search-engine
   (name "github-hippie-gpl")
   (uri (string-append "https://github.com/search?ref=simplesearch&q=%s"
                       "+-language:objectivec"
                       "+-language:java"
                       "+-language:javascript"
                       "+-language:csharp"
                       "+-language:kotlin"
                       "+-language:swift"
                       "+-language:php"
                       "+-language:vue"
                       "+-language:autohotkey"
                       "+license%3Agpl"))))

(define-public google
  (search-engine
   (name "google")
   (uri "https://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")))

(define-public google-instant
  (search-engine
   (name "google-instant")
   (uri "https://www.google.com/webhp?#q=%s&btnI=I")))

(define-public google-door-music
  ;; https://github.com/gotbletu/dotfiles/blob/66b2ce9744564a48717c97163a5c34ad1b56d50e/surfraw/.config/surfraw/elvi/opendir_music
  (search-engine
   (name "google-door-music")
   (uri (string-append "https://www.google.com/search?q=%s"
                       "%20%2B(.ogg|.mp3|.wav|.ac3|.flac|.wma|.m4a)"
                       "%20%2Bintitle:%22index%20of%22%20"
                       "-inurl:(jsp|pl|php|html|aspx|htm|cf|shtml)%20"
                       "-inurl:(listen77|mp3raid|mp3toss|mp3drug|index_of|wallywashis)"))))

(define-public google-video
  (search-engine
   (name "google-video")
   (uri "https://www.google.com/search?q=%s&tbm=vid")))

(define-public guix-hydra
  (search-engine
   (name "guix-hydra")
   (uri "https://hydra.gnu.org/search?query=%s")))

(define-public guix-hydra-job
  ;; e.g. gource-0.47
  (search-engine
   (name "guix-hydra-job")
   (uri "https://hydra.gnu.org/job/gnu/master/%s")))

(define-public nixos-hydra
  (search-engine
   (name "nixos-hydra")
   (uri "https://hydra.nixos.org/search?query=%s")))

(define-public nixos-hydra-job
  (search-engine
   (name "nixos-hydra-job")
   (uri "https://hydra.nixos.org/job/gnu/master/%s.x86_64-linux")))

(defmacro wi-define-public-ml-gnu (idxname &optional message-id)
  `(define-public ,(if message-id
                   (intern (string-append (symbol-name idxname) "-message-id"))
                 idxname)
     (string-append "https://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
             (if ,message-id "" "%s")
             "&submit=Search%21"
             (if ,message-id "%2Bmessage-id%3A%s" "")
             "&idxname=" ,(symbol-name idxname)
             "&max=20"
             "&result=normal"
             "&sort=score")))

(define-public listinfo-gnu
  (search-engine
   (name "gnu-listinfo")
   (uri "https://lists.gnu.org/mailman/listinfo/%s")))

(wi-define-public-ml-gnu info-gnus-english)
(wi-define-public-ml-gnu emacs-devel t)
(wi-define-public-ml-gnu emacs-devel)
(wi-define-public-ml-gnu emacs-orgmode t)
(wi-define-public-ml-gnu emacs-orgmode)
(wi-define-public-ml-gnu guix-devel t)
(wi-define-public-ml-gnu guix-devel)
(wi-define-public-ml-gnu guix-help t)
(wi-define-public-ml-gnu guix-help)
(wi-define-public-ml-gnu help-gnu-emacs t)
(wi-define-public-ml-gnu help-gnu-emacs)
(wi-define-public-ml-gnu info-gnus-english-message-id)

(define-public guix-help+devel
  (search-engine
   (name "guix-help+devel")
   (uri (string-append "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
                       "?query=%s"
                       "&submit=Search%21"
                       "&idxname=guix-devel"
                       "&idxname=help-guix"
                       "&max=20"
                       "&result=normal"
                       "&sort=score"))))

(define-public guix-all
  (search-engine
   (name "guix-all")
   (uri (string-append "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
                       "?query=%s"
                       "&submit=Search%21"
                       "&idxname=bug-guix"
                       "&idxname=guix-patches"
                       "&idxname=guix-devel"
                       "&idxname=help-guix"
                       "&max=20"
                       "&result=normal"
                       "&sort=score"))))

(define-public guix-all-date
  (search-engine
   (name "guix-all-date")
   (uri (string-append "https://lists.gnu.org/archive/cgi-bin/namazu.cgi"
                       "?query=%s"
                       "&submit=Search%21"
                       "&idxname=guix-devel"
                       "&idxname=help-guix"
                       "&idxname=bug-guix"
                       "&idxname=guix-patches"
                       "&max=20"
                       "&result=normal"
                       "&sort=date%3Alate"))))

(define-public mankier
  (search-engine
   (name "mankier")
   (uri "https://www.mankier.com/?q=%s")))

(define-public melpa
  (search-engine
   (name "melpa")
   (uri "https://melpa.org/#/?q=%s")))

(define-public openhub
  (search-engine
   (name "openhub")
   (uri "https://www.openhub.net/p?ref=homepage&query=%s")))

(define-public reddit-unixporn
  (search-engine
   (name "reddit-unixporn")
   (uri "https://www.reddit.com/r/unixporn/search?q=%s&restrict_sr=on")))

(define-public rfcs
  (search-engine
   (name "rfcs")
   (uri "http://pretty-rfc.herokuapp.com/search?q=%s")))

(define-public searx
  (search-engine
   (name "searx")
   (uri "http://searx.tk/?q=%s")))

(define-public stack-overflow
  (search-engine
   (name "stack-overflow")
   (uri "https://stackoverflow.com/search?q=%s")))

(define-public startpage
  (search-engine
   (name "startpage")
   (uri "https://www.startpage.com/do/search?query=%s")))

(define-public startpage-hippie
  (search-engine
   (name "startpage-hippie")
   (uri (string-append "https://www.startpage.com/do/dsearch?query=%s"
                       "+c"
                       "+-c%2B%2B"
                       "+-c%23&cat=web"
                       "&pl=opensearch"
                       "&language=english"))))

(define-public tldr
  (search-engine
   (name "tldr")
   (uri "https://tldr.ostera.io/%s")))

(define-public wikipedia
  (search-engine
   (name "wikipedia")
   (uri "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")))

(define-public wiktionary
  (search-engine
   (name "wiktionary")
   (uri (string-append "https://www.wikipedia.org/search-redirect.php?family=wiktionary"
                       "&language=en" "&go=Go" "&search=%s"))))

(define-public metal-archives
  (search-engine
   (name "metal-archives")
   (uri "https://www.metal-archives.com/search?searchString=%s&type=band_name")))

(define-public libgen
  (search-engine
   (name "libgen")
   (uri (string-append "http://libgen.io/search.php?req=%s&"
                       "lg_topic=libgen&"
                       "open=0&"
                       "view=simple&"
                       "res=25&"
                       "phrase=1&"
                       "column=def"))))

(define-public youtube
  (search-engine
   (name "youtube")
   (uri "https://www.youtube.com/results?aq=f&oq=&search_query=%s")))

(define-public youtube-latest
  (search-engine
   (name "youtube-latest")
   (uri "https://www.youtube.com/results?sp=CAJQFA%253D%253D&search_query=%s")))

(define-public youtube-live
  (search-engine
   (name "youtube-live")
   (uri "https://www.youtube.com/results?sp=EgJAAQ%253D%253D&search_query=%s")))

(define-public youtube-rss-channel
  (search-engine
   (name "youtube-rss-channel")
   (uri "https://www.youtube.com/feeds/videos.xml?channel_id=%s")))

(define-public youtube-rss-user
  (search-engine
   (name "youtube-rss-user")
   (uri "https://www.youtube.com/feeds/videos.xml?user=%s")))

(define-public webarchive
  (search-engine
   (name "webarchive")
   (uri "https://web.archive.org/web/*/%s")))

