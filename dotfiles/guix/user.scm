(use-modules (gnu)
             (guix records))

(use-package-modules emacs)

(define (spec->packages spec)
  (call-with-values (lambda () (specification->package+output spec)) list))



;;;
;;; Emacs
;;;

(define %emacs-base-spec-list
  (list "emacs" "emacs-async" "emacs-use-package"))

(define %emacs-rainbow-spec-list
  (list "emacs-rainbow-mode"
        "emacs-rainbow-delimiters" "emacs-rainbow-identifiers"))

(define %emacs-org-spec-list (list "emacs-org"))

(define %emacs-navigation-spec-list
  (list "emacs-beginend" "emacs-aggressive-indent" "emacs-multiple-cursors"))

(define %emacs-numbers-spec-list
  (list "emacs-shift-number" "emacs-seq"))

(define %emacs-yasnippet-spec-list
  (list "emacs-yasnippet" "emacs-yasnippet-snippets"))

(define %emacs-emms-spec-list
  (list "emacs-emms" "emacs-emms-player-mpv"))

(define %packaging-emacs-spec-list
  (list "emacs-nix-mode" "emacs-ebuild-mode" "emacs-rpm-spec-list-mode"))

(define %emacs-spec-list
  (append %emacs-base-spec-list %emacs-navigation-spec-list
          %emacs-yasnippet-spec-list %emacs-org %emacs-rainbow))

(define %games-rpg-spec-list (list "tome4"))
(define %games-spec-list (append '() %games-rpg-spec-list))

(define %tor (list "torsocks"))

(define %tex-base (list "texlive"))
(define %tex-emacs (list "emacs-auctex" "emacs-org-edit-latex"))
(define %tex (append %tex-emacs %tex-emacs))

(define %spelling (list "aspell" "aspell-dict-en" "aspell-dict-ru"))
(define %tranlaste (list "translate-shell"))

(define %cli-base (list "wget"))
(define %cli-desktop (list "desktop-file-utils"))
(define %cli (append %cli-base %cli-desktop))

(define %icons (list "adwaita-icon-theme"))


;;;
;;; Fonts
;;;

(define %fonts-base-spec-list (list "font-dejavu" "font-liberation"))
(define %fonts-programming-spec-list
  (list "font-adobe-source-han-sans" "font-awesome" "font-hack"))
(define %fonts-japanese (list "font-wqy-zenhei"))
(define %fonts-utils-spec-list (list "fontconfig"))
(define %fonts
  (append %fonts-base-spec-list %fonts-programming-spec-list
          %fonts-utils-spec-list))

(define %desktop (append %cli-desktop %icons))

(define %record-video (list "ffmpeg"))
(define %play-video (list "mpv"))

(define %image-utils-spec-list (list "imagemagick"))
(define %image-svg-spec-list (list "inkscape"))
(define %image-viewers-spec-list (list "feh"))
(define %image-spec-list
  (append %image-viewers-spec-list %image-utils-spec-list))

(define %gui-terminal
  (list "xterm"))

(define %xorg-utils
  (list "xlsfonts"
        "xmodmap" "xorg-server" "xprop" "xrdb" "xset" "xsetroot"))

(define %desktop-utils
  (list "dbus" ))

(define %cli-browsers-spec-list
  (list "lynx" "w3m"))

(define %gui-browsers-spec-list
  (list "icecat" "conkeror" "chromium"))

(define %web)

(define %android (list "adb"))

(define %autotools (list "autoconf" "autoconf-archive" "automake"))

(define %virtualization-spec-list
  (list "qemu" "virt-viewer"))

(define %misc
  (list
   "alsa-utils"
   "aria2"
   "audacity"
   "bc"
   "bind"
   "bundler"
   "bzip2"
   "cflow"
   "cloc"
   "clojure"
   "complexity"
   "cool-retro-term"
   "coq"
   "cowsay"


   "emacs-avy"
   "emacs-bash-completion"

   "emacs-benchmark-init"
   "emacs-browse-at-remote"
   "emacs-cider"
   "emacs-company"
   "emacs-company-lua"
   "emacs-company-quickhelp"
   "emacs-company-restclient"
   "emacs-company-tern"
   "emacs-csv-mode"
   "emacs-dashboard"
   "emacs-debbugs"
   "emacs-dired-hacks"
   "emacs-discover-my-major"
   "emacs-dumb-jump"

   "emacs-edit-indirect"
   "emacs-edit-server"
   "emacs-elfeed"
   "emacs-elisp-refs"
   "emacs-engine-mode"
   "emacs-erc-hl-nicks"
   "emacs-eval-in-repl"
   "emacs-expand-region"
   "emacs-flx"
   "emacs-flycheck"
   "emacs-ggtags"
   "emacs-git-auto-commit-mode"
   "emacs-git-gutter"
   "emacs-git-messenger"
   "emacs-gitpatch"
   "emacs-git-timemachine"
   "emacs-goto-chg"
   "emacs-helm-make"
   "emacs-helm-notmuch"
   "emacs-helm-youtube"
   "emacs-helpful"
   "emacs-highlight-defined"
   "emacs-highlight-numbers"
   "emacs-highlight-symbol"
   "emacs-htmlize"
   "emacs-ido-ubiquitous"
   "emacs-ido-vertical-mode"
   "emacs-indium"
   "emacs-irfc"
   "emacs-json-mode"
   "emacs-json-reformat"
   "emacs-json-snatcher"
   "emacs-lacarte"
   "emacs-list-utils"
   "emacs-loop"
   "emacs-lua-mode"
   "emacs-makey"
   "emacs-markdown-mode"
   "emacs-move-text"


   "emacs-nnreddit"
   "emacs-npm-mode"
   "emacs-org-pomodoro"
   "emacs-org-protocol-capture-html"
   "emacs-org-ref"
   "emacs-parent-mode"
   "emacs-pdf-tools"
   "emacs-pos-tip"
   "emacs-projectile"
   "emacs-request"
   "emacs-restclient"
   "emacs-rfcview"
   "emacs-rjsx-mode"

   "emacs-seq"

   "emacs-simple-httpd"
   "emacs-skewer-mode"
   "emacs-slime"
   "emacs-slime-company"
   "emacs-smartparens"
   "emacs-smex"
   "emacs-sourcemap"
   "emacs-sr-speedbar"
   "emacs-stickyfunc-enhance"
   "emacs-stumpwm-mode"
   "emacs-suggest"
   "emacs-sx"
   "emacs-tern"
   "emacs-transpose-frame"
   "emacs-tuareg"
   "emacs-undo-tree"
   "emacs-web-beautify"
   "emacs-web-mode"
   "emacs-websocket"
   "emacs-wget"
   "emacs-which-key"
   "emacs-wordgen"
   "emacs-writegood-mode"
   "emacs-writeroom"
   "ethtool"
   "evince"
   "fbida"
   "feh"
   "figlet"
   "file"
   "gcc-toolchain"
   "gcl"
   "gdb"
   "geiser"
   "gettext"
   "ghc"
   "ghc-pandoc"
   "ghostscript-with-x"
   "gimp"
   "git"
   "git-modes"
   "global"
   "gnu-c-manual"
   "gnunet"
   "gnunet-gtk"
   "gnupg"
   "gnuplot"
   "graphviz"
   "grub"
   "gs-fonts"
   "guile"
   "guile-colorized"
   "guile-git"
   "guile-ncurses"
   "guile-sdl2"
   "guile-ssh"
   "guile-syntax-highlight"
   "guix"
   "gwl"
   "haskell-mode"
   "haunt"
   "hdparm"
   "heimdall"
   "hello"
   "help2man"
   "hlint"
   "htop"
   "icedtea"
   "iperf"
   "isync"
   "json-c"
   "kodi"
   "kodi-cli"
   "lcov"
   "libgcrypt"
   "libgnome-keyring"
   "libreoffice"
   "libtool"
   "lm-sensors"
   "lua"
   "luajit"
   "m4"
   "magit"
   "mailutils"
   "make"
   "man-pages"
   "maxima"
   "mb2md"
   "mercurial"
   "mesa"
   "mlocate"
   "mps-youtube"
   "mpv"
   "mysql"
   "ncdu"
   "ncurses"
   "neofetch"
   "netcat"
   "nginx-documentation"
   "nmap"
   "node"
   "notmuch"
   "obs"
   "ocaml"
   "openssh"
   "openssl"
   "paperkey"
   "parallel"
   "parted"
   "password-store"
   "pavucontrol"
   "perf"
   "perl"
   "php"
   "pinentry"
   "pkg-config"
   "ponymix"
   "proof-general"
   "pulseaudio"
   "pulsemixer"
   "python"
   "python-dbus"
   "python-ghp-import"
   "python-glances"
   "python-internetarchive"
   "python-livereload"
   "python-ranger-fm"
   "python-starred"
   "rdiff-backup"
   "recutils"
   "redshift"
   "rsnapshot"
   "rsync"
   "ruby"
   "ruby-gitlab"
   "rxvt-unicode"
   "samba"
   "sane-backends"
   "sbcl"
   "sbcl-stumpwm-with-slynk"
   "screen"
   "scrot"
   "sdl2"
   "sdl2-image"
   "sdl2-mixer"
   "sdl2-ttf"
   "seahorse"
   "setxkbmap"
   "shared-mime-info"
   "shellcheck"
   "sicp"
   "simple-scan"
   "skribilo"
   "sqlite"
   "stb-0.0.1"
   "stow"
   "strace"
   "stress"
   "tcpdump"
   "texinfo"
   "tmux"
   "tree"
   "unzip"
   "vc-dwim"
   "vdirsyncer"
   "vera"
   "w3m"
   "wakelan"
   "wget"
   "wgetpaste"
   "wireshark"
   "xclip"
   "xdg-utils"
   "xdotool"
   "xev"
   "youtube-dl"
   "zip"))

(define %packages-spec-list
  (append %android %desktop %emacs %spelling %tranlaste %play-video))

(packages->manifest
 (map spec->packages %packages-spec-list))
