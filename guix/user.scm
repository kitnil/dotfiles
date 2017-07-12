(use-package-modules admin
		     android
		     aspell
		     autotools
		     backup
		     bittorrent
		     commencement
		     compression
		     conkeror
		     databases
		     dictionaries
		     emacs
		     fonts
		     gcc
		     gettext
		     gdb
		     ghostscript
		     gnu-doc
		     gnupg
		     gnuzilla
		     graphviz
		     guile
		     image-viewers
		     imagemagick
		     java
		     libreoffice
		     lisp
		     llvm
		     m4
		     mail
		     man
		     maths
		     ncdu
		     networking
		     ocaml
		     package-management
		     parallel
		     password-utils
		     perl
		     pkg-config
		     pulseaudio
		     qemu
		     qt
		     rsync
		     samba
		     scanner
		     scheme
		     shells
		     spice
		     ssh
		     tex
		     texinfo
		     tmux
		     version-control
		     video
		     web
		     wget
		     xdisorg
		     xorg)

(packages->manifest
 (list adb
       aria2
       aspell
       aspell-dict-en
       aspell-dict-ru
       autoconf
       automake
       bzip2
       conkeror
       emacs-auctex
       emacs-cider
       emacs-company
       emacs-debbugs
       emacs-dired-hacks
       emacs-expand-region
       emacs-git-gutter
       emacs-git-timemachine
       emacs-multiple-cursors
       emacs-org
       emacs-pdf-tools
       emacs-projectile
       emacs-rainbow-delimiters
       emacs-rainbow-identifiers
       emacs-rainbow-mode
       emacs-slime
       emacs-sx
       emacs-transpose-frame
       emacs-undo-tree
       emacs-use-package
       emacs-which-key
       emacs-writegood-mode
       emacs-yasnippet
       ethtool
       feh
       ffmpeg
       (list font-adobe-source-han-sans "jp")
       font-wqy-zenhei
       gdb
       ghostscript/x
       gnuplot
       git
       (list git "send-email")
       gnu-c-manual
       gnu-gettext
       gnu-make
       gnupg
       gs-fonts
       guile-2.2
       guile-ssh
       guix
       haunt
       help2man
       htop
       icecat
       icedtea
       iperf
       isync
       json-c
       libgcrypt
       libreoffice
       m4
       magit
       maxima
       mercurial
       mpv
       mps-youtube
       ncdu
       neofetch
       nmap
       notmuch
       ocaml
       openssh
       paperkey
       parallel
       paredit
       password-store
       pavucontrol
       perl
       pinentry
       pkg-config
       ponymix
       proof-general
       pulseaudio
       qemu
       racket
       redshift
       rsnapshot
       rsync
       rxvt-unicode
       samba
       setxkbmap
       sicp
       sqlite
       stow
       texinfo
       texlive
       tmux
       translate-shell
       tree
       vera
       wget
       xclip
       xdotool
       xev
       xmodmap
       xprop
       xrdb
       youtube-dl
       wakelan
       xlsfonts
       xset))
